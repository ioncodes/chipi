//! Exercise the packaged binary through real JSON-RPC frames, including its lifecycle.

use lsp_server::Message;
use serde_json::{json, Value};
use std::{
    io::BufReader,
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc,
    time::Duration,
};

struct Client {
    child: Child,
    input: ChildStdin,
    messages: mpsc::Receiver<Value>,
}

impl Client {
    fn new() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_chipi-lsp"))
            .arg("--stdio")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        let input = child.stdin.take().unwrap();
        let output = child.stdout.take().unwrap();
        let (tx, messages) = mpsc::channel();
        std::thread::spawn(move || {
            let mut output = BufReader::new(output);
            while let Some(message) = Message::read(&mut output).unwrap() {
                if tx.send(serde_json::to_value(message).unwrap()).is_err() {
                    break;
                }
            }
        });
        Self {
            child,
            input,
            messages,
        }
    }

    fn send(&mut self, message: Value) {
        let message: Message = serde_json::from_value(message).unwrap();
        message.write(&mut self.input).unwrap();
    }

    fn recv(&self) -> Value {
        self.messages
            .recv_timeout(Duration::from_secs(15))
            .expect("server response within 15s")
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        self.send(json!({"jsonrpc":"2.0", "id":1, "method":method, "params":params}));
        let response = self.recv();
        assert_eq!(response["id"], 1, "{response}");
        response
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({"jsonrpc":"2.0", "method":method, "params":params}));
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

const URI: &str = "file:///test/spec.chipi";
const SPEC: &str = "# \u{1f600} unicode comment\r\ndecoder D { width = 8 }\r\nselector op [7:4]\r\noperand reg = u4\r\nadd op=0 r:reg[3:0] | \"add {r}\"\r\nsub op=1 r:reg[3:0] | \"sub {r}\"\r\n";

fn at(line: u32, character: u32) -> Value {
    json!({"textDocument":{"uri":URI}, "position":{"line":line,"character":character}})
}

#[test]
fn editor_workflow_over_stdio() {
    let mut client = Client::new();
    let init = client.request(
        "initialize",
        json!({"processId":null,"capabilities":{},"rootUri":null}),
    );
    assert_eq!(init["result"]["capabilities"]["positionEncoding"], "utf-16");
    assert_eq!(
        init["result"]["capabilities"]["documentFormattingProvider"],
        true
    );
    client.notify("initialized", json!({}));
    client.notify(
        "textDocument/didOpen",
        json!({"textDocument":{"uri":URI,"languageId":"chipi","version":1,"text":SPEC}}),
    );
    let diagnostics = client.recv();
    assert_eq!(diagnostics["method"], "textDocument/publishDiagnostics");
    assert_eq!(diagnostics["params"]["version"], 1);
    assert!(diagnostics["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .all(|d| d["severity"] != 1));

    let hover = client.request("textDocument/hover", at(4, 12));
    assert!(hover["result"]["contents"]["value"]
        .as_str()
        .unwrap()
        .contains("operand reg = u4"));
    let definition = client.request("textDocument/definition", at(4, 12));
    assert_eq!(
        definition["result"]["range"]["start"],
        json!({"line":3,"character":8})
    );
    let symbols = client.request(
        "textDocument/documentSymbol",
        json!({"textDocument":{"uri":URI}}),
    );
    assert!(symbols["result"]
        .as_array()
        .unwrap()
        .iter()
        .any(|s| s["name"] == "add"));
    assert_eq!(symbols["result"][0]["location"]["uri"], URI);
    let mut reference_params = at(3, 9);
    reference_params["context"] = json!({"includeDeclaration":true});
    let refs = client.request("textDocument/references", reference_params);
    assert_eq!(refs["result"].as_array().unwrap().len(), 3);
    let completions = client.request("textDocument/completion", at(4, 11));
    let items = completions["result"]["items"].as_array().unwrap();
    for label in ["reg", "op", "r", "fetch", "concat", "u16"] {
        assert!(
            items.iter().any(|i| i["label"] == label),
            "missing completion {label}"
        );
    }
    let edits = client.request(
        "textDocument/formatting",
        json!({"textDocument":{"uri":URI},"options":{"tabSize":2,"insertSpaces":true}}),
    );
    let formatted = edits["result"][0]["newText"].as_str().unwrap();
    assert!(formatted.contains("add op = 0 r:reg[3:0]"));
    assert!(formatted.contains("\r\n"));
    assert!(chipi_core::compile(formatted).is_ok());

    // A sequential UTF-16 edit after an astral character, then a semantic error.
    client.notify("textDocument/didChange", json!({"textDocument":{"uri":URI,"version":2},"contentChanges":[
        {"range":{"start":{"line":0,"character":5},"end":{"line":0,"character":12}},"text":"edited"},
        {"range":{"start":{"line":4,"character":11},"end":{"line":4,"character":14}},"text":"missing"}
    ]}));
    let errors = client.recv();
    assert_eq!(errors["params"]["version"], 2);
    assert!(errors["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .any(|d| d["code"] == "UnknownName" && d["range"]["start"]["line"] == 4));

    // Invalid JSON-RPC request parameters return an error without losing the session.
    let bad = client.request(
        "textDocument/hover",
        json!({"textDocument":{"uri":URI}, "position":{"line":-1,"character":0}}),
    );
    assert_eq!(bad["error"]["code"], -32602);
    assert_eq!(
        client.request("unknown/request", json!({}))["error"]["code"],
        -32601
    );

    // Completion still finds declarations while the current instruction is incomplete.
    let incomplete = format!("{SPEC}next op=2 x:");
    client.notify(
        "textDocument/didChange",
        json!({"textDocument":{"uri":URI,"version":3},"contentChanges":[{"text":incomplete}]}),
    );
    client.recv();
    let completion = client.request("textDocument/completion", at(6, 12));
    assert!(completion["result"]["items"]
        .as_array()
        .unwrap()
        .iter()
        .any(|i| i["label"] == "reg"));

    // A stale document version must not replace the buffer.
    client.notify(
        "textDocument/didChange",
        json!({"textDocument":{"uri":URI,"version":2},"contentChanges":[{"text":"bad"}]}),
    );
    assert_eq!(
        client.request("textDocument/completion", at(6, 12))["id"],
        1
    );
    client.notify(
        "textDocument/didChange",
        json!({"textDocument":{"uri":URI,"version":4},"contentChanges":[{"text":SPEC}]}),
    );
    let fixed = client.recv();
    assert!(fixed["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .all(|d| d["severity"] != 1));
    client.notify("textDocument/didClose", json!({"textDocument":{"uri":URI}}));
    assert_eq!(client.recv()["params"]["diagnostics"], json!([]));
    assert!(client.request("textDocument/hover", at(4, 12))["result"].is_null());
    assert!(client.request("shutdown", Value::Null)["result"].is_null());
    assert_eq!(
        client.request("textDocument/hover", at(4, 12))["error"]["code"],
        -32600
    );
    client.notify("exit", Value::Null);
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(status) = client.child.try_wait().unwrap() {
            assert!(status.success());
            break;
        }
        assert!(std::time::Instant::now() < deadline, "server did not exit");
        std::thread::sleep(Duration::from_millis(10));
    }
}
