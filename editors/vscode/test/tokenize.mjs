// Dependency-light grammar check: validate the TextMate grammar's structure, confirm every regex
// pattern compiles, and assert the headline token classes match the regression fixture.
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const grammar = JSON.parse(readFileSync(join(here, "../syntaxes/chipi.tmLanguage.json"), "utf8"));
const fixture = readFileSync(join(here, "fixtures/regression.chipi"), "utf8");

let failures = 0;
const fail = (msg) => {
  console.error("FAIL:", msg);
  failures++;
};

if (grammar.scopeName !== "source.chipi") fail("scopeName must be source.chipi");
if (!Array.isArray(grammar.patterns) || grammar.patterns.length === 0) fail("missing top-level patterns");
if (!grammar.repository || typeof grammar.repository !== "object") fail("missing repository");

// Every match/begin/end regex must compile as a JS RegExp.
let regexCount = 0;
const walk = (node) => {
  if (Array.isArray(node)) return node.forEach(walk);
  if (!node || typeof node !== "object") return;
  for (const key of ["match", "begin", "end"]) {
    if (typeof node[key] === "string") {
      try {
        new RegExp(node[key]);
        regexCount++;
      } catch (e) {
        fail(`regex in '${key}' does not compile: ${node[key]} (${e.message})`);
      }
    }
  }
  for (const v of Object.values(node)) walk(v);
};
walk(grammar.patterns);
walk(grammar.repository);
if (regexCount < 8) fail(`expected several regexes, found ${regexCount}`);

// Headline token classes must match what the fixture contains.
const expect = (name, ok) => ok || fail(`grammar fails to recognise ${name}`);
const re = (id) => new RegExp(grammar.repository[id].match);
expect("declarations", re("declaration").test("decoder Sample {"));
expect("keywords", re("keyword").test("uses X when"));
expect("constants", re("constant").test("bit_order = msb0"));
expect("builtins", re("builtin").test("bit_width(concat(n))"));
expect("types", re("type").test("rd:u5"));
expect("hex numbers", re("number").test("0xFF"));
expect("binary numbers", re("number").test("0b001101"));
expect("operators", re("operator").test("=> -> .. == !="));
expect("axis names", re("axis").test("lda.imm op=0xA9"));
expect("axis patterns", re("axis").test("dispatch loads { lda.* }"));
expect("name interpolation", re("interpolation").test("bbs_b{n} op = 0x03"));
expect("for keyword", re("keyword").test("for n in 0..8 {"));
expect("fetch keyword", re("keyword").test("fetch(m ? 8 : 16)"));
expect("fixture parses as text", fixture.includes("decoder Sample"));

if (failures > 0) {
  console.error(`\n${failures} grammar check(s) failed.`);
  process.exit(1);
}
console.log(`chipi grammar OK: ${regexCount} regexes compiled, token classes verified.`);
