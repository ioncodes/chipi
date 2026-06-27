//! Resolution: turn a parsed `Spec` into the [`Resolved`] IR. This does name resolution,
//! normalises stream ranges, infers widths over the transform pipeline, checks computed operands
//! and `fn`s, then assembles tags and dispatch groups. All errors are accumulated, so the pass
//! fails if any error was produced. Range and name diagnostics live here, leaf and overlap checks
//! live in `check` and ambiguity in `tree`.

use crate::check::{check_expr, check_fn_expr};
use crate::compute::mask_u64;
use crate::model::*;
use chipi_syntax::ast::{self, Item, SrcRange};
use chipi_syntax::{Diag, Span};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// The resolved-but-not-yet-lowered program (no decode tree yet).
pub struct Resolved {
    pub decoder: Decoder,
    pub selectors: Vec<Selector>,
    pub types: Vec<TypeDef>,
    pub forms: Vec<Form>,
    pub fns: Vec<Func>,
    pub modes: Vec<Mode>,
    pub instrs: Vec<Insn>,
    pub tags: Vec<String>,
    pub groups: Vec<Group>,
    pub subdecoders: Vec<SubDecoder>,
    pub length: Option<Length>,
    pub prefix: Option<Prefix>,
}

pub fn resolve(spec: &ast::Spec) -> Result<Resolved, Vec<Diag>> {
    let mut errs = Vec::new();

    // ---- decoder ----
    let decoders: Vec<&ast::Decoder> = spec.items.iter().filter_map(as_decoder).collect();
    if decoders.is_empty() {
        return Err(vec![Diag::error(
            "MissingDecoder",
            "spec has no `decoder { ... }` block",
            Span::at(0),
        )]);
    }
    if decoders.len() > 1 {
        errs.push(Diag::error(
            "DuplicateDecoder",
            "more than one `decoder` block (exactly one is supported)",
            decoders[1].span,
        ));
    }
    let d = decoders[0];
    let unit_bits = match d.width {
        Some(w) if w.node == 0 || w.node > 64 => {
            errs.push(Diag::error(
                "BadWidth",
                "decoder `width` must be in 1..=64 bits",
                w.span,
            ));
            32u8
        }
        Some(w) => w.node as u8,
        None => {
            errs.push(Diag::error(
                "MissingWidth",
                "decoder is missing `width = N`",
                d.span,
            ));
            32
        }
    };
    let bit_order = d.bit_order.map(|b| b.node).unwrap_or(BitOrder::Lsb0);
    let endian = d.endian.map(|e| e.node).unwrap_or(Endian::Little);

    let context: Vec<CtxField> = d
        .context
        .iter()
        .map(|c| CtxField {
            name: c.name.text.clone(),
            width: c.width,
            default: c.default & mask_u64(c.width),
        })
        .collect();

    let decoder = Decoder {
        name: d.name.text.clone(),
        unit_bits,
        bit_order,
        endian,
        context,
    };
    let window = unit_bits as u16;

    // ---- modes ----
    let modes: Vec<Mode> = d
        .modes
        .iter()
        .map(|m| {
            let cardinality = match &m.kind {
                ModeKind::Bool => 2,
                ModeKind::Enum(vs) => (vs.len() as u64).max(1),
                ModeKind::Uint(w) => 1u64 << (*w).min(20),
            };
            Mode {
                name: m.name.text.clone(),
                kind: m.kind.clone(),
                cardinality,
                default: m.default,
            }
        })
        .collect();

    // ---- selectors ----
    let mut selectors: Vec<Selector> = Vec::new();
    for s in spec.items.iter().filter_map(as_selector) {
        dup_name(
            selectors.iter().map(|x| x.name.as_str()),
            "selector",
            &s.name.text,
            s.name.span,
            &mut errs,
        );
        match canon_range(s.range, bit_order, window) {
            Ok(range) => selectors.push(Selector {
                name: s.name.text.clone(),
                range,
                span: s.span,
            }),
            Err(e) => errs.push(e),
        }
    }

    // ---- types / operands (shared namespace) ----
    let mut types: Vec<TypeDef> = Vec::new();
    for v in spec.items.iter().filter_map(as_value) {
        dup_name(
            types.iter().map(|t| t.name.as_str()),
            "type/operand",
            &v.name.text,
            v.name.span,
            &mut errs,
        );
        let base = match parse_base(&v.base.text) {
            Some(b) => b,
            None => {
                errs.push(Diag::error(
                    "UnknownName",
                    format!(
                        "`{}` is not a primitive base type (expected u<N>, i<N>, or bool)",
                        v.base.text
                    ),
                    v.base.span,
                ));
                BaseTy::U(8)
            }
        };
        if v.source.is_some() && !v.xforms.is_empty() {
            errs.push(Diag::error(
                "Unsupported",
                format!(
                    "operand `{}` has a value source (`fetch`/`assemble`) and transforms; a sourced \
                     operand cannot also declare transforms",
                    v.name.text
                ),
                v.span,
            ));
        }
        types.push(TypeDef {
            name: v.name.text.clone(),
            base,
            xforms: v.xforms.clone(),
            disp: conv_disp(v.disp.as_ref()),
            is_operand: v.kind == ast::ValueKind::Operand,
            source: v.source.clone(),
            span: v.span,
        });
    }

    // ---- forms (reference resolved types) ----
    let mut forms: Vec<Form> = Vec::new();
    for f in spec.items.iter().filter_map(as_form) {
        dup_name(
            forms.iter().map(|x| x.name.as_str()),
            "form",
            &f.name.text,
            f.name.span,
            &mut errs,
        );
        let mut fields = Vec::new();
        for ff in &f.fields {
            match resolve_field(
                &ff.name.text,
                &ff.ty.text,
                ff.range,
                ff.span,
                &types,
                bit_order,
                window,
            ) {
                Ok(field) => fields.push(field),
                Err(e) => errs.push(e),
            }
        }
        forms.push(Form {
            name: f.name.text.clone(),
            fields,
            span: f.span,
        });
    }

    // ---- fns (names collected first so a fn may call any other one or itself) ----
    let fn_names: Vec<String> = spec
        .items
        .iter()
        .filter_map(as_func)
        .map(|f| f.name.text.clone())
        .collect();
    let mut fns: Vec<Func> = Vec::new();
    for f in spec.items.iter().filter_map(as_func) {
        dup_name(
            fns.iter().map(|x| x.name.as_str()),
            "fn",
            &f.name.text,
            f.name.span,
            &mut errs,
        );
        let mut params = Vec::new();
        for (pn, pt) in &f.params {
            match parse_base(&pt.text) {
                Some(b) => params.push((pn.text.clone(), b)),
                None => errs.push(Diag::error(
                    "UnknownName",
                    format!("`{}` is not a primitive parameter type", pt.text),
                    pt.span,
                )),
            }
        }
        let ret = match parse_base(&f.ret.text) {
            Some(b) => b,
            None => {
                errs.push(Diag::error(
                    "UnknownName",
                    format!("`{}` is not a primitive return type", f.ret.text),
                    f.ret.span,
                ));
                BaseTy::U(64)
            }
        };

        // body: sequential scope (params, then each let visible to later lets and the return)
        let mut scope: Vec<String> = f.params.iter().map(|(n, _)| n.text.clone()).collect();
        for (ln, le) in &f.lets {
            check_fn_expr(le, &scope, &fn_names, &mut errs);
            scope.push(ln.text.clone());
        }
        check_fn_expr(&f.ret_expr, &scope, &fn_names, &mut errs);

        fns.push(Func {
            name: f.name.text.clone(),
            params,
            ret,
            lets: f
                .lets
                .iter()
                .map(|(n, e)| (n.text.clone(), e.clone()))
                .collect(),
            ret_expr: f.ret_expr.clone(),
            span: f.span,
        });
    }

    // ---- subdecoders (reference resolved types; their fields are display-only) ----
    let mut subdecoders: Vec<SubDecoder> = Vec::new();
    for sd in spec.items.iter().filter_map(as_subdecoder) {
        dup_name(
            subdecoders.iter().map(|x| x.name.as_str()),
            "subdecoder",
            &sd.name.text,
            sd.name.span,
            &mut errs,
        );
        match resolve_subdecoder(sd, &types, &fn_names) {
            Ok(s) => subdecoders.push(s),
            Err(mut e) => errs.append(&mut e),
        }
    }

    // ---- length ----
    let mut length: Option<Length> = None;
    for ld in spec.items.iter().filter_map(as_length) {
        if length.is_some() {
            errs.push(Diag::error(
                "DuplicateLength",
                "more than one `length` expression",
                ld.span,
            ));
            continue;
        }
        let mut arms = Vec::new();
        let n = ld.arms.len();
        for (i, arm) in ld.arms.iter().enumerate() {
            if arm.bits == 0 || arm.bits > 64 {
                errs.push(Diag::error(
                    "BadLength",
                    "`length` arm must select 1..=64 bits (the handle cap)",
                    arm.span,
                ));
            }
            if arm.cond.is_none() && i != n - 1 {
                errs.push(Diag::error(
                    "BadLength",
                    "an `else` length arm must be last",
                    arm.span,
                ));
            }
            arms.push(LengthArm {
                cond: arm.cond.clone(),
                bits: arm.bits as u16,
            });
        }
        if ld.arms.last().map_or(true, |a| a.cond.is_some()) {
            errs.push(Diag::error(
                "BadLength",
                "`length` must end with an `| else : <bits>` catch-all arm",
                ld.span,
            ));
        }
        length = Some(Length { arms });
    }

    // ---- prefix ----
    let mut prefix: Option<Prefix> = None;
    for pd in spec.items.iter().filter_map(as_prefix) {
        if prefix.is_some() {
            errs.push(Diag::error(
                "DuplicatePrefix",
                "more than one `prefix` scan",
                pd.span,
            ));
            continue;
        }
        let mut arms = Vec::new();
        for arm in &pd.arms {
            for (name, _) in &arm.assigns {
                if !decoder.context.iter().any(|c| c.name == name.text) {
                    errs.push(Diag::error(
                        "UnknownContext",
                        format!(
                            "prefix assigns `{}`, which is not a declared `context` field",
                            name.text
                        ),
                        name.span,
                    ));
                }
            }
            arms.push(PrefixArm {
                pat: arm.pat,
                assigns: arm
                    .assigns
                    .iter()
                    .map(|(n, e)| (n.text.clone(), e.clone()))
                    .collect(),
                term: arm.term,
            });
        }
        match pd
            .arms
            .iter()
            .find(|a| matches!(a.pat, PrefixPat::Wildcard))
        {
            None => errs.push(Diag::error(
                "PrefixNotTotal",
                "`prefix` scan needs a `_ => done` catch-all so it stops on a non-prefix byte",
                pd.span,
            )),
            Some(a) if a.term != PrefixTerm::Done => errs.push(Diag::error(
                "PrefixNotTotal",
                "the `_` catch-all of a `prefix` scan must be `done` (stop without consuming the \
                 opcode byte); `finish`/assignment arms would consume it",
                a.span,
            )),
            Some(_) => {}
        }
        prefix = Some(Prefix { arms });
    }

    // ---- instructions ----
    let mut instrs: Vec<Insn> = Vec::new();
    for inst in spec.items.iter().filter_map(as_instr) {
        match resolve_instr(
            inst,
            &selectors,
            &types,
            &forms,
            &modes,
            &fn_names,
            &subdecoders,
            bit_order,
            window,
        ) {
            Ok(i) => instrs.push(i),
            Err(mut e) => errs.append(&mut e),
        }
    }

    // ---- tags / dispatch groups (after instructions) ----
    // Index instructions by name once (first occurrence wins) so resolving group
    // members stays O(members) instead of rescanning every instruction per member.
    let mut name_to_idx: HashMap<String, usize> = HashMap::new();
    for (i, inst) in instrs.iter().enumerate() {
        name_to_idx.entry(inst.name.clone()).or_insert(i);
    }
    let mut all_tags: BTreeSet<String> = BTreeSet::new();
    let mut dispatch_owner: BTreeMap<String, String> = BTreeMap::new();
    let mut groups: Vec<Group> = Vec::new();
    for g in spec.items.iter().filter_map(as_group) {
        if name_to_idx.contains_key(g.tag.text.as_str()) {
            errs.push(Diag::error(
                "GroupNameClash",
                format!("group `{}` has the same name as an instruction", g.tag.text),
                g.span,
            ));
        }
        all_tags.insert(g.tag.text.clone());
        let mut members = Vec::new();
        for m in &g.members {
            match name_to_idx.get(m.text.as_str()) {
                Some(&idx) => {
                    let inst = &mut instrs[idx];
                    if !inst.tags.contains(&g.tag.text) {
                        inst.tags.push(g.tag.text.clone());
                    }
                    if g.dispatch {
                        if let Some(other) = dispatch_owner.get(&m.text) {
                            errs.push(Diag::error(
                                "MultipleDispatchGroups",
                                format!(
                                    "`{}` is in two dispatch groups (`{}` and `{}`); an instruction \
                                     may be in at most one",
                                    m.text, other, g.tag.text
                                ),
                                g.span,
                            ));
                        } else {
                            dispatch_owner.insert(m.text.clone(), g.tag.text.clone());
                        }
                        members.push(m.text.clone());
                    }
                }
                None => errs.push(Diag::error(
                    "UnknownInstruction",
                    format!(
                        "group `{}` lists `{}`, which is not an instruction",
                        g.tag.text, m.text
                    ),
                    m.span,
                )),
            }
        }
        if g.dispatch {
            groups.push(Group {
                name: g.tag.text.clone(),
                members,
            });
        }
    }
    let tags: Vec<String> = all_tags.into_iter().collect();

    if errs.is_empty() {
        Ok(Resolved {
            decoder,
            selectors,
            types,
            forms,
            fns,
            modes,
            instrs,
            tags,
            groups,
            subdecoders,
            length,
            prefix,
        })
    } else {
        Err(errs)
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_instr(
    inst: &ast::Instr,
    selectors: &[Selector],
    types: &[TypeDef],
    forms: &[Form],
    modes: &[Mode],
    fn_names: &[String],
    subdecoders: &[SubDecoder],
    bit_order: BitOrder,
    window: u16,
) -> Result<Insn, Vec<Diag>> {
    let mut errs: Vec<Diag> = Vec::new();
    let mut fixed: Vec<Fixed> = Vec::new();
    let mut fields: Vec<Field> = Vec::new();
    let mut computed: Vec<Computed> = Vec::new();
    let mut mode_constraints: Vec<(usize, u64)> = Vec::new();

    // constraints
    for c in &inst.constraints {
        match c {
            ast::Constraint::Named { name, value } => {
                if let Some(sel) = selectors.iter().find(|s| s.name == name.text) {
                    let w = sel.range.width();
                    if !fits(value.value, w) {
                        errs.push(Diag::error(
                            "BitConflict",
                            format!(
                                "value {} does not fit in {w}bit selector `{}`",
                                value.value, name.text
                            ),
                            value.span,
                        ));
                    }
                    fixed.push(Fixed {
                        range: sel.range,
                        value: value.value as u64,
                        span: name.span.to(value.span),
                        label: name.text.clone(),
                    });
                } else if let Some((mi, m)) =
                    modes.iter().enumerate().find(|(_, m)| m.name == name.text)
                {
                    if value.value >= m.cardinality as u128 {
                        errs.push(Diag::error(
                            "BitConflict",
                            format!("mode `{}` only has {} value(s)", name.text, m.cardinality),
                            value.span,
                        ));
                    }
                    mode_constraints.push((mi, value.value as u64));
                } else {
                    errs.push(Diag::error(
                        "UnknownSelector",
                        format!("`{}` is not a declared selector or mode", name.text),
                        name.span,
                    ));
                }
            }
            ast::Constraint::Range { range, value } => match canon_range(*range, bit_order, window)
            {
                Ok(r) => {
                    let w = r.width();
                    if !fits(value.value, w) {
                        errs.push(Diag::error(
                            "BitConflict",
                            format!("value {} does not fit in {w}bit field", value.value),
                            value.span,
                        ));
                    }
                    fixed.push(Fixed {
                        range: r,
                        value: value.value as u64,
                        span: range.span.to(value.span),
                        label: "bits".to_string(),
                    });
                }
                Err(e) => errs.push(e),
            },
        }
    }

    // bindings
    let mut sourced: Vec<(&ast::Binding, &TypeDef)> = Vec::new();
    for b in &inst.bindings {
        let Some(range) = b.range else {
            // No bit range. This is allowed only when the bound operand carries a value source
            // (`fetch`/`assemble`), in which case it lowers to a computed operand once `fields` is
            // complete. Anything else is the auto-ranged immediate error.
            if let Some(td) = types.iter().find(|t| t.name == b.ty.name.text) {
                if td.source.is_some() {
                    if !b.ty.args.is_empty() {
                        errs.push(Diag::error(
                            "Unsupported",
                            "parameterised types (e.g. `gpr(reg(sf))`) are not supported",
                            b.ty.span,
                        ));
                    }
                    sourced.push((b, td));
                    continue;
                }
            }
            errs.push(Diag::error(
                "Unsupported",
                format!("binding `{}` has no explicit bit range; auto-ranged immediates are not supported", b.name.text),
                b.span,
            ));
            continue;
        };

        // A field whose type is a subdecoder decodes/encodes as plain bits, but its `{field.output}`
        // references run the subdecoder. The field may be narrower than the subdecoder (the value is
        // zero-extended for the lookup), as for the GameCube DSP's 7bit extension byte.
        if let Some(sd) = subdecoders.iter().find(|s| s.name == b.ty.name.text) {
            let r = match canon_range(range, bit_order, window) {
                Ok(r) => r,
                Err(e) => {
                    errs.push(e);
                    continue;
                }
            };
            let raw_width = r.width();
            if raw_width > sd.unit_bits as u16 {
                errs.push(Diag::error(
                    "WidthMismatch",
                    format!(
                        "field `{}` is {raw_width} bits but subdecoder `{}` decodes {} bits",
                        b.name.text, sd.name, sd.unit_bits
                    ),
                    b.span,
                ));
                continue;
            }
            let sdn = sd.name.clone();
            fields.push(Field {
                name: b.name.text.clone(),
                range: r,
                ty: FieldTy {
                    base: BaseTy::U(raw_width),
                    xforms: Vec::new(),
                    disp: Disp::None,
                    type_name: Some(sdn.clone()),
                    raw_width,
                    value_width: raw_width,
                    signed: false,
                    subdecoder: Some(sdn),
                },
                span: b.span,
            });
            continue;
        }

        match resolve_field(
            &b.name.text,
            &b.ty.name.text,
            range,
            b.span,
            types,
            bit_order,
            window,
        ) {
            Ok(f) => fields.push(f),
            Err(e) => errs.push(e),
        }
        if !b.ty.args.is_empty() {
            errs.push(Diag::error(
                "Unsupported",
                "parameterised types (e.g. `gpr(reg(sf))`) are not supported",
                b.ty.span,
            ));
        }
    }

    // uses FORM
    if let Some(form_name) = &inst.uses {
        match forms.iter().find(|f| f.name == form_name.text) {
            Some(form) => fields.extend(form.fields.iter().cloned()),
            None => errs.push(Diag::error(
                "UnknownName",
                format!("`{}` is not a declared form", form_name.text),
                form_name.span,
            )),
        }
    }

    // computed operands. The declared type is either a primitive base, or a named `operand`/`type`
    // whose base and display (e.g. a `names { ... }` table) the computed operand inherits. The
    // type's transform pipeline is not applied: the computed expression already yields the value.
    for c in &inst.computed {
        let (base, disp, type_name) = if let Some(b) = parse_base(&c.ty.text) {
            (b, Disp::None, None)
        } else if let Some(td) = types.iter().find(|t| t.name == c.ty.text) {
            (td.base, td.disp.clone(), Some(td.name.clone()))
        } else {
            errs.push(Diag::error(
                "UnknownName",
                format!(
                    "`{}` is not a primitive base type, operand, or type for a computed operand",
                    c.ty.text
                ),
                c.ty.span,
            ));
            continue;
        };
        let mut cerrs = Vec::new();
        check_expr(&c.expr, &fields, window, fn_names, &mut cerrs);
        if !cerrs.is_empty() {
            errs.append(&mut cerrs);
            continue;
        }

        let value_width = base.width().clamp(1, 64);
        if let ast::Expr::Assemble { out_width, .. } = &c.expr {
            if *out_width as u16 != value_width {
                errs.push(Diag::error(
                    "WidthMismatch",
                    format!(
                        "assemble produces {out_width} bits but `{}` is declared `{}` ({value_width} bits)",
                        c.name.text, c.ty.text
                    ),
                    c.span,
                ));
                continue;
            }
        }
        computed.push(Computed {
            name: c.name.text.clone(),
            ty: FieldTy {
                base,
                xforms: Vec::new(),
                disp,
                type_name,
                raw_width: value_width,
                value_width,
                signed: base.signed(),
                subdecoder: None,
            },
            expr: c.expr.clone(),
            span: c.span,
        });
    }

    // computed operands from no-range bindings of sourced operands (Layer 2). Resolved here so the
    // recipe can reference the leaf's fields. Tagged with the operand `type_name` so the accessor
    // disambiguates by addressing mode or format (`addr_abs`) rather than raw width.
    for (b, td) in sourced {
        let src = td.source.as_ref().expect("sourced operand has a source");
        let mut cerrs = Vec::new();
        check_expr(src, &fields, window, fn_names, &mut cerrs);
        if !cerrs.is_empty() {
            errs.append(&mut cerrs);
            continue;
        }

        let value_width = td.base.width().clamp(1, 64);
        if let ast::Expr::Assemble { out_width, .. } = src {
            if *out_width as u16 != value_width {
                errs.push(Diag::error(
                    "WidthMismatch",
                    format!(
                        "assemble produces {out_width} bits but operand `{}` is {value_width} bits",
                        td.name
                    ),
                    b.span,
                ));
                continue;
            }
        }

        computed.push(Computed {
            name: b.name.text.clone(),
            ty: FieldTy {
                base: td.base,
                xforms: Vec::new(),
                disp: td.disp.clone(),
                type_name: Some(td.name.clone()),
                raw_width: value_width,
                value_width,
                signed: td.base.signed(),
                subdecoder: None,
            },
            expr: src.clone(),
            span: b.span,
        });
    }

    // guard
    if let Some(g) = &inst.guard {
        check_expr(g, &fields, window, fn_names, &mut errs);
    }

    // display arms
    let mut display = Vec::new();
    for arm in &inst.display {
        let segs = match crate::render::parse_template(&arm.template.text, arm.template.span) {
            Ok(s) => s,
            Err(e) => {
                errs.push(e);
                continue;
            }
        };
        for seg in &segs {
            match seg {
                crate::render::Seg::Field { name, .. } => {
                    let known = fields.iter().any(|f| &f.name == name)
                        || computed.iter().any(|c| &c.name == name);
                    if !known {
                        errs.push(Diag::error(
                            "UnknownName",
                            format!("display references `{{{name}}}`, which is not a bound field or computed operand"),
                            arm.template.span,
                        ));
                    }
                }
                crate::render::Seg::SubField { field, output } => {
                    let Some(f) = fields.iter().find(|f| &f.name == field) else {
                        errs.push(Diag::error(
                            "UnknownName",
                            format!("display references `{{{field}.{output}}}`, but `{field}` is not a bound field"),
                            arm.template.span,
                        ));
                        continue;
                    };
                    let Some(sdn) = &f.ty.subdecoder else {
                        errs.push(Diag::error(
                            "UnknownName",
                            format!(
                                "`{field}.{output}`: field `{field}` is not a subdecoder field"
                            ),
                            arm.template.span,
                        ));
                        continue;
                    };
                    let sd = subdecoders.iter().find(|s| &s.name == sdn);
                    if !sd.is_some_and(|s| s.outputs.iter().any(|o| o == output)) {
                        errs.push(Diag::error(
                            "UnknownName",
                            format!(
                                "`{field}.{output}`: subdecoder `{sdn}` has no output `{output}`"
                            ),
                            arm.template.span,
                        ));
                    }
                }
                crate::render::Seg::Lit(_) | crate::render::Seg::Cond { .. } => {}
            }
        }
        display.push(DisplayArm {
            cond: arm.cond.clone(),
            segs,
            span: arm.span,
        });
    }

    if errs.is_empty() {
        Ok(Insn {
            name: inst.name.text.clone(),
            fixed,
            fields,
            computed,
            mode_constraints,
            guard: inst.guard.clone(),
            display,
            tags: Vec::new(),
            span: inst.span,
        })
    } else {
        Err(errs)
    }
}

fn resolve_subdecoder(
    sd: &ast::SubDecoder,
    types: &[TypeDef],
    _fn_names: &[String],
) -> Result<SubDecoder, Vec<Diag>> {
    let mut errs: Vec<Diag> = Vec::new();

    let unit_bits = match sd.width {
        Some(w) if w.node == 0 || w.node > 64 => {
            errs.push(Diag::error(
                "BadWidth",
                "subdecoder `width` must be in 1..=64 bits",
                w.span,
            ));
            8u8
        }
        Some(w) => w.node as u8,
        None => {
            errs.push(Diag::error(
                "MissingWidth",
                "subdecoder is missing `width = N`",
                sd.span,
            ));
            8
        }
    };
    let bit_order = sd.bit_order.map(|b| b.node).unwrap_or(BitOrder::Lsb0);
    let window = unit_bits as u16;

    let outputs: Vec<String> = sd.outputs.iter().map(|o| o.text.clone()).collect();
    if outputs.is_empty() {
        errs.push(Diag::error(
            "MissingOutputs",
            format!(
                "subdecoder `{}` declares no outputs; add `outputs {{ name, ... }}`",
                sd.name.text
            ),
            sd.span,
        ));
    }

    let mut arms: Vec<SubArm> = Vec::new();
    for a in &sd.arms {
        let mut aerrs: Vec<Diag> = Vec::new();

        // fold the constraints into a fixed mask and value over the sub-window.
        let mut mask = 0u64;
        let mut val = 0u64;
        for c in &a.constraints {
            match c {
                ast::Constraint::Range { range, value } => {
                    match canon_range(*range, bit_order, window) {
                        Ok(r) => {
                            let w = r.width();
                            if !fits(value.value, w) {
                                aerrs.push(Diag::error(
                                    "BitConflict",
                                    format!("value {} does not fit in {w}bit field", value.value),
                                    value.span,
                                ));
                            }
                            let m = r.mask();
                            mask |= m;
                            val |= ((value.value as u64) << r.lo) & m;
                        }
                        Err(e) => aerrs.push(e),
                    }
                }
                ast::Constraint::Named { name, .. } => aerrs.push(Diag::error(
                    "Unsupported",
                    "subdecoder arms use `[range] = value` constraints, not selector/mode names",
                    name.span,
                )),
            }
        }

        // turn the bindings into fields over the sub-window.
        let mut fields: Vec<Field> = Vec::new();
        for b in &a.bindings {
            let Some(range) = b.range else {
                aerrs.push(Diag::error(
                    "Unsupported",
                    format!(
                        "subdecoder binding `{}` needs an explicit bit range",
                        b.name.text
                    ),
                    b.span,
                ));
                continue;
            };
            match resolve_field(
                &b.name.text,
                &b.ty.name.text,
                range,
                b.span,
                types,
                bit_order,
                window,
            ) {
                Ok(f) => fields.push(f),
                Err(e) => aerrs.push(e),
            }
        }

        // parse each output into a template over the arm's fields.
        let mut out_segs: Vec<(String, Vec<crate::render::Seg>)> = Vec::new();
        for o in &a.outputs {
            if !outputs.contains(&o.name.text) {
                aerrs.push(Diag::error(
                    "UnknownName",
                    format!(
                        "`{}` is not a declared output of this subdecoder",
                        o.name.text
                    ),
                    o.span,
                ));
                continue;
            }
            if out_segs.iter().any(|(n, _)| n == &o.name.text) {
                aerrs.push(Diag::error(
                    "DuplicateName",
                    format!("output `{}` is set more than once in this arm", o.name.text),
                    o.span,
                ));
                continue;
            }
            let segs = match crate::render::parse_template(&o.template.text, o.template.span) {
                Ok(s) => s,
                Err(e) => {
                    aerrs.push(e);
                    continue;
                }
            };
            check_sub_segs(&segs, &fields, o.template.span, &mut aerrs);
            out_segs.push((o.name.text.clone(), segs));
        }

        // every declared output must be set.
        for on in &outputs {
            if !out_segs.iter().any(|(n, _)| n == on) {
                aerrs.push(Diag::error(
                    "MissingOutput",
                    format!("arm `{}` does not set output `{on}`", a.name.text),
                    a.span,
                ));
            }
        }

        if aerrs.is_empty() {
            arms.push(SubArm {
                name: a.name.text.clone(),
                mask,
                val,
                fields,
                outputs: out_segs,
                span: a.span,
            });
        } else {
            errs.append(&mut aerrs);
        }
    }

    // Match most-specific-first: an arm with more fixed bits wins ties of overlap. Stable, so arms
    // with the same number of fixed bits keep declaration order.
    arms.sort_by_key(|a| std::cmp::Reverse(a.mask.count_ones()));

    if errs.is_empty() {
        Ok(SubDecoder {
            name: sd.name.text.clone(),
            unit_bits,
            bit_order,
            outputs,
            arms,
        })
    } else {
        Err(errs)
    }
}

/// Validate a subdecoder output template: every `{field}` must name one of the arm's fields, and
/// `{field.output}` (nested subdecoding) is not allowed. Recurses into conditional branches.
fn check_sub_segs(segs: &[crate::render::Seg], fields: &[Field], span: Span, errs: &mut Vec<Diag>) {
    use crate::render::Seg;
    for seg in segs {
        match seg {
            Seg::Lit(_) => {}
            Seg::Field { name, .. } => {
                if !fields.iter().any(|f| &f.name == name) {
                    errs.push(Diag::error(
                        "UnknownName",
                        format!("subdecoder output references `{{{name}}}`, which is not a field of this arm"),
                        span,
                    ));
                }
            }
            Seg::SubField { field, output } => errs.push(Diag::error(
                "Unsupported",
                format!(
                    "subdecoder output `{{{field}.{output}}}` cannot itself use `.` field access"
                ),
                span,
            )),
            Seg::Cond { .. } => errs.push(Diag::error(
                "Unsupported",
                "in-template conditionals (`{c ? a : b}`) are not supported in subdecoder outputs",
                span,
            )),
        }
    }
}

fn resolve_field(
    name: &str,
    ty_name: &str,
    range: SrcRange,
    span: Span,
    types: &[TypeDef],
    bit_order: BitOrder,
    window: u16,
) -> Result<Field, Diag> {
    let r = canon_range(range, bit_order, window)?;
    let raw_width = r.width();
    let ty = resolve_tyref(ty_name, raw_width, range.span, types)?;
    Ok(Field {
        name: name.to_string(),
        range: r,
        ty,
        span,
    })
}

fn resolve_tyref(
    ty_name: &str,
    raw_width: u16,
    span: Span,
    types: &[TypeDef],
) -> Result<FieldTy, Diag> {
    let (base, xforms, disp, type_name) = if let Some(t) = types.iter().find(|t| t.name == ty_name)
    {
        (
            t.base,
            t.xforms.clone(),
            t.disp.clone(),
            Some(t.name.clone()),
        )
    } else if let Some(b) = parse_base(ty_name) {
        (b, Vec::new(), Disp::None, None)
    } else {
        return Err(Diag::error(
            "UnknownName",
            format!("`{ty_name}` is not a known type, operand, or primitive"),
            span,
        ));
    };

    let has_resize = xforms.iter().any(|x| {
        matches!(
            x,
            Xform::SignExtend(_)
                | Xform::ZeroExtend(_)
                | Xform::RotateLeft(..)
                | Xform::RotateRight(..)
        )
    });
    if !has_resize && raw_width != base.width() {
        return Err(Diag::error(
            "WidthMismatch",
            format!(
                "field is {raw_width} bits but type `{ty_name}` is {} bits (add a sign_extend/zero_extend, or fix the range)",
                base.width()
            ),
            span,
        ));
    }

    for x in &xforms {
        match x {
            Xform::ShiftLeft(n) | Xform::ShiftRight(n) if *n >= 64 => {
                return Err(Diag::error(
                    "BadXform",
                    format!("shift amount {n} must be < 64 (chipi values are at most 64 bits)"),
                    span,
                ))
            }
            Xform::RotateLeft(_, w) | Xform::RotateRight(_, w) => {
                if *w == 0 || *w > 64 {
                    return Err(Diag::error(
                        "BadXform",
                        format!("rotate width {w} must be in 1..=64"),
                        span,
                    ));
                }
                if raw_width != *w {
                    return Err(Diag::error(
                        "WidthMismatch",
                        format!("rotate operates on {w} bits but the field is {raw_width} bits"),
                        span,
                    ));
                }
            }
            _ => {}
        }
    }

    // walk the pipeline to derive the post-transform value width and signedness.
    let mut w = raw_width;
    let mut signed = base.signed();
    for x in &xforms {
        match x {
            Xform::ShiftLeft(n) => w = w.saturating_add(*n),
            Xform::ShiftRight(_) => {}
            Xform::SignExtend(_) => {
                w = base.width();
                signed = true;
            }
            Xform::ZeroExtend(n) => {
                w = w.max(*n);
                signed = false;
            }
            Xform::RotateLeft(_, width) | Xform::RotateRight(_, width) => w = *width,
        }
    }
    let value_width = w.clamp(1, 64);

    Ok(FieldTy {
        base,
        xforms,
        disp,
        type_name,
        raw_width,
        value_width,
        signed,
        subdecoder: None,
    })
}

/// Turn a source-order stream range into numeric `[lo, hi]` over the window.
fn canon_range(r: SrcRange, bit_order: BitOrder, window: u16) -> Result<BitRange, Diag> {
    let map = |k: u32| -> Result<u16, Diag> {
        if k >= window as u32 {
            return Err(Diag::error(
                "RangeOutOfBounds",
                format!("bit {k} is outside the {window}bit decode window"),
                r.span,
            ));
        }
        Ok(match bit_order {
            BitOrder::Lsb0 => k as u16,
            BitOrder::Msb0 => window - 1 - k as u16,
        })
    };

    let na = map(r.a)?;
    let nb = map(r.b)?;
    Ok(BitRange {
        lo: na.min(nb),
        hi: na.max(nb),
    })
}

fn parse_base(s: &str) -> Option<BaseTy> {
    if s == "bool" {
        return Some(BaseTy::Bool);
    }
    let ctor: fn(u16) -> BaseTy = match s.bytes().next()? {
        b'u' => BaseTy::U,
        b'i' => BaseTy::I,
        _ => return None,
    };

    let rest = &s[1..];
    if rest.is_empty() || !rest.bytes().all(|c| c.is_ascii_digit()) {
        return None;
    }

    let n: u16 = rest.parse().ok()?;
    (1..=64).contains(&n).then(|| ctor(n))
}

fn conv_disp(d: Option<&ast::DispAttr>) -> Disp {
    match d {
        None => Disp::None,
        Some(ast::DispAttr::Pattern(s)) => Disp::Pattern(s.text.clone()),
        Some(ast::DispAttr::Hint(h)) => Disp::Hint(*h),
        Some(ast::DispAttr::Names(t)) => Disp::Names(NamesTable {
            entries: t
                .entries
                .iter()
                .map(|(k, s)| (*k as u64, s.clone()))
                .collect(),
            default: match &t.default {
                ast::NameDefault::Str(s) => NameDefault::Str(s.clone()),
                ast::NameDefault::Hint(h) => NameDefault::Hint(*h),
            },
        }),
    }
}

fn fits(v: u128, w: u16) -> bool {
    w >= 128 || v < (1u128 << w)
}

/// Emit a `DuplicateName` diagnostic if `name` is already among the declared `existing`.
fn dup_name<'a>(
    mut existing: impl Iterator<Item = &'a str>,
    kind: &str,
    name: &str,
    span: Span,
    errs: &mut Vec<Diag>,
) {
    if existing.any(|x| x == name) {
        errs.push(Diag::error(
            "DuplicateName",
            format!("{kind} `{name}` is already declared"),
            span,
        ));
    }
}

// ---- item filters ----
macro_rules! item_filter {
    ($name:ident, $variant:ident, $ty:ty) => {
        fn $name(i: &Item) -> Option<&$ty> {
            if let Item::$variant(x) = i {
                Some(x)
            } else {
                None
            }
        }
    };
}

item_filter!(as_decoder, Decoder, ast::Decoder);
item_filter!(as_selector, Selector, ast::Selector);
item_filter!(as_value, Value, ast::ValueDecl);
item_filter!(as_form, Form, ast::Form);
item_filter!(as_func, Func, ast::FuncDecl);
item_filter!(as_length, Length, ast::LengthDecl);
item_filter!(as_prefix, Prefix, ast::PrefixDecl);
item_filter!(as_instr, Instr, ast::Instr);
item_filter!(as_group, Group, ast::GroupDecl);
item_filter!(as_subdecoder, SubDecoder, ast::SubDecoder);
