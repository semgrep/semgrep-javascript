(**
   Boilerplate to be used as a template when mapping the javascript CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_unescaped_double_string_fragment (env : env) (tok : CST.unescaped_double_string_fragment) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_anon_choice_let_ca16eb3 (env : env) (x : CST.anon_choice_let_ca16eb3) =
  (match x with
  | `Let tok -> R.Case ("Let",
      (* "let" *) token env tok
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  )

let map_jsx_text (env : env) (tok : CST.jsx_text) =
  (* pattern [^{}<>]+ *) token env tok

let map_import (env : env) (tok : CST.import) =
  (* import *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  (* pattern #!.* *) token env tok

let map_private_property_identifier (env : env) (tok : CST.private_property_identifier) =
  (* private_property_identifier *) token env tok

let map_ternary_qmark (env : env) (tok : CST.ternary_qmark) =
  (* ternary_qmark *) token env tok

let map_jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok

let map_unescaped_single_string_fragment (env : env) (tok : CST.unescaped_single_string_fragment) =
  (* pattern "[^'\\\\]+" *) token env tok

let map_template_chars (env : env) (tok : CST.template_chars) =
  (* template_chars *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_regex_flags (env : env) (tok : CST.regex_flags) =
  (* pattern [a-z]+ *) token env tok

let map_regex_pattern (env : env) (tok : CST.regex_pattern) =
  (* regex_pattern *) token env tok

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  )

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Get tok -> R.Case ("Get",
      (* "get" *) token env tok
    )
  | `Set tok -> R.Case ("Set",
      (* "set" *) token env tok
    )
  | `Async tok -> R.Case ("Async",
      (* "async" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Export tok -> R.Case ("Export",
      (* "export" *) token env tok
    )
  )

let map_imm_tok_slash (env : env) (tok : CST.imm_tok_slash) =
  (* "/" *) token env tok

let map_import_export_specifier (env : env) ((v1, v2) : CST.import_export_specifier) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_anon_choice_id_b8f8ced (env : env) (x : CST.anon_choice_id_b8f8ced) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Deco_member_exp x -> R.Case ("Deco_member_exp",
      map_decorator_member_expression env x
    )
  )

and map_decorator_member_expression (env : env) ((v1, v2, v3) : CST.decorator_member_expression) =
  let v1 = map_anon_choice_id_b8f8ced env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_namespace_import_export (env : env) ((v1, v2, v3) : CST.namespace_import_export) =
  let v1 = (* "*" *) token env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Nested_id x -> R.Case ("Nested_id",
        map_nested_identifier env x
      )
    )
  in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_identifier_ (env : env) (x : CST.identifier_) =
  (match x with
  | `Unde tok -> R.Case ("Unde",
      (* "undefined" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  (match x with
  | `Jsx_id tok -> R.Case ("Jsx_id",
      (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_unes_double_str_frag_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_double_str_frag tok -> R.Case ("Unes_double_str_frag",
              (* pattern "[^\"\\\\]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_unes_single_str_frag_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_single_str_frag tok -> R.Case ("Unes_single_str_frag",
              (* pattern "[^'\\\\]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> R.Case ("Auto_semi",
      (* automatic_semicolon *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_anon_choice_rese_id_9a83200 (env : env) (x : CST.anon_choice_rese_id_9a83200) =
  (match x with
  | `Choice_get x -> R.Case ("Choice_get",
      map_reserved_identifier env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_anon_choice_id_0e3c97f (env : env) (x : CST.anon_choice_id_0e3c97f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Choice_get x -> R.Case ("Choice_get",
      map_reserved_identifier env x
    )
  )

let map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d (env : env) ((v1, v2) : CST.anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d) =
  let v1 = map_import_export_specifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_import_export_specifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = map_jsx_identifier_ env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_jsx_identifier_ env v3 in
  R.Tuple [v1; v2; v3]

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 = map_string_ env v2 in
  R.Tuple [v1; v2]

let map_export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  (match x with
  | `Choice_jsx_id x -> R.Case ("Choice_jsx_id",
      map_jsx_identifier_ env x
    )
  | `Jsx_name_name x -> R.Case ("Jsx_name_name",
      map_jsx_namespace_name env x
    )
  )

let map_jsx_element_name (env : env) (x : CST.jsx_element_name) =
  (match x with
  | `Choice_jsx_id x -> R.Case ("Choice_jsx_id",
      map_jsx_identifier_ env x
    )
  | `Nested_id x -> R.Case ("Nested_id",
      map_nested_identifier env x
    )
  | `Jsx_name_name x -> R.Case ("Jsx_name_name",
      map_jsx_namespace_name env x
    )
  )

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Name_import_export x -> R.Case ("Name_import_export",
      map_namespace_import_export env x
    )
  | `Named_imports x -> R.Case ("Named_imports",
      map_named_imports env x
    )
  | `Id_opt_COMMA_choice_name_import_export (v1, v2) -> R.Case ("Id_opt_COMMA_choice_name_import_export",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | `Name_import_export x -> R.Case ("Name_import_export",
                  map_namespace_import_export env x
                )
              | `Named_imports x -> R.Case ("Named_imports",
                  map_named_imports env x
                )
              )
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_jsx_closing_element (env : env) ((v1, v2, v3, v4) : CST.jsx_closing_element) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* "/" *) token env v2 in
  let v3 = map_jsx_element_name env v3 in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Spread_elem x -> R.Case ("Spread_elem",
      map_spread_element env x
    )
  )

and map_anon_choice_exp_9cd0ed5 (env : env) (x : CST.anon_choice_exp_9cd0ed5) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  )

and map_anon_choice_id_940079a (env : env) (x : CST.anon_choice_id_940079a) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Dest_pat x -> R.Case ("Dest_pat",
      map_destructuring_pattern env x
    )
  )

and map_anon_choice_pair_20c9acd (env : env) (x : CST.anon_choice_pair_20c9acd) =
  (match x with
  | `Pair (v1, v2, v3) -> R.Case ("Pair",
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Spread_elem x -> R.Case ("Spread_elem",
      map_spread_element env x
    )
  | `Meth_defi x -> R.Case ("Meth_defi",
      map_method_definition env x
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_id_0e3c97f env x
    )
  )

and map_anon_choice_pair_pat_3ff9cbe (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) =
  (match x with
  | `Pair_pat (v1, v2, v3) -> R.Case ("Pair_pat",
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_formal_parameter env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rest_pat x -> R.Case ("Rest_pat",
      map_rest_pattern env x
    )
  | `Obj_assign_pat (v1, v2, v3) -> R.Case ("Obj_assign_pat",
      let v1 =
        (match v1 with
        | `Choice_choice_get x -> R.Case ("Choice_choice_get",
            map_anon_choice_rese_id_9a83200 env x
          )
        | `Dest_pat x -> R.Case ("Dest_pat",
            map_destructuring_pattern env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_id_0e3c97f env x
    )
  )

and map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env) (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_9818c1b env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env) (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_anon_choice_exp_9818c1b env x
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_augmented_assignment_lhs (env : env) (x : CST.augmented_assignment_lhs) =
  (match x with
  | `Member_exp x -> R.Case ("Member_exp",
      map_member_expression env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Choice_get x -> R.Case ("Choice_get",
      map_reserved_identifier env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_inst_exp (v1, v2, v3) -> R.Case ("Exp_inst_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "instanceof" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_in_exp (v1, v2, v3) -> R.Case ("Exp_in_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Exp_choice_args (v1, v2) -> R.Case ("Exp_choice_args",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Args x -> R.Case ("Args",
            map_arguments env x
          )
        | `Temp_str x -> R.Case ("Temp_str",
            map_template_string env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Prim_exp_QMARKDOT_args (v1, v2, v3) -> R.Case ("Prim_exp_QMARKDOT_args",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "?." *) token env v2 in
      let v3 = map_arguments env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_signature (env : env) (x : CST.call_signature) =
  map_formal_parameters env x

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_anon_choice_id_940079a env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = map_statement_block env v3 in
  R.Tuple [v1; v2; v3]

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Meth_defi_opt_SEMI (v1, v2) -> R.Case ("Meth_defi_opt_SEMI",
          let v1 = map_method_definition env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* ";" *) token env tok
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        )
      | `Field_defi_choice_auto_semi (v1, v2) -> R.Case ("Field_defi_choice_auto_semi",
          let v1 = map_field_definition env v1 in
          let v2 = map_semicolon env v2 in
          R.Tuple [v1; v2]
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_class_heritage (env : env) ((v1, v2) : CST.class_heritage) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Func_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Func_decl",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_call_signature env v4 in
      let v5 = map_statement_block env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Gene_func_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Gene_func_decl",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* "*" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 = map_call_signature env v5 in
      let v6 = map_statement_block env v6 in
      let v7 =
        (match v7 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Class_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Class_decl",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 = (* "class" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_class_heritage env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_class_body env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Lexi_decl x -> R.Case ("Lexi_decl",
      map_lexical_declaration env x
    )
  | `Var_decl x -> R.Case ("Var_decl",
      map_variable_declaration env x
    )
  )

and map_decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Deco_member_exp x -> R.Case ("Deco_member_exp",
        map_decorator_member_expression env x
      )
    | `Deco_call_exp x -> R.Case ("Deco_call_exp",
        map_decorator_call_expression env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_decorator_call_expression (env : env) ((v1, v2) : CST.decorator_call_expression) =
  let v1 = map_anon_choice_id_b8f8ced env v1 in
  let v2 = map_arguments env v2 in
  R.Tuple [v1; v2]

and map_destructuring_pattern (env : env) (x : CST.destructuring_pattern) =
  (match x with
  | `Obj_pat (v1, v2, v3) -> R.Case ("Obj_pat",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pair_pat_3ff9cbe env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_pair_pat_3ff9cbe env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Array_pat (v1, v2, v3) -> R.Case ("Array_pat",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_formal_parameter env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_formal_parameter env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2) -> R.Case ("Export_choice_STAR_from_clause_choice_auto_semi",
      let v1 = (* "export" *) token env v1 in
      let v2 =
        (match v2 with
        | `STAR_from_clause_choice_auto_semi (v1, v2, v3) -> R.Case ("STAR_from_clause_choice_auto_semi",
            let v1 = (* "*" *) token env v1 in
            let v2 = map_from_clause env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Name_import_export_from_clause_choice_auto_semi (v1, v2, v3) -> R.Case ("Name_import_export_from_clause_choice_auto_semi",
            let v1 = map_namespace_import_export env v1 in
            let v2 = map_from_clause env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Export_clause_from_clause_choice_auto_semi (v1, v2, v3) -> R.Case ("Export_clause_from_clause_choice_auto_semi",
            let v1 = map_export_clause env v1 in
            let v2 = map_from_clause env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Export_clause_choice_auto_semi (v1, v2) -> R.Case ("Export_clause_choice_auto_semi",
            let v1 = map_export_clause env v1 in
            let v2 = map_semicolon env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Rep_deco_export_choice_decl (v1, v2, v3) -> R.Case ("Rep_deco_export_choice_decl",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 = (* "export" *) token env v2 in
      let v3 =
        (match v3 with
        | `Decl x -> R.Case ("Decl",
            map_declaration env x
          )
        | `Defa_choice_decl (v1, v2) -> R.Case ("Defa_choice_decl",
            let v1 = (* "default" *) token env v1 in
            let v2 =
              (match v2 with
              | `Decl x -> R.Case ("Decl",
                  map_declaration env x
                )
              | `Exp_choice_auto_semi (v1, v2) -> R.Case ("Exp_choice_auto_semi",
                  let v1 = map_expression env v1 in
                  let v2 = map_semicolon env v2 in
                  R.Tuple [v1; v2]
                )
              )
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
      map_jsx_element_ env x
    )
  | `Jsx_frag x -> R.Case ("Jsx_frag",
      map_jsx_fragment env x
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Choice_member_exp x -> R.Case ("Choice_member_exp",
            map_lhs_expression env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Augm_assign_exp (v1, v2, v3) -> R.Case ("Augm_assign_exp",
      let v1 = map_augmented_assignment_lhs env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ",
            (* ">>>=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
            (* "**=" *) token env tok
          )
        | `AMPAMPEQ tok -> R.Case ("AMPAMPEQ",
            (* "&&=" *) token env tok
          )
        | `BARBAREQ tok -> R.Case ("BARBAREQ",
            (* "||=" *) token env tok
          )
        | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ",
            (* "??=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Await_exp (v1, v2) -> R.Case ("Await_exp",
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `Typeof tok -> R.Case ("Typeof",
            (* "typeof" *) token env tok
          )
        | `Void tok -> R.Case ("Void",
            (* "void" *) token env tok
          )
        | `Delete tok -> R.Case ("Delete",
            (* "delete" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ternary_qmark *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `New_exp x -> R.Case ("New_exp",
      map_new_expression env x
    )
  | `Yield_exp (v1, v2) -> R.Case ("Yield_exp",
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | `STAR_exp (v1, v2) -> R.Case ("STAR_exp",
            let v1 = (* "*" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_exp opt -> R.Case ("Opt_exp",
            (match opt with
            | Some x -> R.Option (Some (
                map_expression env x
              ))
            | None -> R.Option None)
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expressions env v1 in
  let v2 = map_semicolon env v2 in
  R.Tuple [v1; v2]

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Seq_exp x -> R.Case ("Seq_exp",
      map_sequence_expression env x
    )
  )

and map_field_definition (env : env) ((v1, v2, v3, v4) : CST.field_definition) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "static" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_property_name env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_initializer_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_statement_block env v2 in
  R.Tuple [v1; v2]

and map_for_header (env : env) ((v1, v2, v3, v4, v5) : CST.for_header) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Choice_choice_member_exp x -> R.Case ("Choice_choice_member_exp",
        (match x with
        | `Choice_member_exp x -> R.Case ("Choice_member_exp",
            map_lhs_expression env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        )
      )
    | `Var_choice_id_opt_init (v1, v2, v3) -> R.Case ("Var_choice_id_opt_init",
        let v1 = (* "var" *) token env v1 in
        let v2 = map_anon_choice_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_initializer_ env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Choice_let_choice_id (v1, v2) -> R.Case ("Choice_let_choice_id",
        let v1 = map_anon_choice_let_ca16eb3 env v1 in
        let v2 = map_anon_choice_id_940079a env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 =
    (match v3 with
    | `In tok -> R.Case ("In",
        (* "in" *) token env tok
      )
    | `Of tok -> R.Case ("Of",
        (* "of" *) token env tok
      )
    )
  in
  let v4 = map_expressions env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Assign_pat (v1, v2, v3) -> R.Case ("Assign_pat",
      let v1 = map_pattern env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_formal_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_formal_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_jsx_attribute_ (env : env) (x : CST.jsx_attribute_) =
  (match x with
  | `Jsx_attr (v1, v2) -> R.Case ("Jsx_attr",
      let v1 = map_jsx_attribute_name env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_jsx_attribute_value env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Jsx_exp x -> R.Case ("Jsx_exp",
      map_jsx_expression env x
    )
  )

and map_jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  (match x with
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Jsx_exp x -> R.Case ("Jsx_exp",
      map_jsx_expression env x
    )
  | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
      map_jsx_element_ env x
    )
  | `Jsx_frag x -> R.Case ("Jsx_frag",
      map_jsx_fragment env x
    )
  )

and map_jsx_child (env : env) (x : CST.jsx_child) =
  (match x with
  | `Jsx_text tok -> R.Case ("Jsx_text",
      (* pattern [^{}<>]+ *) token env tok
    )
  | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
      map_jsx_element_ env x
    )
  | `Jsx_frag x -> R.Case ("Jsx_frag",
      map_jsx_fragment env x
    )
  | `Jsx_exp x -> R.Case ("Jsx_exp",
      map_jsx_expression env x
    )
  )

and map_jsx_element_ (env : env) (x : CST.jsx_element_) =
  (match x with
  | `Jsx_elem (v1, v2, v3) -> R.Case ("Jsx_elem",
      let v1 = map_jsx_opening_element env v1 in
      let v2 = R.List (List.map (map_jsx_child env) v2) in
      let v3 = map_jsx_closing_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Jsx_self_clos_elem (v1, v2, v3, v4, v5) -> R.Case ("Jsx_self_clos_elem",
      let v1 = (* "<" *) token env v1 in
      let v2 = map_jsx_element_name env v2 in
      let v3 = R.List (List.map (map_jsx_attribute_ env) v3) in
      let v4 = (* "/" *) token env v4 in
      let v5 = (* ">" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Seq_exp x -> R.Case ("Seq_exp",
            map_sequence_expression env x
          )
        | `Spread_elem x -> R.Case ("Spread_elem",
            map_spread_element env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_jsx_fragment (env : env) ((v1, v2, v3, v4, v5, v6) : CST.jsx_fragment) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* ">" *) token env v2 in
  let v3 = R.List (List.map (map_jsx_child env) v3) in
  let v4 = (* "<" *) token env v4 in
  let v5 = (* "/" *) token env v5 in
  let v6 = (* ">" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_jsx_element_name env v2 in
  let v3 = R.List (List.map (map_jsx_attribute_ env) v3) in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration) =
  let v1 = map_anon_choice_let_ca16eb3 env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_semicolon env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lhs_expression (env : env) (x : CST.lhs_expression) =
  (match x with
  | `Member_exp x -> R.Case ("Member_exp",
      map_member_expression env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Choice_unde x -> R.Case ("Choice_unde",
      map_identifier_ env x
    )
  | `Choice_get x -> R.Case ("Choice_get",
      map_reserved_identifier env x
    )
  | `Dest_pat x -> R.Case ("Dest_pat",
      map_destructuring_pattern env x
    )
  )

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 = map_anon_choice_exp_9cd0ed5 env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `QMARKDOT tok -> R.Case ("QMARKDOT",
        (* "?." *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
        (* private_property_identifier *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.method_definition) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "static" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Get tok -> R.Case ("Get",
            (* "get" *) token env tok
          )
        | `Set tok -> R.Case ("Set",
            (* "set" *) token env tok
          )
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_property_name env v5 in
  let v6 = map_call_signature env v6 in
  let v7 = map_statement_block env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_new_expression (env : env) ((v1, v2, v3) : CST.new_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 =
    (match v2 with
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    | `New_exp x -> R.Case ("New_exp",
        map_new_expression env x
      )
    )
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Choice_member_exp x -> R.Case ("Choice_member_exp",
      map_lhs_expression env x
    )
  | `Rest_pat x -> R.Case ("Rest_pat",
      map_rest_pattern env x
    )
  )

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Member_exp x -> R.Case ("Member_exp",
      map_member_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Choice_unde x -> R.Case ("Choice_unde",
      map_identifier_ env x
    )
  | `Choice_get x -> R.Case ("Choice_get",
      map_reserved_identifier env x
    )
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Temp_str x -> R.Case ("Temp_str",
      map_template_string env x
    )
  | `Regex (v1, v2, v3, v4) -> R.Case ("Regex",
      let v1 = (* "/" *) token env v1 in
      let v2 = (* regex_pattern *) token env v2 in
      let v3 = map_imm_tok_slash env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* pattern [a-z]+ *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "null" *) token env tok
    )
  | `Import tok -> R.Case ("Import",
      (* import *) token env tok
    )
  | `Obj (v1, v2, v3) -> R.Case ("Obj",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pair_20c9acd env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_pair_20c9acd env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Array (v1, v2, v3) -> R.Case ("Array",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Func (v1, v2, v3, v4, v5) -> R.Case ("Func",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_call_signature env v4 in
      let v5 = map_statement_block env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Arrow_func (v1, v2, v3, v4) -> R.Case ("Arrow_func",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Choice_choice_get x -> R.Case ("Choice_choice_get",
            map_anon_choice_rese_id_9a83200 env x
          )
        | `Formal_params x -> R.Case ("Formal_params",
            map_call_signature env x
          )
        )
      in
      let v3 = (* "=>" *) token env v3 in
      let v4 =
        (match v4 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Stmt_blk x -> R.Case ("Stmt_blk",
            map_statement_block env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Gene_func (v1, v2, v3, v4, v5, v6) -> R.Case ("Gene_func",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* "*" *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_call_signature env v5 in
      let v6 = map_statement_block env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Class (v1, v2, v3, v4, v5) -> R.Case ("Class",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 = (* "class" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_class_heritage env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_class_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Meta_prop (v1, v2, v3) -> R.Case ("Meta_prop",
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "target" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  )

and map_property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_id_0e3c97f env x
    )
  | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
      (* private_property_identifier *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Comp_prop_name (v1, v2, v3) -> R.Case ("Comp_prop_name",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_lhs_expression env v2 in
  R.Tuple [v1; v2]

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    (match v3 with
    | `Seq_exp x -> R.Case ("Seq_exp",
        map_sequence_expression env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Export_stmt x -> R.Case ("Export_stmt",
      map_export_statement env x
    )
  | `Import_stmt (v1, v2, v3) -> R.Case ("Import_stmt",
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | `Import_clause_from_clause (v1, v2) -> R.Case ("Import_clause_from_clause",
            let v1 = map_import_clause env v1 in
            let v2 = map_from_clause env v2 in
            R.Tuple [v1; v2]
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Debu_stmt (v1, v2) -> R.Case ("Debu_stmt",
      let v1 = (* "debugger" *) token env v1 in
      let v2 = map_semicolon env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Stmt_blk x -> R.Case ("Stmt_blk",
      map_statement_block env x
    )
  | `If_stmt (v1, v2, v3, v4) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Switch_stmt (v1, v2, v3) -> R.Case ("Switch_stmt",
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Lexi_decl x -> R.Case ("Lexi_decl",
            map_lexical_declaration env x
          )
        | `Var_decl x -> R.Case ("Var_decl",
            map_variable_declaration env x
          )
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Empty_stmt tok -> R.Case ("Empty_stmt",
            (* ";" *) token env tok
          )
        )
      in
      let v4 =
        (match v4 with
        | `Exp_stmt x -> R.Case ("Exp_stmt",
            map_expression_statement env x
          )
        | `Empty_stmt tok -> R.Case ("Empty_stmt",
            (* ";" *) token env tok
          )
        )
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `For_in_stmt (v1, v2, v3, v4) -> R.Case ("For_in_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_for_header env v3 in
      let v4 = map_statement env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `While_stmt (v1, v2, v3) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Do_stmt (v1, v2, v3, v4, v5) -> R.Case ("Do_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Try_stmt (v1, v2, v3, v4) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_statement_block env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_catch_clause env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_finally_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `With_stmt (v1, v2, v3) -> R.Case ("With_stmt",
      let v1 = (* "with" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Brk_stmt (v1, v2, v3) -> R.Case ("Brk_stmt",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_stmt (v1, v2, v3) -> R.Case ("Cont_stmt",
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Throw_stmt (v1, v2, v3) -> R.Case ("Throw_stmt",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Labe_stmt (v1, v2, v3) -> R.Case ("Labe_stmt",
      let v1 = map_anon_choice_id_0e3c97f env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_expression) =
  let v1 = map_anon_choice_exp_9cd0ed5 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?." *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_expressions env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Switch_case x -> R.Case ("Switch_case",
          map_switch_case env x
        )
      | `Switch_defa x -> R.Case ("Switch_defa",
          map_switch_default env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = R.List (List.map (map_statement env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = R.List (List.map (map_statement env) v3) in
  R.Tuple [v1; v2; v3]

and map_template_string (env : env) ((v1, v2, v3) : CST.template_string) =
  let v1 = (* "`" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Temp_chars tok -> R.Case ("Temp_chars",
          (* template_chars *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      | `Temp_subs x -> R.Case ("Temp_subs",
          map_template_substitution env x
        )
      )
    ) v2)
  in
  let v3 = (* "`" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution) =
  let v1 = (* "${" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) -> R.Case ("Exp_choice_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_PLUSPLUS_exp (v1, v2) -> R.Case ("Choice_PLUSPLUS_exp",
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.variable_declaration) =
  let v1 = (* "var" *) token env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_semicolon env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) =
  let v1 = map_anon_choice_id_940079a env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_initializer_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* pattern #!.* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_statement env) v2) in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
