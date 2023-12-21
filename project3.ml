

(***SCANNNER***)

type token = Tok_Char of char 
  | Tok_OR
  | Tok_Q
  | Tok_Period
  | Tok_LPAREN
  | Tok_RPAREN
  | Tok_END

exception IllegalExpression of string

let re_char = Str.regexp "[0-9A-Za-z ]"

let tokenize str =
  let rec tok pos s =
    if pos >= String.length s then
      [Tok_END]
    else if Str.string_match re_char s pos then
      let token = Str.matched_string s in
       Tok_Char token.[0] :: tok (pos+1) s
    else
      match s.[pos] with
      | '|' -> Tok_OR :: tok (pos+1) s
      | '?' -> Tok_Q :: tok (pos+1) s
      | '.' -> Tok_Period :: tok (pos+1) s
      | '(' -> Tok_LPAREN :: tok (pos+1) s
      | ')' -> Tok_RPAREN :: tok (pos+1) s
      | _ ->
        raise (IllegalExpression "tokenize")
  in
  tok 0 str


(***PARSER***)

exception ParseError of string

let tok_list = ref []

let lookahead () =
 match !tok_list with
   [] -> raise (ParseError "no tokens")
 | (h::_) -> h
 
let match_tok a =
 match !tok_list with
 (* checks lookahead; advances on match *)
 | (h::t) when a = h -> tok_list := t
 | _ -> raise (ParseError "bad match")

type re = C of char 
  | AnyChar
  | Concat of re * re
  | Optional of re
  | Alternation of re * re

let rec parse_E ()=
 let a1 = parse_T () in
  let t = lookahead () in
   match t with
    Tok_OR ->
     match_tok Tok_OR;
     let a2 = parse_E () in
      Alternation(a1,a2)
    |_ -> a1

and parse_T () =
 let a1 = parse_F () in
  let t = lookahead () in
   match t with
    Tok_Char _->
     let a2 = parse_T () in
      Concat(a1,a2)
    |Tok_LPAREN|Tok_Period ->
     let a2 = parse_T () in
      Concat(a1,a2)
    | _ -> a1	
 
and parse_F () = 
 let a1 = parse_A () in
  let t = lookahead () in
   match t with
    Tok_Q ->
     match_tok Tok_Q;
     Optional(a1)
    |_ -> a1

and parse_A () = 
 let t = lookahead () in
  match t with
   |Tok_LPAREN ->
    match_tok Tok_LPAREN;
    let a1 = parse_E () in
     let p = lookahead () in
      (match p with
       Tok_RPAREN ->
        match_tok Tok_RPAREN;
        a1
       |_ -> raise (ParseError "Parenthesis not closed"))
   |_ ->
    parse_C ()

and parse_C () =
 let t = lookahead () in
  match t with
   Tok_Char c ->
    match_tok(Tok_Char c);
    C (c)
   |Tok_Period ->
    match_tok Tok_Period;
    AnyChar
   |_ -> raise (ParseError "parse_C") 

let parse str =
 tok_list := (tokenize str);
 let exp = parse_E () in
  if !tok_list <> [Tok_END] then
   raise (ParseError "parse_E")
  else
   exp
;;
 

(***Matcher***)
let match_pattern pattern str =
 let rec match_expr expr str pos =
   match expr with
   | C c ->
     if pos < String.length str && str.[pos] = c then
       Some (pos + 1)
     else
       None
   | AnyChar ->
     if pos < String.length str then
       Some (pos + 1)
     else
       None
   | Concat (e1, e2) ->
     (match match_expr e1 str pos with
     | Some new_pos -> 
      (match match_expr e2 str new_pos with
      |Some final_pos -> Some final_pos
      |None -> None)
     | None -> None)
   | Optional e ->
     (match match_expr e str pos with
     | Some(new_pos) -> Some new_pos
     | None -> Some pos)
   | Alternation (e1, e2) ->
     match match_expr e1 str pos with 
     | Some _ as result -> result
     | None -> match_expr e2 str pos
 in match match_expr pattern str 0 with
  |Some pos when pos = String.length str -> print_endline("match")
  |_ -> print_endline("no match")


let main =
 print_string "pattern? ";
 flush stdout;
 let input_pattern = input_line stdin in
 let pattern = parse input_pattern in
 let rec ask_match() =
  print_string "match? ";
  flush stdout;
  let input_match = input_line stdin in
   match_pattern pattern input_match;
   ask_match() 	
  in ask_match()


let _ = main 
