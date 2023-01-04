type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* 1 Convert Grammar *)
let rec generate_fun_rules r = function
	| [] -> []
	| (nt, rules)::tail -> if r = nt then rules::(generate_fun_rules r tail)
	else generate_fun_rules r tail

let convert_grammar gram1 = 
	(fst gram1, fun r -> generate_fun_rules r (snd gram1))

(* 2 Parse Tree *)
let rec parse_tree_helper t = match t with
  | [] -> []
  | head::tail -> match head with
    | Leaf l -> l::(parse_tree_helper tail)
    | Node(_, child) -> (parse_tree_helper child)@(parse_tree_helper tail)

let parse_tree_leaves t = match t with
  | Leaf l -> [l];
  | Node(_, child) -> parse_tree_helper child

(* 3 Match Grammar *)
let rec match_frag g_fun t_rule symbol acceptor frag = match frag with
  | [] -> None
  | head::tail -> if symbol = head then match_one_rule g_fun t_rule acceptor tail
                  else None

and match_one_rule g_fun r_list acceptor frag = match r_list with
  | [] -> acceptor frag
  | head::tail -> match_symbol_type head tail g_fun acceptor frag

and match_symbol_type h_rule t_rule g_fun acceptor frag = match h_rule with
  | T symbol -> match_frag g_fun t_rule symbol acceptor frag
  | N symbol -> let next_acceptor = match_one_rule g_fun t_rule acceptor in
                let next_rules = g_fun symbol in
                matcher g_fun next_rules next_acceptor frag

and matcher g_fun r_list acceptor frag = match r_list with
  | [] -> None (* no more rules *)
  | head::tail -> let h_match = match_one_rule g_fun head acceptor frag in
                 let t_match = matcher g_fun tail acceptor frag in
                 if h_match = None then t_match
                 else h_match

let make_matcher gram = 
    let g_fun = (snd gram) in
    let r_start = (g_fun (fst gram)) in
  fun acceptor frag -> matcher g_fun r_start acceptor frag


(* 4 Parse Grammar *)
let empty_acceptor frag path = match frag with
  | [] -> Some path
  | _ -> None

let rec parse_frag g_fun t_rule symbol acceptor frag path = match frag with
  | [] -> None
  | head::tail -> if symbol = head then parse_one_rule g_fun t_rule acceptor tail path
                  else None 

and parse_one_rule g_fun r_list acceptor frag path = match r_list with 
  | [] -> acceptor frag path
  | head::tail -> parse_symbol_type head tail g_fun acceptor frag path

and parse_symbol_type h_rule t_rule g_fun acceptor frag path = match h_rule with
  | T symbol -> parse_frag g_fun t_rule symbol acceptor frag path
  | N symbol -> let next_acceptor = parse_one_rule g_fun t_rule acceptor in
                let next_rules = g_fun symbol in
                parser g_fun next_rules next_acceptor symbol path frag

and parser g_fun r_list acceptor start path frag = match r_list with
  | [] -> None
  | head::tail -> let match_result = parse_one_rule g_fun head acceptor frag ((start, head)::path) in
                  match match_result with 
                    | None -> parser g_fun tail acceptor start path frag
                    | _ -> match_result

(* Generating the Tree *)
let rec get_children rest_path r = match r with
  | [] -> rest_path, []
  | head::tail -> match_rule_symbol rest_path head tail

and match_rule_symbol rest_path h_rule t_rule = match h_rule with 
  | T symbol -> let children = get_children rest_path t_rule in
             (fst children), (Leaf symbol)::(snd children)
  | N symbol -> let tree = generate_tree rest_path in
                let children = get_children (fst tree) t_rule in
                (fst children), (snd tree)::(snd children)
    
and generate_tree path = match path with
  | head::tail -> let symbol = fst head in
                  let result = get_children tail (snd head) in
                  let children = snd result in
                  (fst result), Node (symbol, children)

let make_parse_tree parser frag = 
  let tree_path = parser frag in 
  match tree_path with
  | Some p -> let this_path = List.rev p in 
              Some (snd (generate_tree this_path))
  | _ -> None

let make_parser gram = 
  let g_fun = (snd gram) in
  let start_symbol = (fst gram) in
  let r_start = (g_fun start_symbol) in
  let path_fun = parser g_fun r_start empty_acceptor start_symbol [] in
  make_parse_tree path_fun

