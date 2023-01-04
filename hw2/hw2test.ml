let accept_all x = Some x
let accept_empty_suffix = function
	[] -> Some []
	| _ -> None

type amys_nonterminals =
  | A | B | C | D | E

let amys_grammar =
  (A,
   function
     | A -> [[N B; N C]]
     | B -> [[N D]; [N D]; [N E]]
     | C -> [[T "car"]; [T "cap"]; [T "costco"]]
     | D -> [[T "dollar"]; [T "dollop"]]
     | E -> [[T "egg"]; [T "element"]; [T "episode"]])

let test_frag = ["episode";"costco"]

(* let rec_grammar = 
  (A, 
    function
      | A -> [[N B; N C]]
      | B -> [[N C]]
      | C -> [[N B]]) *)
  
(* let make_test = ((make_matcher rec_grammar accept_empty_suffix test_frag)) *)

(* 5 Test case for make_matcher *)
let make_matcher_test = ((make_matcher amys_grammar accept_empty_suffix test_frag) = Some [])

(* 6 Test case for make_matcher *)
let make_parser_test = match make_parser amys_grammar test_frag with
  | Some t -> parse_tree_leaves t = test_frag
  | _ -> false 