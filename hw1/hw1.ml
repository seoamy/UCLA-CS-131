type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* 1 Subset *)
(* Base case: If a is an empty list, then it will always be a subset of b *)
(* Recursive case: If there is at least one element in b that matches head of a, 
		then call subset on the tail of a with b *)
(* End case: If no element in b matches the head of a, then a is not a subset of b *)
let rec subset a b = match a with 
	| [] -> true
	| h::t -> if List.exists (fun x -> x = h) b then subset t b
		else false;;

(* 2 Equal Sets *)
(* By mathematical definition, a and b are equal sets if a is a subset of b and b is a subset of a *)
let rec equal_sets a b =
	(subset a b) && (subset b a);;

(* 3 Set Union *)
let rec set_union a b = match a with
	| [] -> b
	| h::t -> if List.exists (fun x -> x = h) b then (set_union t b)
		else (set_union t b)@[h];;

(* 4 Set All Union *)
let rec set_all_union a = match a with
 	| [] -> []
 	| h::t -> (set_all_union t)@h;;

(* 5 Russell's Paradox 
	It is NOT possible to write an OCaml function to check if a set is a member of itself because we are
	representing sets as a list with type 'a list. To see if 'a list is a member of itself, 
	then the entire set would be a set of 'a list, which would be represented as 'a list list, which is
	contradictory since they would be different types ('a list vs. 'a list list). Therefore, it is NOT
	possible to check if a set is a member of itself with a function written in OCaml.
*)

(* 6 Computed Fixed Point *)
let rec computed_fixed_point eq f x = 
	if eq (f x) x then x
	else computed_fixed_point eq f (f x);;

(* 7 Computed Periodic Point *)
let rec cpphelper f p x =
	if p = 0 then x
	else if p = 1 then f x
	else f(cpphelper f (p - 1) x);;

let rec computed_periodic_point eq f p x =
	if eq (cpphelper f p x) x then x
	else computed_periodic_point eq f p (f x);;


(* 8 Whileseq *)
let rec whileseq s p x = 
	if not (p x) then []
	else x::(whileseq s p (s x));;

(* ================================================= *)
(* 9 Blind alley rules *)
let equal_sets_tuple (a1, b1) (a2, b2) = equal_sets b1 b2

(* 	Inputs: r is a singular rule e.g. "N Num" 
			tlist is a list of terminal rules 
	Output: boolean of whether r is terminal
*)
let is_terminal r tlist = match r with
  | T _ -> true
  | N name -> if List.exists (fun x -> x = name) tlist then true
  	else false

(* 	Inputs: rlist is a list of rules e.g. [N Incrop; N Lvalue]
			tlist is a list of terminal rules 
	Output: boolean of whether all of r is terminal
*)
let rec check_rule rlist tlist  = match rlist with 
	| [] -> true
	| h::t -> (is_terminal h tlist) && (check_rule t tlist)

(* 	Inputs: rpair is the tuple of type and rules list e.g. (Expr, [N Expr; N Lvalue])
			tlist is a list of terminal rules 
	Output: list of all terminal rules
*)
let rec generate_list rpair tlist = match rpair with
	| [] -> tlist
	| (gtype, rlist)::tl -> if (check_rule rlist tlist) then (generate_list tl (gtype::tlist))
			else (generate_list tl tlist)

let generate_list_wrapper (r, l) = (r, (generate_list r l))

(* 	Inputs: r is the tuple of grammar type and all the rules
			l is the list of all terminal rules
			result is the current list of terminal grammars
	Output: result is the grammar with all blind alley rules removed
*)
let rec find_terminable_rules r l result = match r with
	| [] -> result
	| (gtype, rlist)::t -> if (check_rule rlist l) then (find_terminable_rules t l (result@[gtype, rlist]))
			else (find_terminable_rules t l result)

let filter_blind_alleys g = match g with 
	| a, b -> ((a), (find_terminable_rules b (snd(computed_fixed_point equal_sets_tuple generate_list_wrapper (b, []))) []))





