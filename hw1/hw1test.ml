(* 1 Subset test cases *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;2;3] [3;6;7;2;2;2;9;1;1;1;1;1]
let my_subset_test2 = not (subset [0;5;6;3;7] [1])

(* 2 Equal sets test cases *)
let my_equal_sets_test0 = equal_sets [0;2] [0;0;2;2;0]
let my_equal_sets_test1 = not (equal_sets [1] [1;2;1])

(* 3 Set union test cases *)
let my_set_union_test0 = equal_sets (set_union [] [4;5;6]) [4;5;6]
let my_set_union_test1 = equal_sets (set_union [4;4;1] [4;5;7]) [1;4;5;7]

(* 4 Set all union test cases *)
let my_set_all_union_test0 =
	equal_sets (set_all_union [[1;2;3]; [3]; []; [2]; [1]; [1;2]]) [1;2;3]
let my_set_all_union_test1 =
  equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]

(* 6 Fixed point test cases *)
let my_computed_fixed_point_test0 =
	computed_fixed_point (=) (fun x -> x / 10) 1000 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 50.) 2. = infinity

(* 7 Periodic point test cases *)
let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x - (x * x)) 0 (2) = 2
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 2.) 4 (2.) = 2.

(* 8 Whileseq *)
let my_whileseq_test0 =
	whileseq ((+) 3) ((>) 10) 2 = [2;5;8]
let my_whileseq_test1 =
	whileseq ((+) 5) ((>) 15) 1 = [1;6;11]
let my_whileseq_test2 =
	whileseq (( * ) 2) ((>) 10) 1 = [1;2;4;8]

(* 9 Blind alley rules *)
type amys_nonterminals =
  | Phrase | Value | Op | Char

let amys_rules =
   [Phrase, [T"("; N Phrase; T")"];
    Phrase, [N Char];
    Phrase, [N Phrase; N Op];
    Phrase, [N Value];
    Phrase, [N Op; N Value];
    Phrase, [N Value; N Op];
    Value, [T"$"; N Phrase];
    Value, [T"#"; N Phrase];
    Value, [T"&"; N Phrase];
    Op, [T"+"];
    Op, [T"-"];
    Op, [T"/"];
    Op, [T"*"];
    Char, [T "a"];
    Char, [T "b"];
    Char, [T "c"]]

let amys_grammar = Phrase, amys_rules

let my_filter_blind_alleys_test0 =
  filter_blind_alleys amys_grammar = amys_grammar

let my_filter_blind_alleys_test1 =
  filter_blind_alleys (Phrase, List.tl amys_rules) = (Phrase, List.tl amys_rules)

let my_filter_blind_alleys_test2 =
    filter_blind_alleys (Phrase,
        [Phrase, [N Char];
        Phrase, [N Value];
        Phrase, [N Phrase; N Value];
        Phrase, [N Value; N Phrase];
        Phrase, [N Phrase; N Op; N Phrase];
        Value, [N Value; N Phrase];
        Value, [N Phrase; N Value];
        Value, [N Op; N Value];
        Value, [N Value; N Op];
        Op, [T"+"];
        Op, [T"-"];
        Op, [T"/"];
        Op, [T"*"];
        Char, [T "a"];
        Char, [T "b"];
        Char, [T "c"]])
    = (Phrase,
        [Phrase, [N Char];
        Phrase, [N Phrase; N Op; N Phrase];
        Op, [T"+"];
        Op, [T"-"];
        Op, [T"/"];
        Op, [T"*"];
        Char, [T "a"];
        Char, [T "b"];
        Char, [T "c"]])

