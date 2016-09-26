(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test11 = only_capitals ["a", "def", "CEFa"] = ["CEFa"];

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bce","CDE"] = "bce";

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["A","bce","CDE"] = "CDE";

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized ["A", "bc", "DE"] = "DE";

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;

val test71 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] handle NoAnswer => 99) = 99;
										    
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x >= 1 then SOME [x] else NONE) [2,3,4,5,6,7,1] = SOME [2,3,4,5,6,7,1];

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (Variable("10")) = 0;

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1;
val test9b1 = count_wild_and_variable_lengths (Variable("abcd")) = 4;
val c = TupleP [Wildcard, Variable("a"), Variable("efd")];
val test9b2 = count_wild_and_variable_lengths c = 5;
	
val test9c = count_some_var ("x", Variable("x")) = 1;
val b = TupleP [Wildcard, Wildcard, Wildcard, Variable("a"), Variable("cd"), Variable("a")];
val test9c2 = count_some_var("a", b) = 2;

val test10 = check_pat (Variable("x")) = true;
val test101 = check_pat (TupleP [Wildcard, Variable "a", Variable "c", Variable "efsdf", Variable "c"]) = false;
val test102 = check_pat (TupleP [Wildcard, Variable "a", Variable "c", Variable "efsdf", Variable "c1"]) = true;

val test11 = match (Const(1), UnitP) = NONE
val test110 = match(Constructor("a", Const 3), ConstructorP("a", ConstP 3));
val test111 = match(Unit, Variable "a") = SOME [("a", Unit)];
val test112 = match(Tuple [Const 5, Const 3], TupleP [ConstP 5, ConstP 3]) = SOME [];
val test113 = match(Tuple [Const 5, Const 3], TupleP [ConstP 5, ConstP 6]) = NONE;

val test12 = first_match Unit [UnitP] = SOME [];


