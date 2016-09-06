(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option("string", ["string1","abc"]) = NONE;
val test12 = all_except_option("string", ["string","abc"]) = SOME ["abc"];
val test13 = all_except_option("string", ["d", "string","abc"]) = SOME ["d", "abc"]
								  
val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Fredrick", "Freddie", "F"];
val test22 = get_substitutions1([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"];

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test31 = get_substitutions2([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Fredrick", "Freddie", "F"];
val test32 = get_substitutions2([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"];

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2;
val test61 = card_value(Clubs, Num 10) = 10;
val test62 = card_value(Clubs, Ace) = 11;
val test63 = card_value(Clubs, King) = 10;
					    
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test71 = remove_card ([(Hearts, Ace), (Hearts, King)], (Hearts, King), IllegalMove) = [(Hearts, Ace)];

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true;
val test81 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace)] = false;

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4;
val test91 = sum_cards [(Clubs, Num 2),(Clubs, King)] = 12;

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test11 = score ([(Hearts, Num 2),(Clubs, Num 10)],10) = 6;
val test12 = score ([(Hearts, Num 2),(Hearts, Num 10)],10) = 3;

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
									   
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val testb1 = careful_player([(Spades, Num 7)], 10);
val testb2 = careful_player([(Diamonds, Num 2), (Clubs, Ace)], 11);
val testb3 = careful_player([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18)
			   
