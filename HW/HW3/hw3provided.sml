(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(strs) =
  List.filter (fn x => Char.isUpper(String.sub(x, 0)))  strs

fun longest_string1(strs) =
  foldl (fn (x, y) => if (String.size x) > (String.size y) then x else y) "" strs

fun longest_string2(strs) =
  foldl (fn (x, y) => if (String.size x) >= (String.size y) then x else y) "" strs

(* the type for f doesn't quite meet with the requirement *)
fun longest_string_helper f strs =
  foldl f "" strs
	
fun longest_string3(strs) =
  let fun f (x, y) = if (String.size x) > (String.size y)
		     then x
		     else
			 y
  in
      longest_string_helper f strs
  end

fun longest_string4(strs) =
  let fun f (x, y) = if (String.size x) >= (String.size y)
		     then x
		     else
			 y
  in
      longest_string_helper f strs
  end


fun longest_capitalized(strs) =
  let fun f(x, y) = if (String.size x) > (String.size y) andalso Char.isUpper(String.sub(x,0))
		    then
			(* (print "b "; print x; print "_"; print y; x) *)
			x
		    else
			(* (print "a "; print x; print "_"; print y; y) *)
			y
  in
      longest_string_helper f strs
  end

fun rev_string(str) =
    (String.implode o List.rev o String.explode)  str

fun first_answer f a =
  let fun g (x, y) = case (f x) of
		      NONE => (case (f y) of
				  NONE => NONE
				       | SOME v' => SOME v' )
		    | SOME v => SOME v
  in
      foldr g NONE a
  end
      
