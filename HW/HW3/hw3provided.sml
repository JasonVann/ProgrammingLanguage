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

fun count_wildcards p =
  let fun f1 x = 1
      fun f2 x = 0	     
  in
      g f1 f2 p
  end

fun count_wild_and_variable_lengths p =
  let fun f1 x = 1
      fun f2 x = String.size x
  in
      g f1 f2 p
  end

fun count_some_var (str, p) =
  let fun f1 x = 0
      fun f2 x = if x = str then 1 else 0
  in
      g f1 f2 p
  end

fun check_pat p =
    let 
	fun f1 x = []
	fun f2 x = [x]	       
	fun get_all_vars p =
	  case p of
	      Wildcard          => f1 ()
	    | Variable x        => f2 x
	    | TupleP ps         => List.foldl (fn (p,i) => (get_all_vars p) @ i) [] ps 
	    | ConstructorP(_,p) => get_all_vars p
	    | _                 => []
	val all_vars = get_all_vars p
				    
	fun has_dup x =
	  case x of
	      [] => false
	    | x1::xs => if List.exists (fn y => y = x1) xs then true
			else
			    has_dup xs
    in
	not (has_dup all_vars)
    end

fun match (v, p) =
  
  
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

(* the type for f doesn't quite match with the requirement *)
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

(* ?? returns 'b list option ?? as oppsed to 'a list option *)
fun first_answer f a =
  let val ans = List.filter (fn x => case f x of
					 NONE => false
				       | SOME v => true) a
  in
      case ans of
	  [] => raise NoAnswer
	| x::xs => x
  end

fun all_answers f a =
  let val check_NONE = List.filter (fn x => case f x of
					 NONE => true
				       | SOME v => false) a
  in
      case check_NONE of
	  [] => SOME a
	| _ => NONE
  end
      
(*
fun all_answers f a =
  let fun g (x, y) = case (f x) of
			 NONE => NONE
		       | SOME v => SOME v @ 				     
			 (case (f y) of
				  NONE => NONE
				  | SOME v' => SOME v' )

  in
      SOME a
  end
*)


  
