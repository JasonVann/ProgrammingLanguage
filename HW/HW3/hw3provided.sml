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

fun longest_string_helper f =
  foldl (fn (x,y) =>
	 if f((String.size x), (String.size y))
		     then x
		     else
			 y)
	     ""
 
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string =
    (String.implode o List.rev o String.explode)

fun first_answer f a =
  let val ans = List.filter (fn x => case f x of
					 NONE => false
				       | SOME v => true) a
  in
      case ans of
	  [] => raise NoAnswer
	| x::xs => (case (f x) of
		       SOME x' => x'
		    )
  end

fun all_answers f a =
  let val check_NONE = List.filter (fn x => case f x of
					 NONE => true
				       | SOME v => false) a
  in
      case check_NONE of
	  [] => (*SOME List.foldl(fn (x,y) => f x @ (f y)) [] a *)
	  SOME (List.concat (List.mapPartial f a))
	| _ => NONE
  end
      
fun match (v, p) =
	case p of
	    Wildcard => SOME []
	  | Variable s => SOME [(s,v)]
	  | UnitP => (case v of
			  Unit => SOME []
			| _ => (print "a"; NONE) ) 
	  | ConstP s => (case v of
			     Const s' => if s = s' then  SOME []
					    else
						NONE
			   | _ => NONE)
	  | TupleP ps => (case v of
			      Tuple vs => if List.length(ps) = List.length(vs)
					  then
					      (*List.foldl(fn (p,i) => match(p,i) ) []
							ListPair.zip(vs, ps) *)
					       all_answers(fn x =>
							     case x of
								 (a,b) => match(a,b)
							 )
							(* has to use brackets here *)
							(ListPair.zip(vs, ps))
					  else
					      NONE
			   | _ => NONE ) 
	  | ConstructorP(s1, p) => (case v of
					Constructor(s2, v) => let val temp = match (v, p)
							      in
								  if s1 = s2
								     andalso
								     not (temp = NONE)
								  then
								      temp
								  else
								      NONE
							      end 
					
				     | _ => NONE) 
(*	  | _ => NONE *)
      	      
fun first_match v ps =
  SOME (first_answer (fn p => if match(v, p) = NONE then NONE else match(v, p) )
	       ps)  handle NoAnswer => NONE 
