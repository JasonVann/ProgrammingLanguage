(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, strs) =
  let fun aux(has_seen, str, strs,res) =
	case strs of
	    [] => (has_seen, [])
	  | str'::strs' => if same_string(str, str')
			   then
			       (true,res @ strs')
			   else
			       aux(false, str, strs', res@[str'])
      val (has_seen, res) = aux(false,str, strs, [])
  in
      if has_seen
      then SOME (res)
      else
	  NONE
  end

fun get_substitutions1(substitutions, s) =
  case substitutions of
      [] => []
    | xs :: xss =>
      let val ans = all_except_option(s, xs)
      in
	  case ans of
	      NONE => get_substitutions1(xss,s)
	    | SOME res => res @ get_substitutions1(xss, s) 
      end

fun get_substitutions2(substitutions, s) =
  let fun aux(substitutions, s, res) =
	case substitutions of
	    [] => res
	  | xs :: xss =>
	    let val ans = all_except_option(s, xs)
	    in
		case ans of
		    NONE => aux(xss, s, res)
		  | SOME temp => aux(xss, s, res @ temp)
	    end	
  in
      aux(substitutions, s, [])
  end

fun similar_names(substitutions, {first = x, middle = y, last = z}) =
  let fun aux(substitutions) =
      case substitutions of
	  [] => []
	| xs::xss => {first=xs, middle = y, last = z} :: aux(xss)
							    
      val sub = get_substitutions2(substitutions, x)
  in
      {first = x, middle = y, last = z} :: aux(sub)
  end
      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
