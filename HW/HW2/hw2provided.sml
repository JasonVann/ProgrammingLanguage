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
fun card_color(a_suit, a_rank) =
  case a_suit of
      Clubs => Black
    | Spades => Black
    | _ => Red

fun card_value(a_suit, a_rank) =
  case a_rank of
      Num i => i
    | Ace => 11
    | _ => 10

fun remove_card(cs, c, e) =
  let fun aux(cs, res) =
	case cs of
	    [] => raise e
	  | c'::cs' => if c' = c
		      then res @ cs'
		      else
			  aux(cs', res @ [c'])
  in
      aux(cs, [])
  end

fun all_same_color(cs) =
  case cs of
      [] => true
    | c::[] => true
    | c::c2::cs => if card_color c = card_color c2
		   then all_same_color(c2::cs)
		   else
		       false
		       
fun sum_cards(cs) =
  let fun aux(cs, res) =
	case cs of
	    [] => res
	    | c::cs' => aux(cs', res + card_value(c))
  in
      aux(cs, 0)
  end

fun score(cs, goal) =
  let val sum = sum_cards(cs)
      val preliminary_score = if sum > goal
			      then 3 * (sum - goal)
			      else
				  goal - sum
  in
      if all_same_color(cs)
      then
	  preliminary_score div 2
      else
	  preliminary_score
  end

fun officiate(card_list, move_list, goal) =
  let val e = IllegalMove
      fun aux(held_list, card_list, move_list) =
	case move_list of
	    [] => score(held_list, goal)
	  | Discard c :: ml => aux(remove_card(held_list, c, e), card_list, ml)
	  | Draw :: ml => case card_list of
			      [] => score(held_list, goal)
			   | c::cs => 
			     if sum_cards(held_list@[c]) > goal
			     then
				 score(held_list@[c], goal)
			     else
				 aux(held_list @ [c], cs, ml)
  in
      aux([], card_list, move_list)
  end

(*
fun score_challenge(cs, goal) =
  let fun has_A(cs) =
	case cs of
	    [] => false
	  | c::cs' => if card_value(c) = 11
		      then true
		      else
			  has_A(cs')

      val sum = sum_cards(cs)
      fun preli_score(sum) =
	let val temp = 
	    if sum > goal
	    then 3 * (sum - goal)
	    else
		goal - sum
	in
	    if all_same_color(cs)
	    then
		temp div 2
	    else
		temp
	end
	    
  end
  in
      case cs of
	  [] => 
  end
*)

fun careful_player(cs, goal) =
  let val e = IllegalMove
      fun should_discard(cs, hl, goal, temp) =
      (* true if by discarding a card and then drawing the 1st card of cs will give a score of 0 *)
	case cs of
	    [] => (false, (Hearts, Num 1)) (* return a dummy card so we can easily extract card in the true case*)
	  | c::cs' =>  case hl of
			  [] => (false, (Hearts, Num 1))
			| h::hl' =>  if score(temp @ remove_card(hl, h, e)@[c], goal) = 0
				     then
					 (true, h)
				     else
					 should_discard(cs, hl', goal, temp@[h])
      (* fun discard_largest(cs, hl, goal, temp) =
	*)
      fun aux(cs, hl, ml) =
	if score(hl, goal) = 0
	then
	    ml
	else
	    let val (ans, discard_card) =  should_discard(cs, hl, goal, [])
	    in
	    if ans = true
	    then
		ml @ [Discard(discard_card)] @ [Draw]
	    else
		
		if goal > score(hl, goal) + 10
		then
		    case cs of
			[] => aux(cs, hl, ml @ [Draw])
		      | c::cs' => aux(remove_card(cs, c, e), hl@[c], ml @ [Draw])
		else
		    case cs of
			c::cs' => if score(hl @ [c], goal) < goal
				  (* if safe to draw *)
				  then
				      aux(remove_card(cs, c, e), hl @[c], ml @ [Draw])
				  else
				      (* pop the 1st card *)
				      (case hl of
					  [] => aux(remove_card(cs, c, e), hl@[c], ml@[Draw])
					| h::hl' => aux(cs, remove_card(hl, h, e), ml @ [Discard(h)])
				      )
		      | _ =>
			(* card list is empty *)
			ml
			(*
			case hl of
			   [] => ml
			   | h::hl' => aux(cs, remove_card(hl, h, e), ml @ [Discard(h)]) *)

	    end
		
  in
      aux(cs, [], [])
  end
      
	
	
