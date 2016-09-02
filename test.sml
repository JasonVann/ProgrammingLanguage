(* this is a 
comment 
; is not necessary in a file; necessary in REPL
*)

val x = 5;

(* x = 6 (* evaluates to false *)
 *)

val z = ~2; (* -2 *)
val z0 = abs z;
val e1 = true;
val e2 = 6;
val e3 = 7;
if e1 then e2 else e3; (* e2, e3 must have same type *)

val int_div = 3 div 2;
val real_div = 3.0 / 2.0;

val abs_of_z = if z < 0 then 0 - z else z;

(* y >= 0 *)
fun cube(x: int) = x * x * x
			       
fun pow (x: int, y: int) =
  if y = 0
  then 1
  else x * pow(x, y - 1)

val x = (2,3)
val ans = pow x

(* pair: ta * tb *)
val e = (3, 2);

#1 e;
#2 e;	      



fun swap (pr: int*bool) =
  (#2 pr, #1 pr)


(* Tuples: fixed number of pieces that may have different types *)

	      
(* Lists: any number of pieces that have the same type *)

val e1 = 0;
val e2 = [1,2,3];
e1 :: e2;

null e2;
hd e2;
tl e2;

fun sum_list(xs: int list) =
  if null xs
  then 0
  else
      hd xs + sum_list(tl xs)


fun append(xs: int list, ys: int list) =
  if null xs
  then ys
  else (hd xs) :: append((tl xs), ys)

fun firsts(xs: (int * int) list) =
  if null xs
  then []
  else (#1 (hd xs)) :: firsts(tl xs)

fun silly(z:int) =
  let
      val x = if z > 0 then z else 42
      val y = x + z + 9
  in
      if x > y then x * 2 else y * y
  end

(* nested function *)
fun countup_from1(x: int) =
  let
      fun count (from : int) =
	if from = x
	then [x]
	else from :: count(from + 1)
  in
      count(1)
  end

(* options *)
fun max(xs : int list) =
  if null xs
  then NONE
  else if null (tl xs)
  then SOME (hd xs)
  else
      let val tl_ans = max(tl xs)
      in
	  if isSome tl_ans andalso hd xs > (valOf tl_ans)
	  then SOME (hd xs)
	  else
	      tl_ans
      end


	  (*
SOME (1); (* int option *)
*)

valOf (max [1,2,3,4]);


fun max2 (xs : int list) =
  if null xs
  then NONE
  else let
      fun max_nonempty (xs: int list) =
	if null (tl xs)
	then hd xs
	else let val tl_ans = max_nonempty(tl xs)
	     in
		 if hd xs > tl_ans
		 then hd xs
		 else tl_ans
	     end
  in
      SOME (max_nonempty xs)
  end
	   

