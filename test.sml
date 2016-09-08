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
6 mod 3;

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
	  


(* ?? Below only works in interactive mode
SOME (1); (* int option *)

valOf (max [1,2,3,4]);
*)


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
; (* ?? Not sure why need this *)
  
true orelse false;

not true;

Int.toString(2);
"Hello" ^ "," ^ "World";


Real.fromInt 2 > 0.5;

(* record *)
val x = {bar = (1+2, true), foo = 3 + 4, baz = (false, 9)};
#foo x;

val y={3="hi", 1=true, 2=3+2};
(* val y = (true, 5, "hi") *)

(* datatype binding *)
datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza
		      
(* pattern matching*)
fun f x =
  case x of
      Pizza => 3
   |  Str s => 8
   | TwoInts(i1, i2) => i1 + i2
				 
val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a;

f Pizza;
f (Str "hi");
f (TwoInts (3, 4));
	    
String.size "hello";
Int.max(1,2);

datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
	    | Name of string * (string option) * string

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

fun eval e =
  case e of
      Constant i => i
   |  Negate e2  => ~ (eval e2)
   |  Add(e1,e2) => (eval e1) + (eval e2)
   |  Multiply(e1,e2) => (eval e1) * (eval e2)

(* Function Patterns *)
fun eval2 (Constant i) = i
  | eval2  (Negate e2) = ~ (eval e2)
  | eval2 (Add(e1, e2)) = (eval2 e1) + (eval2 e2)
  | eval2 (Multiply(e1, e2)) = (eval2 e1) * (eval2 e2)
;
  eval2(Add(Constant 1, Constant 2));

fun max_constant e =
  let fun max_of_two(e1, e2) =
	let val m1 = max_constant e1
	    val m2 = max_constant e2
	in Int.max(m1, m2) end
  in
      case e of
	  Constant i => i
	| Negate e2  => max_constant e2
	| Add(e1, e2) => max_of_two(e1, e2)
	| Multiply(e1, e2) => max_of_two(e1, e2)
  end
      
fun append(xs, ys) =
  case xs of
      [] => ys
    | x :: xs' => x :: append(xs', ys)

val (a1, a2, a3) = (1,2,3);

exception ListLengthMismatch

(* nested pattern *)
fun zip3 list_triple =
  case list_triple of
      ([], [], []) => []
    | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3) :: zip3(tl1, tl2, tl3)
    | _ => raise ListLengthMismatch

fun unzip3 lst =
  case lst of
      [] => ([], [], [])
    | (a,b,c)::tl => let val (l1, l2, l3) = unzip3 tl
		     in
			 (a::l1, b::l2, c::l3)
		     end
			 
exception MyUndersirableCondition

fun maxlist(xs, ex) =
  case xs of
      [] => raise ex
    | x::[] => x
    | x::xs' => Int.max(x, maxlist(xs', ex))

		       
val x = maxlist([3,4,5], MyUndersirableCondition)
	handle MyUndersirableCondition => 42
					      
val x1 = maxlist([], MyUndersirableCondition)
	handle MyUndersirableCondition => 42

val int_list_option = SOME[1,2,3];
val int_option_list = [SOME 1, SOME 2, SOME 3];

fun n_times(f, n, x) =
  if n = 0
  then x
  else
      f (n_times(f, n - 1, x))

fun double x = x + x
fun increment x = x + 1

val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7);

(* checks convergency *)
fun times_until_zero(f, x) =
  if x = 0 then 0
  else
      1 + times_until_zero(f, f x)

fun triple_n_times(n, x) =
  n_times(let fun triple x = 3 * x in triple end, n, x)

	 (* Anonymous function *)
fun triple_n_times_anony(n, x) =
  n_times(fn x => 3 * x, n, x)

fun rev xs = List.rev xs
val res2 = List.rev;

fun map(f, xs) =
  case xs of
      [] => []
    | x::xs' => (f x)::(map(f, xs'))
;
  
fun filter(f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x
		then x::(filter (f, xs'))
		else
		    filter(f, xs')

fun is_even v =
  (v mod 2 = 0)

fun all_even_snd xs = filter((fn (_, v) => is_even v), xs)

(* x11 is undefined where g is defined 
fun f g =
  let val x11 = 3
  in g 2
  end

fun g y =
  y + x11;
 *)

(* fold left *)
fun fold (f, acc, xs) =
  case xs of
      [] => acc
    | x::xs => fold(f, f(acc, x), xs)

fun f1 xs = fold((fn (x, y) => x + y), 0, xs) (* sum list *)

(* ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) *)
fun compose(f, g) = fn x => f(g x)
			     
(* f o g *)
			     
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

(* Pipeline *)
infix !>
fun x !> f = f x
fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup (f,g) = fn x =>
		      case f x of
			  NONE => g x
			| SOME y => y
					
fun backup2(f, g) = fn x => f x handle _ => g x;
					      
(* currying *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t2 = ((sorted 3 7) 9) 11;

(* syntactic sugar *)
fun sorted3_nicer x y z = z >= y andalso y >= x
						  
(* Equivalent *)
fun other_curry f = fn x => fn y => f y x 
fun other_curry2 f x y = f y x

structure x = List;
signature x = LIST;

#"a" (* type char *)
    
