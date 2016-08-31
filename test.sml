(* this is a 
comment 
; is not necessary in a file; necessary in REPL
*)

val x = 5;

x = 6 (* evaluates to false *)
	
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
	      
