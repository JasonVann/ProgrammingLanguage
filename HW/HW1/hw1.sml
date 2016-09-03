(* Q1 *)
fun is_older(a: int * int * int, b: int * int * int) =
  if #1 a > #1 b orelse (#1 a = #1 b andalso #2 a > #2 b) orelse (#1 a = #1 b andalso #2 a = #2 b andalso #3 a > #3 b)
  then
      false
  else
      true

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates orelse  null [ (hd dates) ]
  then
      0
  else
      if (#2 (hd dates)) = month
      then 1 + number_in_month(tl dates, month)
      else
	  0 + number_in_month(tl dates, month)

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then
      0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if (null dates)
  then
      NONE
  else
      let val ans = dates_in_month(tl dates, month)
      in
      if #2 (hd dates) = month
      then
	  if isSome ans
	  then
	      [(SOME (hd dates))] @ ans
	  else
	      [SOME (hd dates)]
      else
	  ans
      end
	  
