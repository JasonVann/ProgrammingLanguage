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
  if null dates
  then
      (* [(2000,1,1)] *)
      []
  else
      let val ans = dates_in_month(tl dates, month)
      in
	  if #2 (hd dates) = month
	  then
	      (hd dates) :: ans
	  else
	      ans
      end
	  
fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then
      []
  else
      dates_in_month(dates, hd months) :: dates_in_months(dates, tl months)

fun get_nth(str: string list, n: int) =
  if n = 1
  then
      hd str
  else
      get_nth(tl str, n - 1)

fun date_to_string(date: (int * int * int)) =
  let val months = ["January", "Feburary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ Int.toString(#3 date) ^ "," ^ Int.toString(#1 date)
  end
      
