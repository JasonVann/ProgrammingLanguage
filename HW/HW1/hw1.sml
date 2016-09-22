(* Q1 *)
fun is_older(a: int * int * int, b: int * int * int) =
  if #1 a > #1 b orelse (#1 a = #1 b andalso #2 a > #2 b) orelse (#1 a = #1 b andalso #2 a = #2 b andalso #3 a >= #3 b)
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
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(str: string list, n: int) =
  if n = 1
  then
      hd str
  else
      get_nth(tl str, n - 1)

(* For Q13 *)
fun get_nth_int(a: int list, n: int) =
  if n = 1
  then
      hd a
  else
      get_nth_int(tl a, n - 1)
		 
fun date_to_string(date: (int * int * int)) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^  Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum: int, nums: int list) =
  let fun iter(cur_sum: int, count: int, sum: int, nums: int list) =
	if cur_sum < sum
	then
	    if cur_sum + hd nums >= sum
	    then count
	    else
		iter(cur_sum + hd nums, count + 1, sum, tl nums)
	else
	    0 (* Impossible *)
					
  in
      iter(0, 0, sum, nums) 
  end

(* Suggested sol *)
fun number_before_reaching_sum2 (sum: int, lst: int list) =
  if sum <= hd lst
  then 0
  else
      1 + number_before_reaching_sum2(sum - hd lst, tl lst)
				     
fun what_month(day: int) =
  let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1 + number_before_reaching_sum(day, months)
  end
      
fun month_range(day1: int, day2: int) = 
  let val month1 = what_month(day1)
      val month2 = what_month(day2)
      fun all_days(day1: int, day2: int) =
	if day1 = day2
	then
	    [day2]
	else
	    day1 :: all_days(day1 + 1, day2)
      fun iter(days: int list) =
	  if null days
	  then
	      []
	  else
	      what_month(hd days) :: iter(tl days)
  in
      if day1 > day2
      then
	  []
      else
	  iter(all_days(day1, day2))
  end
      
fun oldest(dates: (int * int * int) list) =
  let fun iter(cur: (int * int * int), dates: (int * int * int) list) =
	if null dates
	then
	    SOME cur
	else
	    if is_older(cur, hd dates)
	    then
		iter(cur, tl dates)
	    else
		iter(hd dates, tl dates)
  in
      if null dates
      then
	  NONE
      else
	  iter(hd dates, dates)
  end

(* Assumes months are sorted *)
fun remove_duplicates(months: int list, cur: int list) =
  let fun len_list(a: int list) =
	if null a
	then 0
	else
	    1 + len_list(tl a)
  in
      
  if null months
  then
      cur
  else
      (*
      if null (tl months)
      then
	  [hd months]
      else *)
      if len_list(cur) > 0 andalso hd months = get_nth_int(cur, len_list(cur))
      then
	  remove_duplicates(tl months, cur)
      else
	  remove_duplicates(tl months, cur @ [hd months])
  end

fun remove_duplicates2(nums: int list) = 
  let fun is_in(num: int, nums: int list) =
	if null nums
	then
	    false
	else
	    if num = hd nums
	    then true
	    else
		is_in(num, tl nums)
  in
      if null nums
      then
	  []
      else
	  if is_in(hd nums, tl nums)
	  then
	      remove_duplicates2(tl nums)
	  else
	      (hd nums) :: remove_duplicates2(tl nums)
  end
      
fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
  let val months = remove_duplicates2(months)
  in
      number_in_months(dates, months)
  end
      
fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  let val months = remove_duplicates2(months)
  in
      dates_in_months(dates, months)
  end
      
fun reasonable_date(date: int * int * int) =
  let val days = [31, 28, 31, 30, 31, 30,31,31,30,31,30,31]
      fun is_leap() =
	if (#1 date mod 400 = 0)  orelse ( (#1 date mod 4 = 0)  andalso (not (#1 date mod 100 = 0)) )
	then
	    true
	else
	    false
  in
      if #1 date > 0 andalso #2 date > 0 andalso #2 date <= 12
      then
	  if is_leap() andalso #2 date = 2 andalso #3 date <= 29
	  then
	      true
	  else
	      if #3 date <= get_nth_int(days, #2 date)
	      then
		  true
	      else
		  false
      else
	  false
  end						  
