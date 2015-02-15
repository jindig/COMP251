exception DateError

type date = int*int*int

(* Some testing dates *)
val date1 = (2015,01,01)
val date2 = (2015,01,02)
val date3 = (2015,01,31)
val date4 = (2015,02,28)
val date5 = (2000,02,28)
val date6 = (1900,02,28)
val date7 = (1999,12,31)

fun last_day ((year, month, day):date):int =
	case month of
		1 => 31
		| 2 => let
					fun leap_year (year:int):bool = 
					if year mod 4 = 0 then (* it's probably a leap year, check which century *)
						if year mod 100 = 0 then (* only centuries divisible by 400 are leap years *)
							if year mod 400 = 0 then true else false
						else true
					else false
				in
					if leap_year year then 29 else 28
				end
		| 3 => 31
		| 4 => 30
		| 5 => 31
		| 6 => 30
		| 7 => 31
		| 8 => 31
		| 9 => 30
		| 10 => 31
		| 11 => 30
		| 12 => 31
		| _ => raise DateError
	
fun valid_date ((year, month, day):date):bool =
let
	val max_day : int = last_day (year, month, day)
	handle DateError => 0 (* Forces false since day can't be both > 0 and <= 0 *)
in
	if 0 < day andalso day <= max_day then true else false
end

fun next_day ((year, month, day):date):date =
let
	val day_plus_one : int = day + 1
	val max_day : int = last_day (year, month, day)
	val (the_year:int, the_month:int, the_day:int) =
		if day_plus_one > max_day then (* the next day is in the next month *)
			if month + 1 <= 12 then (* the next month is within the current year *)
				(year, month + 1, 1)
			else (year + 1, 1, 1) (* the next month is next year *)
		else (year, month, day_plus_one) (* the next day is within the current month and current year *)
in
	(the_year, the_month, the_day)
end

fun precedes ((date1, date2):date*date):bool =
let
	val (year1, month1, day1):date = date1
	val (year2, month2, day2):date = date2
in
	if year1 < year2 then true (* if date1 is in an earlier year, it definitely precedes date 2 *)
	else if year1 = year2 then
		(* check if month1 is before month2. if same month, then check for an earlier date *)
		month1 < month2 orelse (month1 = month2 andalso day1 < day2)
	else false
end
infix precedes = precedes (* because it's a lot easier to test/type as an infix operator *)

fun earliest (dates : date list):date = 
let
	fun helper ([], earliest_so_far:date) = earliest_so_far (* if we only have 1 date, it's by definition the earliest *)
	| helper (dates:date list, earliest_so_far:date):date =
		(* always pass along the tail of the list (which we haven't seen yet) and the earlier of the head or earliest_so_far *)
		if hd dates precedes earliest_so_far then helper (tl dates, hd dates) else helper (tl dates, earliest_so_far)
in
	helper (tl dates, hd dates)
end
