(*Test cases for the HW1 problems*)

val test11 = is_older((2019,1,1), (2020,1,1)) = true
val test12 = is_older((2019,1,1), (2019,2,1)) = true
val test13 = is_older((2019,1,1), (2019,1,10)) = true
val test14 = is_older((2020,1,1), (2019,1,1)) = false
val test15 = is_older((2019,2,1), (2019,1,1)) = false
val test16 = is_older((2019,1,10), (2019,1,1)) = false
val test17 = is_older((2018,12,31), (2019,1,1)) = true
val test18 = is_older((2019,1,1), (2018,12,31)) = false

val test21 = number_in_month([(2019,2,1), (2020,2,1), (2019,2,10)], 2) = 3
val test22 = number_in_month([(2019,2,1), (2020,2,1), (2019,2,10)], 12) = 0
val test23 = number_in_month([], 12) = 0


val test31 = number_in_months([(2019,2,1), (2020,2,1), (2019,2,10)], [2]) = 3
val test32 = number_in_months([(2019,2,1), (2020,2,1), (2019,2,10)], [12]) = 0
val test33 = number_in_months([(2019,2,1), (2020,2,1), (2019,1,10)], [12,2]) = 2
val test34 = number_in_months([(2019,2,1), (2020,2,1), (2019,1,10)], [12,2,1]) = 3
val test35 = number_in_months([], []) = 0

val test41 = dates_in_month([(2019,2,1), (2020,2,1), (2019,2,10)], 2) = [(2019,2,1), (2020,2,1), (2019,2,10)]
val test42 = dates_in_month([(2019,2,1), (2020,2,1), (2019,2,10)], 12) = []
val test43 = dates_in_month([(2019,2,1), (2020,2,1), (2019,2,10), (2019,12,10), (2019,12,11)], 12) = [(2019,12,10), (2019,12,11)]
val test44 = dates_in_month([], 12) = []

val test51 = dates_in_months([(2019,2,1), (2020,2,1), (2019,2,10)], [2]) = [(2019,2,1), (2020,2,1), (2019,2,10)]
val test52 = dates_in_months([(2019,2,1), (2020,2,1), (2019,2,10)], [12]) = []
val test53 = dates_in_months([(2019,2,1), (2020,2,1), (2019,2,10), (2019,12,10), (2019,12,11)], [12]) = [(2019,12,10), (2019,12,11)]
val test54 = dates_in_months([], [12]) = []

val test61 = get_nth (["hi", "there", "this", "is", "cool"],1) = "hi"
val test62 = get_nth (["hi", "there", "this", "is", "cool"],2) = "there"
val test63 = get_nth (["hi", "there", "this", "is", "cool"],3) = "this"
val test64 = get_nth (["hi", "there", "this", "is", "cool"],4) = "is"

val test71 = date_to_string((2020,1,1)) = "January 1, 2020"
val test72 = date_to_string((2020,3,1)) = "March 1, 2020"
val test73 = date_to_string((2020,12,10)) = "December 10, 2020"

val test81 = number_before_reaching_sum(4, [1,2,3,4]) = 2
val test82 = number_before_reaching_sum(6, [1,2,3,4]) = 2
val test83 = number_before_reaching_sum(7, [1,2,3,4,5,6,7,8]) = 3

val test91 = what_month (50) = 2
val test92 = what_month (200) = 7
val test93 = what_month (350) = 12 
val test94 = what_month (100) = 4

(*[31,59,90,120,151,181,212,243,273,304,334,365]*)
val test101 = month_range (30,34) = [1,1,2,2,2]