(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["A","B","C", "aa"] = ["A","B","C"]
val test1_2 = only_capitals ["A","B","C", "aa", "Ba", "bb"] = ["A","B","C", "Ba"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","Caprice"] = "Caprice"
val test2_2 = longest_string1 ["A","b","C"] = "A"
val test2_3 = longest_string1 [] = ""


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","Caprice"] = "Caprice"
val test3_2 = longest_string2 ["A","b","C"] = "C"
val test3_3 = longest_string2 [] = ""


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A","bc","Caprice"] = "Caprice"
val test4a_2 = longest_string3 ["A","b","C"] = "A"
val test4a_3 = longest_string3 [] = ""


val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A","bc","C"] = "bc"
val test4b_2 = longest_string4 ["A","bc","Caprice"] = "Caprice"
val test4b_3 = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x > 5 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test82 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]


val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (TupleP [Wildcard, UnitP, Wildcard, ConstP 2]) = 2


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (TupleP [Variable("a"), Wildcard, Variable("abcd")]) = 6


val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", TupleP [Variable("x"),  Variable("xx"),  Variable("x"),  Variable("abcd")]) = 2
val test9c_2 = count_some_var ("x", Variable("y")) = 0


val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP [Variable("x"),  Variable("xx"),  Variable("xy"),  Variable("abcd")]) = true
val test10_2 = check_pat (TupleP [Variable("x"),  Variable("x"),  Variable("x"),  Variable("x")]) = false

(*)
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)