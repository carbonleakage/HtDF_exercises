(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test12 = all_except_option ("hi", ["hi", "there", "this", "is", "cool", "hey"]) = SOME ["there", "this", "is", "cool", "hey"]
val test13 = all_except_option ("hey", ["hi", "there", "this", "is", "cool", "hey"]) = SOME ["hi","there", "this", "is", "cool"]
val test14 = all_except_option ("string", ["strung"]) = NONE
val test15 = all_except_option ("hoppala", ["hi", "there", "this", "is", "cool", "hey"]) = NONE


val test21 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["Fredrick","Freddie","F"]
val test23 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test31 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test32 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["Fredrick","Freddie","F"]
val test33 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]


val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test71 = remove_card ([(Hearts, Ace), (Hearts, Queen), (Hearts, King)], (Hearts, Ace), IllegalMove) = [(Hearts, Queen), (Hearts, King)]
val test72 = remove_card ([(Hearts, Ace),(Hearts, Ace), (Hearts, Queen), (Hearts, King)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Hearts, Queen), (Hearts, King)]
val test73 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test14 = officiate ([(Hearts, Num 2),(Clubs, Num 4),(Spades, Num 4)],[Draw, Draw, Draw], 3) = 9

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
