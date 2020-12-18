(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
 
fun get_substitutions1 (search_str, search_list) =
   case search_list of 
   [] => NONE
   | hdlist::tllist => case (same_string (search_str, hdlist), tllist) of 
                        (true, []) => SOME []
                        | (false, []) => NONE
                        | (true,_) => SOME tllist
                        | (false,_) => case (get_substitutions1(search_str, tllist)) of 
                                       NONE => SOME []
                                       | SOME t =>


(*
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

*)