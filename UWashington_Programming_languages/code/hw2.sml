(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(x:string, y: string list) = 
    case y of 
    [] => NONE
    | _ => let fun except_helper (y1, z) = 
                        let val hd_y = hd y1
                        val tl_y = tl y1
                        in 
                        if same_string (x, hd_y) 
                        then SOME (z @ tl_y) 
                        else except_helper (tl_y, z @ [hd])
                        end
            in except_helper (y,[])
            end

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