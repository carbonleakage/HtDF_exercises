(*
fun str_str (x1::x2,y) =
    case x2 of
    [] => []
    | hd_x::tl_x => x2

input: givenstr: string, strlist: string list
if strlist does not contain givenstr then NONE
if strlist contains givenstr then strlist - givenstr
*)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun search_string_in_list (search_str, search_list) =
    case search_list of
    [] => false
    | hdlist::tllist => same_string (search_str, hdlist) orelse search_string_in_list(search_str, tllist)


datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

fun max_constant e1 =
    case e1 of 
        Constant i => i
        | Negate e2 => ~ max_constant e2
        | Add (e2, e3) => if max_constant e2 > max_constant e3 then max_constant e2 else max_constant e3
        | Multiply (e2, e3) => if max_constant e2 > max_constant e3 then max_constant e2 else max_constant e3

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp