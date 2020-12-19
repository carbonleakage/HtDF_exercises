(* Coursera Programming Languages, Homework 3, Provided Code *)

infix !>

fun only_capitals input_list = List.filter (fn x => Char.isUpper (String.sub(x, 0))) input_list

fun longest_string1 input_list = List.foldl (fn (x,y) => if String.size (x) > String.size (y) then x else y) "" input_list
fun longest_string2 input_list = List.foldl (fn (x,y) => if String.size (y) <= String.size (x) then x else y) "" input_list


fun longest_string_helper f = List.foldl (fn (x,y) => if f (String.size(x), String.size (y)) then x else y) "" 

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => y <= x)


fun longest_capitalized input_list = (longest_string1 o only_capitals) input_list

val rev_string = (implode o rev o explode)

exception NoAnswer

fun first_answer f input_list = 
    case input_list of 
    [] => raise NoAnswer
    | hdlist::tllist => case f(hdlist) of
                            NONE => first_answer f tllist
                            | SOME v => v

fun all_answers f input_list = 
    let fun all_answers_helper (input_list1, acc) = 
    case input_list1 of
    [] => acc
    | hdlist::tllist => case (f(hdlist), acc) of
                        (NONE, _) => NONE
                        | (SOME v, SOME acc_list) => all_answers_helper (tllist, SOME (acc_list @ v))

    in
    all_answers_helper (input_list, SOME [])
    end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards p = g (fn x=>1) (fn x=>0) p
fun count_wild_and_variable_lengths p = g (fn x=>1) String.size p

fun count_some_var (search_str, p) = g (fn x=>0) (fn x => if x=search_str then 1 else 0) p


fun check_pat p = 
    let fun variable_names p acc = 
        case p of 
        Variable x1 => [x1]
        | TupleP ps => foldl (fn (Variable s1,acc1) => s1::acc1) acc ps
        | ConstructorP (_,p1) =>  
    in
        let fun check_duplicates lst =
            case lst of 
            [] => false
            | hdlist::tllist => (List.exists (fn x=> x = hdlist) tllist) orelse (check_duplicates tllist)
        in
            not (check_duplicates (variable_names p []))
        end
    end

(*
fun match (v, p) =
    case (v,p) of 
    (_, Wildcard) => []
    | (v1, Variable s1) => [s1, v1]
    | (Unit, UnitP) => []
    | (Const v1, ConstP p1) => []
    | (Tuple vshead::vstail, TupleP pshead::pstail) => if List.size (vstail) = List.size (pstail) 
                                                        then [match (vshead, pshead)] @ match (vstail, pstail)
                                                        else []
    | (Constructor (s2,v), ConstructorP(s1,p)) => 



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

check_pat: Called check_pat on input: TupleP[Variable "x",ConstructorP ("wild",Wildcard)], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"], should have gotten: false but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"], should have gotten: false but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[Wildcard,Wildcard], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[ConstP 4,Wildcard,Variable "ba",TupleP[Variable "ab"]], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]], should have gotten: true but your function returned otherwise. [Match exception thrown]
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",Variable "x"]), should have gotten: false but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])]), should have gotten: false but your function returned otherwise. [incorrect answer]
prob11 tests failed to run (most likely caused by an incorrect function signature in the submission)
prob12 tests failed to run (most likely caused by an incorrect function signature in the submission)
prob13 tests failed to run (most likely caused by an incorrect function signature in the submission)



*)