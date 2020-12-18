(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (search_string, string_list) =
    case string_list of
        [] => NONE
        | hdlist::tllist => case (same_string (search_string, hdlist), all_except_option (search_string, tllist)) of
                                (true,_) => SOME tllist
                                | (false, NONE) => NONE
                                | (false, SOME i) => SOME (hdlist :: i)

fun get_substitutions1 (string_list_list, search_string) = 
    case string_list_list of
        [] => []
        | hdlist::tllist => case all_except_option (search_string, hdlist) of 
                                NONE => get_substitutions1(tllist, search_string)
                                | SOME i => i @ get_substitutions1(tllist, search_string)


fun get_substitutions2 (string_list_list, search_string) = 
    let fun subs_helper (helper_list, search_string, acc) = 
        case helper_list of
            [] => acc (*base case with accumulator is always the accumulator!!*)
            | hdlist::tllist => case all_except_option (search_string, hdlist) of 
                                    NONE => subs_helper(tllist, search_string, acc)
                                    | SOME i => subs_helper(tllist, search_string, acc @ i)

    in
        subs_helper(string_list_list, search_string, [])
    end


fun similar_names (string_list_list, {first=fname, middle=mname, last=lname}) =
    let fun add_names (name_list1) = 
            case name_list1 of
            [] => []
            | hdlist::tllist =>  {first=hdlist, middle=mname, last=lname}::add_names (tllist)
    in
        {first=fname, middle=mname, last=lname} :: add_names (get_substitutions2 (string_list_list, fname))
    end
