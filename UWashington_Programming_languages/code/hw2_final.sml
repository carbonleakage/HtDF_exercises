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


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (card_suit:suit, card_rank:rank) = 
    case card_suit of
        Clubs => Black
        | Spades => Black
        | Diamonds => Red
        | Hearts => Red

fun card_value (card_suit:suit, card_rank:rank) = 
    case card_rank of 
        Num i => i
        | Ace => 11
        | Jack => 10
        | Queen => 10
        | King => 10

fun remove_card (cs, c, e) = 
    case cs of
    [] => raise e
    | hdlist::tllist => if hdlist = c 
                        then tllist 
                        else hdlist::remove_card(tllist, c, e)

fun all_same_color (cs) =
    case cs of 
    [] => true
    | c::[] => true
    | hc::nc::tlc => (card_color (hc) = card_color (nc) andalso all_same_color(nc::tlc))

fun sum_cards (cs) = 
    let fun sum_helper (cs_helper, acc)= 
        case cs_helper of
        [] => acc
        | hdc::tlc => sum_helper(tlc, card_value (hdc) + acc)
    in 
        sum_helper(cs, 0)
    end

fun score (cs, goal) =
    let val sumc = sum_cards (cs)
        val prelim_score = if sumc > goal then 3 * (sumc - goal) else (goal - sumc)
    in 
    case all_same_color (cs) of
        true => prelim_score div 2
        | false => prelim_score
    end

fun officiate (card_list, move_list, goal) = 
    let fun game_state (card_list, move_list, hand_cards,game_continue) = 

        let fun game_move (card_list, move_list, hand_cards) =
            case (card_list, move_list) of 
            (_, []) => game_state (card_list, move_list, hand_cards, false)
            | ([], Draw::_) => game_state (card_list, move_list, hand_cards, false)
            | (next_card::card_tail, Draw::move_tail) => game_state (card_tail, move_tail, next_card::hand_cards, true)
            | (_, Discard c1::move_tail) => game_state (card_list, move_tail, remove_card (hand_cards, c1, IllegalMove), true)
        in
            if game_continue andalso (sum_cards (hand_cards) < goal)
            then game_move (card_list, move_list, hand_cards)
            else score (hand_cards, goal)
        end

    in 
        game_state (card_list, move_list, [], true)
    end
