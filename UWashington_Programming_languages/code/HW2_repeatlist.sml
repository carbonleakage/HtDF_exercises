fun repeat_list(l1: int list, l2: int list) =
    let fun repeat_helper (l1_helper:int list, l2_helper:int list, rep_counter) =
            if null l2_helper orelse null l1_helper
            then []
            else if rep_counter = 0 andalso null (tl l2_helper)
            then repeat_helper (tl l1_helper, tl l2_helper, hd l2_helper)
            else if rep_counter = 0 
            then repeat_helper (tl l1_helper, tl l2_helper, hd (tl l2_helper))
            else hd l1_helper :: repeat_helper (l1_helper, l2_helper, rep_counter - 1)
    in repeat_helper (l1, l2, hd l2)
    end
