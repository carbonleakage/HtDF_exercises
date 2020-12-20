fun addAllOpt (x: int option list) =
    let fun addAllOpt_helper(x_helper: int option list, tempsum: int option) =
        let fun addOpt (x1: int option, y1: int option) =
                if isSome x1 andalso isSome y1
                then SOME (valOf x1 + valOf y1)
                else if isSome x1
                then x1
                else y1
        in
        if null x_helper
        then tempsum
        else addAllOpt_helper(tl x_helper, addOpt (hd x_helper, tempsum))
        end
    in addAllOpt_helper (x, NONE)
    end