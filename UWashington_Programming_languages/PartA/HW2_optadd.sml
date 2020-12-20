fun addOpt (x: int option, y: int option) =
    if isSome x andalso isSome y
    then SOME (valOf x + valOf y)
    else NONE