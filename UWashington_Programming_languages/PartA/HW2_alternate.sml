fun alternate (x : int list) =
    if null x
    then 0
    else hd x - alternate (tl x)
   