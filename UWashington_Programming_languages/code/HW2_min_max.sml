fun min_max (x: int list) =
    if null x
    then (0, 0)
    else if null (tl x)
    then (hd x, hd x)
    else 
    let val (min_val, max_val) = min_max (tl x)
    in 
        if hd x < min_val
        then (hd x, max_val)
        else if hd x > max_val
        then (min_val, hd x)
        else (min_val, max_val)
    end