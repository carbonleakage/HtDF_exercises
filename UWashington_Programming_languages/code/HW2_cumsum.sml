fun cumsum (x: int list) =
    let fun cumsum_helper (tempsum: int, x_helper : int list) = 
            if null x_helper
            then []
            else 
            tempsum + hd x_helper :: cumsum_helper (tempsum + hd x_helper, tl x_helper)
    in 
        cumsum_helper (0, x)
    end