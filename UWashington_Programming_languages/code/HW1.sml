fun is_older (x:int * int * int, y: int * int * int) =
    if (#1 x) < (#1 y) 
    then true
    else if (#1 x) > (#1 y)
         then false
         else if (#2 x) < (#2 y) 
              then true
              else if (#2 x) > (#2 y)
                   then false
                   else (#3 x) < (#3 y)

fun number_in_month (date_list: (int * int * int) list, month:int) = 
    if null date_list
    then 0
    else if #2 (hd date_list) = month
         then 1 + number_in_month (tl date_list, month)
         else number_in_month (tl date_list, month)

fun number_in_months (date_list: (int * int * int) list, month_list: int list) = 
    if null month_list
    then 0
    else number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

fun dates_in_month (date_list: (int * int * int) list, month:int) = 
    if null date_list
    then []
    else if #2 (hd date_list) = month
         then hd date_list::dates_in_month (tl date_list, month)
         else dates_in_month (tl date_list, month)

fun dates_in_months (date_list: (int * int * int) list, month_list: int list) = 
    if null month_list
    then []
    else dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list)

fun get_nth (x: string list, n:int) = 
    if n = 1
    then hd x
    else get_nth (tl x, n-1)

fun date_to_string(x:int * int * int) =
    let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
        get_nth(months, #2 x) ^ " " ^ Int.toString (#3 x) ^ "," ^  " " ^ Int.toString (#1 x)
    end

fun number_before_reaching_sum(sum:int, x: int list) =
    let fun helper (sum1:int, x1:int list, n) = 
            if null x1 orelse  sum1 <= hd x1
            then n
            else helper (sum1 - (hd x1), (tl x1), n+1)
    in helper (sum, x, 0)
    end

fun what_month (day: int) = 
    let val monthlims = [0,31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum (day, monthlims)
    end

fun month_range (strt_date:int, end_date:int) =
    if strt_date > end_date
    then []
    else what_month(strt_date)::month_range(strt_date+1,end_date)

fun oldest (xs: (int * int * int ) list ) = 
    if null xs
    then NONE
    else let fun oldest_helper (xs) =     
                if null (tl xs) 
                then hd xs 
                else let val tl_ans = oldest_helper (tl xs)
                     in if is_older (hd xs, tl_ans)
                     then hd xs
                     else tl_ans
                     end
        in SOME (oldest_helper xs)
        end
