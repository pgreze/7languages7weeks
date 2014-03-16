// Ignore program name
launcher := System args at(0)
args := System args slice(1)

if(args size != 2, 
    writeln("Usage: ", launcher, " (loop|rec) number")
    System exit 1
)

fibo_recursion := method(n, l_res, ll_res,
    if(n < 2, 1,
        if(n > 2, fibo_recursion(n-1, l_res + ll_res, l_res), l_res)
    )
)

fibo_loop := method(n,
    if(n < 2,
        1,
        ll_res := 1
        l_res := 1
        while(n > 2,
            l_res = l_res + ll_res
            ll_res = l_res - ll_res
            ; n = n - 1)
        l_res
    )
)

n := args at(1) asNumber
if(args at(0) == "loop",
    fibo_loop(n),
    fibo_recursion(n, 1, 1)
) println

// Return 0
System exit