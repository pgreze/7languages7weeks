List myAverage := method(
    res := 0
    numbers := 0
    self foreach(e,
        i := e asNumber
        res = if(i isNan, res,
            numbers = numbers + 1
            res + i)
    )
    if(numbers != 0, res / numbers, Exception raise("Numbers not found"))
)

if(list(1, "hello", 3) myAverage == 2, "ok", "nok") println
list("hello", "world") myAverage // Throw an exception