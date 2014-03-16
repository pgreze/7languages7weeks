Matrix := List clone do(
    dim := method(x, y,
        i := 0
        while(i < y,
            new_l := List clone
            j := 0
            while(j < x, new_l push(0); j = j + 1)
            self push(new_l); i = i + 1)
        self
    )

    set := method(x, y, value,
        self at(y) atPut(x, value)
        self
    )

    get := method(x, y,
        self at(y) at(x)
    )
)


assert := method(found, expected, message,
    if(found == expected,
        "ok" println,
        if(message == nil,
            writeln("Expected ", expected, " but found ", found),
            message println))
)


// Original matrix
mat := Matrix clone
// Create ((0, 0, 0), (0, 0, 42), (0, 0, 0), (0, 0, 0), (0, 39, 0))
mat dim(3, 5) set(2, 1, 42) set(1, 4, 39)

assert(mat get(2, 1), 42, "mat[2,1] != 42")
assert(mat get(1, 4), 39, "mat[1,4] != 39")
assert(mat get(0, 0), 0, "mat[0,0] != 0")
assert(mat get(2, 4), 0, "mat[2,4] != 0")

