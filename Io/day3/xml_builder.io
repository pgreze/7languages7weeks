//
// NOTE: you have to launch collections.io
// See https://github.com/stevedekorte/io/issues/259
//

Builder := Object clone do(
    indent := List clone
)

Builder forward := method(
    s := "<" .. call message name
    self indent append("  ")
    i := 0
    call message arguments foreach(arg,
        content := self doMessage(arg)
        if(i == 0,
            // Add attributes
            if(content proto == Map, content foreach(k, v, s = s .. " " .. k .. "=\"" .. v .. "\""))
            // Close tag
            s = s .. ">\n"
        )
        if(content type == "Sequence",
            s = s .. self indent join .. content .. "\n"
        )
        i = i + 1
    )
    self indent pop
    s = s .. self indent join .. "</" .. call message name .. ">"
    s
)

Builder ul({"hello" : "world", "io": "you ROCK"},
    li("Io"), 
    li("Lua"), 
    li("JavaScript")
) println