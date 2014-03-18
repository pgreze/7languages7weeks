// Map support with curly brackets {} and operator :

OperatorTable addAssignOperator(":", "atPutNumber")
curlyBrackets := method(
    r := Map clone
    call message arguments foreach(arg,
        r doMessage(arg)
    )
    r
)

Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
        call evalArgAt(1)
    )
)

// List support with square brackets []

squareBrackets := method(
    l := List clone
    call message arguments foreach(arg,
        e := doMessage(arg)
        l append(if(e type == "Sequence",
            e asMutable removePrefix("\"") removeSuffix("\""),
            e
        ))
    )
    l
)

doRelativeFile("xml_builder.io")