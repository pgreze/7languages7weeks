// Use "Protos Core" to see all core objects

// Io is strongly typed
"Try number + string operation" println
e := try(
    1 + "one"
)
e catch(Exception,
    "can't 1 + one" println
    e println
)

// Test that 0 and "" are true
if(0, "0 is true", "0 is false") println
if("") then("empty string is true" println) else("empty string is false" println)

// Object discovery
Vehicle := Object clone
Vehicle description := "Something to take you far away"
Car := Vehicle clone
Car run := method("Vrooom !" println)

"Display Car's parent slotNames (so Vehicle)" println
Car proto println

"Display Car's slotNames" println
Car slotNames println
"" println

// ::= vs := vs =
Car speed ::= 5
Car numWheels := 4
Car description = "Vehicle with 4 wheels and a motor"
Car slotNames println // Display list(speed, run, type, numWheels, setSpeed, description)
"" println

"Execute code in slot given its name" println
Car getSlot("run") call