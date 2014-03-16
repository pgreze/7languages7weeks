// while
i := 1
while(i <= 3, i println; i = i + 1); "This one goes up to 3" println
for(i, 1, 3, i println); "This one goes up to 3" println 1
for(i, 1, 3, 1, i println, "extra argument") // Use optionnal increment (1) and extra argument
// Display:
1
2
3
This one goes up to 3


// If syntax
if(true, "It is true.", "It is false.")          // => Return "It is true."
if(false) then("It is true") else("It is false") // => Return nil


// Message reflection
postOffice := Object clone
postOffice packageSender := method(call sender)          // Return message sender: mailer with mailer.deliver call
postOffice messageTarget := method(call target)          // Return target of message: postOffice
postOffice messageArgs := method(call message arguments) // Message arguments
postOffice messageName := method(call message name)      // Message name: "messageName"

mailer := Object clone
mailer deliver := method(postOffice packageSender)

postOffice messageArgs("one", 2, :three)                 // Return: list("one", 2, : three)


// Unless implementation
// Note: doMessage execute a message
unless := method(
    (call sender doMessage(call message argAt(0))) ifFalse(
    call sender doMessage(call message argAt(1))) ifTrue(
    call sender doMessage(call message argAt(2)))
)
unless(1 == 2, write("One is not two\n"), write("one is two\n"))


// Recursive display of all ancestors slot names
Object ancestors := method(
    prototype := self proto
    if(prototype != Object,
        writeln("Slots of ", prototype type, "\n---------------")
        prototype slotNames foreach(slotName, writeln(slotName)) writeln
        prototype ancestors))
