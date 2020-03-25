// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Types
open System.Threading

[<EntryPoint>]
let main argv = 
    actor.Post (OrderDrink(Cup(Espresso, Large),2))
    actor.Post (LeaveAComment("The coffee was amazing"))
    actor.Post (LeaveAComment("The coffee was really good"))
    actor.Post (LeaveAComment("The coffee was terrible"))
    actor.Post (LeaveAComment(null))

    while actor.CurrentQueueLength > 0 do
        Thread.Sleep(50)

    0 // return an integer exit code
