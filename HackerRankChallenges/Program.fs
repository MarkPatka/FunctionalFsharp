open InputHelper
open System
open LambdaCalculus

[<EntryPoint>]
let main argv =

    let lambdaInput = Console.ReadLine()
    printfn "%s" (reduceLambda lambdaInput)

    0