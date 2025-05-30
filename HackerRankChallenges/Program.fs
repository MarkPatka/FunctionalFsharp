open System
open IntroductionModule;
open InputHelper

[<EntryPoint>]
let main argv =

    let factors = inputToIntList ()
    let powers = inputToIntList ()
    let range = inputToIntList ()

    let left = float range.Head
    let right = float (List.last range)

    let (area, volume) = defineIntegral factors powers left right

    area |> (printfn "%.4f")
    volume |> (printfn "%.4f")

    0