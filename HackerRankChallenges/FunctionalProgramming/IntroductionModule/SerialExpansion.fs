module SerialExpansionModule


let calculateSerialExpansion (N: float) =
    let rec factorial = function
        | 0L | 1L -> 1L
        | k -> (int64 k) * factorial(k - 1L)

    let terms =
        [0..9]
        |> List.map (fun k ->
            let power = float N ** float k
            let fact = float (factorial (int64 k))
            power / fact)

    List.sum terms

let serialExpansion (elements: float list) =
    elements |> List.map calculateSerialExpansion