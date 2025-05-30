module DefineIntegralModule

let defineIntegral (factors: int list) (powers: int list) (left: float) (right: float) : float * float =
    let calcSubintervalLength (L: float) (R: float) (dx: float) : int =
        int ((R - L) / dx) + 1

    let defineFunction (x: float) : float =
        List.zip factors powers
        |> List.sumBy (fun (ai, bi) -> float ai * (x ** float bi))

    let integrate (f: float -> float) (L: float) (R: float) (dx: float) : float=
        let N = calcSubintervalLength L R dx
        let xVals = [for i in 0..N - 1 -> L + float i * dx]

        let total =
            xVals 
            |> List.skip 1
            |> List.take (N - 2)
            |> List.sumBy f

        (dx / 2.0) * (f L + 2.0 * total + f R)

    let area = integrate defineFunction left right 0.001
    
    let squareFunction x = (defineFunction x) ** 2.0
    let volume = Math.PI * integrate squareFunction left right 0.001
    
    (area, volume)
