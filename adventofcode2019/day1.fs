module day1

open System
open System.Numerics

let calculate mass =
    let divided = floor ((double mass) / (double 3))
    let floored_int = BigInteger divided
    let result = floored_int - (BigInteger 2)
    if result < bigint.Zero then bigint.Zero
    else result

let rec calculate_fuel mass acc recurse =
    if recurse && mass > bigint.Zero then
        let calc_mass = calculate mass
        calculate_fuel calc_mass (acc + calc_mass) recurse
    elif recurse then acc
    else calculate mass

let calculate_fuels (sum1, sum2) line =
    let line_parsed = BigInteger.Parse (line)
    let fuel = calculate_fuel line_parsed (BigInteger 0) false
    (sum1 + fuel, sum2 + (calculate_fuel fuel fuel true))

// [<EntryPoint>]
let main argv = 
    let lines = 
        try
            IO.File.ReadLines @"..\..\inputs\1-small.in"
        with
            | :? System.IO.IOException as ex -> printfn "%s" ex.Message; Seq.empty
    let result = Seq.fold calculate_fuels (BigInteger 0, BigInteger 0) lines
    System.Console.WriteLine ("result: {0}", result) |> ignore
    0
