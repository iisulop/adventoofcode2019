module day3

open System

let func (path: seq<int * int * int>) (input: String) =
    let num = int input.[1..]
    let (x, y, distance) = Seq.last path
    let steps = [ 1..num ]
    let new_path =
        match input.[0] with
            | 'U' -> Seq.append path (Seq.map (fun n -> (x, y + n, distance + n)) steps)
            | 'R' -> Seq.append path (Seq.map (fun n -> (x + n, y, distance + n)) steps)
            | 'D' -> Seq.append path (Seq.map (fun n -> (x, y - n, distance + n)) steps)
            | 'L' -> Seq.append path (Seq.map (fun n -> (x - n, y, distance + n)) steps)
    new_path

let generate_path input =
    let init = seq { yield (0, 0, 0) }
    let result = Array.fold func init input
    result

let find_distances first_path second_path x y =
    let (_, _, first_distance) = Seq.head (Seq.filter (fun (x1, y1, _) -> (x, y) = (x1, y1)) first_path)
    let (_, _, second_distance) = Seq.head (Seq.filter (fun (x1, y1, _) -> (x, y) = (x1, y1)) second_path)
    first_distance + second_distance

let find_crosses first_path second_path =
    let first = Set.ofSeq (Seq.map (fun (x, y, z) -> (x, y)) first_path)
    let second = Set.ofSeq (Seq.map (fun (x, y, z) -> (x, y)) second_path)
    let intersection = Set.intersect first second
    let crosses = Seq.map (fun (x, y) -> (x, y, (find_distances first_path second_path x y))) intersection
    crosses

let find_closest crosses distance_func =
    Seq.fold (fun result (x, y, steps) ->
        let distance = distance_func x y steps
        match (result, distance) with
            | None, 0 -> None
            | Some(closest), 0 -> Some(closest)
            | None, distance -> Some(distance)
            | Some(closest), distance -> if distance < closest then Some(distance) else Some(closest)
        )
        None crosses

let closest_cross_distance crosses =
    let distance_func = fun x y _ -> (abs x) + (abs y)
    find_closest crosses distance_func

let shortest_distance crosses =
    let distance_func = fun _ _ steps -> steps
    find_closest crosses distance_func

// [<EntryPoint>]
let main argv =
    let lines = IO.File.ReadAllLines @"..\..\inputs\3-small.in"
    let first_input = (Seq.item 0 lines).Split([| ',' |])
    let second_input = (Seq.item 1 lines).Split([| ',' |])
    
    let first_run = generate_path first_input
    let second_run = generate_path second_input

    let crosses = find_crosses first_run second_run
    let closest = closest_cross_distance crosses
    System.Console.WriteLine("part 1: {0}", (closest))

    let shortest_distance = shortest_distance crosses
    System.Console.WriteLine("part 2: {0}", (shortest_distance))
    0
