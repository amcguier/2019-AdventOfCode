// Learn more about F# at http://fsharp.org

open System
open System.IO

//Roough plan of attack
// define begining point as coordinate 0,0
// build active pattern for turning coordinate point, and direction into new coordinate point
// generate list of all coordinates touched by each wire
// intersect the lists
// find smallest manhattan distance from 00


let strInt (str : string) =
  str.Substring(1) |> Int32.Parse



let (|R|L|U|D|) (str : string) =
  match str with
    | s when s.StartsWith("D") -> D
    | s when s.StartsWith("U") -> U
    | s when s.StartsWith("L") -> L
    | s when s.StartsWith("R") -> R
    | _ -> printfn "Invalid coordinate provided"; exit -1
    
type Coordinate = int * int

let coordinatesFromDirection ((x,y) : Coordinate) (direction : string) =
  let num = strInt direction
  match direction with
    | R -> seq { for i in 1..num do yield (x + i, y)}
    | L -> seq { for i in 1..num do yield (x - i, y)}
    | U -> seq { for i in 1..num do yield (x, y+i)}
    | D -> seq { for i in 1..num do yield (x, y - i)}
  |> Array.ofSeq


let directionsToCoordinates dirs =
  let folder coordinates direction =
    Array.append
      coordinates
      (coordinatesFromDirection (Array.last coordinates) direction)
      
  dirs
  |> Array.fold folder [|(0,0)|]

let manhattan ((x1,y1) : Coordinate) ((x2,y2):Coordinate) =
  Math.Abs(x1 - x2) + Math.Abs(y1 - y2)


[<EntryPoint>]
let main argv =
  let arr =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun x -> x.Split(",", StringSplitOptions.RemoveEmptyEntries))
    |> Array.ofSeq

  if arr.Length <> 2 then
    printfn "Invalid input provided"
    exit -1



  let wire1 =
    arr.[0]
    |> directionsToCoordinates


  let wire2 =
    arr.[1]
    |> directionsToCoordinates

    
  
  let intersections =
    Set.intersect (wire1 |> Set.ofArray) (wire2 |> Set.ofArray)
    |> Set.toArray
    |> Array.filter (fun x -> x <> (0,0))


  intersections
  |> Array.map (fun x -> (Array.findIndex ((=) x)  wire1) + (Array.findIndex ((=) x) wire2))
  |> Array.sort
  |> printfn "%A"
  
  0 // return an integer exit code
