// Learn more about F# at http://fsharp.org

open System

type File = System.IO.File

type Orbit = string * string

type ID = string

type Node = 
  | Node of ID * Node list
  | Leaf of ID



let strToOrbit (str : string) =
  str.Split(')')
  |> fun x -> (x.[0],x.[1])
  

let rec orbitsToNode (orbits : Orbit list) idToProcess =
  orbits
  |> List.partition (fun o -> fst o = idToProcess)
  |> function
     | ([],lst) -> Leaf(idToProcess)
     | (orbiting,lst) ->
           orbiting
           |> List.map (snd >> orbitsToNode lst)
           |> fun rem -> (Node(idToProcess , rem))
           
        
let rec flattenNode =
  function
    | Leaf(leaf) -> [leaf]
    | Node(i,children) ->
        children
        |> List.map flattenNode
        |> List.collect id
        |> fun l -> i::l
        

let rec countOrbits i node =
  match node with
    | Leaf(_) -> i
    | Node(_,lst) -> List.map (countOrbits (i+1)) lst |> List.sum |> ((+) i)

let validate orbits (node : Node) =
  let set = node |> flattenNode |> Set.ofList
  orbits
  |> List.filter (fun (x,y) -> not (set.Contains(x) && set.Contains(y)))
  |> List.isEmpty

[<EntryPoint>]
let main argv =
  let orbits =
    File.ReadAllLines("input.txt")
    |> Array.map(strToOrbit)
    |> Array.sortBy fst
    |> List.ofArray

  let orbitTree =
    orbits
    |> fun l -> orbitsToNode l "COM"


  printfn "%A" orbitTree
  countOrbits 0 orbitTree |> printfn "There are %i total orbits"
  
  0 // return an integer exit code
