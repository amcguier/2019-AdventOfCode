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
  |> fun x -> (x.[0].Trim(),x.[1].Trim())
  

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


let inline (+.) (x1,y1) (x2,y2) = (x1+x2, y1+y2)

let rec findYouSanta = function
  | Leaf("SAN") -> (0,1)
  | Leaf("YOU") -> (1,0)
  | Leaf(_) -> (0,0)
  | Node(id,lst) ->
      let childResults =
        lst
        |> List.map findYouSanta
        |> List.filter ((<>) (0,0))
      match childResults.Length with
        | 0 -> (0,0)
        | 2 -> ((List.reduce (+.) childResults) : int*int)
        | 1 ->
            let (you,san) = childResults.Head
            if you > 0 && san > 0 then
              (you,san)
            elif you > 0 then
              (you + 1,san)
            else
              (you,san + 1)
        | rs -> sprintf "Invalid state %A" rs |> failwith
              
            
      

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


  //printfn "%A" orbitTree
  countOrbits 0 orbitTree |> printfn "There are %i total orbits"

  findYouSanta orbitTree
  |> function |(x,y) -> x + y - 2
  |> printfn "There are %i hops between you and Sanata"
  
  0 // return an integer exit code
