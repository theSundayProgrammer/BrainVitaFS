module JoeChakra.Main
open Microsoft.FSharp.Math
open Microsoft.FSharp.Collections
open System.Collections.Generic;
 
type Move= {
   d:JoeChakra.BrainVita.Direction
   x:int
   y:int
}
 
let moves = new System.Collections.Generic.List<Move> ()
//let mutable dir=North
let mutable count=32
let mutable found=true
let dirs = [
    JoeChakra.BrainVita.Direction.East;
    JoeChakra.BrainVita.Direction.South;
    JoeChakra.BrainVita.Direction.West;
    JoeChakra.BrainVita.Direction.North]
 
let mutable totalMoves = 0;
let printMove X =  printfn "%d,%d %A" X.x X.y X.d
 
let rec BVMove i0 j0 d0 =
    let (found,d,i,j) = JoeChakra.BrainVita.findMove d0 i0 j0
    if found then
        JoeChakra.BrainVita.move dirs.[d] i j |> ignore
        totalMoves<-totalMoves+1
        count<-count-1
        let move = {d=dirs.[d]; x=i; y=j}
        moves.Add(move)
        if count=1 then
            moves |> Seq.iter printMove |>ignore
            printfn ";--------total moves %d----------" totalMoves
            true
        else if BVMove 0 0 0 then
            true
        else
            JoeChakra.BrainVita.reverse dirs.[d] i j |> ignore
            moves.RemoveAt(moves.Count-1)
            count<-count+1
            let (k,l,m) = JoeChakra.BrainVita.incr i j d
            BVMove k l m
    else
        false
 

// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    BVMove 0 0 0  |> ignore// Learn more about F# at http://fsharp.net
    0 // return an integer exit code
