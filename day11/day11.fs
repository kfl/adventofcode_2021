module Day11

let readLines filePath = System.IO.File.ReadLines filePath

let digitToInt c = int c - int '0'

let parse line = Seq.map digitToInt line

let test = Seq.map parse [ "5483143223"
                           "2745854711"
                           "5264556173"
                           "6141336146"
                           "6357385478"
                           "4167524645"
                           "2176841721"
                           "6882881134"
                           "4846848554"
                           "5283751526" ]

let input =
    Seq.map parse <| readLines "input.txt"

let neighbours grid i j =
    seq {for x in i-1 .. i+1 do
         if 0 <= x && x < Array2D.length1 grid then
            for y in j-1 .. j+1 do
            if 0 <= y && (x,y) <> (i,j) && y < Array2D.length2 grid then x,y}


let increase (grid: int [,]) idxs =
    Seq.collect (fun ((i,j) as idx)->
                 let v = (grid.[i,j] + 1) % 10
                 grid.[i,j] <- v
                 if v = 0 then seq { idx } else Seq.empty) idxs

let rec flood grid flashes = function
    | [] -> flashes
    | ((i,j) as idx) :: worklist ->
        let relevant = seq { for x,y in neighbours grid i j do
                             if grid.[x,y] <> 0 then x,y }
        let flashed = increase grid relevant |> List.ofSeq
        flood grid (flashes + List.length flashed) (flashed @ worklist)

let step grid =
    let idxs = seq { for i in 0 .. Array2D.length1 grid - 1 do
                     for j in 0 .. Array2D.length2 grid - 1 -> i,j }
    let flashed = increase grid idxs |> List.ofSeq
    flood grid (List.length flashed) flashed


let part1 ns =
    let grid = array2D ns
    let mutable total = 0
    for i = 1 to 100 do
        total <- total + step grid
    total

let answer1 = part1 input


// Why is this not a library function???
let exists p (grid : 'a[,]) =
    Seq.exists p <| Seq.cast<'a> grid

let part2 ns =
    let grid = array2D ns
    let mutable total = 0
    while exists ((<>) 0) grid do
        step grid |> ignore
        total <- total + 1
    total

let answer2 = part2 input
