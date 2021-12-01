module Day1

let readLines filePath = System.IO.File.ReadLines filePath

let input() =
    readLines "input.txt"
    |> Seq.map int

let test = seq { 199;
                 200;
                 208;
                 210;
                 200;
                 207;
                 240;
                 269;
                 260;
                 263}


let part1 measurements =
    let first = Seq.head measurements
    Seq.fold (fun (prev, n) cur -> (cur, if cur > prev then n+1 else n))
             (first, 0) (Seq.tail measurements)
    |> snd

let answer1 = input() |> part1

let part2 (measurements : int seq) =
    Seq.windowed 3 measurements
    |> Seq.map Array.sum
    |> part1

let answer2 = input() |> part2
