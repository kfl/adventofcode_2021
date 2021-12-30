
let readLines filePath = System.IO.File.ReadLines filePath

let digitToInt c = int c - int '0'
let isDigit c = System.Char.IsDigit c

(* Small ad-hoc parser combinator lib, overkill for the task at hand *)
type 'a parser = string -> ('a * string) option

let ret x inp = Some(x, inp)
let pchar c inp =
    if String.length inp = 0 || inp.[0] <> c then None
    else Some(c, inp.[1..])

let satisfy pred inp =
    if String.length inp = 0 || not (pred inp.[0]) then None
    else Some(inp.[0], inp.[1..])

let fmap (f: 'a -> 'b) (p: 'a parser) inp =
    p inp |> Option.bind(fun (x, inp) -> Some(f x, inp))
let ( |>> ) p f = fmap f p

let andThen parser1 parser2 inp =
    parser1 inp |> Option.bind (fun (r1, inp) ->
    parser2 inp |> Option.bind (fun (r2, inp) ->
    Some((r1,r2), inp)))

let (.>>.) : 'a parser -> 'b parser -> ('a * 'b ) parser = andThen
let (.>>) p1 p2 = p1 .>>. p2 |> fmap fst
let (>>.) p1 p2 = p1 .>>. p2 |> fmap snd

let orElse (parser1 : 'a parser) (parser2 : 'a parser) inp =
    parser1 inp |> Option.orElseWith (fun() -> parser2 inp)
let ( <|> ) : 'a parser -> 'a parser -> 'a parser = orElse

let between start close inner = start >>. inner .>> close

let parseWith p inp =
    match p inp with
        | Some(x, "") -> Some x
        | _ -> None
(* end of parser combinator lib *)


type Pair =
    | Reg of int
    | Pair of Pair * Pair

let parse line =
    let rec pair inp = (regular <|> actual) inp
    and actual = between (pchar '[') (pchar ']') (pair .>> pchar ',' .>>. pair |>> Pair)
    and regular = satisfy isDigit |>> (Reg << digitToInt)

    Option.get <| parseWith pair line

let input =
    readLines "input.txt"
    |> Seq.map parse
    |> Seq.toList


let rec addLeft k = function
    | Reg n -> Reg (k+n)
    | Pair(p1, p2) -> Pair (addLeft k p1, p2)

let rec addRight k = function
    | Reg n -> Reg (k+n)
    | Pair (p1, p2) -> Pair (p1, addRight k p2)

let rec explodeAt level = function
    | Reg _ -> None
    | Pair (Reg r1, Reg r2) when level > 3 -> Some (Reg 0, r1, r2)
    | Pair (p1, p2) ->
        match explodeAt (level+1) p1 with
            | Some (p1, r1, r2) -> Some (Pair (p1, addLeft r2 p2), r1, 0)
            | _ -> explodeAt (level+1) p2
                   |> Option.bind(fun (p2, r1, r2) ->
                      Some (Pair (addRight r1 p1, p2), 0, r2))

let rec splits = function
    | Reg r when r > 9 -> Some(Pair(Reg (r / 2), Reg ((r + 1) / 2)))
    | Reg _ -> None
    | Pair (p1, p2) ->
        match splits p1 with
            | Some p1 -> Some(Pair(p1, p2))
            | _ -> splits p2
                   |> Option.bind(fun p2 -> Some(Pair(p1, p2)))

let action n =
    match explodeAt 0 n with
        | Some (n, _, _) -> Some n
        | _  -> splits n

let rec reduce n =
    match action n with
        | None -> n
        | Some n -> reduce n

let add n1 n2 = reduce(Pair(n1, n2))

let rec magnitude = function
    | Reg n -> n
    | Pair (p1, p2) -> 3 * magnitude p1 + 2 * magnitude p2

let part1 input =
    List.fold add (List.head input) (List.tail input)
    |> magnitude

let answer1 = part1 input

let rec combinations ns =
    match ns with
        | [] -> Seq.empty
        | n1 :: rest -> seq { for n2 in rest do yield n1, n2
                              yield! combinations rest }

let part2 input =
    seq { for (x,y) in combinations input do
            yield add x y
            yield add y x }
    |> Seq.map magnitude
    |> Seq.max

let answer2 = part2 input

do printfn "Part 1 answer: %A (expected 3806)" answer1
   printfn "Part 2 answer: %A (expected 4727)" answer2
