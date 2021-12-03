
fun readLines filename =
    let val ins = TextIO.openIn filename
    in  String.tokens Char.isSpace (TextIO.inputAll ins)
        before TextIO.closeIn ins
    end

infix |>
fun x |> f = f x

fun zipWith f xs ys =
    let fun h (x::xr) (y::yr) res = h xr yr (f (x, y) :: res)
	  | h _       _       res = List.rev res
    in h xs ys [] end;


fun parse line = String.explode line

fun input() = readLines "input.txt" |> map parse


val test = map parse [ "00100"
                     , "11110"
                     , "10110"
                     , "10111"
                     , "10101"
                     , "01111"
                     , "00111"
                     , "11100"
                     , "10000"
                     , "11001"
                     , "00010"
                     , "01010"]

(* A useful monoid *)
datatype bal = Bal of int list
fun mempty n = Bal (List.tabulate(n, fn _ => 0))
infix ++
fun (Bal xs) ++ (Bal ys) =
    Bal(zipWith op+ xs ys)

fun effect line = Bal(map (fn #"0" => ~1 | #"1" => 1
                           | _ => raise Fail "shouldn't happen") line)

fun readout (bal, (gamma, epsilon)) =
    if bal > 0 then (2*gamma+1, 2*epsilon) else (2*gamma, 2*epsilon+1)

fun part1 input =
    let val init = hd input |> length |> mempty
        val Bal balances = foldl (op ++) init (map effect input)
        val (gamma, epsilon) = foldl readout (0,0) balances
    in  gamma * epsilon
    end

val answer1 = part1(input())


fun partition(cs, (cz, zeros, co, ones)) =
    if hd cs = #"0" then (cz+1, (tl cs) :: zeros, co, ones)
    else (cz, zeros, co+1, (tl cs) :: ones)

fun rating [result] _ acc = (rev acc) @ result
  | rating input select acc =
    let val init = (0, [], 0, [])
        val (cz, zeros, co, ones) = foldl partition init input
        val (c, filtered) = if select (co-cz) then (#"1", ones)
                            else (#"0", zeros)
    in  rating filtered select (c :: acc)
    end

fun parseBin cs =
    Int.scan StringCvt.BIN List.getItem cs |> valOf |> #1

fun part2 input =
    let val oxygen = rating input (fn x => x >= 0) []
        val co2 = rating input (fn x => x < 0) []
    in parseBin oxygen * parseBin co2
    end

val answer2 = input() |> part2
