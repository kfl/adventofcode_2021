fun readLines filename =
    let val ins = TextIO.openIn filename
    in  String.tokens (Char.contains "\n\r") (TextIO.inputAll ins)
        before TextIO.closeIn ins
    end

infix |>
fun x |> f = f x

fun sum ns = foldl op+ 0 ns

fun parse line = String.explode line

fun input() = readLines "input.txt" |> map parse

val test = map parse [ "[({(<(())[]>[[{[]{<()<>>"
                     , "[(()[<>])]({[<{<<[]>>("
                     , "{([(<{}[<>[]}>{[]{[(<()>"
                     , "(((({<>}<{<{<>}{[]{[]{}"
                     , "[[<[([]))<([[{}[[()]]]"
                     , "[{[{({}]{}}([{[{{{}}([]"
                     , "{<[[]]>}<{[{[{[]{()[[[]"
                     , "[<(<(<(<{}))><([]([]()"
                     , "<{([([[(<>()){}]>(<<{{"
                     , "<{([{{}}[<[[[<>{}]]]>[]]"
                     ]

val isOpen  = Char.contains "([{<"
val isClose = Char.contains ")]}>"

fun mismatch s e =
    case (s, e) of
        (#"(", #")") => NONE
      | (#"[", #"]") => NONE
      | (#"{", #"}") => NONE
      | (#"<", #">") => NONE
      | _ => SOME e

fun score #")" = 3
  | score #"]" = 57
  | score #"}" = 1197
  | score #">" = 25137
  | score c = raise Fail ("this shouldn't happen: "^str c)

fun part1 input =
    let fun loop stack [] = 0
          | loop stack (i :: input) =
            case (isOpen i, stack) of
                (true, _) => loop (i :: stack) input
              | (_, []) => score i
              | (_, s :: rest) => case mismatch s i of
                                      NONE => loop rest input
                                    | SOME c => score c
    in sum (map (loop []) input)
    end

val answer1 = part1(input())


fun points #"(" = 1
  | points #"[" = 2
  | points #"{" = 3
  | points #"<" = 4
  | points c = raise Fail ("points this shouldn't happen: "^str c)

fun part2 input =
    let val score = foldl (fn (p, acc) => p + 5 * acc) 0
        fun loop stack [] = SOME(map points stack |> score)
          | loop stack (i :: input) =
            case (isOpen i, stack) of
                (true, _) => loop (i :: stack) input
              | (_, []) => NONE
              | (_, s :: rest) => case mismatch s i of
                                       NONE => loop rest input
                                     | SOME _ => NONE
        val scores = List.mapPartial (loop []) input
    in  List.nth(Listsort.sort Int.compare scores, length scores div 2)
    end

val answer2 = part2(input())
