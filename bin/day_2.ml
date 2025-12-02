let day_2_file = "data/day_2.txt"

let first list =
  match list with
  | [ x ] -> x
  | _ -> failwith "Expected a single line in the input file"

let parse str =
  let str = String.split_on_char '-' str in
  match str with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> failwith "Invalid range format"

let expand (a, b) = List.init (b - a) (fun i -> a + i)

let digit_count n =
  let float = float_of_int n in
  Float.(ceil (log10 (float +. 1.0))) |> int_of_float

let rec check_combinations = function
  | d, total, _ when d > total / 2 -> false
  | d, total, n when total mod d <> 0 && d <= total / 2 ->
      check_combinations (d + 1, total, n)
  | d, total, n -> (
      let k = total / d in
      let divisor =
        (int_of_float (10.0 ** float_of_int (d * k)) - 1)
        / (int_of_float (10.0 ** float_of_int d) - 1)
      in
      let remainder = n mod divisor in
      match remainder with
      | r when r = 0 -> true
      | _ -> check_combinations (d + 1, total, n))

let is_repeated n =
  let digits = digit_count n in
  check_combinations (1, digits, n)

let run () =
  In_channel.with_open_text day_2_file In_channel.input_lines
  |> first |> String.split_on_char ',' |> List.map parse |> List.map expand
  |> List.flatten |> List.filter is_repeated |> List.fold_left ( + ) 0
  |> Printf.printf "Day 2 - Final Result: %d\n"
