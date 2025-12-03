let day_3_file = "data/day_3.txt"
let explode s = List.init (String.length s) (String.get s)
let char_to_int c = int_of_string (String.make 1 c)

let rec drop_until_after v lst =
  match lst with
  | [] -> []
  | x :: xs -> if x = v then xs else drop_until_after v xs

let find_best_digit digits need =
  let len = List.length digits in
  match digits with
  | [] -> (0, [])
  | [ x ] -> (x, [])
  | head :: tail when len <= need -> (head, tail)
  | _ ->
      let best =
        digits
        |> List.filteri (fun i _ -> i <= len - need - 1)
        |> List.fold_left max 0
      in
      (best, drop_until_after best digits)

let rec pick remaining need acc =
  if need = 0 then acc
  else
    let best, rest = find_best_digit remaining (need - 1) in
    pick rest (need - 1) ((acc * 10) + best)

let highest_n_digit_number n str =
  let digits = str |> explode |> List.map char_to_int in
  pick digits n 0

let highest_number str = highest_n_digit_number 12 str

let run () =
  In_channel.with_open_text day_3_file In_channel.input_lines
  |> List.map highest_number |> List.fold_left ( + ) 0
  |> Printf.printf "Day 3 - Final Result: %d\n"
