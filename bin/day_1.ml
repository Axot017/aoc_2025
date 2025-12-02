let day_1_file = "data/day_1.txt"

type direction = Left | Right
type instruction = { dir : direction; dist : int }

let parse line =
  if String.length line = 0 then None
  else
    let dir =
      match line.[0] with
      | 'L' -> Left
      | 'R' -> Right
      | _ -> failwith ("Invalid direction: " ^ line)
    in
    let dist = int_of_string (String.sub line 1 (String.length line - 1)) in
    Some { dir; dist }

let next_position ~current { dir; dist } =
  let offset = match dir with Right -> dist | Left -> -dist in
  let target = current + offset in
  let next = ((target mod 100) + 100) mod 100 in

  let crossings =
    match dir with
    | Left ->
        if target < 0 then
          if current = 0 then -target / 100 else (-target / 100) + 1
        else if next = 0 && dist < 100 then 1
        else 0
    | Right -> if target > 99 then ((target - 100) / 100) + 1 else 0
  in

  (next, crossings)

let count_zeros instructions =
  instructions
  |> List.fold_left
       (fun (count, pos) instr ->
         let new_pos, increment = next_position ~current:pos instr in
         let new_count = count + increment in
         (new_count, new_pos))
       (0, 50)
  |> fst

let run () =
  In_channel.with_open_text day_1_file In_channel.input_lines
  |> List.filter_map parse |> count_zeros
  |> Printf.printf "Day 1 - Final Result: %d\n"
