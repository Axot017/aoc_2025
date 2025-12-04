let day_4_file = "data/day_4.txt"

let parse_grid lines =
  let arr = Array.of_list lines in
  let height = Array.length arr in
  let width = Array.fold_left (fun m s -> max m (String.length s)) 0 arr in
  let grid = Array.make_matrix height width false in
  for y = 0 to height - 1 do
    let line = arr.(y) in
    let len = String.length line in
    for x = 0 to len - 1 do
      grid.(y).(x) <- line.[x] = '@'
    done
  done;
  (grid, height, width)

let count_adjacent grid height width y x =
  let count = ref 0 in
  for dy = -1 to 1 do
    for dx = -1 to 1 do
      if not (dy = 0 && dx = 0) then begin
        let ny = y + dy in
        let nx = x + dx in
        if ny >= 0 && ny < height && nx >= 0 && nx < width && grid.(ny).(nx)
        then incr count
      end
    done
  done;
  !count

let find_accessible grid height width =
  let accessible = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if grid.(y).(x) then begin
        let adjacent = count_adjacent grid height width y x in
        if adjacent < 4 then accessible := (y, x) :: !accessible
      end
    done
  done;
  !accessible

let solve grid height width =
  let total = ref 0 in
  let continue = ref true in
  while !continue do
    let accessible = find_accessible grid height width in
    let count = List.length accessible in
    if count > 0 then begin
      List.iter (fun (y, x) -> grid.(y).(x) <- false) accessible;
      total := !total + count
    end
    else continue := false
  done;
  !total

let run () =
  let grid, height, width =
    In_channel.with_open_text day_4_file In_channel.input_lines |> parse_grid
  in

  solve grid height width |> Printf.printf "Day 4 - Final Result: %d\n"
