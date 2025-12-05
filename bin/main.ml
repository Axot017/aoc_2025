let time_it f =
  let t = Sys.time () in
  let fx = f () in
  Printf.printf "Execution time: %fms\n" ((Sys.time () -. t) *. 1000.);
  fx

let () =
  time_it Day_1.run;
  time_it Day_2.run;
  time_it Day_3.run;
  time_it Day_4.run;
  time_it Day_5.run
