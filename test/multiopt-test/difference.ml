let main () = begin
  if Array.length Sys.argv < 3 then begin
    Printf.printf "%g\n" infinity 
  end else begin 
    let a = float_of_string Sys.argv.(1) in 
    let b = float_of_string Sys.argv.(2) in 
    Printf.printf "%g\n" (abs_float (a -. b))
  end 
end ;;
main () ;;
