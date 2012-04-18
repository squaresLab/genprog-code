let main () = begin
  try Scanf.scanf " %g" (fun x ->
    Printf.printf "%g" (1.0 /. x)
  ) with _ -> Printf.printf "0." 
end ;;
main () ;;
