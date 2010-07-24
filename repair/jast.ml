let version = 0.5
let path_to_atomizer = "javaatomizer.py"
let path_to_atomizer = "current/javaatomizer.py"

(*make next id 0 in replaces etc*)
(*administrator object that keeps track of id numbers*)
(*detect method names in the lexer*)

type ast_node =
|Empty
|Leaf_node of int*string
|Branch_node of int*ast_node list
|Trunk_node of string*ast_node list (*trunks have no ID and thus cannot be swapped*)

(*grabs the string value of a leaf, or raises an *
 * error for other tyupes of branches            *)
let string_value node =
  match node with
  |Trunk_node (text, ast_list) -> text
  |Branch_node(id, node_list) -> failwith "Attempted to call string_value on a Branch node: Branch nodes do not have text"  
  |Leaf_node(id, text) -> text
  |Empty -> failwith "Attemped to call string_value on the Empty node: Empty nodes do not have text"
  

let grab_imports token_list = begin
  let imports = ref [] in
  
  (*Catch all import statements*)
  let import_flag = ref false in
  begin try
    while !import_flag == false do
      let current_token = List.hd !token_list in
      if String.length current_token < 7
        then import_flag := true
        else
          let substring = String.sub current_token 0 6 in
          if ((substring == "import") or (substring == "packag"))
            then imports := current_token::!imports
            else import_flag := true
    done
  with _ -> Printf.printf "%s\n" "Error with imports" end;          
  List.rev !imports
end


let build_ast filepath = begin
  let _ = Unix.system ("python "^path_to_atomizer^" "^ filepath ^ " empty False") in
  (* Build the tokens list from a .java.atoms file*)
  let file = open_in (filepath ^ ".atoms") in
  let tokens = ref [] in
  begin try
    while true do 
      let line = input_line file in
      tokens := line::!tokens
    done
  with End_of_file -> () end;
  let tokens = ref (List.rev !tokens) in        
  let current_id = ref 1 in      
  
  let is_import_or_package str =
    if String.length str > 6
      then match String.sub str 0 6 with
      |"import" -> true
      |"packag" -> true
      | _ -> false
      else false in
   
  let is_class_or_method str = 
    if String.length str > 6
      then match String.sub str 0 6 with
      | "public"
      | "privat" -> true
      | _ -> false
      else false in
  
  (*both consumes a token and increments the counter *)
  let consume_token () =
    tokens := List.tl !tokens in
  let increment_id () = 
    current_id := (!current_id + 1) in
  
  (*The actual parsing*)
  let rec parse_block () = 
    match !tokens with
    | "{"::remaining_tokens -> 
          consume_token ();
          let result_node = Branch_node(!current_id, parse_list ()) in
          increment_id ();
          result_node
    | _ -> failwith "Failure in parse_block inside Jast.build_ast: Error in source, left brace expected"
    
  and parse_list () = 
    match !tokens with
    | "}"::remaining_tokens -> 
          consume_token ();
          []
    | other_tokens -> let element = parse_element () in 
          element::parse_list ()
    
  and parse_element () = 
    match !tokens with
    | "{"::remaining_tokens -> parse_block ()
    | any_other_token -> 
          let current_token_text = List.hd !tokens in
          consume_token ();
          let result_node = Leaf_node(!current_id, current_token_text) in
          increment_id ();
          result_node
          
          
  and parse_trunk () =
    match !tokens with
    | token::remaining_tokens when (is_class_or_method token) -> 
        consume_token ();
        Trunk_node(token, [parse_trunk ()])
    |token::remaining_tokens when (is_import_or_package token) ->
        consume_token ();
        Trunk_node(token, [parse_trunk ()])
    |"{"::remaining_tokens -> parse_block ()
    |any_other_token -> parse_element () in
          
  Trunk_node("//Trunk of ast", [parse_trunk ()])
end
      
(*prints the raw ast, _not_ pretty printing*)
let rec print_ast ast = 
  match ast with
  | Trunk_node (text, ast_list) -> Printf.printf "Trunk with text: %s\n" text; List.iter (fun inner_ast -> print_ast inner_ast) ast_list
  | Branch_node (id, ast_list) -> Printf.printf "Branch with ID:%d\n" id; List.iter (fun inner_ast -> print_ast inner_ast) ast_list
  | Leaf_node (id, text) -> Printf.printf "Leaf#%d with text: %s\n" id text
  | Empty -> ()
  
(* returns the node in "ast" with id "id", or raises a not found error*)
let get_node ast id = 
  let correct_node = ref Empty in
  let rec get ast id = 
    match ast with
    |Trunk_node (text, ast_list) -> List.iter (fun inner_node -> get inner_node id) ast_list
    |Branch_node (node_id, ast_list) -> 
      if node_id == id 
        then begin 
            correct_node := Branch_node (node_id, ast_list) 
          end
        else List.iter (fun inner_node -> get inner_node id) ast_list
    |Leaf_node (node_id, text) -> 
      if node_id == id
        then correct_node := Leaf_node (node_id, text)
        else ()
    |Empty -> () in
  get ast id;
  let correct_node = !correct_node in
  match correct_node with
  | Empty -> failwith "Error in get_node: No node with that ID found"
  | otherwise -> otherwise
  

(*pretty prints the AST to filepath*)
let write ast filepath =
  let file = open_out filepath in
  let rec write_node ast = 
  match ast with
  |Trunk_node (text, ast_list) -> output_string file (text ^ "\n");
                                  List.iter (fun x -> write_node x) ast_list
  |Branch_node (id, ast_list) -> begin output_string file "{\n"; 
                                  List.iter (fun x -> write_node x) ast_list;
                                  output_string file "}\n"
                                  end
  |Leaf_node (id, text) -> output_string file (text ^ "\n")
  |Empty -> () in
  write_node ast;
  close_out file


let get_max_id ast = 
  let current_max = ref 0 in
  let rec max_id ast =
    match ast with 
    |Trunk_node (_, ast_list) -> List.iter (fun x -> max_id x) ast_list
    |Leaf_node (id,_) -> if id > !current_max then current_max := id else ()
    |Branch_node (id, ast_list) -> if id > !current_max then current_max := id else List.iter (fun x -> max_id x) ast_list 
    |Empty -> () in
  let _ = max_id ast in
  if !current_max == 0 
    then failwith "No nodes found or current max is 0"
    else !current_max - 1 (*we increment after every token, whether there is a next one or not*)
  
let copy ast=
  let rec make_copy ast = 
    match ast with 
    |Trunk_node (text, ast_list) -> Trunk_node(text, List.map (fun x -> make_copy x) ast_list)
    |Branch_node (id, ast_list) -> Branch_node (id, List.map (fun x -> make_copy x) ast_list)
    |Leaf_node (id, text) -> Leaf_node(id, text)
    |Empty -> Empty in
   make_copy ast

let swap ast id1 id2 = 
  let node1 = get_node ast id1 in
  let node2 = get_node ast id2 in
  let rec swap_nodes ast node1 node2 id1 id2 = 
    match ast with
    |Trunk_node (text, ast_list) -> Trunk_node (text, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list)
    |Branch_node (id, ast_list) ->
        if id == id1
          then node2
        else if id == id2
          then node1
          else Branch_node(id, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list) 
    |Leaf_node (id, text) -> 
        if id == id1 
          then node2 
        else if id == id2
          then node1
          else Leaf_node (id, text)
    |Empty -> Empty in
  swap_nodes ast node1 node2 id1 id2

(*Replaces the node with id with new_node*)
let replace ast id1 new_node =
  let rec replace_node ast id1 new_node =
    match ast with
    |Trunk_node (text, ast_list) -> Trunk_node (text, List.map (fun x -> replace_node x id1 new_node) ast_list )
    |Leaf_node (id, text) -> if id1 == id then new_node else Leaf_node (id, text)
    |Branch_node (id, ast_list) ->
      if id == id1 
        then new_node
        else Branch_node (id, List.map (fun x-> replace_node x id1 new_node) ast_list) 
    |Empty -> Empty in
  replace_node ast id1 new_node
  
let delete ast id1 =
  let delete_flag = ref false in
  let rec delete_node ast id1 =
    match ast with 
    |Trunk_node (text, ast_list) -> Trunk_node (text, List.map (fun x -> delete_node x id1) ast_list)
    |Leaf_node (id, text) -> if id == id1 
                              then begin delete_flag := true; Empty end
                              else Leaf_node (id, text)
    |Branch_node (id, ast_list) -> if id1 == id 
                                    then begin delete_flag := true; Empty  end
                                    else Branch_node (id, List.map (fun x -> delete_node x id1) ast_list)
   |Empty -> Empty in
  let result = delete_node ast id1 in
  if !delete_flag == true
    then result
    else failwith "Node to delete was not found"
let get_id node =
  match node with 
  |Trunk_node (_,_) -> failwith "Attempted to call get_id on node Trunk"
  |Branch_node (id, _ ) -> id
  |Leaf_node (id, _ ) -> id
  |Empty -> failwith "Attempted to call get_id on node Empty"
  
let append ast append_after_id what = 
  let success = ref false in
  let rec append_node ast append_after_id what =
    match ast with
    |Trunk_node (text, ast_list) -> Trunk_node (text, List.map (fun x -> append_node x append_after_id what) ast_list)
    |Leaf_node (id, text) -> Leaf_node (id, text)
    |Branch_node (id, ast_list) -> let new_ast_list = ref [] in
                                     List.iter (
                                       fun node -> if node != Empty 
                                                    then if get_id node == append_after_id
                                                           then begin
                                                             new_ast_list := node::what::!new_ast_list;
                                                             success := true
                                                             end
                                                           else new_ast_list := node::!new_ast_list
                                                    else () (*this meant that Empty's are dropped!!*)
                                     )ast_list;
                                     if !success == true 
                                       then Branch_node(id, !new_ast_list) 
                                       else Branch_node(id, List.map (fun x -> append_node x append_after_id what)!new_ast_list)
    |Empty -> Empty in
  let appended = append_node ast append_after_id what in
  if !success == true
    then appended
    else failwith "Attempted to call append on a node after an id which is not in the ast."
    
let dummyfile = Empty

(*
let main () = begin
let my_ast = build_ast "C:/cygwin/home/Mike/current/gcd.java" in
print_ast my_ast;
print_int (get_max_id my_ast);
write my_ast "current/gcd.java"


end;;

main ()
*) 

  
