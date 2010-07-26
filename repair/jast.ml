
(*path where our atomizer is located*)
let path_to_atomizer = "javaatomizer.py"

(*add parent filename to these so we know which file the came from*)
type pfilename = string (*what file should be write this node to?*)
type atom_id = int
type atom_text = string

type ast_node =
|Empty
|Leaf_node of pfilename*atom_id*atom_text
|Branch_node of pfilename*atom_id*ast_node list
|Trunk_node of pfilename*atom_text*ast_node list (*trunks have no ID and thus cannot be swapped*)
|Import_node of pfilename*atom_text*ast_node list

(*grabs the string value of a leaf or trunk, or 
  raises an error for other types of branches*)
let string_value node =
  match node with
  |Import_node (_, text, ast_list) -> text
  |Trunk_node (_, text, ast_list) -> text
  |Branch_node(_, id, node_list) -> failwith "Attempted to call string_value on a Branch node: Branch nodes do not have text"  
  |Leaf_node(_, id, text) -> text
  |Empty -> failwith "Attemped to call string_value on the Empty node: Empty nodes do not have text"


  
  
  
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
  
  (*is this token an import/package? *)
  let is_import_or_package str =
    if String.length str > 6
      then match String.sub str 0 6 with
      |"import" -> true
      |"packag" -> true
      | _ -> false
      else false in
  
  (*is this token a class or method? this only works for GCD right now *)  
  let is_class_or_method str = 
    if String.length str > 6
      then match String.sub str 0 6 with
      | "public"
      | "privat" -> true
      | _ -> false
      else false in
  
  let consume_token () =
    tokens := List.tl !tokens in
  let increment_id () = 
    current_id := (!current_id + 1) in
  

  (*The actual parsing*)
  let rec parse_block () = 
    match !tokens with
    | "{"::remaining_tokens -> 
          consume_token ();
          let result_node = Branch_node(filepath, !current_id, parse_list ()) in
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
          let result_node = Leaf_node(filepath, !current_id, current_token_text) in
          increment_id ();
          result_node
          
          
  and parse_trunk () =
    match !tokens with
    | token::remaining_tokens when (is_class_or_method token) -> 
        consume_token ();
        Trunk_node(filepath, token, [parse_trunk ()])
    |token::remaining_tokens when (is_import_or_package token) ->
        consume_token ();
        Import_node(filepath, token, [parse_trunk ()])
    |"{"::remaining_tokens -> parse_block ()
    |any_other_token -> parse_element () in
          
  Trunk_node(filepath, "//Trunk of ast", [parse_trunk ()])
end
      
(*prints the raw ast, _not_ pretty printing*)
let rec print_ast ast = 
  match ast with
  | Import_node (_, text, ast_list) -> Printf.printf "Import with text %s\n" text; List.iter (fun inner_ast -> print_ast inner_ast) ast_list
  | Trunk_node (_, text, ast_list) -> Printf.printf "Trunk with text: %s\n" text; List.iter (fun inner_ast -> print_ast inner_ast) ast_list
  | Branch_node (_, id, ast_list) -> Printf.printf "Branch with ID:%d\n" id; List.iter (fun inner_ast -> print_ast inner_ast) ast_list
  | Leaf_node (_, id, text) -> Printf.printf "Leaf#%d with text: %s\n" id text
  | Empty -> ()
  
(* returns the node in "ast" with id "id", or raises a not found error*)
let get_node ast id = 
  let correct_node = ref Empty in
  let rec get ast id = 
    match ast with
    |Import_node (_, _, ast_list) -> List.iter (fun inner_node -> get inner_node id) ast_list
    |Trunk_node (_, _, ast_list) -> List.iter (fun inner_node -> get inner_node id) ast_list
    |Branch_node (file, node_id, ast_list) -> 
      if node_id == id 
        then begin 
            correct_node := Branch_node (file, node_id, ast_list) 
          end
        else List.iter (fun inner_node -> get inner_node id) ast_list
    |Leaf_node (file, node_id, text) -> 
      if node_id == id
        then correct_node := Leaf_node (file, node_id, text)
        else ()
    |Empty -> () in
  get ast id;
  let correct_node = !correct_node in
  match correct_node with
  | Empty -> let str = Printf.sprintf "Error in get_node: No node with id %d found" id in
               failwith str
  | otherwise -> otherwise
  

(*pretty prints the AST to filepath*)
let write ast filepath =
  let file = open_out filepath in
  let rec write_node ast = 
  match ast with
  |Import_node (_, text, ast_list) -> output_string file (text ^ "\n");
                                      List.iter (fun x -> write_node x) ast_list
  |Trunk_node (_, text, ast_list) -> output_string file (text ^ "\n");
                                  List.iter (fun x -> write_node x) ast_list
  |Branch_node (_, id, ast_list) -> begin output_string file "{\n"; 
                                  List.iter (fun x -> write_node x) ast_list;
                                  output_string file "}\n"
                                  end
  |Leaf_node (_, id, text) -> output_string file (text ^ "\n")
  |Empty -> () in
  write_node ast;
  close_out file

(*gets the number of used IDs*)
let get_max_id ast = 
  let current_max = ref 0 in
  let rec max_id ast =
    match ast with 
    |Import_node (_, _, ast_list) -> List.iter (fun x-> max_id x) ast_list
    |Trunk_node (_, _, ast_list) -> List.iter (fun x -> max_id x) ast_list
    |Leaf_node (_, id, _) -> if id > !current_max then current_max := id else ()
    |Branch_node (_, id, ast_list) -> if id > !current_max then current_max := id else List.iter (fun x -> max_id x) ast_list 
    |Empty -> () in
  let _ = max_id ast in
  if !current_max == 0 
    then failwith "No nodes found or current max is 0"
    else !current_max - 1 (*we increment after every token, whether there is a next one or not*)

    (*
let copy ast=
  let rec make_copy ast = 
    match ast with 
    |Trunk_node (file, text, ast_list) -> Trunk_node(file, text, List.map (fun x -> make_copy x) ast_list)
    |Branch_node (id, ast_list) -> Branch_node (id, List.map (fun x -> make_copy x) ast_list)
    |Leaf_node (id, text) -> Leaf_node(id, text)
    |Empty -> Empty in
   make_copy ast
*)
(*swaps the nodes with the two given ID's*)

let swap ast id1 id2 = 
  let node1 = get_node ast id1 in
  let node2 = get_node ast id2 in
  let rec swap_nodes ast node1 node2 id1 id2 = 
    match ast with
    |Import_node (file, text, ast_list) -> Import_node (file, text, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list)
    |Trunk_node (file, text, ast_list) -> Trunk_node (file, text, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list)
    |Branch_node (file, id, ast_list) ->
        if id == id1
          then node2
        else if id == id2
          then node1
          else Branch_node(file, id, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list) 
    |Leaf_node (file, id, text) -> 
        if id == id1 
          then node2 
        else if id == id2
          then node1
          else Leaf_node (file, id, text)
    |Empty -> Empty in
  swap_nodes ast node1 node2 id1 id2

(*Replaces the node with id with new_node*)
let replace ast id1 new_node =
  let rec replace_node ast id1 new_node =
    match ast with
    |Import_node (file, text, ast_list) -> Import_node (file, text, List.map (fun x -> replace_node x id1 new_node) ast_list )
    |Trunk_node (file, text, ast_list) -> Trunk_node (file, text, List.map (fun x -> replace_node x id1 new_node) ast_list )
    |Leaf_node (file, id, text) -> if id1 == id then new_node else Leaf_node (file, id, text)
    |Branch_node (file, id, ast_list) ->
      if id == id1 
        then new_node
        else Branch_node (file, id, List.map (fun x-> replace_node x id1 new_node) ast_list) 
    |Empty -> Empty in
  replace_node ast id1 new_node

(*replaces the node with id id1 with Empty*)
let delete ast id1 =
  let delete_flag = ref false in
  let rec delete_node ast id1 =
    match ast with 
    |Import_node (file, text, ast_list) -> Import_node (file, text, List.map (fun x -> delete_node x id1) ast_list)
    |Trunk_node (file, text, ast_list) -> Trunk_node (file, text, List.map (fun x -> delete_node x id1) ast_list)
    |Leaf_node (file, id, text) -> if id == id1 
                              then begin delete_flag := true; Empty end
                              else Leaf_node (file, id, text)
    |Branch_node (file, id, ast_list) -> if id1 == id 
                                    then begin delete_flag := true; Empty  end
                                    else Branch_node (file, id, List.map (fun x -> delete_node x id1) ast_list)
   |Empty -> Empty in
  let result = delete_node ast id1 in
  if !delete_flag == true
    then result
    else failwith "Node to delete was not found"
    
(*get the id from a node, or raise an error if the node has none*)
let get_id node =
  match node with 
  |Import_node (_, _, _) -> failwith "Attempted to call get_id on node Import"
  |Trunk_node (_, _, _) -> failwith "Attempted to call get_id on node Trunk"
  |Branch_node (_, id, _) -> id
  |Leaf_node (_, id, _) -> id
  |Empty -> failwith "Attempted to call get_id on node Empty"

  (*fix append*)
(*append the node with id "what_id" after id "append_after_id"*)
let append ast append_after_id what_id = 
  let success = ref false in
  let rec append_node ast append_after_id what_id =
    match ast with
    |Import_node (file, text, ast_list) -> Import_node (file, text, List.map (fun x -> append_node x append_after_id what_id) ast_list)
    |Trunk_node (file, text, ast_list) -> Trunk_node (file, text, List.map (fun x -> append_node x append_after_id what_id) ast_list)
    |Leaf_node (file, id, text) -> Leaf_node (file, id, text)
    |Branch_node (file,id, ast_list) -> let new_ast_list = ref [] in
                                     List.iter (
                                       fun node -> if node != Empty 
                                                    then if what_id == append_after_id
                                                           then begin
                                                             new_ast_list := node::(get_node ast what_id)::!new_ast_list;
                                                             success := true
                                                             end
                                                           else new_ast_list := node::!new_ast_list
                                                    else () (*this means that Empty's are dropped!*)
                                     )ast_list;
                                     if !success == true 
                                       then Branch_node(file, id, !new_ast_list) 
                                       else Branch_node(file, id, List.map (fun x -> append_node x append_after_id what_id)!new_ast_list)
    |Empty -> Empty in
  let appended = append_node ast append_after_id what_id in
  if !success == true
    then appended
    else failwith "Attempted to call append on a node after an id which is not in the ast."
    
let dummyfile = Empty



  
