(*path where our atomizer is located*)
let path_to_atomizer = "../javaatomizer.py"

(*add parent filename to these so we know which file the came from*)
type pfilename = string (*what file should we write this node to? (for the not-yet-implemented multifile repairs)*)
type atom_id = int
type atom_text = string

type ast_node =
|Empty
|Leaf_node of pfilename*atom_id*atom_text 
|Branch_node of pfilename*atom_id*ast_node list
|Stem_node of pfilename*atom_text*ast_node (*Stems hold imports*)
|Trunk_node of pfilename*atom_text*ast_node list (*trunks hold classes/method declarations*)

let dummyfile = Empty

(*grabs the string value of a leaf or trunk, or 
  raises an error for other types of branches*)
let string_value node =
  match node with
  |Trunk_node (_, text, _) -> text
  |Stem_node (_, text, _) -> text
  |Branch_node(_, _, _) -> failwith "Attempted to call string_value on a Branch node: Branch nodes do not have text"  
  |Leaf_node(_, _, text) -> text
  |Empty -> failwith "Attemped to call string_value on the Empty node: Empty nodes do not have text"
  
let build_ast filepath = begin
  (*to debug atomizer, write True instead of False (case sensitive) *)
  let atomizer_cmd = Printf.sprintf "python %s %s empty False" path_to_atomizer filepath in
  let _ = Unix.system atomizer_cmd in
  (* Build the tokens list from a .java.atoms file*)
  let atomizer_filepath = Printf.sprintf "%s.atoms" filepath in
  let file = open_in atomizer_filepath in
  
  (*grab all tokens from the file *)
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
  
  (*is this token a class or method? this only works for GCD right now.
    Do all classes/methods in java start with "public" or "private"?*)  
  let is_class_or_method str = 
    if String.length str > 6
      then match String.sub str 0 6 with
      | "public"
      | "class "
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
    | token::remaining_tokens when (is_class_or_method token) ->  
          let current_token_text = List.hd !tokens in
          consume_token () ;
          Trunk_node(filepath, current_token_text, [parse_element ()])
    | any_other_token -> 
          let current_token_text = List.hd !tokens in
          consume_token ();
          let result_node = Leaf_node(filepath, !current_id, current_token_text) in
          increment_id ();
          result_node
             
  and parse_trunk () =
    match !tokens with
    |token::remaining_tokens when (is_class_or_method token) -> 
        consume_token ();
        Trunk_node(filepath, token, [parse_trunk ()])
    |token::remaining_tokens when (is_import_or_package token) ->
        consume_token ();
        Stem_node(filepath, token, parse_trunk ())
    |"{"::remaining_tokens -> parse_block ()
    |any_other_token -> parse_element () in
          
  Trunk_node(filepath, "//Trunk of ast", [parse_trunk ()])
end
      
(*prints the raw ast, _not_ pretty printing*)
let rec print ast = 
  match ast with
  | Trunk_node (_, text, ast_list) -> 
        Printf.printf "Trunk with text: %s\n" text; List.iter (fun inner_ast -> print inner_ast) ast_list
  | Stem_node (_, text, ast_node) -> 
        Printf.printf "Stem with text: %s\n" text; print ast_node
  | Branch_node (_, id, ast_list) -> 
        Printf.printf "Branch with ID:%d\n" id; List.iter (fun inner_ast -> print inner_ast) ast_list
  | Leaf_node (_, id, text) -> 
        Printf.printf "Leaf#%d with text: %s\n" id text
  | Empty -> ()
  
(* returns the node in "ast" with id "id", or raises a not found error*)
let get_node ast id = begin
  let correct_node = ref Empty in
  let rec get ast id = 
    match ast with
    |Trunk_node (_, _, ast_list) -> List.iter (fun inner_node -> get inner_node id) ast_list
    |Stem_node (_, _, ast_node) -> get ast_node id
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
  end
  

(*pretty prints the AST "ast" to "filepath"*)
let write ast filepath = begin
  let file = open_out filepath in
  let rec write_node ast = 
  match ast with
  |Trunk_node (_, text, ast_list) -> 
        output_string file (text ^ "\n");
        List.iter (fun x -> write_node x) ast_list
  |Stem_node (_, text, ast_node) -> 
        output_string file (text ^ "\n");
        write_node ast_node
  |Branch_node (_, id, ast_list) -> 
        begin output_string file "{\n"; 
        List.iter (fun x -> write_node x) ast_list;
        output_string file "}\n"
        end
  |Leaf_node (_, id, text) -> 
        output_string file (text ^ "\n")
  |Empty -> () in
  write_node ast;
  close_out file;
  end

(*gets the number of used IDs*)
(*perhaps this could just be !current_id if we move the scope?*)
let get_max_id ast = begin
  let current_max = ref 0 in
  let rec max_id ast =
    match ast with 
    |Trunk_node (_, _, ast_list) -> 
          List.iter (fun x -> max_id x) ast_list
    |Stem_node (_, _, ast_node) -> 
          max_id ast_node
    |Leaf_node (_, id, _) -> 
          if id > !current_max 
            then current_max := id 
            else ()
    |Branch_node (_, id, ast_list) -> 
          if id > !current_max 
            then current_max := id 
            else List.iter (fun x -> max_id x) ast_list 
    |Empty -> () in
  let _ = max_id ast in
  if !current_max == 0 
    then failwith "No nodes found or current max is 0"
    else !current_max - 1 (*we increment after every token, whether there is a next one or not*)
  end

(*swap the nodes with id's "id1" and "id2" *)
let swap ast id1 id2 = begin
  let node1 = get_node ast id1 in
  let node2 = get_node ast id2 in
  let rec swap_nodes ast node1 node2 id1 id2 = 
    match ast with
    |Trunk_node (file, text, ast_list) -> 
          Trunk_node (file, text, List.map (fun x -> swap_nodes x node1 node2 id1 id2) ast_list)
    |Stem_node (file, text, ast_node) -> 
          Stem_node (file, text, swap_nodes ast_node node1 node2 id1 id2)
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
  end

(*Replaces the node with id "id1" with the node "new_node"*)
let replace ast id1 new_node = begin
  let rec replace_node ast id1 new_node =
    match ast with
    |Trunk_node (file, text, ast_list) -> 
          Trunk_node (file, text, List.map (fun x -> replace_node x id1 new_node) ast_list )
    |Stem_node (file, text, ast_node) -> 
          Stem_node (file, text, replace_node ast_node id1 new_node)
    |Leaf_node (file, id, text) -> 
          if id1 == id then new_node else Leaf_node (file, id, text)
    |Branch_node (file, id, ast_list) ->
            if id == id1 
              then new_node
              else Branch_node (file, id, List.map (fun x-> replace_node x id1 new_node) ast_list) 
    |Empty -> Empty in
  replace_node ast id1 new_node
  end

(*replaces the node with id id1 with Empty*)
let delete ast id1 = begin
  let delete_flag = ref false in
  let rec delete_node ast id1 =
    match ast with 
    |Trunk_node (file, text, ast_list) -> 
          Trunk_node (file, text, List.map (fun x -> delete_node x id1) ast_list)
    |Stem_node (file, text, ast_node) -> 
          Stem_node (file, text, delete_node ast_node id1)
    |Leaf_node (file, id, text) -> 
          if id == id1 
            then begin delete_flag := true; Empty end
            else Leaf_node (file, id, text)
    |Branch_node (file, id, ast_list) -> 
          if id1 == id 
            then begin delete_flag := true; Empty  end
            else Branch_node (file, id, List.map (fun x -> delete_node x id1) ast_list)
   |Empty -> Empty in
  let result = delete_node ast id1 in
  if !delete_flag == true
    then result
    else failwith "Node to delete was not found"
  end
    
(*get the id from a node, or raise an error if the node has none*)
let get_id node =
  match node with 
  |Trunk_node (_, _, _) -> failwith "Attempted to call get_id on node Trunk"
  |Stem_node (_, _, _) -> failwith "Attempted to call get_id on node Stem"
  |Branch_node (_, id, _) -> id
  |Leaf_node (_, id, _) -> id
  |Empty -> failwith "Attempted to call get_id on node Empty"

(*helper function for append. This looks to see if a Node has a 
  node with id "append_after_id" in ONLY its own ast_list*)
let has_id (ast_with_list:ast_node) (append_after_id:atom_id) : bool = begin
  match (ast_with_list:ast_node) with 
  |Trunk_node (_,_,ast_list)
  |Branch_node (_,_,ast_list) -> 
    let result = ref false in  
    List.iter (fun x -> try   
                          let id = get_id x in
                          if id = append_after_id
                            then result := true
                            else ()
                        with Failure _ -> () ) ast_list;
    !result
  |otherwise -> false end

(*takes a copy of the node with id "append_after_id" and 
  places it after the node with id "what_id"*)
let append (ast:ast_node) (append_after_id:atom_id) (what_id:atom_id) : ast_node = begin
  let what_atom = get_node ast what_id in
  let success = ref false in
  
  (*how it works: look in the node's ast_list. if you find the correct node,
    go through your list and append the new node right after it. otherwise call
    yourself recursively until you find the node which has the correct id'ed node
    in its ast_list.*)
  let rec append_ast ast after_id what_id : ast_node = begin
  match ast with
  |Trunk_node (file, text, ast_list) -> 
      let node = Trunk_node (file, text, ast_list) in
      if (has_id node after_id)
        then begin
          let newlist = ref [] in
          List.iter (fun x -> 
            match x with 
            |Trunk_node (file, text, ast_list)-> 
                  newlist := (Trunk_node (file, text, ast_list))::!newlist
            |Stem_node (file, text, ast_node)-> 
                  newlist := (Stem_node (file, text, ast_node))::!newlist
            |Branch_node (file, id, ast_list)-> 
                  if id = append_after_id 
                    then newlist := what_atom::(Branch_node (file, id, ast_list))::!newlist
                    else newlist := (Branch_node (file, id, ast_list))::!newlist                                        
            |Leaf_node (file, id, text) -> 
                  if id = append_after_id 
                    then newlist := what_atom::(Leaf_node (file, id, text))::!newlist
                    else newlist := (Leaf_node (file, id, text))::!newlist
            |Empty -> ()
                    ) ast_list;
        success := true;
        Trunk_node (file, text, List.rev !newlist)
        end
      else Trunk_node (file, text, List.map (fun x-> append_ast x after_id what_id) ast_list)
      
  |Stem_node (file, text, ast_node) -> 
      Stem_node (file, text, (append_ast ast_node after_id what_id)) 
  |Branch_node (file, id, ast_list)-> 
      let node = Branch_node (file, id, ast_list) in
      if has_id node after_id
        then begin
          let newlist = ref [] in
          List.iter (fun x -> 
            match x with 
            |Trunk_node (file, text, ast_list)-> 
                  newlist := (Trunk_node (file, text, ast_list))::!newlist
            |Stem_node (file, text, ast_node)-> 
                  newlist := (Stem_node (file, text, ast_node))::!newlist
            |Branch_node (file, id, ast_list)-> 
                  if id = after_id 
                    then newlist := what_atom::(Branch_node (file, id, ast_list))::!newlist
                    else newlist := (Branch_node (file, id, ast_list))::!newlist                                        
            |Leaf_node (file, id, text)-> 
                  if id = after_id 
                    then newlist := what_atom::(Leaf_node (file, id, text))::!newlist
                    else newlist := (Leaf_node (file, id, text))::!newlist
            |Empty -> ()
                    ) ast_list;
          success := true;
          Branch_node (file, id, List.rev !newlist)
          end
        else Branch_node (file, id, (List.map (fun x -> append_ast x after_id what_id) ast_list))
  |Leaf_node (file, id, test)  -> Leaf_node (file, id, test) 
  |Empty -> Empty 
  end in
let result = ref Empty in 
let appended = append_ast ast append_after_id what_id in 
if !success == true
  then result := appended
  else failwith (Printf.sprintf "Attempted to call append on a node after an id (%d) which is not in the ast." what_id);
  let answer = !result in 
  answer
end 
  

  

