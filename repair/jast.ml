open Global
open Rep

(*this will be printed at the top of every file we make. optional*)
let master_ast_text = ""
(*add parent filename to these so we know which file the came from*)
type pfilename = string (*what file should we write this node to? *)
type atom_id = int
type atom_text = string

type ast_node =
| Empty
| Leaf_node of pfilename * atom_id * atom_text 
| Branch_node of pfilename * atom_id * (ast_node list)
| Stem_node of pfilename * atom_text * ast_node (*Stems hold imports*)
| File_node of pfilename * (ast_node list)
(*trunks hold classes/method declarations*)
| Trunk_node of pfilename * atom_text * (ast_node list) 

let dummyfile = Empty
let current_id = ref 1

let file_list_path = ref "" 
let path_to_atomizer = ref ""


let _ = 
  options := !options @
  [
    "--atomizer-loc", Arg.Set_string path_to_atomizer, "X path to javaatomizer.py";
    "--file-list-path", Arg.Set_string file_list_path, " X use X as the list of files to compile" ;
  ] 
  
(*get all the files from the file list*)
let get_files () = 
  match !file_list_path with 
  | "" -> []
  | otherwise -> begin
    let filepath = !file_list_path in 
    let file = open_in filepath in 
    let filepaths = ref [] in
    begin try 
      while true do
        let line = input_line file in
        filepaths := line::!filepaths
      done
    with End_of_file -> () end;
    close_in file;
    List.rev !filepaths
    end
    
  
(*grabs the string value of a leaf or trunk, or 
  raises an error for other types of branches*)
let string_value node =
  match node with
  | Trunk_node (_, text, _)
  | Stem_node (_, text, _)
  | Leaf_node (_, _, text) -> text
  | File_node (_) -> 
        failwith "Attempted to call string_value on a File node: " ^ 
        "File nodes to not have text"
  | Branch_node (_) -> 
        failwith "Attempted to call string_value on a Branch node: " ^
        "Branch nodes do not have text"  
  | Empty -> 
        failwith "Attemped to call string_value on the Empty node: " ^ 
        "Empty nodes do not have text"
  
let build_ast main_file = begin
  
  let grab_tokens filepath = 
    (*to debug atomizer, write True instead of False (case sensitive) *)
    let atomizer_cmd = 
      Printf.sprintf "python %s %s empty False" !path_to_atomizer filepath in
    let result = 
    match Stats2.time "atomizer" Unix.system atomizer_cmd with
    | Unix.WEXITED(0) -> Printf.printf "javaRep: atomizing successful for %s\n" filepath
    | _ -> failwith "error atomizing" in 
    result;  (*FIXME how to make this trace back the error?*)
    (* Build the tokens list from a .java.atoms file*)
    let atoms_filepath = Printf.sprintf "%s.atoms" filepath in
    let file = open_in atoms_filepath in
    
    (*grab all tokens from the file *)
    let tokens = ref [] in
    begin try
      while true do 
        let line = input_line file in
        tokens := line::!tokens
      done
    with End_of_file -> () end;
    let tokens = ref (List.rev !tokens) in 
    close_in file; 
    tokens in    
  
  (*is this token an import/package? *)
  (*FIXME use regexps*)
  let is_import_or_package str =
    if String.length str > 6
      then match String.sub str 0 6 with
      | "import" -> true
      | "packag" -> true
      | _ -> false
      else false in
  
  (*is this token a class or method? this only works for GCD right now.*)
  (*FIXME - might fail for certain variable names, make sure it doesnt*)  
  let is_class_or_method str = 
    if String.length str > 5
      then match String.sub str 0 5 with
      | "publi"
      | "class"
      | "priva" 
      | "prote" -> true
      | _ -> false
      else false in
  
  let consume_token tokens =
    tokens := List.tl !tokens in
  let increment_id () = 
    current_id := (!current_id + 1) in
  

  (*The actual parsing*)
  let rec parse_block (tokens: string list ref) (fp:pfilename) = 
    match !tokens with
    | "{"::remaining_tokens -> 
          consume_token tokens;
          let result_node = 
            Branch_node(fp, !current_id, parse_list tokens fp) in
          increment_id ();
          result_node
    | _ -> failwith ("Failure in parse_block inside Jast.build_ast: " ^ 
                    "Error in source, left brace expected")
    
  and parse_list tokens fp = 
    match !tokens with
    | "}"::remaining_tokens -> 
          consume_token tokens;
          []
    | other_tokens -> let element = parse_element tokens fp in 
          element::parse_list tokens fp 
    
  and parse_element tokens fp = 
    try 
      match !tokens with
      | "{"::remaining_tokens -> parse_block tokens fp 
      | token::remaining_tokens when (is_class_or_method token) -> 
            let current_token_text = List.hd !tokens in
            consume_token tokens;
            Trunk_node(fp, current_token_text, [parse_element tokens fp])
      | any_other_token -> 
            let current_token_text = List.hd !tokens in
            consume_token tokens;
            let result_node = 
              Leaf_node(fp, !current_id, current_token_text) in
            increment_id ();
            result_node
    with Failure("hd") -> failwith "Error in parse_element, no elements found"
             
  and parse_trunk tokens fp : ast_node=
    match !tokens with
    | token::remaining_tokens when (is_class_or_method token) -> 
        consume_token tokens;
        Trunk_node(fp, token, [parse_trunk tokens fp])
    | token::remaining_tokens when (is_import_or_package token) ->
        consume_token tokens;
        Stem_node(fp, token, parse_trunk tokens fp)
    | "{"::remaining_tokens -> parse_block tokens fp
    | any_other_token -> parse_element tokens fp 
    
  and parse_file (tokens:string list ref) (fp:string) : ast_node= 
    File_node (fp, [parse_trunk tokens fp])
    
    in
    
  match !file_list_path with 
  | "" -> 
      let tokens = grab_tokens main_file in
      Trunk_node (master_ast_text, "", [parse_file tokens main_file])
  | otherwise -> 
      let node_list = List.map (fun x -> 
        let tokens = grab_tokens x in
        parse_file tokens x
        
        ) (get_files ()) in
      Trunk_node(master_ast_text, "", node_list)
  end
      
(*prints the raw ast, _not_ pretty printing*)
let rec print ast = 
  match ast with
  | Trunk_node (_, text, ast_list) -> 
        Printf.printf "Trunk with text: %s\n" text; 
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | File_node (fp, ast_list) ->
        Printf.printf "Beginning to write from file %s\n" fp;
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | Stem_node (_, text, ast_node) -> 
        Printf.printf "Stem with text: %s\n" text; 
        print ast_node
  | Branch_node (_, id, ast_list) -> 
        Printf.printf "Branch with ID:%d\n" id; 
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | Leaf_node (_, id, text) -> 
        Printf.printf "Leaf#%d with text: %s\n" id text
  | Empty -> ()
  
(* returns the node in "ast" with id "id", or raises a not found error*)
let get_node ast id1 = begin
  let correct_node = ref Empty in
  let rec get ast = 
    match ast with
    | Trunk_node (_, _, ast_list)
    | File_node (_, ast_list) ->  
          List.iter (fun inner_node -> get inner_node) ast_list
    | Stem_node (_, _, ast_node) -> 
          get ast_node
    | Branch_node (_,id,_)
    | Leaf_node (_,id,_) when id = id1 -> 
          correct_node := ast
    | Branch_node (fp, id, ast_list) -> 
          List.iter (fun inner_node -> get inner_node) ast_list
    | _ -> () in
  get ast;
  match !correct_node with
  | Empty -> 
        let str = 
          Printf.sprintf "Error in get_node: No node with id %d found" id1 in
          failwith str
  | otherwise -> otherwise
  end
  
(*writes the ast to filepath, creating the necessary directories first*)
let write ast filepath = begin
  (*does the source contain more than one file?*)
  let multi_flag = 
    match !file_list_path with
    |"" -> false
    |otherwise -> true in 
  let base_folder = Filename.dirname filepath in 
  if multi_flag = true
  then begin
    let required_folders = 
      List.map (fun path -> Printf.sprintf "%s/%s" base_folder (Filename.dirname path)) (get_files ()) in
    List.iter (fun path -> 
      let cmd = Printf.sprintf "mkdir -p %s" path in
      match Stats2.time "making directories" Unix.system cmd with
      |Unix.WEXITED(0) -> ()
      | _ -> 
          let err = 
          (Printf.sprintf "Failed in Jast.write making folder %s" path) in
          failwith err
      ) required_folders
    end
  else ();
  
  let current_filepath = ref filepath in
  let current_file = ref (open_out filepath) in
  
  let rec write_node ast = 
  match ast with
  | File_node (fp, ast_list) when fp != !current_filepath ->
        if multi_flag = true
        then begin
          close_out !current_file;
          current_filepath := (Filename.concat base_folder fp);
          current_file := open_out !current_filepath;
          end
        else ();
        List.iter (fun x -> write_node x) ast_list 
  | File_node (fp, ast_list) ->
        List.iter (fun x -> write_node x) ast_list
  | Trunk_node (_, text, ast_list) -> 
        output_string !current_file (text ^ "\n");
        List.iter (fun x -> write_node x) ast_list
  | Stem_node (_, text, ast_node) -> 
        output_string !current_file (text ^ "\n");
        write_node ast_node
  | Branch_node (_, id, ast_list) -> 
        begin 
        output_string !current_file "{\n"; 
        List.iter (fun x -> write_node x) ast_list;
        output_string !current_file "}\n"
        end
  | Leaf_node (_, id, text) -> 
        output_string !current_file (text ^ "\n")
  | Empty -> ()  in
  (*why do i need to write "let _ =" and not just "try... with"?*)
  let _ = 
  try 
  write_node ast
  with _ -> debug "failure to open(?) file in write_node: %s" !current_filepath in

  close_out !current_file;
  end

(*gets the number of used IDs*)
let get_max_id () = !current_id - 1

(*swap the nodes with id's "id1" and "id2" *)
let swap ast id1 id2 = begin
  let node1 = get_node ast id1 in
  let node2 = get_node ast id2 in
  let rec swap_nodes ast = 
    match ast with
    | Trunk_node (fp, text, ast_list) -> 
          Trunk_node (fp, text, List.map (fun x -> swap_nodes x) ast_list)  
    | File_node (fp, ast_list) -> 
          File_node (fp, List.map (fun x -> swap_nodes x) ast_list)
    | Stem_node (fp, text, ast_node) -> 
          Stem_node (fp, text, swap_nodes ast_node)
    | Branch_node (_,id,_) 
    | Leaf_node (_,id,_) when id = id1 -> 
          node2
    | Branch_node (_,id,_)
    | Leaf_node (_,id,_) when id = id2 -> 
          node1
    | Branch_node (fp, id, ast_list) ->
          Branch_node(fp, id, List.map (fun x -> swap_nodes x) ast_list)
    | otherwise -> otherwise in
  swap_nodes ast
  end

(*Replaces the node with id "id1" with the node "new_node"*)
let replace ast id1 new_node = begin
  let rec replace_node ast=
    match ast with
    | Trunk_node (fp, text, ast_list) -> 
          Trunk_node (fp, text, List.map (fun x-> replace_node x) ast_list)
    | File_node (fp, ast_list) ->
          File_node (fp, List.map (fun x -> replace_node x) ast_list)
    | Stem_node (fp, text, ast_node) -> 
          Stem_node (fp, text, replace_node ast_node)
    | Leaf_node (_,id,_) 
    | Branch_node (_,id,_) when id = id1 -> 
          new_node
    | Branch_node (fp, id, ast_list) ->
          Branch_node (fp, id, List.map (fun x-> replace_node x) ast_list) 
    | otherwise -> otherwise in
  replace_node ast
  end

(*replaces the node with id id1 with Empty*)
let delete ast id1 = begin
  let delete_flag = ref false in
  let rec delete_node ast=
    match ast with 
    | Trunk_node (fp, text, ast_list) -> 
          Trunk_node (fp, text, List.map (fun x -> delete_node x) ast_list)
    | File_node (fp, ast_list) -> 
          File_node (fp, List.map (fun x -> delete_node x) ast_list)
    | Stem_node (fp, text, ast_node) -> 
          Stem_node (fp, text, delete_node ast_node)
    | Leaf_node (_, id,_) 
    | Branch_node (_, id,_) when id = id1-> 
          delete_flag := true; 
          Empty
    | Branch_node (fp, id, ast_list) -> 
          Branch_node (fp, id, List.map (fun x -> delete_node x) ast_list)
   | otherwise -> otherwise in
  let result = delete_node ast in
  if !delete_flag == true
    then result
    else failwith "Node to delete was not found"
  end
    
(*get the id from a node, or raise an error if the node has none*)
let get_id node =
  match node with 
  | Branch_node (_, id, _)
  | Leaf_node (_, id, _) -> id
  | Trunk_node (_) -> failwith "Attempted to call get_id on node Trunk"
  | File_node (_) -> failwith "Attempted to call get_id on File_node"
  | Stem_node (_) -> failwith "Attempted to call get_id on node Stem"
  | Empty -> failwith "Attempted to call get_id on node Empty"

(*helper function for append. This looks to see if a Node has a 
  node with id "append_after_id" in ONLY its own ast_list*)
let has_id (ast_with_list:ast_node) (append_after_id:atom_id) : bool = begin
  match (ast_with_list:ast_node) with 
  | Trunk_node (_,_,ast_list)
  | Branch_node (_,_,ast_list) -> 
    let result = ref false in  
    List.iter (fun x -> try   
                          let id = get_id x in
                          if id = append_after_id
                            then result := true
                            else ()
                        with Failure _ -> () ) ast_list;
    !result
  | otherwise -> false end

(*takes a copy of the node with id "append_after_id" and 
  places it after the node with id "what_id"*)
let append ast (append_after_id:atom_id) (what_id:atom_id) : ast_node = begin
  let what_atom = get_node ast what_id in
  let success = ref false in
  
  (*helper function to create a new node_list*)
  let create_new_ast_list node ast_list = begin
    let newlist = ref [] in
    List.iter (fun inner_node ->
      match inner_node with
      | Branch_node (_,id,_) 
      | Leaf_node (_,id,_) when id = append_after_id -> 
          newlist := what_atom::inner_node::!newlist
      | Empty -> ()
      | otherwise -> newlist := inner_node::!newlist
              ) ast_list;
    List.rev !newlist
    end in
    
  (*how it works: look in the node's ast_list. if you find the correct node,
    go through your list and append the new node right after it. otherwise call
    yourself recursively until you find the node which has the correct id'ed node
    in its ast_list.*)
  let rec append_ast (node:ast_node) : ast_node = begin
  match node with
  | Trunk_node (fp, text, ast_list) -> 
      if (has_id node append_after_id)
        then begin
          let newlist = create_new_ast_list node ast_list in
          success := true;
          Trunk_node (fp, text, newlist)
          end
        else Trunk_node (fp, text, List.map (fun x-> append_ast x) ast_list)
  | File_node (fp, ast_list) ->
      if (has_id node append_after_id)
        then begin
        let newlist = create_new_ast_list node ast_list in
        success := true;
        File_node (fp, newlist)
        end
      else File_node (fp, List.map (fun x -> append_ast x) ast_list)
  | Stem_node (fp, text, ast_node) -> 
      Stem_node (fp, text, (append_ast ast_node)) 
  | Branch_node (fp, id, ast_list)-> 
      if (has_id node append_after_id)
        then begin
          let newlist = create_new_ast_list node ast_list in
          success := true;
          Branch_node (fp, id, newlist)
          end
        else Branch_node (fp, id, (List.map (fun x -> append_ast x) ast_list))
  | otherwise -> otherwise
  end in
  let result = ref Empty in 
  let appended = append_ast ast in 
  if !success == true
    then result := appended
    else failwith (Printf.sprintf "Attempted to call append on a node after an id (%d) which is not in the ast." what_id);
    let answer = !result in 
    answer
  end 
  

  

