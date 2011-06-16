open Global
open Rep

(*this will be printed at the top of every file we make. optional*)
let master_ast_text = ""
(*add parent filename to these so we know which file the came from*)
type pfilename = string (*what file does this node belong to *)
type atom_id = int
type atom_text = string
type lineno = int

type ast_node =
| Empty
| Leaf_node of (lineno list) * atom_id * atom_text 
| Branch_node of (lineno list) * atom_id * (ast_node list)
| Stem_node of (lineno list) * atom_text * ast_node (*Stems hold imports*)
| File_node of pfilename * (ast_node list)
(*trunks hold classes/method declarations*)
| Trunk_node of (lineno list) * atom_text * (ast_node list) 

let dummyfile = Empty
let current_id = ref 1

let file_list_path = ref "" 
let path_to_atomizer = ref ""
let debug_jast_mutations = ref false
let debug_parser_flag = ref false
let _ = 
  options := !options @
  [
    "--atomizer-loc", Arg.Set_string path_to_atomizer, "X path to javaatomizer.py";
    "--debug-jast-mutations", Arg.Set debug_jast_mutations, " print debug info on mutation failures (breaks the GA search strategy)" (*fixme*);
    "--debug-java-parser", Arg.Set debug_parser_flag, " print debug info on parser operation"
  ] 
  
  
let debug_parser parser tokens =
  try
    print_endline ("parsing " ^ parser ^ ":" ^ (snd (List.hd !tokens)) ^ "\n")
  with Failure ("hd") -> 
    let fstring = ("Failed to parse " ^ parser ^ ": List.hd" ^ "\n") in
    print_endline fstring
  
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
  | _ -> "none"
  
  
let build_ast main_file = begin
  
  let grab_tokens filepath = 
    let atomizer_cmd = 
      Printf.sprintf "python %s %s empty False" !path_to_atomizer filepath in
    let result = 
    match Stats2.time "atomizer" Unix.system atomizer_cmd with
    | Unix.WEXITED(0) -> Printf.printf "javaRep: atomizing successful for %s\n" filepath
    | _ -> failwith "error atomizing" in 
    result;  
    (* Build the tokens list from a .java.atoms file*)
    let atoms_filepath = Printf.sprintf "%s.atoms" filepath in
    let file = open_in atoms_filepath in
    
    (*grab all tokens from the file *)
    let tokens:(((lineno list) * string) list ref)  = ref [] in
    begin try
      while true do 
        let lineno_list : (string list) = Str.split (Str.regexp " ") (input_line file) in
        let lineno_list = List.map (fun x -> int_of_string x) lineno_list in
        let new_lineno_list = ref [] in
        (*filter out repeats *)
        List.iter 
            (fun x -> 
                if List.mem x !new_lineno_list 
                   then () 
                   else new_lineno_list := x :: !new_lineno_list) lineno_list;
        
        let line = input_line file in
        tokens := (!new_lineno_list, line)::!tokens
      done
    with End_of_file -> () end;
    let tokens = ref (List.rev !tokens) in 
    close_in file; 
    tokens in    
    
  let is_import str = 
    let import_regexp = Str.regexp "[ \t]*\\(import\\|package\\)" in
    Str.string_match import_regexp str 0 in
    
  let is_class str = 
    let class_regexp = Str.regexp "\\(public\\|private\\|protected\\|final\\|abstract\\|[ \t]\\)*\\(class\\|interface\\)" in
    Str.string_match class_regexp str 0 in
  
  let is_branch str = 
    let branch_regexp = Str.regexp "[ \t]*\\(if\\|switch\\|while\\|else\\|for\\)" in
    Str.string_match branch_regexp str 0 in
    
  let is_method str = 
    let method_regexp = Str.regexp ".*)[ \t]*throws[ \t]+[a-zA-z_$][a-zA-Z0-9_$]*\\|.*)[ \t]*$" in
    Str.string_match method_regexp str 0 in

   
  (*FIXME use regexp*)
  let is_weimerclass str = 
    if String.length str > 41
      then match String.sub str 0 41 with
      | "class weimer_runnable implements Runnable" -> true
      | _ -> false
      else false in 

  
  let current_linenum_list = ref [] in
  let consume_token (tokens:(((lineno list) * string) list) ref) =
    tokens := List.tl !tokens;
    (*try*)
    if List.length !tokens > 0
      then current_linenum_list := fst (List.hd !tokens)
      else ()
    in
    
  let increment_id () = 
    current_id := (!current_id + 1) in
  let get_text tokens = snd (List.hd !tokens) in
  let get_lineno_list tokens = fst (List.hd !tokens) in

  (*The actual parsing*)
  let rec parse_block (tokens: (((lineno list) * string) list) ref) = 
    if !debug_parser_flag == true
      then debug_parser "block" tokens;
    match !tokens with
    | (_, "{")::remaining_tokens -> 
          consume_token tokens;
          let result_node = 
            Branch_node(!current_linenum_list, !current_id, parse_list tokens "parse_branch") in
          increment_id ();
          result_node
    | _ -> failwith ("Failure in parse_block inside Jast.build_ast: " ^ 
                    "Error in source, left brace expected")
    
  and parse_list (tokens: (((lineno list) * string) list) ref) from = 
    if !debug_parser_flag == true
      then begin print_endline ("parsing list from " ^ from);
           debug_parser "list" tokens
           end;
    match !tokens with
    | (_,"}")::remaining_tokens -> begin
          consume_token tokens;
          [] end
    | other_tokens -> 
          let element = parse_element tokens in 
          element::parse_list tokens "parse_list" 
    
  and parse_element tokens = 
    if !debug_parser_flag == true
      then debug_parser "element" tokens;
    let current_lineno_list = fst (List.hd !tokens) in
    let current_text = snd (List.hd !tokens) in
    try 
      match !tokens with
      | (_,"{")::remaining_tokens -> parse_block tokens
      | token::remaining_tokens when (is_import current_text) ->
            parse_import tokens
      | token::remaining_tokens when (is_class current_text) ->
            parse_class tokens
      | token::remaining_tokens when (is_branch current_text) -> 
            consume_token tokens;
            let result_node = 
              Leaf_node(current_lineno_list, !current_id, current_text) in
            increment_id ();
            result_node
      | token::remaining_tokens when (is_method current_text) ->
            parse_method tokens
      (*same as (when is_branch), make sure order isnt important then merge*)
      | any_other_token -> 
            consume_token tokens;
            let result_node = 
              Leaf_node(current_lineno_list, !current_id, current_text) in
            increment_id ();
            result_node
    with Failure ("hd") -> failwith "Error in parse_element, no elements found"

  
  and parse_import (*or package*) tokens = 
    if !debug_parser_flag == true
      then debug_parser "import" tokens;
    let current_text = get_text tokens in
    let current_lineno_list = get_lineno_list tokens in 
    consume_token tokens;
    Stem_node(current_lineno_list, current_text, parse_element tokens)
    
  and parse_class (*or interface*) tokens = 
    if !debug_parser_flag == true
      then debug_parser "class" tokens;
    let current_text = get_text tokens in
    let current_lineno_list = get_lineno_list tokens in
    match !tokens with
    | token::remaining_tokens when (is_weimerclass current_text) ->
        let weimerclass_text = ref "" in
        let counter = ref 0 in
        while List.length !tokens > 0 do
          let current_text = snd (List.hd !tokens) in 
          weimerclass_text := !weimerclass_text ^ current_text;
          consume_token tokens;
          counter := 1 + !counter;
          if !counter > 100
            then failwith "Error in parse_trunk, while-loop lasted for too long";
        done;
        Trunk_node (current_lineno_list, !weimerclass_text, [])
    |token::remaining_tokens when (is_class current_text) ->
        consume_token tokens;
        Trunk_node (current_lineno_list, current_text, [parse_block tokens])
    | _ -> failwith "error in parse_class, token was not a class"
    
  and parse_method tokens =
    if !debug_parser_flag == true
      then debug_parser "method" tokens;
    let current_text = get_text tokens in
    let current_lineno_list = get_lineno_list tokens in
    consume_token tokens;
    Trunk_node (current_lineno_list, current_text, [parse_block tokens])

  and parse_file tokens fp = 
    if !debug_parser_flag == true
      then debug_parser "file" tokens;
    let trunk_list = ref [] in
    let counter = ref 0 in
    while List.length !tokens > 0 do
      let trunk = parse_element tokens in
      trunk_list := trunk::!trunk_list;
      counter := !counter + 1;
      if !counter > 100
        then failwith ("Either parse_file was stuck in an infinite loop," ^ 
                " or your .java file contains over 100 classes");
    done;
    File_node (fp, List.rev !trunk_list)
    
    in
    
  match !file_list_path with 
  | "" -> 
      let tokens = grab_tokens main_file in
      parse_file tokens main_file
  | otherwise ->
      let node_list = List.map (fun x -> 
        let tokens = grab_tokens x in
        parse_file tokens x ) (get_files ()) in
      Trunk_node(!current_linenum_list, "", node_list)
  
  end 
  
let make_string lineno_list = 
    let text = ref "" in
    List.iter (fun number -> text := !text ^ ", " ^ (string_of_int number)) lineno_list;
    !text
    
(*prints the raw ast, _not_ pretty printing*)
let rec print ast = 
  match ast with
  | Trunk_node (lineno_list, text, ast_list) ->     
        Printf.printf "Trunk(L[%s]) with text: %s\n" (make_string lineno_list) text; 
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | File_node (fp, ast_list) ->
        Printf.printf "Beginning to print from file %s\n" fp;
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | Stem_node (lineno_list, text, ast_node) -> 
        Printf.printf "Stem(L[%s]) with text: %s\n" (make_string lineno_list) text; 
        print ast_node
  | Branch_node (lineno_list, id, ast_list) -> 
        Printf.printf "Branch(#%d,L[%s])\n" id (make_string lineno_list) ; 
        List.iter (fun inner_ast -> print inner_ast) ast_list
  | Leaf_node (lineno_list, id, text) -> 
        Printf.printf "Leaf(#%d,L[%s]) with text: %s\n" id (make_string lineno_list) text
  | Empty -> ()
  
(* returns the node in "ast" with id "id", or raises a not found error*)
let get_node ast id1 = begin
  let correct_node = ref Empty in
  let rec get ast = 
    match ast with
    | Trunk_node (_, _, ast_list)
    | File_node ( _, ast_list) ->  
          List.iter (fun inner_node -> get inner_node) ast_list
    | Stem_node (_, _, ast_node) -> 
          get ast_node
    | Branch_node (_,id,_)
    | Leaf_node (_,id,_) when id = id1 -> 
          correct_node := ast
    | Branch_node (_, _, ast_list) -> 
          List.iter (fun inner_node -> get inner_node) ast_list
    | _ -> () in
  get ast;
  match !correct_node with
  | Empty -> 
        let str = 
          Printf.sprintf "Error in get_node: No node with id %d found" id1 in
          if (!debug_jast_mutations == true)  
            then failwith str
            else ast
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
  | File_node (_, ast_list) ->
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
  (try 
    (write_node ast)
  with _ -> debug "failure to open(?) file in write_node: %s" !current_filepath);

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
    | Trunk_node (linenum_list, text, ast_list) -> 
          Trunk_node (linenum_list, text, 
                      List.map (fun x -> swap_nodes x) ast_list)  
    | File_node (fp, ast_list) -> 
          File_node (fp, List.map (fun x -> swap_nodes x) ast_list)
    | Stem_node (linenum_list, text, ast_node) -> 
          Stem_node (linenum_list, text, swap_nodes ast_node)
    | Branch_node (_,id,_) 
    | Leaf_node (_,id,_) when id = id1 -> 
          node2
    | Branch_node (_,id,_)
    | Leaf_node (_,id,_) when id = id2 -> 
          node1
    | Branch_node (linenum_list, id, ast_list) ->
          Branch_node(linenum_list, id, List.map (fun x -> swap_nodes x) ast_list)
    | otherwise -> otherwise in
  swap_nodes ast
  end

(*Replaces the node with id "id1" with the node "new_node"*)
let replace ast id1 new_node = begin
  let rec replace_node ast=
    match ast with
    | Trunk_node (linenum_list, text, ast_list) -> 
          Trunk_node (linenum_list, text, List.map (fun x-> replace_node x) ast_list)
    | File_node (fp, ast_list) ->
          File_node (fp, List.map (fun x -> replace_node x) ast_list)
    | Stem_node (linenum_list, text, ast_node) -> 
          Stem_node (linenum_list, text, replace_node ast_node)
    | Leaf_node (_,id,_) 
    | Branch_node (_,id,_) when id = id1 -> 
          new_node
    | Branch_node (linenum_list, id, ast_list) ->
          Branch_node (linenum_list, id, List.map (fun x-> replace_node x) ast_list) 
    | otherwise -> otherwise in
  replace_node ast
  end

(*replaces the node with id id1 with Empty*)
let delete ast id1 = begin
  let delete_flag = ref false in
  let rec delete_node ast=
    match ast with 
    | Trunk_node (linenum_list, text, ast_list) -> 
          Trunk_node (linenum_list, text, List.map (fun x -> delete_node x) ast_list)
    | File_node (fp, ast_list) -> 
          File_node (fp, List.map (fun x -> delete_node x) ast_list)
    | Stem_node (linenum_list, text, ast_node) -> 
          Stem_node (linenum_list, text, delete_node ast_node)
    | Leaf_node (_, id,_) 
    | Branch_node (_, id,_) when id = id1-> 
          delete_flag := true; 
          Empty
    | Branch_node (linenum_list, id, ast_list) -> 
          Branch_node (linenum_list, id, List.map (fun x -> delete_node x) ast_list)
   | otherwise -> otherwise in
  let result = delete_node ast in
  if !delete_flag == true
    then result
    else if (!debug_jast_mutations == true) 
      then failwith (Printf.sprintf "Error in delete: Node with id %d not found" id1)
      else ast;
  end
    
(*get the id from a node, or raise an error if the node has none*)
let get_id node =
  match node with 
  | Branch_node (_, id, _)
  | Leaf_node (_, id, _) -> id
  | Trunk_node (_) -> failwith "Attempted to call get_id on node Trunk node"
  | File_node (_) -> failwith "Attempted to call get_id on File node"
  | Stem_node (_) -> failwith "Attempted to call get_id on Stem node"
  | Empty -> failwith "Attempted to call get_id on Empty node"

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
let append (ast:ast_node) (append_after_id:atom_id) (what_atom:ast_node) : ast_node = begin
  (*let what_atom = get_node ast what_id in*)
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
  | Trunk_node (linenum_list, text, ast_list) -> 
      if (has_id node append_after_id)
        then begin
          let newlist = create_new_ast_list node ast_list in
          success := true;
          Trunk_node (linenum_list, text, newlist)
          end
        else Trunk_node (linenum_list, text, List.map (fun x-> append_ast x) ast_list)
  | File_node (fp, ast_list) ->
      if (has_id node append_after_id)
        then begin
        let newlist = create_new_ast_list node ast_list in
        success := true;
        File_node (fp, newlist)
        end
      else File_node (fp, List.map (fun x -> append_ast x) ast_list)
  | Stem_node (linenum_list, text, ast_node) -> 
      Stem_node (linenum_list, text, (append_ast ast_node)) 
  | Branch_node (linenum_list, id, ast_list)-> 
      if (has_id node append_after_id)
        then begin
          let newlist = create_new_ast_list node ast_list in
          success := true;
          Branch_node (linenum_list, id, newlist)
          end
        else Branch_node (linenum_list, id, (List.map (fun x -> append_ast x) ast_list))
  | otherwise -> otherwise
  end in
  let result = ref Empty in 
  let appended = append_ast ast in 
  if !success == true
    then result := appended
    else if (!debug_jast_mutations == true) 
      then failwith "Attempted to call append on a node after an id (%d) which is not in the ast."
      else result := ast;
    let answer = !result in 
    answer
  end 
  
let rec atom_id_of_lineno_ht ast = 
  let max_id = !current_id - 1 in
  let result_ht = Hashtbl.create max_id in
  let current_file = ref "" in
  let file_assertion () = 
    assert (!current_file != "") in
  let rec make_ht ast = 
    match ast with 
    | File_node (fp, ast_list) ->
      current_file := Filename.basename fp;
      List.iter (fun x -> make_ht x) ast_list
    | Empty -> ()
    | Leaf_node (lineno_list, id, text) -> 
        file_assertion ();
        List.iter (fun num -> Hashtbl.add result_ht (!current_file, num) id) lineno_list;
    | Branch_node (lineno_list, id, ast_list) ->
        file_assertion ();
        List.iter (fun num -> Hashtbl.add result_ht (!current_file, num) id) lineno_list;
        List.iter (fun x -> make_ht x) ast_list
    | Stem_node (lineno_list, text, ast_node) ->
        make_ht ast_node
    | Trunk_node (lineno_list, text, ast_list) ->
        List.iter (fun x -> make_ht x) ast_list in
  make_ht ast;
  result_ht
  
        
let rec copy ast = 
  match ast with 
  | Empty -> Empty
  | Leaf_node(_) -> ast
  | Branch_node (linenum_list, id, ast_list) ->
      Branch_node (linenum_list, id, List.map (fun x -> copy x) ast_list)
  | Stem_node (linenum_list, text, ast_node) ->
      Stem_node (linenum_list, text, copy ast_node)
  | File_node (fp, ast_list) ->
      File_node (fp, List.map (fun x -> copy x) ast_list)
  | Trunk_node (linenum_list, text, ast_list) ->
      Trunk_node (linenum_list, text, List.map (fun x -> copy x) ast_list)

