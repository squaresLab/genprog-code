open Printf 
open Utils

let string_align() = (
  let mkmatrix rows cols opt =(
	let m = A.make_matrix rows cols 0 in
	if opt=1 then ( 
	  for i=1 to rows -1 do m.(i).(0) <- i done;
	  for j=1 to cols -1 do m.(0).(j) <- j done;
	);
	m
  )in 

  let min3 r = (
	let smallest = ref r.(0) and smallest_idx = ref 0 in 
	if r.(1) < !smallest then (smallest := r.(1); smallest_idx := 1);
	if r.(2) < !smallest then (smallest := r.(2); smallest_idx := 2);
	!smallest_idx
  )in
  
  let print_matrix m_edit m_trace rl cl= (
	let tb_a = [|'L';'T';'D'|] in

	printf "\t";
	for i = 0 to (S.length cl)-1 do  printf "\t%c" cl.[i] done;
	printf "\n";

	for i = 0 to (A.length m_edit) - 1 do 
	  if i > 0 then printf "%c" rl.[i-1] ;
	  for j = 0 to (A.length m_edit.(i)) - 1 do 
		printf "\t%d,%c" m_edit.(i).(j) tb_a.(m_trace.(i).(j));
	  done;
	  printf "\n"
	done;
  )in


  let edit_dist editm tracem rl cl = (
	print_matrix editm tracem rl cl; printf "\n";
	(*print_matrix tracem rl cl; printf "\n";*)

	let carray = [| 0; 0 ; 0 |]in 
	for i = 0 to (S.length rl) - 1 do 
	  for j = 0 to (S.length cl) - 1 do 
		let mi = i+1 and ji = j+1 in (*matrix value assoc w/ i,j is 1 bigger*)
		let l = editm.(mi-1).(ji) and  (*left*)
			t = editm.(mi).(ji-1) and  (*top*)
			d = editm.(mi-1).(ji-1) in (*diagonal*)

		if (rl.[i]=cl.[j]) then (carray.(0)<-l+1; carray.(1)<-t+1; carray.(2)<-d)
		else(carray.(0)<-l+1; carray.(1)<-t+1; carray.(2)<-d+1);
		let smallest_idx = min3 carray in
		editm.(mi).(ji)<- carray.(smallest_idx);
		tracem.(mi).(ji)<- smallest_idx
	  done;
	done;

	print_matrix editm tracem rl cl;printf "\n";
	
	printf "\nscore: %d\n" editm.((S.length rl)+1 -1).((S.length cl)+1-1)
  )in

  let print_str s = S.iter(fun c -> printf "%c " c) s;  printf " (s: %d)" (S.length s) in

  (*  let s1 = "ALGORITHM" and s2 = "ALTRUISTIC" in *)
  let s1 = "abcdefghgi" and s2 = "ajcklfgifhgi" in 
  print_str s1;printf"\n";  print_str s2;printf"\n";
  
  let s1_length = (S.length s1) and s2_length = (S.length s2) in 

  let editmatrix = mkmatrix (s1_length+1) (s2_length+1) 1 in 
  let tracebackmatrix = mkmatrix (s1_length+1)  (s2_length+1) 0 in 
  edit_dist editmatrix tracebackmatrix s1 s2;

)
