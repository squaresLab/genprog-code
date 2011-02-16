(* ocamlbuild plugin for building Batteries.  
 * Copyright (C) 2010 Michael Ekstrand
 * 
 * Portions (hopefully trivial) from build/myocamlbuild.ml and the
 * Gallium wiki. *)

open Ocamlbuild_plugin
open Command

let ocamlfind x = S[A"ocamlfind"; A x]
let m4 = (A"m4")
let lex = (A"ocamllex")
(*let elkhound = P"/home/claire/elkhound"*)
let elkhound = P"/Users/csl9q/research/taxonomy/cabs/elsa-2005.08.22b/elkhound/elkhound"
let packs = String.concat "," ["batteries"]

let _ = dispatch begin function
  | Before_options ->
      (* Set up to use ocamlfind *)
      Options.ocamlc     := ocamlfind "ocamlopt";
      Options.ocamlopt   := ocamlfind "ocamlopt";
      Options.ocamldep   := ocamlfind "ocamldep";
      Options.ocamldoc   := ocamlfind "ocamldoc";
      Options.ocamlmktop := ocamlfind "ocamlmktop";
  | Before_rules -> ()
  | After_rules ->
	  rule "mllm-to-mll-rule"
		~prod:"%.mll"
		~deps:["%.mllm";"tokens.lexint";"tokens.type";"tokens.lexer"]
		begin fun env _build ->
		  let mll = env "%.mll" and mllm = env "%.mllm" in
		  let tags = tags_of_pathname mll++"compile"++"mllm" in
			Cmd(S[m4; T tags; P mllm; Sh (" > "^mll)])
		end;
	  rule "mlm-to-ml-rule"
		~prod:"%.ml"
		~deps:["%.mlm";"tokens.lexint";"tokens.type";"tokens.lexer"]
		begin fun env _build ->
		  let ml = env "%.ml" and mlm = env "%.mlm" in
			Cmd(S[m4; P mlm; Sh (" > "^ml)])
		end;
	  rule "gr-to-grp-rule"
		~prod:"%.grp"
		~deps:["%.gr";"c.tok";"partial.gr"]
		begin fun env _build -> 
		  let grp = env "%.grp" and gr = env "%.gr" in
			Cmd(S[m4; P gr; Sh (" > "^grp)])
		end;
	  rule "grp-to-ml-rule"
		~prod:"%.ml"
		~dep:"%.grp"
		begin fun env _build ->
		  let _grp = env "%.grp" and out = env "%" in
		  let build = Cmd(S[elkhound;A"-ocaml";A"-v";A"-o";A out;A _grp]) in
		  let rm_mli = Cmd(S[Sh("rm "^out^".mli")]) in
			Seq[build;rm_mli]
		end;
	  (* When one links an OCaml program, one should use -linkpkg *)
	  flag ["ocaml"; "link"; "program"] & A"-linkpkg";
	  flag ["ocaml"; "compile"] & S[A"-package"; A packs];
	  flag ["ocaml"; "ocamldep"] & S[A"-package"; A packs];
	  flag ["ocaml"; "doc"] & S[A"-package"; A packs];
	  flag ["ocaml"; "link"] & S[A"-package"; A packs];
	  flag ["ocaml"; "infer_interface"] & S[A"-package"; A packs];

	  flag ["ocaml"; "infer_interface"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
	  flag ["ocaml"; "ocamldep"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
	  flag ["ocaml"; "compile"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];
	  flag ["ocaml"; "link"; "pkg_oUnit"] & S[A"-package"; A"oUnit"];

	  (* DON'T USE TAG 'thread', USE 'threads' 
		 for compatibility with ocamlbuild *)
	  flag ["ocaml"; "compile"; "threads"] & A"-thread";
	  flag ["ocaml"; "link"; "threads"] & A"-thread";
	  flag ["ocaml"; "doc"; "threads"] & S[A"-I"; A "+threads"];

	  flag ["ocaml"; "doc"] & S[A"-hide-warnings"; A"-sort"];
	  
	  flag ["ocaml"; "compile"; "camlp4rf"] &
		S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4rf"];
	  flag ["ocaml"; "ocamldep"; "camlp4rf"] &
		S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4rf"];

	  flag ["ocaml"; "compile"; "camlp4of"] &
		S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4of"];
	  flag ["ocaml"; "ocamldep"; "camlp4of"] &
		S[A"-package"; A"camlp4.lib"; A"-pp"; A"camlp4of"];
  | _ -> ()
end
