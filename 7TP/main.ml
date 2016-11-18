open Format
open Lexing

let usage = "usage: compilo [options] file.ml"

let interpret  = ref false
let compile    = ref true

let spec = [ "-i", Arg.Tuple [Arg.Set interpret; Arg.Clear compile],
	     "  interpreter only";
	   ]

let file = 
  let file = ref None in
  let set_file s =  
    if not (Filename.check_suffix s ".ml") then 
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.prog Lexer.token lb in
    close_in c;
    if   !interpret
    then ignore (Interpreter.interpret_prog p)
    else ();
    if !compile
    then let rp = Resolve_var.resolve_prog p in
	 let op = Constant_folding.fold_constants
	   (Constant_propagation.propagate_constants rp)
	 in
	 let tp = Type.type_prog op in
	 Generate.generate_prog tp;
    exit 0
  with
      e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
