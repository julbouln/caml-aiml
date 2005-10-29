
open Aiml_brain;;

let brain=new aiml_brain;;
let dir="aiml-std-fr";;
let da=Sys.readdir dir;;
Array.iter(fun f->
	     match f with
	       | "." -> ()
	       | ".." -> ()
	       | otf -> 
		   print_string ("+ load "^otf^"...");print_newline();
		   try 
		     brain#parse_aiml (dir^"/"^otf)		 
		   with Xml.Error(r,v)->
		     print_string "Error loading file!";print_newline();
	  ) da;;


print_string "*** OCAML AIML ***";print_newline();;

while (true) do
  let say=read_line() in
  let resp=brain#resolve_brain say true in
    print_string resp;print_newline();
done;;

