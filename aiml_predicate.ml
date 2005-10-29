exception Predicate_not_found of string;;

class aiml_predicates=
object
  val mutable predicates=Hashtbl.create 2
						   
  method set_predicate (n:string) (v:string)=
    try 
      Hashtbl.replace predicates n v
    with Not_found -> Hashtbl.add predicates n v

  method get_predicate (n:string)=
    try 
      Hashtbl.find predicates n 
    with Not_found -> ""
	
end;;
