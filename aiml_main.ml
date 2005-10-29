
open Aiml_topic;;

class aiml=
object

  val mutable categories=DynArray.create();
  
  method foreach_category f=
    DynArray.iter f categories

  method parse x=
    match x with
      | Xml.Element (t,args,childs)->
	  List.iter (
	    fun c->
	      match c with
		| Xml.Element (ct,cargs,cchilds)-> 
		    (match ct with 
		      | "topic" -> ()
		      | "category" -> 
			  let cat=new aiml_category in
			    cat#parse c;
			    DynArray.add categories cat
		      | _   -> ()
		    )
		| _ -> ()
	  ) childs
      | _ -> ()

end;;
