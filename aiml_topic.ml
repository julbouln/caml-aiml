
open Aiml_base;;
open Aiml_pattern;;
open Aiml_templ;;

(* CATEGORY *)

exception No_pattern;;
exception No_template;;


class aiml_category=
object

  val mutable pattern=None
  val mutable template=None			

  method get_pattern=
    match pattern with
      | Some p->p
      | None -> raise No_pattern

  method get_template=
    match template with
      | Some t->t
      | None -> raise No_template

  method parse x=
    match x with
      | Xml.Element (t,args,childs)->
	  List.iter (
	    fun c->
	      match c with
		| Xml.Element (ct,cargs,cchilds)->	    
		    (match ct with 
		       | "pattern" ->
			   let pat=new aiml_pattern in
			     pat#parse c;
			     pat#init_regexp_pattern();
			     pattern<-(Some pat)

		       | "that" ->

			   (match pattern with
			     | Some pat ->
(*				 print_string "that!";print_newline(); *)
				 pat#set_that (Xml.pcdata (List.hd cchilds));
				 pat#init_regexp_pattern();

			     | None ->()
			   )
		       | "template" -> 
			   let tem=new aiml_template in
			     tem#parse c;
			     template<-(Some tem)
		       | _ -> ()	  
		    )
		| _ -> ()	  
	  ) childs
      | _ -> ()	  
end;;

(* TOPIC *)

class aiml_topic=
object
  val mutable name=None
  val mutable regexp_name=None

  val mutable categories=DynArray.create();

  method parse x=
    name<-Some (Xml.attrib x "name");
    regexp_name=(Some (regexp_of_pattern (Xml.attrib x "name")));
    match x with
      | Xml.Element (t,args,childs)->
	  List.iter (
	    fun c->
	      match c with
		| Xml.Element (ct,cargs,cchilds)-> 
		    (match ct with 
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
