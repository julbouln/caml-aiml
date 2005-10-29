(* TEMPLATE *)

type aiml_templ_element=
  | Text of string
  | Star of int
  | That of int      
  | Input of int
  | ThatStar of int      
  | TopicStar of int      
  | Set of string * aiml_templ_element option list
  | Get of string
  | Bot of string

  | Srai of aiml_templ_element option list

  | Think of aiml_templ_element option list

  | Random of aiml_templ_element option list
  | Condition of aiml_templ_element option list

  | Li of string * string * aiml_templ_element option list

  | Formal of aiml_templ_element option list

;;

  
class aiml_template=
object

  val mutable template=DynArray.create()
  method get_template=template


  method parse x=
    let rec parse_element els=
      let templa=DynArray.create() in
	List.iter (
	  fun cc->
	    match cc with			    
	      | Xml.Element (cct,ccargs,ccchilds)->
		  (match cct with
		     | "star" ->
			 DynArray.add templa (
			   Some (
			     Star  (try
				      (int_of_string (Xml.attrib cc "index"))
				    with Xml.No_attribute v->1)
			   )
			 )
		     | "srai" ->
			 DynArray.add templa (
			   Some (
			     Srai (
			       DynArray.to_list (parse_element ccchilds)
			     )
			   )
			 )

		     | "sr" ->
			 DynArray.add templa (
			   Some (
			     Srai (
			       [Some (Star 1)]
			     )
			   )
			 )
		     | "think" ->
			 DynArray.add templa (
			   Some (
			     Think (
			       DynArray.to_list (parse_element ccchilds)
			     )
			   )
			 )
		     | "random" ->
			 DynArray.add templa (
			   Some (
			     Random (
			       DynArray.to_list (parse_element ccchilds)
			     )
			   )
			 )
		     | "condition" ->
			 DynArray.add templa (
			   Some (
			     Condition (
			       DynArray.to_list (parse_element ccchilds)
			     )
			   )
			 )
		     | "li" ->
			 let (n,v)=
			   (try 
			      Xml.attrib cc "name",Xml.attrib cc "value"
			    with
			       _ -> ("","")) in
			   DynArray.add templa (
			     Some (
			       Li (n,v,
				 DynArray.to_list (parse_element ccchilds)
			       )
			     )
			   )
		     | "formal" ->			 
			 DynArray.add templa (
			   Some (
			     Formal (
			       DynArray.to_list (parse_element ccchilds)
			     )
			   )
			 )
			   
		     | "input" ->
			 DynArray.add templa (
			   Some (
			     Input (
			       try
				 int_of_string (Xml.attrib cc "index")
			       with _->1
			     )
			   )
			 )
		     | "that" ->
			 DynArray.add templa (
			   Some (
			     Input (
			       try
				 int_of_string (Xml.attrib cc "index")
			       with _->1
			     )
			   )
			 )
		     | "get" ->
			 DynArray.add templa (
			   Some (
			     Get (Xml.attrib cc "name")
			   )
			 )
		     | "set" ->
			 DynArray.add templa (
			   Some (
			     Set ((Xml.attrib cc "name"),
				  DynArray.to_list (parse_element ccchilds)
				 )
			   )
			 )
		     | _ ->()
		  )
	      | Xml.PCData cs->
		  DynArray.add templa (Some (Text cs));
	) els;
	templa
    in
      
      match x with
	| Xml.Element (t,args,childs)->
	    template<-parse_element childs
	      (*	  List.iter (
			  fun c->
			  match c with		  
			  | Xml.Element (ct,cargs,cchilds)->
		      template<-parse_element cchilds
			  | Xml.PCData s->
			  DynArray.add template (Some (Text s));
			  ) childs
	      *)
	| _->()
end;;
