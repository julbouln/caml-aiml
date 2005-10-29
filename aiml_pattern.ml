
exception Pattern_not_set;;

(* INUSED *)
type pattern_element=
  | Word of string
  | Willcard;;


class aiml_pattern=
object (self)
  val mutable pattern=None

  val mutable regexp_pattern=None
  val mutable regexp_pattern_table=Hashtbl.create 2

  method get_pattern=
    match pattern with      
      | Some p->p 
      | None -> raise Pattern_not_set
  
  val mutable that=None				     
  method set_that t=
    that<-Some t;


  method get_that=
    match that with      
      | Some p->"<that> " ^p 
      | None -> "<that> *"

  method get_word_list_pattern=
    List.append 
      (Str.split (Str.regexp " +") (self#get_pattern))
      (Str.split (Str.regexp " +") (self#get_that))
 
(*
     (Str.split (Str.regexp " ") self#get_pattern)
      (Str.split (Str.regexp " ") self#get_that) 
*)

  method count_word_list_pattern=
    List.length (self#get_word_list_pattern)

  method get_regexp_pattern=
    match regexp_pattern with      
      | Some p->p 
      | None -> raise Pattern_not_set

  method get_regexp_pattern_table=regexp_pattern_table

  method get_star n=
    try 
    (Hashtbl.find regexp_pattern_table n)
    with
	Not_found -> n

  method init_regexp_pattern()=
    let p=self#get_pattern and
      t=self#get_that in
    let rpattern=Str.full_split (Str.regexp "\*") (p) in
(*^" "^t) in*)
(*^" "^self#get_that) in *)
    let fstr=ref "^" in
    let cdel=ref 1 in
    let cstar=ref 1 in
      List.iter (
	fun del->
	  (match del with 
	    | Str.Text t->
(*		if !cstar=1 then (  *)
		let tt=
(*		  Str.global_replace (Str.regexp " $") "" *)
		    (Str.global_replace (Str.regexp "^ ") "" t)
		in
		fstr:=!fstr^("\("^tt^"\)");
(*		)  *)
	    | Str.Delim d->
		Hashtbl.add regexp_pattern_table !cstar !cdel;
		fstr:=!fstr^("\(.+\)");
		cstar:= !cstar+1;
	  );
	  cdel:= !cdel+1;
		
      )rpattern ;
(*      fstr:=!fstr^(" \(<pattern>.+\)"); *)
      fstr:= !fstr^"$"; 
      regexp_pattern<-(Some (!fstr))
    
  method parse x=
    (match x with
      | Xml.Element (t,args,childs)->
	  List.iter (
	    fun c->
	      match c with
		| Xml.PCData s->
		    pattern<-(Some s)
		| _->()
	  ) childs
      | _->());

end;;
