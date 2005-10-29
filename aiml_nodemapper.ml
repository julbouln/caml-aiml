
open Aiml_pattern;;
open Aiml_templ;;

(* Node Mapper *)

exception Parent_not_set;;
exception Word_not_set;;
exception Template_not_set;;

exception Child_not_found of string




class aiml_nodemapper=
object(self)
  (** node parent *)
  val mutable parent=None
  method set_parent (p:aiml_nodemapper)=parent<-(Some p)
  method get_parent=
    (
      match parent with
	| Some p->p
	| _->
	    raise Parent_not_set
    )
  (** the word *)
  val mutable word=None
  method set_word w=word<-(Some w)
  method get_word:string=
    match word with
      | Some w->w
      | _->raise Word_not_set

  (** entire pattern *)
  val mutable pattern=None
  method set_pattern t=pattern<-(Some t);
  method is_pattern=
    match pattern with
      | Some t->true
      | _-> false
  method get_pattern:aiml_pattern=
    match pattern with
      | Some t->t
      | _->
	  raise Pattern_not_set

  (** associated template *)
  val mutable template=None
  method set_template t=template<-(Some t);
  method is_template=
    match template with
      | Some t->true
      | _->false
  method get_template:aiml_template=
    match template with
      | Some t->t
      | _->
	  raise Template_not_set

  (** children *)
  val mutable children=Hashtbl.create 2
  method foreach_child f=
    Hashtbl.iter f children

  method count_children=Hashtbl.length children

  method add_child (c:aiml_nodemapper)=
    c#set_parent (self:>aiml_nodemapper);
    Hashtbl.add children c#get_word c

  method get_child w=
    try
      Hashtbl.find children w
    with Not_found->raise (Child_not_found w)

  method is_child w=
    Hashtbl.mem children w

  method merge_node (c:aiml_nodemapper)=
    if self#is_child c#get_word then (
      let nc=self#get_child c#get_word in
	c#foreach_child (
	  fun cn child->
	    nc#merge_node child
	);



(*	if c#count_children=0 then (    
	  nc#set_template c#get_template;
	  nc#set_pattern c#get_pattern;
	) ;  *)

    )
    else
      (
	self#add_child c;
      )

end;;

class aiml_graphmaster=
object
  val mutable root=new aiml_nodemapper

  initializer
    root#set_word "<root>";

  method add_match_path (patt:aiml_pattern) (templ:aiml_template)=
    let rec set_parent n=
      (try
	 let p=n#get_parent in
	   if p#is_pattern=false then (
	     p#set_template n#get_template;
	     p#set_pattern n#get_pattern;
	   );
	   set_parent p;  
       with Parent_not_set -> ())
    in
    let rec rec_add l=
      let cn=new aiml_nodemapper in
	cn#set_word (List.hd l);

	if List.length l>1 then (
	  let ccn=(rec_add (List.tl l)) in
	    cn#merge_node ccn;
	) else
	  (
(*	    print_string ("adding "^(List.hd l));
	    print_newline();*)
	    cn#set_template templ;
	    cn#set_pattern patt;
	  );
	cn
    in
      root#merge_node (rec_add (patt#get_word_list_pattern))

	
  method get_match_path wl=
    let debug1 n=
      Printf.printf ("+ (%s) ... ") n;
    in

    let debug2 n=
      Printf.printf ("%s\n") n;
    in

    let rec rec_match0 l n=
      (try 
	debug1 ("<_>");
	let cn=n#get_child "_" in
	debug2 ("match!");      
	   
	   (try
	      rec_match0 (List.tl l) cn
	    with Failure _ ->
	      debug2 ("FOUND");
	      (cn#get_pattern,cn#get_template)
	   )
      with Child_not_found _ ->
	debug2 ("no match - goto phase 1");
	rec_match1 wl root
      )
    and
      rec_match1 l n=
      (try 
	debug1 (List.hd l); 
	 let cn=n#get_child (List.hd l) in
	 debug2 ("match!");
	   (try
	      rec_match1 (List.tl l) cn
	    with Failure _->
	      debug2 ("FOUND");
	      (cn#get_pattern,cn#get_template)
	   ) 
       with Child_not_found _ ->
	 debug2 ("no match - goto phase 2");
	 rec_match2 l n
      )
    and
      rec_match2 l n=
      (try
	debug1 ("<*> (retry)");	   
	let cn=n#get_child "*" in	   
	 debug2 ("match "^(List.hd l));
	   (try
	      rec_match1 (List.tl l) cn 
	    with Failure _ ->
	      debug2 ("FOUND");
	      (cn#get_pattern,cn#get_template) 
	   )
       with Child_not_found _ ->
	 debug2 ("no match");
	 debug1 ("<*>");
	 rec_match2 l n#get_parent 
      )
    in

      rec_match0 wl root

end;;





