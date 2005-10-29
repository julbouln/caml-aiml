
open Aiml_base;;
open Aiml_predicate;;
open Aiml_pattern;;
open Aiml_templ;;
open Aiml_main;;

open Aiml_nodemapper;;

class aiml_brain=
object(self)
  val mutable graph=new aiml_graphmaster
  method get_graph=graph
  val mutable preds=new aiml_predicates
  method get_preds=preds

  val mutable that=DynArray.create()
  method push_that (t:string)=DynArray.add that t
  method get_that i=
    let asi=DynArray.length that in
      DynArray.get that (asi-(i))

  val mutable input=DynArray.create()
  method push_input (i:string)=DynArray.add input i
  method get_input i=
    let asi=DynArray.length input in
      DynArray.get input (asi-(i))

  method parse_aiml file=
    let xe=Xml.parse_file file in
    let aiml=new aiml in
      aiml#parse xe;
      aiml#foreach_category (
	fun cat->
	  graph#add_match_path cat#get_pattern cat#get_template
      )


  method resolve_template template pattern str =
    let rec resolve_element t=
      let regexp=pattern#get_regexp_pattern and
	get_star=pattern#get_star in	
	print_string pattern#get_pattern;print_newline();
	print_string regexp;print_newline();
	print_string str;print_newline();
      let res=ref "" in
	DynArray.iter(
	  fun templ->	
	    match templ with
	      | Some te ->
		  (match te with
		     | Text t-> res:=!res^t;
		     | Star i->
			 res:=!res^(Str.global_replace (Str.regexp regexp) ("\\"^string_of_int (get_star i)) str);
		     | Srai st->
			 let srai=(resolve_element (DynArray.of_list st))  in 
			     res:=!res^(self#resolve_brain srai false) 
(*			   print_string srai;print_newline();  *)

(*
		     			 res:=!res^"(srai)"; *)
		     | Think st->
			 let r=resolve_element (DynArray.of_list st) in
			   print_string("(think "^r^")");print_newline(); 
()
		     | Random st->
			 Random.self_init();
			 let rs=List.length st in
			 let r=Random.int rs in
			 let resli=List.nth st r in
			  (match resli with
			     | Some rli->
				 (match rli with
				    | Li (n,v,l)->
					res:=!res^resolve_element (DynArray.of_list l);
				    | _ -> ()
				 )
			     | _ -> ()
			  )

		     | Condition st->
			 let condres=ref "" in
			   List.iter(
			     fun resli->
			       (match resli with
				  | Some rli->
				      (match rli with
					 | Li (n,v,l)->
					     if preds#get_predicate n=v or !condres="" then
					       condres:=resolve_element (DynArray.of_list l);
					 | _ -> ()
				      )
				  | _ -> ()
			       )
			   ) st;
			   res:=!res^ !condres
		     | Li (n,v,st) -> print_string "(li)";print_newline();
		     | Formal st -> 
			 res:=!res^String.capitalize (String.lowercase (resolve_element (DynArray.of_list st)))
		     | Get s->
			 res:=!res^(preds#get_predicate s)
		     | Set (s,st)->



			 preds#set_predicate s (resolve_element (DynArray.of_list st))
		     | Input i -> 
			 res:=!res^self#get_input i
		     | That i -> 
			 res:=!res^self#get_that i
		     | _ -> print_string "(other)";print_newline();
		  )
	      | _ -> ()	      
	) t;
      !res in
      resolve_element template

  method  resolve_brain si with_history=  
    let s=input_normalize si in 
      print_string ("that: "^
		    (try
		       self#get_that 1
			 
		     with
			 DynArray.Invalid_arg(_,_,_)->
			   ("<that> *")
		    )
		   );print_newline();
      print_string si;print_newline();
      print_string s;print_newline();
    let wl=
      let sthat=
	(s^" <that> "^
	 (try
	    input_normalize(self#get_that 1)
	  with 
	      DynArray.Invalid_arg(_,_,_)->
		"*"
	 )
	)
      in
	print_string sthat;print_newline();
	(Str.split (Str.regexp " +") sthat)
    in
    let (patt,templ)=self#get_graph#get_match_path wl in
      if with_history then
	self#push_input si;
      let rs=self#resolve_template templ#get_template patt s in
	if with_history then
	  self#push_that rs;
	rs
  

end;;



