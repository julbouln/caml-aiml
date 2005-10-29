

let input_normalize s=
  let reg="[\?!\.\+,']" in
  let rs=Str.global_replace (Str.regexp reg) " " s in
    String.uppercase rs;;


let regexp_of_pattern p=
    let rpattern=Str.full_split (Str.regexp "\*") p in
    let fstr=ref "^" in
      List.iter (
	fun del->
	  (match del with 
	    | Str.Text t->fstr:=!fstr^("\("^t^"\)");
	    | Str.Delim d->
		fstr:=!fstr^("\(.*\)");
		);
		
      )rpattern ;
      fstr:= !fstr^"$";
      (Str.regexp !fstr);;

