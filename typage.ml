open Ast


(* stockage des variables*) 

type dvlist = var_decl list

module Orderid = 
  struct
    type t = string
    let compare = compare
  end
module Idmap = Map.Make(Orderid)

type mymap = Idmap

type env =   { var :(string ,c_type) Hashtbl.t ;mutable structure : dvlist Idmap.t ;mutable union : dvlist Idmap.t} 

type funt = {retour : c_type ; fdvl : dvlist  }

let glob = { var =  Hashtbl.create 1007; structure = Idmap.empty ; union = Idmap.empty }
let funglob =ref Idmap.empty



(* exception  *)

exception Type_Error of loc*string 

let error loc msg =
  raise (Type_Error (loc, msg))

exception Epoint
exception Interne

(* fonction annexes*)
let tbon t env = 
  match t with
  | Tstruct ident -> (Idmap.mem ident.node (env.structure))
  | Tunion  ident -> (Idmap.mem ident.node (env.union))
  | _ -> true

(* verification de declaration*)
let vdec v env = 
  if not (Hashtbl.mem env.var v.node) then
    false 
  else 
    error v.loc " Var already declared "  

let sdec v env =  
  if not (Idmap.mem v.node env.structure )  then
    false 
  else 
    error v.loc " Struct already declared "  

let udec v env =  
  if not (Idmap.mem v.node env.union )  then
    false 
  else 
    error v.loc " Union already declared "  

let fdec v =  
  if not (Idmap.mem v.node !funglob )  then
    false 
  else 
    error v.loc " Union already declared "  


let num = function
  |Tnull|Tint|Tchar|Tpointer _ ->true
  |_ -> false

let easy = function 
  |Tnull|Tint|Tchar -> true 
  |_ ->false

let is_pointer = function |Tnull |Tpointer _ -> true |_ -> false

let is_union  = function |Tunion _ -> true| _ ->false

let is_struct = function |Tstruct _ -> true| _ -> false

let valeur = function 
  |Tpointer p -> p  
  | _ as c -> c

let unst_val = function
  |Tstruct d -> d.node
  |Tunion d -> d.node
  |_ -> "null"

let rec equal n1 n2 =
  let intern n1 n2 =
    match n1 with
      |Tnull       -> num n2   
      |Tvoid       -> n2 = Tvoid
      |Tint        -> easy n2
      |Tchar       -> easy n2
      |Tpointer p  -> (n2 = Tpointer Tvoid)||((is_pointer n2) && (equal p (valeur n2))) || (n2 = Tnull) 
      |Tstruct d1   ->(is_struct n2) && (d1.node =  (unst_val n2))
      |Tunion d1    -> (is_union n2) && (d1.node =  (unst_val n2))
      |_ -> false
  in (intern n1 n2) || (intern n2 n1)

let isaccesvalid e x env = 
  match e with 
    |Tstruct id -> 
      begin
       	let dvl =
          try 
          Idmap.find id.node env.structure                    
          with Not_found -> 
	    try
	      Idmap.find id.node glob.structure
	    with Not_found -> raise Epoint
	in
	let t,b = List.fold_left 
	  (fun acc (t,id) ->  if id.node = x.node then (t,true) else acc ) (Tnull,false) dvl
        in
        if b then t else raise Epoint            
      end
    |Tunion id ->
      begin
	let dvl =
        try 
          Idmap.find id.node env.union                    
        with Not_found -> 
	  try
	    Idmap.find id.node glob.union
	  with Not_found -> raise Epoint
	in
        let t,b = List.fold_left 
	  (fun acc (t,id) ->  if id.node = x.node then (t,true) else acc ) (Tnull,false) dvl
        in
        if b then t else raise Epoint  
      end
    |_ -> raise Epoint 

(* valeurs gauches *)

let is_valeurgauche e env = 
  match e.node with
  | Eident x        -> 
    (Hashtbl.mem env.var x.node )||(Hashtbl.mem glob.var x.node)  (* environement local & global*)    
  | Eunop (Ustar,e) -> is_pointer e.loc
  | Edot (e,id)     -> 
    begin
      try isaccesvalid e.loc id env; true with _ -> false
    end
  |_ ->  false

(* expression *)
let rec type_expr e env =
  let l = e.loc in
  match e.node with
  |Enull -> {node = Enull; loc = Tnull}
  |Econst c -> 
    begin
      match c with
      |Cint x ->if x = Int32.zero then 
	 {node = Enull; loc = Tnull}
	else
	  {node = (Econst c); loc = Tint}   (* a veriffier*)
      |Cstring _->{node = (Econst c); loc = (Tpointer Tchar)}
    end
  |Eident id ->
    let t = 
      begin
	try 
	  Hashtbl.find env.var id.node
	with Not_found ->
	  try 
	    Hashtbl.find glob.var id.node
	  with _ -> error l " ID undefind "
      end  
	in
    {node = (Eident id); loc = t}
  |Esizeof t -> 
    if (tbon t env)&&( t != Tvoid ) then 
      {node = (Esizeof t); loc = Tint} 
    else error l "Sizeof type not compatible"
  |Edot(e,id) -> 
    let t = (type_expr e env)
    in
    let tv = try isaccesvalid t.loc id env with _ -> error l " Acces to un excpression without a field"
    in
    {node = Edot(t,id); loc = tv }  
  
  |Eassign(e1,e2) ->
    let t1 = (type_expr e1 env)  in
    let t2 = (type_expr e2 env) in
    let b  = is_valeurgauche t1 env in
    if (b)&&(equal (t1.loc) (t2.loc)) then   (* creer fonction pour veriffier egalite *)
      {node = Eassign(t1,t2); loc = t1.loc}
    else
      error l " affectation de type non compatible"
  
  |Eunop (op1,e) ->  
    let ex = type_expr e env in 
    let b  = is_valeurgauche ex env in
    begin
      match op1 with
      |Upre_inc |Upost_inc  |Upre_dec  |Upost_dec ->
	if (b)&&(num ex.loc)  then 
	  {node = Eunop(op1,ex); loc =  ex.loc} (* increment et decrement*)
	else
	  error l " This operator doesn't apply to this type"
      |Uplus |Uminus ->
	if  (ex.loc = Tint) then
          {node = Eunop(op1,ex); loc = Tint} (* + -  *)
	else
	  error l " This operator doesn't apply to this type" 
      |Unot ->
	if (num ex.loc) then
          {node = Eunop(op1,ex); loc = Tint} (* ! *)
        else
	  error l " This operator doesn't apply to a type different from int"
      |Uamp ->
	if (b) then 
	  {node = Eunop(op1,ex); loc = Tpointer(ex.loc)}
	else
	  error l " This operator doesn't apply to this expression"
      |Ustar ->
	if(is_pointer ex.loc) then
	  {node = Eunop(op1,ex); loc = valeur ex.loc}
	else
	  error l " The type must be pointer type"
    end
  |Ebinop(bop,e1,e2) ->
    let comp  = function |Beq |Bneq |Blt |Ble |Bgt |Bge  -> true | _ -> false
    in
    let arit  = function |Badd |Bsub |Bmul |Bdiv |Bmod |Band |Bor -> true | _ -> false
    in
    let t1 = (type_expr e1 env) 
    in
    let t2 = (type_expr e2 env) 
    in
     
    if (comp bop) then
      begin     
       if (equal t1.loc t2.loc) && (num t1.loc) then (* comparaison *)
	 {node = Ebinop(bop,t1,t2) ; loc = Tint}
       else  error l "  The two types must be compare-able ' INT type' "	 
      end
    else 
      begin
	if (arit bop) then (* artihmetiques logiques et pointeurs*)
	  begin	
	    let returnarith =
	      if (equal t1.loc t2.loc) && (equal t1.loc Tint) then
		{node = Ebinop(bop,t1,t2); loc = Tint}	 
	      else 
		error l " The type must be the same and equal to INT"
	    in
	    begin
	      match bop with
		|Badd ->
		  if (is_pointer t1.loc ) && (equal t2.loc Tint) then (* cas + et -*)
		    {node = Ebinop(bop,t1,t2) ; loc = t1.loc}
		  else returnarith
		|Bsub ->
		  if (is_pointer t1.loc )  then
		    begin
		      if  (equal t2.loc Tint) then (* cas du - et +*)
			{node = Ebinop(bop,t1,t2) ; loc = t1.loc}
		      else
			if (equal t1.loc t2.loc) then (* cas du - *)
			  {node = Ebinop(bop,t1,t2) ; loc = Tint} 
			else  error l " This two types must be the compatible"
		    end
		  else returnarith 
		|_ -> returnarith
	    end 
	  end
	else error l " This operator doesn't apply to this type"
      end    
 
  |Ecall(id,el)  -> 
    begin
      try 
        let funty = Idmap.find id.node !funglob 
        in
        let rec isfun vf pf acc = 
	  match vf,pf with
            |[],[] -> (true,acc)
            |a::r1,b::r2 -> 
	      let t = (type_expr a env)
	      in
              if equal  t.loc (fst  b) 
              then isfun r1 r2 (acc@[t]) 
              else (false, [])
	    |_ -> (false, [])
	in 
	let b,lvar =(isfun el (funty.fdvl) []) 
	in
	if b then 
	  { loc = funty.retour; node = Ecall(id,lvar) }
	else error l " No function with this ID"
      with not_found -> error l  " This function is not declared "
    end 
exception ExpressionNonNumerique
(* on fait les exception apres just pour compiler *)

(*dans typage instruction on supprimer la location du instruction cas il est pas important apres la phase typage*)
let rec typage_instruction env i = (* i avec localisation *)
  let l = i.loc in
  match i.node with 
  |Sskip -> {node= Sskip ;loc= Tnull} 
  |Sexpr(e) ->
    begin
      let te = type_expr e env in (*typer un expression *)
      {node= Sexpr te; loc =  Tnull}
    end

  |Sif(e,ins1,ins2) -> (* typer if else *)
    begin
      let te = type_expr e env in
      let tins1 = typage_instruction env ins1 in
      let tins2 = typage_instruction env ins2 in
      if (num te.loc) 
      then {node= Sif(te,tins1,tins2);loc= Tnull}
      else
	error l " The type of test must be INT"
    end
  
  |Swhile(e,ins) ->
    begin
      let te = type_expr e env  in
      let tins = typage_instruction env ins in 
      if (num te.loc)
      then {node= Swhile(te,tins);loc= Tnull}
      else error l " The type of the test must be INT"
    end


|Sfor(list_ins1,e,list_ins2,ins) ->
  begin
    let te = type_expr e env in 
    if (num te.loc)
    then let tlist_ins1 = List.map (typage_instruction env) list_ins1 in
	 let tlist_ins2 = List.map (typage_instruction env) list_ins2 in
	 let tins = typage_instruction env ins in
	begin(* test si e vide ou non*)
	let r = {node = Econst (Cint Int32.one);loc = Tint } in
	match e.node with
	|Econst(x) ->{node = Sfor(tlist_ins1,r,tlist_ins2,tins); loc = Tnull}(*bizzas*)
	|_-> {node = Sfor(tlist_ins1,te,tlist_ins2,tins); loc = Tnull}
	end
	else error l " Excpression must be numerique "
  end

|Sblock(x) ->
 { node= Sblock(type_block x env);loc = Tnull} 

|Sreturn (eo) ->
  begin
    match eo with
    |Some e -> let te = type_expr e env in
	       { node = Sreturn(Some te);loc = te.loc}
    |None -> { node= Sreturn None;loc= Tvoid}
  end
and

(* typage de block *)
    type_block x env =
  let list_decl = fst x in
  let list_ins  = snd x in
  let y = type_decl env (Dvars list_decl) in
  let tlist_decl = function |Dvars x -> x | _ ->[](* juste pour le warning *) in
  let tlist_ins = List.map (typage_instruction env) list_ins in
  ((tlist_decl y),tlist_ins)

and
(* typage des declaration*)

    type_decl env x = 
  let rec test dvl e acc =
    match dvl with
      |[] -> if (acc = []) then (true,[]) else (false,acc)
      |(t,id)::r -> if (tbon t e)&&(t != Tvoid) then test r e acc else test r e (acc@[(t,id)])
  in
  match x with  
    |Dvars dvl ->
      let test_var e list = 
	List.fold_left 
	  (fun acc (t,id) -> 
	    if(tbon t e) && not(vdec id e) then
	      begin
	      Hashtbl.add e.var id.node t;
		acc
	      end
	    else acc@[(t,id)] ) [] list
      in
      let loc = test_var env dvl 
      in
      test_var glob loc; Dvars dvl

    |Dstruct (id,dvl) ->       
      let test_struct e dvl =
 	let b,ndvl = test dvl e []
	in 
	if (b) && not(sdec id e) then
	  begin
	    e.structure <- Idmap.add id.node dvl e.structure;
	    Dstruct (id,dvl) 
	  end 
	else raise Interne
      in
      let b,ndvl = test dvl env []
      in 
      begin
	try
	  test_struct env dvl
	with Interne -> 
	  begin
	    try
	      test_struct glob ndvl
	    with _ -> error id.loc " Identificator already used"
	  end
      end
    |Dunion  (id,dvl) ->
      let test_union e dvl =
	let b,ndvl = test dvl e []
	in
	if (b) && not(udec id e) then
	  begin
	    e.union  <- Idmap.add id.node dvl e.union;
	    Dunion  (id,dvl)
	  end
	else raise Interne
      in
      let b,ndvl = test dvl env []
      in
      begin
	try
	  test_union env dvl
	with Interne ->
	  begin
	    try 
	      test_union glob ndvl 
	    with _ -> error id.loc " Identificator already used"
	  end
      end
    |Dfun (t,id,dvl,ibk) -> 
      let b,ndvl = test dvl env []
      in
      if( (tbon t env)
	  && (b) 
	  && not(fdec id) ) then 
	begin
	  let localv = Hashtbl.create 1007 in
	 let () = Hashtbl.add localv id.node t
	  in
	  let temp = {var = localv; structure = env.structure ; union = env.union}
	  in
	  let () =
	    List.iter 
	      (fun  (t,id) -> 
		if(tbon t temp) && not(vdec id temp) then
		  Hashtbl.add temp.var id.node t
		else error id.loc " ID already used" ) dvl
	  in
	 
	  let local = {var = localv ; structure = Idmap.empty ; union = Idmap.empty}
	  in	 
	  begin
	    funglob := Idmap.add id.node {retour = t;fdvl = dvl} !funglob;
	     let nbl = type_block ibk local
	     in
	    Dfun (t,id,dvl,nbl);
	   
	  end
	end
      else error id.loc " Function bad prototype"
	
(*typage des fichiers *)

let typage_fichiers  x glob  =  List.map (type_decl glob) x


(* ajout des primitives à l'environement globale*)
let () =
  let id = "n"
  in
  let dvl = [(Tint,{node = id; loc = (Lexing.dummy_pos,Lexing.dummy_pos) })]
  in
  let local = {var = Hashtbl.create 3 ; structure = Idmap.empty ; union = Idmap.empty}
  in 
  let f = "putchar"
  in
  begin
    Hashtbl.add local.var id Tint ;
    funglob := Idmap.add f {retour = Tint;fdvl = dvl} !funglob
  end

let () =
  let id = "n"
  in
  let dvl = [(Tint,{node = id; loc = (Lexing.dummy_pos,Lexing.dummy_pos) })]
  in
  let local = {var = Hashtbl.create 3 ; structure = Idmap.empty ; union = Idmap.empty}
  in 
  let f = "sbrk"
  in
  begin
    Hashtbl.add local.var id Tint ;
    funglob := Idmap.add f {retour = Tpointer Tvoid;fdvl = dvl} !funglob
  end

(* typage du programe*)

let typage p =   
  let retour = typage_fichiers p glob
  in 
  let search = 
    try
      let m = Idmap.find "main" !funglob
      in 
      let is_first_arg = function
	|(Tint,{node = "argc"; loc = _ }) -> true
	|_ -> false
      in
      let is_second_arg = function
	|(Tpointer(Tpointer Tchar),{node = "argv"; loc = _ }) ->true 
	| _ -> false
      in
      if not (m.fdvl = []) then
	begin
	  match m.fdvl with
	    |a::b::[] ->
	      if not(is_first_arg a && is_second_arg b) then
		error (Lexing.dummy_pos,Lexing.dummy_pos) " Main declared with wrong arguments"
	    |_ -> error (Lexing.dummy_pos,Lexing.dummy_pos) " Main declared with wrong arguments"
	end
    with Not_found -> error (Lexing.dummy_pos,Lexing.dummy_pos) " Main undeclared"
  in 
  begin    
    search;
    retour
   
  end
