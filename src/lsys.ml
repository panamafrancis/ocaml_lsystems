open Core.Std

type ltree = Empty | 
             Operand of operand | 
             Branch of branch 
             and operand = { value : string; next: ltree }
             and branch = { left: ltree; right: ltree } 

type lsystem = {
            rules: (string*ltree) list; 
            init: ltree
}

(*
 * iterates up the tree, when it hits the root node it returns the 2nd tree
 * which is then appended to the first 
 * *)
let rec append_tree l1 l2 = 
    match l1 with
    | Empty -> l2
    | Branch {left=l; right=r} -> Branch{left=l; right=append_tree r l2}
    | Operand {value=v; next=t} -> Operand{value=v; next= append_tree t l2}

let rec insert_tree l1 l2 x = 
    match l1 with
    | Empty -> l2
    | Branch {left=l; right=r} -> 
            Branch{left=l; right=insert_tree r l2 x}
    | Operand {value=v; next=t} when v = x -> 
            Operand{value=v; next= insert_tree l2 t x}
    | Operand {value=v; next=t} -> 
            Operand{value=v; next= insert_tree t l2 x}

(* TODO expose? *)
let get_rule lrules r = 
    try
        List.Assoc.find_exn lrules r
    with
        (* If the operand is dead (like dead dna) then don't worry*)
        Not_found -> Empty

(* TODO refactor out tree rewriting? *)
let rewrite lsys ~n =
    let rec aux tree = 
        match tree with
        | Empty -> Empty
        | Branch {left=l; right=r} ->
                Branch{left=l; right=(aux r)}
        | Operand{value=v; next=t} ->
                let subtree = get_rule lsys.rules v 
                in (
                if subtree = Empty then
                    Operand{value=v; next=(aux t)}
                else
                    append_tree subtree (aux t))

    in let rec count lt m =
        (* rewrite the ltree n times *)
        if m > 0 then
            count (aux lt) (m-1)
        else (* return a new lsystem *)
            {rules = lsys.rules; init = lt}

    in count lsys.init n 


let ltree_to_string ltree =
    let rec aux tree = 
        match tree with
        | Empty -> ""
        | Branch {left=l; right=r} ->
                "[" ^ (aux l) ^ "]" ^ (aux r) 
        | Operand {value=v; next=t} ->
                v ^ (aux t)
    in aux ltree 

let lsys_to_string lsys = 
    let rec rules_to_string rule =
        match rule with
        | [] -> ""
        | (axiom, tree)::t -> "{axiom = " ^ axiom ^ 
                              "; rule = " ^
                              (ltree_to_string tree) ^ 
                              "};\n" ^ (rules_to_string t)

    in let all_rules = rules_to_string lsys.rules 
    in "{ rules = [" ^ all_rules ^ "]\n init = " ^ ltree_to_string lsys.init ^ "\n}"
