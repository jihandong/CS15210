functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  (* Remove this line before submitting! *)
  exception NYI

  (* left-recurse *)
  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | SOME {key, value, left, right} =>
           case Tree.expose left
             of NONE => SOME(key, value)
              | _ => first(left);

  (* right-recurse *)
  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | SOME {key, value, left, right} =>
           case Tree.expose right
             of NONE => SOME(key, value)
              | _ => last(right);
		      
  (*前驱节点 算法导论中的实现 bug*)
  (*
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | SOME {key, value, left, right} =>
           case Tree.expose right
             of NONE => NONE
              | SOME {k, _, _, _} => last left
              | _ => if k < key then previous left k
                     else previous right k;
                     *)
  (* previous node *)
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.expose T 
      of NONE => NONE
       | _ =>
           let 
             val (left, _, _) = Tree.splitAt(T, k)
           in
             last left
           end;
  
  (* next node *)
  fun next (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | _ =>
           let
             val (_, _, right) = Tree.splitAt(T, k)
           in
             first right
           end;

  (*这个意义不明*)
  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join(L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T, k)

  (*
  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    case Tree.expose tree
      of NONE => EMPTY
       | SOME {key, value, left, right} =>
           case (low < key, key < high)
             of (true, true) => NODE {key, value, getRange(left,low,high),
                                                  getRange(right,low,high)}
              | (false, true) => NODE {key, value, EMPTY,
                                                   getRange(right,low,high)}
              | (true, false) => NONE {key, value, getRange(left,low,high),
                                                   EMPTY};
                                                   *)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
      val (_, m1, cut1) = split(T, low)
      val (cut2, m2, _) = split(cut1, high)
      fun complete(tree, m, board) =
        case m
          of NONE => tree
           | SOME(value) => join(tree, singleton(board, value));
    in
      complete(complete(cut2, m2, high), m1, low)
    end;

end
