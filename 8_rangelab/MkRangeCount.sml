functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  (* 表中表结构，外表是x，内表是y*)
  type countTable = (Key.t table) table

  (* Remove this line before submitting! *)
  exception NYI

  fun makeCountTable (S : point seq) : countTable =
    let
      val ps = Seq.sort (fn ((i, _), (j, _)) => compareKey(i, j)) S
      (*构造内表：对每个x，把横坐标小于它的点做成内表*)
      val ss = Seq.map (fn (i, j) => singleton(j, i)) ps
      val yts = Seq.scani join (empty()) ss
      (*构造外表对应的串：把所有的“x-内表”整理成串*)
      val xsyt = Seq.map2 (fn((x, _), t) => (x, t)) ps yts
      (*去重：将重复的x清除，确保x唯一，再构成外表*)
      val xcyts = Seq.collect compareKey xsyt
      val xcyt = Seq.map (fn (x, s) => singleton(x, Seq.nth s (Seq.length(s)-1))) xcyts
      val xtyt = Seq.reduce join (empty()) xcyt
    in
      xtyt
    end;

  fun count (T : countTable) ((x1, y2) : point, (x2, y1) : point) :int  = 
    if size(T) = 0 then 0 else
    let
      fun getsize t = size(getRange t (y1, y2))
      (*得到小于x1且纵坐标满足范围限制的点数*)
      fun getcount1(t : countTable, x1) =
        let
          val (l, m, _) = split(t, x1)
        in
          case last(l)
            of NONE => 0
             | SOME(_,yt) => getsize yt 
        end;
      
      (*得到小于等于x2且纵坐标满足范围限制的点数*)
      fun getcount2(t : countTable, x2) =
        let
          val (l, m, _) = split(t, x2)
        in
          case m
            of SOME yt => getsize yt
             | NONE =>
              case last(l)
                of NONE => 0
                 | SOME(_,yt) => getsize yt 
        end;
      val c1 = getcount1(T, x1)
      val c2 = getcount2(T, x2)
    in
      c2 - c1   (*两者相减*)
    end;
end