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
      (* exchange position of x and y, *)
      val ss = Seq.map (fn (i, j) => singleton(j, i)) ps
      val yts = Seq.scani join (empty()) ss
      (* get pair of x * points sets whose points are left to x *)
      val xsyt = Seq.map2 (fn((x, _), t) => (x, t)) ps yts
      (* we collect x * point sets togehter, and keep only one point set, the bigest*)
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