functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type vertex = string
  type edge = vertex * vertex
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (vertex * vertex seq) seq) : thesaurus =
  let
    val edges = Seq.flatten(Seq.map (fn (w, s) => (Seq.map (fn (i) => (w, i)) s)) S)
    val thsrsGraph = makeGraph(edges)
  in thsrsGraph
  end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    numVertices(T)

  fun synonyms (T : thesaurus) (w : string) : string seq =
    outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let
      val w1tree = makeASP T w1
      val w12w2 = report w1tree w2
    in w12w2
    end
end
