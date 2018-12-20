functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  
  (* 保留每个出度大于0的节点，及其邻居 *)
  type graph = set table * int * set
  type asp = vertex * (vertex seq table)
  
  (* Task 2.1 *)
  (* adjacent table, W = O(E*lgE+lgV), S = O((lgE)^2)+lgV,*)
  fun makeGraph (E : edge seq) : graph =
    let
      val vtbusq = Table.collect E
      val vtbust = Table.map Set.fromSeq vtbusq
      val numEdg = Seq.length(E)
      val outers = Set.fromSeq (Seq.map (fn (i, _) => i) E)
      val inters = Set.fromSeq (Seq.map (fn (_, j) => j) E)
      val vtcset = Set.union (outers, inters)
    in
      (vtbust, numEdg, vtcset)
    end

  (* Task 2.2 *)
  (* 预先保存，并添加获得图结构的过程，W = S = O(1) *)
  (* 作用是在graph结构的实现和使用之间添加抽象层ABSTRACT LAYERS，使实现更健壮 *)
  fun numEdges    (G : graph) : int = (#2 G)
  fun numVertices (G : graph) : int = Set.size (#3 G)
  fun getGraph    (G : graph) = (#1 G)
  fun getVertices (G : graph) : set = (#3 G)


  (* Task 2.3 *)
  (* find相应节点，把邻居set变成seq， W = O(lgV + E/V), S = O(lgV) *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case find (getGraph G) v
      of NONE => Seq.empty()
       | SOME aset => Set.toSeq aset

  (* Task 2.4 *)
  (* bfs即可 *)
  exception BUG
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      val g = getGraph(G)
      fun bfs(px : vertex seq table, pf : vertex seq table) =
        case Table.size(pf)
          of 0 => px
           | _ =>
        let
          val tbunion = Table.merge Seq.append
          fun updatef (x : vertex seq table, pf : vertex seq table) =
            let
              fun nghbSet (G : graph) (v : vertex) : set =
                case find (getGraph G) v
                  of NONE => Set.empty()
                  | SOME aset => aset;
              fun getpath (v : vertex, us : vertex seq) =
                let
                  val p2x = case Table.find x v
                              of NONE => (Seq.empty())
                               | SOME value => value
                  val p2us = Seq.map (fn (u) => Table.singleton(v, Seq.append(p2x, Seq.singleton(u)))) us
                in p2us
                end;
              val xd = Table.domain x
              val pfd = Set.toSeq(Table.domain pf)
              val rvus = Seq.map (fn (v) => (v, Set.toSeq(Set.difference((nghbSet G v), xd)))) pfd
              val vus = Seq.filter (fn (v, s) => Seq.length(s) > 0) rvus
              val p2u = Seq.flatten(Seq.map getpath vus)
              val nf = Seq.reduce tbunion (Table.empty()) p2u
            in nf
            end;
          val x = tbunion(px, pf)
          val f = updatef(x, pf)
        in bfs(x, f)
        end;
    in (v , bfs(Table.empty(), Table.singleton(v, Seq.empty())) )
    end;

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
  let
    val paths = Table.collect(Table.toSeq (#2 A))
  in case find paths v
      of NONE => Seq.empty()
       | SOME value => value
  end
end
