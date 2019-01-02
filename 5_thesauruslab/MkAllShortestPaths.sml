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
  
  (* set table 含有每个节点及其邻居 *)
  (* int 边数量 *)
  (* set 节点集合 *)
  type graph = set table * int * set
  (* vertex 起点 *)
  (* vertex seq seq table 到每个节点的路径串的串 *)
  type asp = vertex * (vertex seq seq table)
  
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
  (* 瞎扯：作用是在graph结构的实现和使用之间添加抽象层，使实现更健壮 *)
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
      fun bfs (x : vertex seq seq table, f : vertex seq seq table) =
      (* W = O(|F| * (E/V)^2 * logV) *)
      (* S = O(logV) *)
        case Table.size(f)
          of 0 => x
           | _ =>
        let
          val x_ = Table.merge Seq.append (x, f)
          (* W = O(|F|*log(|X|/|F|)), S = (log(|X|+|F|)) *)
          val fdmn = Table.domain f

          (* get sons and father pairs whose father is from F*)
          (* W = O(|F|*(logV+E/V)), S = O(logV) *)                         
          fun getSonsFather (fa : vertex) =
            Seq.map (fn (so) => (so, fa)) (outNeighbors G fa)
          val foutsT = Table.tabulate getSonsFather fdmn

          (* get new veterx together *)
          (* assume that every veterx in F has E/V sons *)
          (* W = O((|F|*E/V)*log(V)), S = O(log^2 V) *)
          val outfsS = Table.reduce Seq.append (Seq.empty()) foutsT

          (* W = (|F|*E/V)*logV+ |F|*(E/V)^2), S = O(logV) *)
          fun getPath (so : vertex, fa : vertex) = 
            let 
              val p2fa = case Table.find x_ fa
                           of NONE => Seq.singleton(Seq.empty())
                            | SOME paths => paths
              val p2so = map (fn (p) => Seq.append(p, Seq.singleton so)) p2fa
            in Table.singleton(so, p2so)
            end;
          val rawf_seq = Seq.map getPath outfsS

          (* get new paths into collet ... *)
          (* W =  O((|F|*(E/V)^2*logV), S = O(log^2 V) *)
          val rawf_ = Seq.reduce (Table.merge Seq.append) (Table.empty()) rawf_seq
          
          (* ... and erase what we have had *)
          (* W = O( |F|*(E/V)*log((|X|+|F|)/)), S = O(logV) *)
          val f_ = Table.erase(rawf_, Table.domain(x_))
        in bfs(x_, f_)
        end
    in
      (* assume D*|Fi| for i in every recurse is equal to V *)
      (* W = O(D*W(bfs)) = O(D*|F|*(E/V)^2*logV = O(E/V* E*logV)) *)
      (* S = O(D*S(bfs)) = O(D*logV *)
      (v, bfs(Table.empty(), Table.singleton(v, Seq.singleton(Seq.singleton(v)))))
    end
  
(* Task 2.5 *)
(* W = O(logV), S = O(logV) *)
fun report (A : asp) (v : vertex) : vertex seq seq =
  let
    val paths  = case Table.find (#2 A) v
                   of NONE => Seq.empty()
                    | SOME v => v
  in paths
  end
end
