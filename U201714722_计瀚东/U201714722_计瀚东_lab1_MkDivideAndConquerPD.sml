functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210


  fun parenDist (parens : paren seq) : int option =
    let
      fun parenD (p : paren seq) =
        case showt p
          of EMPTY => (0, 0, 0, 0, 0)
           | ELT OPAREN => (0, 0, 1, 0, 1)
           | ELT CPAREN => (0, 1, 0, 1, 0)
           | NODE(s1, s2) =>
              let
                val ((m1, l1, r1, llen1, rlen1),
                     (m2, l2, r2, llen2, rlen2)) =
                  par(fn () => parenD s1, fn () => parenD s2)
                fun max3(a,b,c) =
                  if a > b then if a > c then a else c
                  else if b > c then b else c;
              in
                if r1 > l2 then
                  (m1, l1, r2 + r1-l2, llen1, rlen1+length(s2))
                else if r1 < l2 then
                  (m2, l1 + l2-r1, r2, length(s1)+llen2, rlen2)
                else let val m = max3(m1, m2, rlen1+llen2) in
                  (m, l1, r2, llen1, rlen2) end
              end;
    in
      let val(result, left, right, _, _) = parenD(parens)
      in if not(left=0) orelse not(right=0) orelse length(parens)=0 
            then NONE
         else SOME result end
  end;
end
