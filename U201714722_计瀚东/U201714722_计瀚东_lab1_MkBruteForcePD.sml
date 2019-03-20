functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210


  fun parenDist (parens : paren seq) : int option =
    case showl parens
      of NIL => NONE
       | CONS(h,t) =>
          let
            (*递归地计算一个左括号与其匹配的右括号的距离*)
            fun count(p : paren seq, c, s) =
              case showl p
                of NIL => NONE
                 | CONS(x,xs) => 
                     (case x 
                       of OPAREN => count(xs, c+1, s+1)
                        | CPAREN => if s = 1 then SOME(c+1)
                                    else count(xs, c+1, s-1))
          in
            (*如果是右括号就直接跳过*)
            if h = CPAREN then parenDist(t)
            else 
              let 
                (*串的最长距离可能是第一个左括号或者其他的左括号*)
                val rawa = count(t, 1, 1)
                val rawb = parenDist(t)
              in
                case (rawa, rawb)
                  of (NONE, NONE) => NONE
                   | (SOME a, NONE) => SOME a
                   | (NONE, SOME b) => SOME b
                   | (SOME a, SOME b) => if a > b then SOME a else SOME b
              end 
    end;
end
