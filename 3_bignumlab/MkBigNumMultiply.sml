functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
    case (length(x), length(y))
      of (0, _) => empty()
       | (_, 0) => empty()
       | (1, _) => if nth x 0 = ZERO then empty() else y
       | (_, 1) => if nth y 0 = ZERO then empty() else x
       | _ =>
    let
      fun with0(a : bit seq, b : bit seq) =
        let 
          val len = Int.max(length a, length b)
          val taila = tabulate (fn _ => ZERO) (len - length a)
          val tailb = tabulate (fn _ => ZERO) (len - length b)
        in (append(a, taila), append(b, tailb))
        end;
      val (nx, ny) = with0(x, y)
      val half = length(nx) div 2
      val q = take(nx, half)
      val p = drop(nx, half)
      val s = take(ny, half)
      val r = drop(ny, half)
      val (p1, p2, p3) = 
          par3(fn _ => p ** r,
               fn _ => q ** s,
               fn _ => (p ++ q) ** (r ++ s))
      val mm = tabulate (fn _ => ZERO) (half*2)
      val m  = tabulate (fn _ => ZERO) half
    in
      append(mm,p1) ++ append(m,p3 -- p1 -- p2) ++ p2
    end;
  val mul = op**
end
