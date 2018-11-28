functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
 (*
  * 大整数相减
  * 条件：x和y均为正数且x>y。
  * 1）给y高位补零使x和y等长，便于计算；
  * 2）对y进行取反加一，不考虑符号位；
  * 3）x与y的补码相加取模，结果必然正确；
  * 以上三个步骤分别使用下面的三个“过程”表示
  *)  
  fun x -- y =
    let
      (*1、补0使之等长*)
      fun sameLen(x : bit seq, y : bit seq) = 
        let val tail = tabulate (fn i => ZERO) (length(x)-length(y))
        in append(y, tail) end
      (*2、取补码*)
      fun trueToComp(x : bit seq) : bit seq = 
        (map (fn i => if i = ONE then ZERO else ONE) x) ++ singleton ONE;
      (*3、取模相加实现减法*)
      fun compSub (x: bit seq, cy: bit seq) : bit seq = 
        take(x ++ cy, length(x))
    in
      compSub(x, trueToComp(sameLen(x, y)))
    end;
  val sub = op--
end
