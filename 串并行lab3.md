# 串行与并行数据结构与算法LAB3

​	学号：U201714722

​	姓名：计瀚东

​	日期：2018.11.21

## 1、实验要求

​	实现n位二进制大整数的加法运算。输入a, b和输出s都是二进制位的串。要求算法的时间复杂度满足work=O(n)，span=O(log n)。



## 2、实验思路



## 3、回答问题

### 3.1、加法计算

Task 4.1 (35%). Implement the addition function

++ : bignum * bignum -> bignum

in the functor MkBigNumAdd in MkBigNumAdd.sml. For full credit, on input with *m* and *n* bits, yoursolution must have $O(m+n)$ work and $O(lg(m+n))$ span. Our solution has under 40 lines with comments.

```scheme
 (*
  * 大整数相加
  * 1）首先把两个bit串补为相同长度，方便后续计算；
  * 2）然后使用按位异或的方式得到两个串不考虑进位的和；
  * 3）然后使用carry串储存每一位的进位信息：
  *      1+1用GEN表示；1+0用PROB表示；0+0用STOP表示
  * 4）然后在3 的基础上使用scan对进位情况进行分析，使用的结合函数为：
  *      _, GEN => GEN      如果后一位是GEN则下一位必然会进位；
  *      _, STOP => STOP    如果后一位是STOP则下以为必然不会进位；
  *      some, PROP => some 如果后一位是PROB则它传递之前的进位情况；
  *    可以得出满足结合性；
  *    scan之后得到了每一位的进位情况：
  *      GEN表示进位，STOP表示不进位，PROP不会存在；
  *    多出的一位同时表示是否溢出；
  * 5）然后用4中得到的carry串映射为bit串：
  *      GEN -> ONE, STOP -> ZERO；
  * 6）然后把2中不考虑进位的和与5中表示进位的串在进行异或操作：
  *    就可以得到相加之后的结果；
  *    最后看一下有没有溢出，如果就就在高位补充一个1就行了。
  *)
  fun x ++ y =
    let 
      (*1、高位补零使两串等长*)
      fun with0(a : bit seq, b : bit seq) =
        let val n = Int.max(length(a), length(b))
            val taila = tabulate (fn i => ZERO) (n - length a)
            val tailb = tabulate (fn i => ZERO) (n - length b)
        in (append(a, taila), append(b, tailb))
        end;
    
      (*2&6、异或得到按位相加的和，不考虑进位*)
      fun bitXor(x : bit seq, y : bit seq) = 
        map2 (fn (xi, yi) => if xi = yi then ZERO else ONE) x y;
    
      (*3&4&5，得到进位信息，用bit表示*)
      fun getCarry(x : bit seq, y : bit seq) =
        let (*3，得到朴素carry信息*)
            fun getNaiveCarry(xi : bit, yi : bit) : carry = 
              case (xi, yi)
                of (ONE, ONE) => GEN
                 | (ZERO, ZERO) => STOP
                 | _ => PROP;
            (*4，结合函数，通过朴素carry信息推导真实carry信息*)
            fun getRealCarry(ca1 : carry, ca2 : carry) : carry= 
              case (ca1, ca2)
                of (_, GEN) => GEN
                 | (_, STOP) => STOP
                 | (some, PROP) => some;
            (*5，carry转化为bit，这作为结果的第二部分*)
            fun getBitCarry(n) = if n = GEN then ONE else ZERO;
         in 
           let 
             val naiveCarry = map2 getNaiveCarry x y
             val (realCarry, high) = scan getRealCarry STOP naiveCarry
             val bitCarry = map getBitCarry realCarry
           in 
             (bitCarry, high)
           end
        end;
    
    in
      let
        val (cx, cy) = with0(x, y)
        val rawResult = bitXor(cx, cy)
        val (carryResult, high) = getCarry(cx, cy)
        val result = bitXor(rawResult, carryResult)
      in
        (*6，判断高位是否溢出*)
        if high = GEN then append(result, singleton ONE)
        else rawResult 
      end
    end;


```



### 3.2、减法计算

Task 4.2 (15%). Implement the subtraction function

-- : bignum * bignum -> bignum

in the functor MkBigNumSubtract in MkBigNumSubtract.sml, where *x* -- *y* computes the number

obtained by subtracting *y* from *x*. We will assume that *x*≥*y*; that is, the resulting number will always be non-negative. You should also assume for this problem that ++ has been implemented correctly. For full credit, if *x* has *n* bits, your solution must have *O*(*n*) work and *O*(lg*n*) span. Our solution has fewerthan 20 lines with comments.

```scheme
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
```





### 3.3、乘法计算

Task 4.3 (30%). Implement the function

** : bignum * bignum -> bignum

in MkBigNumMultiply.sml. For full credit, if the larger number has n bits, your solution must satisfy $W(n)=W(n/2)+O(n)$and have $O(lg^{2}n)$ span. You should use the following function in the Primitives structure:

val par3 : (unit -> ’a) * (unit -> ’b) * (unit -> ’c) -> ’a * ’b * ’c

to indicate three-way parallelism in your implementation of **. You should assume for this problem that++ and -- have been implemented correctly, and meet their work and span requirements. Our solutionhas 40 lines with comments.

```

```





### 3.4、迭代计算复杂度分析

Task 5.1 (15%). Determine the complexity of the following recurrences. Give tight *Θ*-bounds, and

justify your steps to argue that your bound is correct. Recall that *f*∈*Θ*(*g*) if and only if *f*∈*O*(*g*) and *g*∈*O*(*f*). You may use any method (brick method, tree method, or substitution) to show that your bound is correct, except that you must use the substitution method for problem 3.

$T(n)=3T(n/2)+\Theta(n)$

$T(n)=2T(n/4)+\Theta(\sqrt n)$

$T(n)=4T(n/4)+\Theta(\sqrt n)$(Prove by substitution)

解：

（1）、$T(n)=3T(n/2)+\Theta(n)$

​	我们猜测$\Theta(n^{log_{2}3})$是问题的答案：

​	假设“存在$c_{1}>0,c_{2}>0, n_{0}>0$使得$n>n_{0}\rightarrow T(n)<c_{1}n^{log_{2}3}-c_{2}n-c_{3}$”对$T(n/2)$成立；

​	则对$T(n)$有：

​		$T(n)<3[c_{1}(\frac {n}{2})^{log_{2}3}-c_{2}\frac {n}{2}-c_{3}]+c_{4}n+d$

​		$T(n)<c_{1}(\frac {n}{2})^{log_{2}3}-3c_{2}\frac {n}{2}-3c_{3}+c_{4}n+d$

​	取$ c_{2}=2c_{4},c_{3}=d/2$则有：

​		$T(n)<c_{1}(\frac {n}{2})^{log_{2}3}-c_{2}n-c_{3}$也成立

​	由算法导论master method知：

​		a=3，b=2；$\Theta(n^{log_{2}3})>\Theta(n)$；根节点占据主体，复杂度$\Theta(n^{log_{2}3})$。

（2）、$T(n)=2T(n/4)+\Theta(\sqrt n)$

​		



