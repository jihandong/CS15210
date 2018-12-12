# Lab8-范围搜索实验

## 2、回答问题

### 2.1、完成函数first和last，简述

​	思路：根据BST的性质，沿着左侧或者右侧递归下降就可以达到最小和最大的key所在节点。

```haskell
  (*左递归下降*)
  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | SOME {key, value, left, right} =>
           case Tree.expose left
             of NONE => SOME(key, value)
              | _ => first(left);

  (*右递归下降*)
  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | SOME {key, value, left, right} =>
           case Tree.expose right
             of NONE => SOME(key, value)
              | _ => last(right);
```

​	测试样例：均通过

```haskell
  val ordSet1 = % [5, 7, 2, 8, 9, 1]
  

  val testsFirst = [
    ordSet1,
    % []
  ]
  val testsLast = [
    ordSet1,
    % []
  ]
```

### 2.2、完成函数previous和next，简述

​	思路：对previous，如果一个节点右子树的key等于我们给出的k，那么左子树的last就是前驱节点；对next，如果一个节点左子树的key等于我们给出的k，那么右子树的first就是后继节点。

```haskell
  (*前驱节点*)
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.expose T 
      of NONE => NONE
       | _ =>
           let 
             val (left, _, _) = Tree.splitAt(T, k)
           in
             last left
           end;
  
  (*后继节点*)
  fun next (T : 'a table) (k : key) : (key * 'a) option =
    case Tree.expose T
      of NONE => NONE
       | _ =>
           let
             val (_, _, right) = Tree.splitAt(T, k)
           in
             first right
           end;
```

​	测试样例：均通过

```haskell
  val testsPrev = [
    (ordSet1, 8),
    (ordSet1, 1),
    (% [], 8)
  ]
  val testsNext = [
    (ordSet1, 8),
    (ordSet1, 9),
    (% [], 8)
  ]
```

### 2.3、完成函数join和split，简述

​	思路：使用已有的轮子

```haskell
  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join(L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T, k)
```

​	测试样例：均通过

```haskell
  val testsJoin = [
    (ordSet1, % [100]),
    (ordSet1, % [3]),
    (ordSet1, mySet1),
    (% [], % [100])
  ]
  val testsSplit = [
    (ordSet1, 7),
    (ordSet1, 100),
    (% [], 7)
  ]
```

### 2.4、完成函数getRange，详叙

​	思路：对low和high边界分别使用split，再基于两次split的结果判断边界的key是否在table中存在，如果存在就把边界的singleton加入结果。

```haskell
  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
      val (_, m1, cut1) = split(T, low)
      val (cut2, m2, _) = split(cut1, high)
      fun complete(tree, m, board) =
        case m
          of NONE => tree
           | SOME(value) => join(tree, singleton(board, value));
    in
      complete(complete(cut2, m2, high), m1, low)
    end;
```

​	测试样例：均通过

```haskell
  val mySet1 = % [3, 2, 14, 13, 34, 8, 1, 60, 21, 5]
  
  val testsRange = [
    (ordSet1, (5,8)),
    (ordSet1, (10,12)),
    (mySet1, (5,40)),
    (% [], (5,8))
  ]
```

### 2.5、完成函数makeCountTable

​	思路：

```haskell

```

​	复杂度分析：

### 2.5、完成函数count，做相关分析

​	思路：

```haskell

```

​	复杂度分析：

​	测试样例