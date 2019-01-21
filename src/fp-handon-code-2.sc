/*
木虎直樹さんによるハンズオン
「関数プログラミングことはじめ in 福岡」
https://www.slideshare.net/kitora/in-128559362
の問題の回答例です。

この回答例では簡単のため、
・例外処理
・厳密な境界条件
を省略して本質的な部分のみとしています。

このコードの責任は藤田正訓 @tockri にあります。
*/

/**
  * 木構造の基底クラス
  */
sealed trait Tree

/**
  * 木構造の末端
  */
case object Empty extends Tree

/**
  * 木構造のノード (leftとrightがEmptyなノード＝葉）
  */
case class Node(value: Int, left: Tree = Empty, right: Tree = Empty) extends Tree

// 木構造のサンプル
val sampleTree = Node(10,
  Node(2,
    Empty,
    Node(3)
  ),
  Node(4,
    Node(15),
    Node(6,
      Empty,
      Node(7)
    )
  )
)

/**
  * p41
  * 指定された二分木内の数の合計を返す
  */
def sum(tree: Tree): Int = tree match {
  case Empty => 0
  case Node(value, left, right) => value + sum(left) + sum(right)
}

sum(sampleTree) // should be 47

/**
  * p49
  * 指定された二分木内の最大値を返す
  */
def max(tree: Tree): Int = tree match {
  case Empty => Int.MinValue
  case Node(value, left, right) =>
    val lmax = max(left)
    val rmax = max(right)
    if (lmax > rmax) {
      if (value > lmax) value
      else lmax
    } else {
      if (value > rmax) value
      else rmax
    }
}

max(sampleTree) // should be 15

/**
  * p50
  * 指定された二分木のNodeの数を返す
  */
def size(tree: Tree): Int = tree match {
  case Empty => 0
  case Node(_, left, right) => size(left) + size(right) + 1
}

size(sampleTree) // should be 7

/**
  * p51
  * 指定された二分木の末端のNodeまでの最長パスの長さを返す
  */
def depth(tree: Tree): Int = tree match {
  case Empty => 0
  case Node(_, left, right) =>
    val ldepth = depth(left)
    val rdepth = depth(right)
    1 + (if (ldepth > rdepth) ldepth else rdepth)
}

depth(sampleTree) // should be 4




