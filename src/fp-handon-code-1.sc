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
  * p30
  * 指定されたリスト内の数の合計を返す
  */
def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case head :: tail => head + sum(tail)
}

sum(List(1, 2, 3, 4, 5)) // should be 15

/**
  * p37
  * 指定されたリスト内の数を掛け合わせた値を返す
  */
def product(ints: List[Int]): Int = ints match {
  case Nil => 1
  case head :: tail => head * product(tail)
}

product(List(3, 3, 3, 3)) // should be 81

/**
  * p38
  * 指定されたリスト内の最大値を返す
  */
def max(ints: List[Int]): Int = ints match {
  case Nil => Int.MinValue
  case head :: tail => {
    val tmax = max(tail)
    if (head > tmax)
      head
    else
      tmax
  }
}

max(List(1, 10, 2, 5, 8)) // should be 10

/**
  * p39
  * 指定されたリストを逆順に並び替えて返す
  */
def reverse(ints: List[Int]): List[Int] = {
  def loop(src: List[Int], dst: List[Int]): List[Int] =
    src match {
      case Nil => dst
      case head :: tail => loop(tail, head :: dst)
    }

  loop(ints, Nil)
}

reverse(List(1, 2, 3, 4, 5)) // should be (5, 4, 3, 2, 1)

/**
  *
  */
def length(ints: List[Int]): Int = ints match {
  case Nil => 0
  case _ :: tail => length(tail) + 1
}

length(List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)) // should be 10

