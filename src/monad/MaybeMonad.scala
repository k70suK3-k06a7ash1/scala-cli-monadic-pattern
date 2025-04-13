// Scala CLI で実行する場合、特にライブラリ依存はないのでディレクティブは不要です。
// ファイル名: MaybeMonadExample.scala (例)

// --- 1. Monad トレイトの定義 ---
/**
 * Monad の基本的な操作を定義するトレイト。
 * @tparam M 型コンストラクタ (例: Maybe[_], List[_])
 */
// trait Monad[M[_]] {
//   /**
//    * 値をモナドのコンテキストに持ち上げる (ラップする)。
//    * @param value 持ち上げる値
//    * @tparam A 値の型
//    * @return モナドコンテキスト内の値
//    */
//   def pure[A](value: A): M[A]

//   /**
//    * モナドの値に関数を適用し、結果のモナドを平坦化する。
//    * Monad の中心的な操作。bind や >>= とも呼ばれる。
//    * @param ma モナドの値
//    * @param f 値を受け取り、新しいモナドの値を返す関数
//    * @tparam A 元のモナドの値の型
//    * @tparam B 新しいモナドの値の型
//    * @return 処理結果のモナドの値
//    */
//   def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

//   /**
//    * モナド内の値に関数を適用する。
//    * flatMap と pure を使って実装できる。
//    * @param ma モナドの値
//    * @param f 値を受け取り、新しい値を返す関数
//    * @tparam A 元のモナドの値の型
//    * @tparam B 新しい値の型
//    * @return 新しい値を持つモナドの値
//    */
//   def map[A, B](ma: M[A])(f: A => B): M[B] =
//     flatMap(ma)(a => pure(f(a)))
// }

// --- 2. Maybe データ型の定義 ---
// Scala 標準の Option 型を模倣した独自のデータ型を作成します。
/**
 * 値が存在するかもしれないことを表す代数的データ型 (ADT)。
 * Option 型の代替。
 * +A は共変性を示し、Nothing が任意の Maybe[A] のサブタイプとして扱えるようにします。
 */
// sealed trait Maybe[+A]
/**
 * 値が存在する場合のケース。
 * @param value 存在する値
 */
// case class Just[+A](value: A) extends Maybe[A]
/**
 * 値が存在しない場合のケース (null や None に相当)。
 */
// case object Nothing extends Maybe[Nothing]



// --- 3. Maybe 用の Monad インスタンスの実装 ---
// Maybe 型コンストラクタに対する Monad トレイトの実装を提供します。
object MaybeMonad extends Monad[Maybe] {

  /**
   * 値を Just でラップして Maybe モナドに持ち上げます。
   */
  override def pure[A](value: A): Maybe[A] = Just(value)

  /**
   * Maybe の flatMap 操作を実装します。
   * - 入力が Just(a) の場合、関数 f を a に適用します。
   * - 入力が Nothing の場合、結果は常に Nothing です。
   */
  override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
    ma match {
      case Just(a) => f(a) // 関数 f を適用
      case Nothing => Nothing // Nothing を伝播させる
    }

  /**
   * Maybe の map 操作も具体的に実装できます（Monad トレイトのデフォルト実装を使ってもOK）。
   * - 入力が Just(a) の場合、関数 f を a に適用し、結果を Just でラップします。
   * - 入力が Nothing の場合、結果は常に Nothing です。
   */
  override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
    ma match {
      case Just(a) => Just(f(a)) // Just の中身に関数を適用して Just でラップ
      case Nothing => Nothing    // Nothing を伝播させる
    }
}

// --- 4. Maybe Monad の使用例 ---
@main def runMaybeMonad(): Unit = {
  println("--- Maybe Monad Usage Example ---")

  // Monad インスタンスの取得
  val M = MaybeMonad

  // Maybe の値を作成
  val maybeFive: Maybe[Int] = Just(5)
  val maybeTen: Maybe[Int] = M.pure(10) // pure を使用
  val noValue: Maybe[Int] = Nothing

  println(s"maybeFive: $maybeFive")
  println(s"maybeTen (via pure): $maybeTen")
  println(s"noValue: $noValue")

  // map の使用例
  val maybeSix = M.map(maybeFive)(_ + 1) // Just(5) -> Just(6)
  val mapNothing = M.map(noValue)(_ + 1)  // Nothing -> Nothing
  println(s"M.map(maybeFive)(_ + 1): $maybeSix")
  println(s"M.map(noValue)(_ + 1): $mapNothing")

  // flatMap の使用例 (安全な除算)
  def safeDivide(numerator: Int, denominator: Int): Maybe[Double] = {
    if (denominator == 0) Nothing
    else Just(numerator.toDouble / denominator)
  }

  // Just(10) を Just(5) で割る
  val divideTenByFive: Maybe[Double] = M.flatMap(maybeTen) { ten => // ten = 10
    M.flatMap(maybeFive) { five => // five = 5
      safeDivide(ten, five)       // safeDivide(10, 5) -> Just(2.0)
    }
  }
  println(s"Divide 10 by 5 using flatMap: $divideTenByFive") // Just(2.0)

  // Just(10) を 0 (Nothing 経由) で割る
  val divideTenByNothing: Maybe[Double] = M.flatMap(maybeTen) { ten =>
    M.flatMap(noValue) { zero => // noValue は Nothing なので、この flatMap は Nothing を返す
      safeDivide(ten, zero)      // この部分は実行されない
    }
  }
  println(s"Divide 10 by Nothing using flatMap: $divideTenByNothing") // Nothing

  // Just(10) を 0 で割る (safeDivide が Nothing を返すケース)
  val divideTenByZero: Maybe[Double] = M.flatMap(maybeTen) { ten =>
    safeDivide(ten, 0) // safeDivide(10, 0) -> Nothing
  }
  println(s"Divide 10 by 0 using flatMap: $divideTenByZero") // Nothing


  println("\n--- About for-comprehension ---")
  println("To use for-comprehensions like `for { x <- maybeX ... }`,")
  println("the `Maybe` type itself needs `flatMap`, `map`, and `withFilter` methods,")
  println("either directly defined or added via implicit conversions.")
  println("Our current `Maybe` type doesn't have them directly.")
  println("You could define an implicit class like this:")

  // for式を使うための implicit class の例 (コメントアウト解除して試せます)
  
  implicit class MaybeOps[+A](ma: Maybe[A]) {
    // MaybeMonad の実装を利用する
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = MaybeMonad.flatMap(ma)(f)
    def map[B](f: A => B): Maybe[B] = MaybeMonad.map(ma)(f)
    def withFilter(p: A => Boolean): Maybe[A] = ma match {
      case Just(a) if p(a) => ma
      case _ => Nothing
    }
  }

  println("\n--- Using for-comprehension with implicit class ---")

  val maybeX: Maybe[Int] = Just(10)
  val maybeY: Maybe[Int] = Just(5)
  val maybeZ: Maybe[Int] = Nothing

  val sumXY: Maybe[Int] = for {
    x <- maybeX
    y <- maybeY
    if x > 0 // フィルター条件
  } yield x + y
  println(s"for { x <- Just(10); y <- Just(5); if x > 0 } yield x + y: $sumXY") // Just(15)

  val sumXZ: Maybe[Int] = for {
    x <- maybeX
    z <- maybeZ // ここで Nothing が入る
    y <- maybeY // 実行されない
  } yield x + y + z // 実行されない
  println(s"for { x <- Just(10); z <- Nothing; ... } yield ...: $sumXZ") // Nothing

}