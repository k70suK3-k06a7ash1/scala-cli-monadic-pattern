import shared.*
import scala.util.Try // Stateの例では直接使わないが、他の場所で使われる可能性

// --- 1. State データ型の定義 ---
case class State[S, +A](runState: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { initialState =>
    val (finalState, resultA) = runState(initialState)
    (finalState, f(resultA))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { initialState =>
    val (intermediateState, resultA) = runState(initialState)
    val nextStateComputation = f(resultA)
    nextStateComputation.runState(intermediateState)
  }
}

// --- 2. State 用の Monad インスタンスとヘルパー関数 ---
object StateMonad {
  def stateMonad[S]: Monad[({ type lambda[+A] = State[S, A] })#lambda] =
    new Monad[({ type lambda[+A] = State[S, A] })#lambda] {
      override def pure[A](value: A): State[S, A] = State(s => (s, value))
      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
        ma.flatMap(f)
    }

  def pure[S, A](value: A): State[S, A] = State(s => (s, value))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](newState: S): State[S, Unit] = State(_ => (newState, ()))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
}

// --- 3. State Monad の使用例 ---
@main def runStateMonad(): Unit = {

  import StateMonad._ // ヘルパー関数をインポート

  println("--- State Monad Example: Counter ---")
  type CounterState[+A] = State[Int, A]
  val counterProgram: CounterState[String] = for {
    _ <- modify[Int](_ + 10)
    initialValue <- get[Int]
    _ <- set[Int](initialValue * 2)
    _ <- modify[Int](_ - 3)
    finalValue <- get[Int]
    message <- inspect[Int, String](s => s"Final counter value is: $s")
    _ <- pure[Int, Unit](())
  } yield message
  val initialCounter = 0
  val (finalCounterState, counterResult) = counterProgram.runState(initialCounter)
  println(s"Initial Counter State: $initialCounter")
  println(s"Counter Program Result: $counterResult")
  println(s"Final Counter State: $finalCounterState")

  println("\n--- State Monad Example: Stack ---")
  type Stack = List[Int]
  type StackState[+A] = State[Stack, A]
  def push(value: Int): StackState[Unit] = modify[Stack](stack => value :: stack)
  def safePop: StackState[Maybe[Int]] = State { stack =>
    stack match {
      case head :: tail => (tail, Just(head))
      case Nil          => (Nil, Nothing)
    }
  }

  val stackProgram: StackState[Maybe[String]] = for {
    _ <- push(10)
    _ <- push(20)
    maybeVal1 <- safePop // Just(20)
    _ <- push(30)
    maybeVal2 <- safePop // Just(30)
    maybeVal3 <- safePop // Just(10)
    maybeVal4 <- safePop // Nothing

    finalMessage <- pure[Stack, Maybe[String]] {
      // implicit class MaybeOps により、Maybe に対する for 式が利用可能になる
      for {
        v1 <- maybeVal1 // Just(20) - implicit flatMap が呼ばれる
        v2 <- maybeVal2 // Just(30) - implicit flatMap
        if v1 > v2     // Just(20) は 30 より大きくないので、ここで Nothing になる (implicit withFilter)
        v3 <- maybeVal3 // この行以降は実行されない
        v4 <- maybeVal4
      } yield s"Popped values (some): v1=$v1, v2=$v2, v3=..., v4=..." // この yield は実行されない
    }
  } yield finalMessage

  val initialStack: Stack = Nil
  val (finalStackState, stackResult) = stackProgram.runState(initialStack)
  println(s"Initial Stack State: $initialStack")
  // maybeVal1(20) > maybeVal2(30) が false なので、for 式内の if で Nothing になる
  println(s"Stack Program Result: $stackResult") // 期待値: Nothing
  println(s"Final Stack State: $finalStackState")   // 期待値: List() (最後の safePop 後の状態)
}

// // --- (前提) Monad トレイト ---
// // 前の Maybe モナドの例で使用したものを再掲します。
// trait Monad[M[_]] {
//   def pure[A](value: A): M[A]
//   def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
//   def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
// }

// // --- 1. State データ型の定義 ---
// /**
//  * 状態 S を引き回しながら計算 A を行うモナド。
//  * @param runState 状態遷移関数 S => (S, A) が本体。
//  * @tparam S 状態の型
//  * @tparam A 計算結果の型 (+A は共変性)
//  */
// case class State[S, +A](runState: S => (S, A)) {

//   /**
//    * State モナド内の計算結果に関数を適用する (状態は変更しない)。
//    * for式で map が使われる。
//    */
//   def map[B](f: A => B): State[S, B] = State { initialState =>
//     val (finalState, resultA) = runState(initialState) // まず元の計算を実行
//     (finalState, f(resultA))                           // 状態はそのまま、結果AをBに変換
//   }

//   /**
//    * State モナドの計算を連鎖させる。
//    * for式で <- が使われる (flatMapに対応)。
//    */
//   def flatMap[B](f: A => State[S, B]): State[S, B] = State { initialState =>
//     // 1. 元の State 計算を実行して、中間状態と結果 A を得る
//     val (intermediateState, resultA) = runState(initialState)
//     // 2. 結果 A を使って次の State 計算 (State[S, B]) を生成する
//     val nextStateComputation = f(resultA)
//     // 3. 生成された次の State 計算を中間状態から実行する
//     nextStateComputation.runState(intermediateState)
//   }
// }

// // --- 2. State 用の Monad インスタンスとヘルパー関数 ---
// object StateMonad {

//   /**
//    * 特定の状態型 S に対する Monad[State[S, *]] インスタンスを生成します。
//    * Type Lambda ({ type L[+A] = State[S, A] })#L を使用して、
//    * State[S, A] を型引数 A のみに依存する型コンストラクタとして扱います。
//    */
//   def stateMonad[S]: Monad[({ type lambda[+A] = State[S, A] })#lambda] =
//     new Monad[({ type lambda[+A] = State[S, A] })#lambda] {
//       override def pure[A](value: A): State[S, A] = State(s => (s, value))

//       override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
//         ma.flatMap(f) // Stateクラスに実装されたflatMapを利用
//     }

//   // --- 状態操作のための便利なヘルパー関数 ---

//   /**
//    * 値を State モナドに持ち上げます (状態は変更しません)。
//    */
//   def pure[S, A](value: A): State[S, A] = State(s => (s, value))

//   /**
//    * 現在の状態 S を計算結果として取得します (状態は変更しません)。
//    */
//   def get[S]: State[S, S] = State(s => (s, s))

//   /**
//    * 状態を特定の値 newState に設定します (計算結果は Unit)。
//    */
//   def set[S](newState: S): State[S, Unit] = State(_ => (newState, ()))

//   /**
//    * 現在の状態に関数 f を適用して状態を変更します (計算結果は Unit)。
//    */
//   def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

//   /**
//    * 状態 S を受け取り、それを使って計算結果 A を生成する State を作成します (状態は変更しません)。
//    * get と map を組み合わせたものに相当します。
//    */
//    def inspect[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
// }

// // --- (前提) Maybe 型 ---
// // スタックの例で使うため、前の例から Maybe 型を再掲します。
// sealed trait Maybe[+A]
// case class Just[+A](value: A) extends Maybe[A]
// case object Nothing extends Maybe[Nothing]

// // --- 3. State Monad の使用例 ---
// @main def runStateMonad(): Unit = {

//   // StateMonadオブジェクトからヘルパー関数をインポート
//   import StateMonad._

//   println("--- State Monad Example: Counter ---")

//   // 型エイリアスを定義すると便利
//   type CounterState[+A] = State[Int, A]

//   // カウンターを操作する一連の処理を for式 で記述
//   val counterProgram: CounterState[String] = for {
//     _ <- modify[Int](_ + 10)       // 状態を 10 増やす
//     initialValue <- get[Int]       // 現在の状態 (10) を取得
//     _ <- set[Int](initialValue * 2)  // 状態を 10 * 2 = 20 に設定
//     _ <- modify[Int](_ - 3)        // 状態を 20 - 3 = 17 にする
//     finalValue <- get[Int]         // 最終的な状態 (17) を取得
//     message <- inspect[Int, String](s => s"Final counter value is: $s") // 状態を使ってメッセージ生成
//     _ <- pure[Int, Unit](())       // 何もしない処理 (pure)
//   } yield message                  // 最終的な計算結果 (String)

//   // プログラムを実行するには、初期状態を与えて runState を呼び出す
//   val initialCounter = 0
//   val (finalCounterState, counterResult) = counterProgram.runState(initialCounter)

//   println(s"Initial Counter State: $initialCounter")
//   println(s"Counter Program Result: $counterResult") // "Final counter value is: 17"
//   println(s"Final Counter State: $finalCounterState")   // 17

//   println("\n--- State Monad Example: Stack ---")

//   // スタックの状態 (List[Int]) とそれに対する State の型エイリアス
//   type Stack = List[Int]
//   type StackState[+A] = State[Stack, A]

//   // スタック操作関数
//   def push(value: Int): StackState[Unit] = modify[Stack](stack => value :: stack)

//   // pop操作 (失敗する可能性があるので Maybe を返す)
//   def safePop: StackState[Maybe[Int]] = State { stack =>
//     stack match {
//       case head :: tail => (tail, Just(head)) // (新しい状態, 結果)
//       case Nil          => (Nil, Nothing)     // 空なら (状態変更なし, 結果Nothing)
//     }
//   }

//   // スタック操作を組み合わせるプログラム
//   val stackProgram: StackState[Maybe[String]] = for {
//     _ <- push(10)              // Stack: [10]
//     _ <- push(20)              // Stack: [20, 10]
//     maybeVal1 <- safePop       // Result: Just(20), Stack: [10]
//     _ <- push(30)              // Stack: [30, 10]
//     maybeVal2 <- safePop       // Result: Just(30), Stack: [10]
//     maybeVal3 <- safePop       // Result: Just(10), Stack: []
//     maybeVal4 <- safePop       // Result: Nothing,   Stack: []
//     // maybeVal1, maybeVal2, maybeVal3, maybeVal4 を使って最終結果を計算
//     finalMessage <- pure[Stack, Maybe[String]] { // pure で Maybe[String] を計算
//       // Maybe の Monadic な性質 (map/flatMap) を利用
//       for {
//         v1 <- maybeVal1 // Just(20)
//         v2 <- maybeVal2 // Just(30)
//         v3 <- maybeVal3 // Just(10)
//         // v4 <- maybeVal4 // ここで Nothing が入るので、以降は評価されず結果は Nothing
//       } yield s"Popped values (some): ${v1}, ${v2}, ${v3}" // この部分は実行されない
//     }
//   } yield finalMessage

//   // スタックプログラムの実行
//   val initialStack: Stack = Nil
//   val (finalStackState, stackResult) = stackProgram.runState(initialStack)

//   println(s"Initial Stack State: $initialStack")
//   println(s"Stack Program Result: $stackResult") // 期待値: Nothing
//   println(s"Final Stack State: $finalStackState")   // 期待値: List()
// }