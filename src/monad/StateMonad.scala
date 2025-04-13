import shared.*

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
