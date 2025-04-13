package shared // 共通定義用のパッケージ

// --- 1. Monad トレイトの定義 ---
trait Monad[M[_]] {
  def pure[A](value: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
}

// --- 2. Maybe データ型の定義 ---
sealed trait Maybe[+A]
case class Just[+A](value: A) extends Maybe[A]
case object Nothing extends Maybe[Nothing]

// --- 3. Maybe 型に for式のためのメソッドを追加する implicit class ---
// これも共通定義として利用可能にする
implicit class MaybeOps[+A](ma: Maybe[A]) {
  /** flatMap を Maybe 自身に実装 */
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = ma match {
    case Just(a) => f(a)
    case Nothing => Nothing
  }
  /** map を Maybe 自身に実装 */
  def map[B](f: A => B): Maybe[B] = ma match {
    case Just(a) => Just(f(a))
    case Nothing => Nothing
  }
  /** withFilter を Maybe 自身に実装 (for式の if 節のため) */
  def withFilter(p: A => Boolean): Maybe[A] = ma match {
    case Just(a) if p(a) => ma // 条件を満たせばそのまま
    case _ => Nothing        // 条件を満たさないか、元が Nothing なら Nothing
  }
}



// --- Maybe 用の Monad インスタンスの実装 ---
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

// --- State データ型の定義 ---
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

// --- State 用の Monad インスタンスとヘルパー関数 ---
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