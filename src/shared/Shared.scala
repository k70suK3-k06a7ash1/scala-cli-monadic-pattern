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