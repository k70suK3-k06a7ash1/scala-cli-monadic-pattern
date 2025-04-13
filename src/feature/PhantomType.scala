import scala.language.implicitConversions // 型等価性のための =:= を使うのに必要

// --- 1. 状態を表す幽霊型を定義 ---
// sealed trait にすることで、状態の種類をコンパイル時に限定できます。
sealed trait FileState

// final class または case object を使って具体的な状態を定義します。
// これらの型自体は実行時に特別な意味を持ちませんが、型パラメータとして使われます。
final class Opened extends FileState
final class Closed extends FileState

// --- 2. 幽霊型を持つクラスを定義 ---
// Fileクラスは型パラメータ `State` を持ちます。これが幽霊型です。
// コンストラクタを private にして、意図しない状態のインスタンス生成を防ぎます。
case class File[State <: FileState] private (name: String) {

  // --- 3. 状態に応じたメソッド制限 ---
  // implicit ev: State =:= Opened という型制約を使っています。
  // これは、このメソッドが呼び出されるインスタンスの State 型が Opened 型と
  // 等しい場合にのみコンパイルが通ることを意味します。
  // これにより、Opened 状態のファイルでのみ read/write/close が可能です。

  /** ファイルから読み込む（Opened状態でのみ可能） */
  def read(implicit ev: State =:= Opened): String = {
    s"Reading content from file '$name'..."
  }

  /** ファイルに書き込む（Opened状態でのみ可能） */
  def write(content: String)(implicit ev: State =:= Opened): Unit = {
    println(s"Writing '$content' to file '$name'...")
  }

  /** ファイルを閉じる（Opened状態でのみ可能） */
  def close(implicit ev: State =:= Opened): File[Closed] = {
    println(s"Closing file '$name'.")
    // 状態遷移: Opened -> Closed
    // 新しい状態に対応するインスタンスを生成して返します。
    new File[Closed](name)
  }
}

// --- 4. 状態遷移を行うメソッド（コンパニオンオブジェクト） ---
// Fileクラスのコンパニオンオブジェクトにファクトリメソッドや状態遷移メソッドを定義します。
object File {

  /** 新しいファイルを作成する（初期状態は Closed） */
  def create(name: String): File[Closed] = {
    println(s"Creating new (closed) file '$name'.")
    new File[Closed](name) // Closed状態でインスタンス化
  }

  /** ファイルを開く（Closed状態 -> Opened状態） */
  def open(file: File[Closed]): File[Opened] = {
    println(s"Opening file '${file.name}'.")
    // 状態遷移: Closed -> Opened
    // 新しい状態に対応するインスタンスを生成して返します。
    new File[Opened](file.name)
  }
}

// --- 5. 具体的な使用例 ---
object PhantomTypeExample extends App {

  // 1. ファイルを作成 (初期状態: Closed)
  val file1: File[Closed] = File.create("document.txt")

  // 以下の操作はコンパイルエラーになります。
  // Closed 状態のファイルに対して read/write/close はできません。
  // file1.read()
  // file1.write("test")
  // file1.close()

  println("-" * 20)

  // 2. ファイルを開く (Closed -> Opened)
  val openedFile: File[Opened] = File.open(file1)

  // 以下の操作はコンパイルエラーになります。
  // Opened 状態のファイルに対して open はできません。
  // File.open(openedFile) // File.open は File[Closed] を期待するためエラー

  println("-" * 20)

  // 3. 開いたファイルに対して操作 (Opened 状態)
  val content = openedFile.read()
  println(content)
  openedFile.write("Hello, Phantom Types!")

  println("-" * 20)

  // 4. ファイルを閉じる (Opened -> Closed)
  val closedFile: File[Closed] = openedFile.close()

  println("-" * 20)

  // 5. 閉じたファイルに対して操作
  // 以下の操作はコンパイルエラーになります。
  // 再び Closed 状態になったため read/write/close はできません。
  // closedFile.read()
  // closedFile.write("more data")
  // closedFile.close()
  // File.open(openedFile) // openedFile は close 後に使えない (値は変わらないが型で防ぐ)

  println("Successfully demonstrated phantom types for file state management.")
  println("Compile-time errors prevent invalid operations.")
}