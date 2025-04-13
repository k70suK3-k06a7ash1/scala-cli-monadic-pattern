import scala.util.control.NonFatal

/**
 * AutoCloseableなリソースに対してLoan Patternを適用する関数
 *
 * @param acquire リソースを取得する処理（名前渡しパラメータ）
 * @param use     取得したリソースを使用する関数
 * @tparam Resource AutoCloseableを実装したリソースの型
 * @tparam Result  リソース使用関数の戻り値の型
 * @return リソース使用関数の結果
 */
def loan[Resource <: AutoCloseable, Result](acquire: => Resource)(use: Resource => Result): Result = {
  var resource: Option[Resource] = None
  try {
    // リソースを取得
    resource = Some(acquire)
    // リソースを使用する関数を実行
    use(resource.get) // acquireが成功した前提（失敗時は例外が飛ぶ）
  } finally {
    // リソースが取得されていれば、必ずクローズする
    resource.foreach { res =>
      try {
        res.close()
      } catch {
        // close時の例外は握りつぶさず、ログ出力などが推奨される場合もある
        case NonFatal(e) => println(s"Warning: Failed to close resource: ${e.getMessage}")
      }
    }
  }
}

// --- 使用例 ---
import java.io._

object LoanPatternSelfImplExample extends App {

  val filePath = "loan_pattern_example.txt"
  val file = new File(filePath)

  println("--- Writing to file using loan pattern ---")
  try {
    // ファイル書き込みの例
    val resultWrite: Unit = loan(new PrintWriter(new FileWriter(file))) { writer =>
      println("PrintWriter acquired.")
      writer.println("Hello from Loan Pattern!")
      writer.println("This ensures the writer is closed.")
      // writer.close() // <- ここで明示的に close する必要はない
      println("Finished writing.")
      // このブロックを抜けると finally で writer.close() が呼ばれる
    }
    println("Write operation completed (implicitly closed).")
  } catch {
    case e: IOException => println(s"Error during write: ${e.getMessage}")
  }

  println("\n--- Reading from file using loan pattern ---")
  try {
    // ファイル読み込みの例 (BufferedReader は AutoCloseable)
    val firstLine: Option[String] = loan(new BufferedReader(new FileReader(file))) { reader =>
      println("BufferedReader acquired.")
      val line = Option(reader.readLine()) // 読み込み実行
      println(s"Read line: $line")
      // このブロックを抜けると finally で reader.close() が呼ばれる
      line
    }
    println(s"Read operation completed (implicitly closed). First line: ${firstLine.getOrElse("Not found or empty")}")
  } catch {
    case e: IOException => println(s"Error during read: ${e.getMessage}")
  } finally {
    // クリーンアップ
    if (file.exists()) {
      file.delete()
      println(s"\nFile '$filePath' deleted.")
    }
  }
}