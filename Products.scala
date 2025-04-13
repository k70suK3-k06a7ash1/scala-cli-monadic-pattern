import scala.util.{Try, Success, Failure}

// --- 1. 不変なデータ構造 (Immutability) ---
// case class はトップレベルに定義できます
case class Product(id: Int, name: String, price: Double, stock: Int)

// --- 2. 純粋関数 (Pure Functions) ---
// 関数定義もトップレベルに定義できます
/**
 * 商品に割引率を適用する純粋関数。
 */
def applyDiscount(product: Product, discountRate: Double): Product = {
  product.copy(price = product.price * (1.0 - discountRate))
}

/**
 * 商品が購入可能か (在庫があるか) を判定する純粋関数。
 */
def isInStock(product: Product): Boolean = product.stock > 0

// --- 3. 代数的データ型 (ADTs) - Option と Either ---
// エラーを表現するための型 (Either の Left で使用)
case class ProcessingError(message: String) // トップレベルに定義できます

/**
 * 在庫がある場合のみ、商品の価格を Option 型で返す関数。
 */
def getPriceIfInStock(product: Product): Option[Double] = {
  if (isInStock(product)) Some(product.price) else None
}

/**
 * 特定の条件に基づいて処理を行い、成功/失敗を Either 型で返す関数。
 */
def processHighValueItem(product: Product): Either[ProcessingError, String] = {
  if (product.price > 500) {
    Try {
      if (product.price > 1000) throw new RuntimeException("Price too high for special processing")
      s"Successfully processed high-value item: ${product.name}"
    } match {
      case Success(message) => Right(message)
      case Failure(exception) => Left(ProcessingError(s"Failed processing ${product.name}: ${exception.getMessage}"))
    }
  } else {
    Left(ProcessingError(s"${product.name} is not a high-value item."))
  }
}

// --- 実行コードのエントリーポイント ---
// 実際にコードを実行する部分は @main アノテーションを付けたメソッド内に記述します
@main def runProductExample(): Unit = {

  // --- サンプルデータ ---
  // val による値の定義は @main の中に移動します
  val products = List(
    Product(1, "Laptop", 1200.0, 5),
    Product(2, "Keyboard", 75.0, 10),
    Product(3, "Mouse", 25.0, 0),
    Product(4, "Monitor", 600.0, 3),
    Product(5, "Webcam", 150.0, 0)
  )

  // println も @main の中に移動します
  println(s"Original products: $products")

  // --- 4. 高階関数 (Higher-Order Functions) の利用 ---
  val discountRate = 0.1
  val discountedEligibleProducts: List[Product] = products
    .filter(p => isInStock(p) && p.price >= 100.0)
    .map(p => applyDiscount(p, discountRate))

  println(s"\nDiscounted products (in stock, price >= 100): $discountedEligibleProducts")

  val availablePrices: List[Double] = products.flatMap(getPriceIfInStock)
  println(s"\nAvailable prices (from in-stock products): $availablePrices")

  val totalStock: Int = products.foldLeft(0)((currentTotal, product) => currentTotal + product.stock)
  println(s"\nTotal stock of all products: $totalStock")

  val processingResults: List[Either[ProcessingError, String]] = products.map(processHighValueItem)
  println("\nProcessing results for high-value items:")
  processingResults.foreach {
    case Right(successMessage) => println(s"  Success: $successMessage")
    case Left(error) => println(s"  Failure: ${error.message}")
  }

  val successfulMessages: List[String] = processingResults.collect { case Right(msg) => msg }
  println(s"Successful processing messages: $successfulMessages")

  // --- 5. 関数合成 (Function Composition) ---
  // 関数リテラル (val 変数 = ...) も @main の中に移動します
  val checkStock: Product => Option[Product] = p => if (isInStock(p)) Some(p) else None
  val apply10PercentDiscount: Product => Product = p => applyDiscount(p, 0.1)
  val applyDiscountIfInStock: Product => Option[Product] =
    checkStock.andThen(optProduct => optProduct.map(apply10PercentDiscount))

  val discountedProductsOption: List[Option[Product]] = products.map(applyDiscountIfInStock)
  println(s"\nApplying discount only if in stock (using composed function): $discountedProductsOption")

  val finalDiscountedProducts: List[Product] = products.flatMap(applyDiscountIfInStock)
  println(s"Final discounted products (flatMapped): $finalDiscountedProducts")

  // --- 6. 参照透過性 (Referential Transparency) ---
  val p1 = Product(10, "TestItem", 200.0, 1)
  val discountedP1_call1 = applyDiscount(p1, 0.1)
  val discountedP1_call2 = applyDiscount(p1, 0.1)
  val expectedValue = p1.copy(price = 180.0)

  // assert も @main の中に移動します
  assert(discountedP1_call1 == discountedP1_call2) // 同じ入力には同じ出力
  assert(discountedP1_call1 == expectedValue)      // 関数呼び出しを結果の値で置き換え可能

  println("\nAssertion for Referential Transparency passed.")
}