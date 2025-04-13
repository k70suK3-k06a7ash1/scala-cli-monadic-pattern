import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YourSpec extends AnyFlatSpec with Matchers {
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = scala.collection.mutable.Stack[Int]()
    stack.push(1)
    stack.push(2)
    stack.pop() shouldBe 2
    stack.pop() shouldBe 1
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = scala.collection.mutable.Stack[String]()
    a [NoSuchElementException] should be thrownBy emptyStack.pop()
  }
}