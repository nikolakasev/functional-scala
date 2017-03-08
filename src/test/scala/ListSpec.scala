import org.scalatest.{FlatSpec, Matchers}
import fpinscala.datastructures.List

class ListSpec extends FlatSpec with Matchers {

  "List" should "tail correctly" in {
    List.tail(List(1,2,3,4)) should be (List(2,3,4))
  }

  it should "replace head correctly" in {
    List.setHead(1, List()) should be (List(1))
    List.setHead(4, List(1,2,3)) should be (List(4,2,3))
  }

  it should "drop elements correctly" in {
    List.drop(0, List(1,2,3)) should be (List(1,2,3))
    List.drop(1, List(1,2)) should be (List(2))
    List.drop(1, List()) should be (List())
    List.drop(0, List()) should be (List())
    List.drop(10, List(1,2,3)) should be (List())
  }

  it should "drop while elements are less than 5" in {
    List.dropWhile(List(1,2,3,4,5,6,4))(i => i < 5) should be (List(5,6,4))
  }

  it should "return but the last element" in {
    List.init(List(1,2,3,4)) should be (List(1,2,3))
  }
}
