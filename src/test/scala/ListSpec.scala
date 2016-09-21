import org.scalatest.{FlatSpec, Matchers}
import fpinscala.datastructures.List

class ListSpec extends FlatSpec with Matchers {

  "List" should "tail correctly" in {
    List.tail(List(1,2,3,4)) should be (List(2,3,4))
  }

  it should "replace head correctly" in {
    List.setHead(4, List(1,2,3)) should be (List(4,2,3))
  }
}
