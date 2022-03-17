package u03

import org.junit.Assert.assertEquals
import org.junit.Test

class StreamTest:

  import u03.Streams.Stream.*

  @Test def testDrop: Unit =
    val s = take(iterate(0)(_ + 1))(10)
    assertEquals(toList(cons(6, cons(7, cons(8, cons(9, empty()))))), toList(drop(s)(6)))

  @Test def testConstant: Unit =
    val value = "x"
    assertEquals(toList(cons(value, cons(value, cons(value, cons(value, cons(value, empty())))))), toList(take(constant(value))(5)))

  @Test def testFibonacci: Unit =
    def mapper(f: Int): Int = f match
      case x if x == 0 || x == 1 => x
      case _ => mapper(f - 1) + mapper(f - 2)

    val fibs = map(iterate(0)(_ + 1))(mapper)
    assertEquals(toList(cons(0, cons(1, cons(1, cons(2, cons(3, cons(5, cons(8, cons(13, empty()))))))))), toList(take(fibs)(8)))

