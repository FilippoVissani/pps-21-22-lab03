package u03
import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.Option.*

class ListTest:

  import List.*
  import u02.Modules.Person.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val lst: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))
    assertEquals(map(l)(_ + 1), map2(l)(_ + 1))
    assertEquals(map(l)(_ + ""), map2(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
    assertEquals(filter(l)(_ >= 20), filter2(l)(_ >= 20))
    assertEquals(filter(l)(_ != 20), filter2(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons (30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  @Test def testAppend(): Unit =
    val tail = Cons(40 , Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))


  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)( v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  @Test def testGetCourses(): Unit =
    assertEquals(Cons("c1", Cons("c2", Nil())), getCourses(Cons(Teacher("t1", "c1"), Cons(Student("s1", 2), Cons(Teacher("t2", "c2"), Nil())))))

  @Test def testFold(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    // assertEquals(-8, foldRight(lst)(0)(_ - _))
