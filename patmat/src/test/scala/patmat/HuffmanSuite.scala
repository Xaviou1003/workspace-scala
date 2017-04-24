package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("times of a list of char") {
   val test = List('a','b','a')
    assert(times(test) === List(('a',2),('b', 1)))

  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton true") {
    assert(singleton(List(Leaf('e',1))) === true)
  }

  test("singleton false") {
    assert(singleton(List(Leaf('e',1), Leaf('e',2))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7))
  }

  test("decode secret") {
      assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("encode secret") {
    assert(encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')) === List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))
  }

  test("encode test") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }

  test("decode test") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === List('a','b'))
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert test") {
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)), ('b',List(1))))
    }
  }

  /*test("convert test2") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0)), ('b',List(1))))
    }
  }*/


  test("createCodeTree test") {
    assert(createCodeTree(List('a', 'b','b','b')) === Fork(Leaf('a',1), Leaf('b',3), List('a','b'), 4))
  }

  test("makeOrderedLeafList  test") {
    assert(makeOrderedLeafList (Nil) === List())
  }

  test("quickEncode  test") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0,1))
    }
  }

}
