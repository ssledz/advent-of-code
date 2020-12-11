package aof

import aof.Day10._
import org.scalatest.funsuite.AnyFunSuite

class TestDay10 extends AnyFunSuite {

  test("test arrangements") {
    assert(arrangements(1 :: Nil) === 1)
    assert(arrangements(1 :: 2 :: Nil) === 2)
    assert(arrangements(1 :: 2 :: 3 :: Nil) === 4)
    assert(arrangements(1 :: 2 :: 3 :: 4 :: Nil) === 7)
    assert(arrangements(2 :: 3 :: Nil) === 2)
    assert(arrangements(List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19)) === 8)
    assert(arrangements(List(1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49)) === 19208)
  }

  test("test arrangements2") {
    assert(arrangements2(1 :: Nil) === 1)
    assert(arrangements2(1 :: 2 :: Nil) === 2)
    assert(arrangements2(1 :: 2 :: 3 :: Nil) === 4)
    assert(arrangements2(1 :: 2 :: 3 :: 4 :: Nil) === 7)
    assert(arrangements2(2 :: 3 :: Nil) === 2)
    assert(arrangements(List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19)) === 8)
    assert(arrangements(List(1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49)) === 19208)
  }

  test("test arrangements3") {
    assert(arrangements3(1 :: Nil) === 1)
    assert(arrangements3(1 :: 2 :: Nil) === 2)
    assert(arrangements3(1 :: 2 :: 3 :: Nil) === 4)
    assert(arrangements3(1 :: 2 :: 3 :: 4 :: Nil) === 7)
    assert(arrangements3(List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19)) === 8)
    assert(arrangements3(List(1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49)) === 19208)
  }
}
