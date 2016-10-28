package com.persist.uw.examples

import org.specs2._

class TestFSet extends mutable.Specification {

  //  empty FSet
  "init" >> {
    val s = FSet()
    s.size mustEqual 0
  }

  "insert one int" >> {
    val s = FSet().add(1)
    s.size mustEqual 1
  }

  "insert three ints" >> {
    val s = FSet().add(1).add(2).add(3)
    s.size mustEqual 3
  }

  "contains nothing" >> {
    val doesMySetContainThisNumber = FSet().contains(42)
    doesMySetContainThisNumber mustEqual false
  }

  "contains the lead int" >> {
    val doesMySetContainThisNumber = FSet().add(1).contains(1)
    doesMySetContainThisNumber mustEqual true
  }

  "contains not the lead int" >> {
    val doesMySetContainThisNumber = FSet().add(1).add(2).contains(1)
    doesMySetContainThisNumber mustEqual true
  }

  "insert" >> {
    val s = FSet().add(1).add(2).add(1)
    (s.size mustEqual 3) and
      (s.contains(1) mustEqual true) and
      (s.contains(2) mustEqual true) and
      (s.contains(0) mustEqual false)
  }

  "delete from empty set yields empty set" >> {
    val mySet = FSet().delete(42)
    mySet.size mustEqual 0
  }

  "delete lead int" >> {
    val mySet = FSet().add(1).delete(1)
    (mySet.size mustEqual 0) and
      (mySet.contains(1) mustEqual false)
  }
//
//  "delete" >> {
//    val s = FSet().add(1).add(2).delete(1)
//    val s0 = s.delete(2)
//    (s.size mustEqual 1) and
//      (s.contains(1) mustEqual false) and
//      (s.contains(2) mustEqual true) and
//      (s0.size mustEqual 0)
//  }
//
//  "equals" >> {
//    val s1 = FSet().add(1).add(2)
//    val s2 = FSet().add(2).add(1).add(2)
//    val s3 = FSet().add(1).add(2).add(3)
//    (s1.equals(s2) mustEqual true) and
//      (s1.equals(s3) mustEqual false)
//  }
//
//  "subset" >> {
//    val s1 = FSet().add(1).add(2)
//    val s2 = s1.add(3)
//    (s1.subset(s1) mustEqual true) and
//      (s1.subset(s2) mustEqual true) and
//      (s2.subset(s1) mustEqual false)
//  }
//
//  "intersect" >> {
//    val s1 = FSet().add(1).add(2).add(3)
//    val s2 = FSet().add(4).add(3).add(2)
//    val s = FSet().add(2).add(3)
//    s1.intersect(s2).equals(s) mustEqual true
//  }
//
//  "union" >> {
//    val s1 = FSet().add(1).add(2).add(3)
//    val s2 = FSet().add(4).add(3).add(2)
//    val s = FSet().add(1).add(2).add(3).add(4)
//    s1.union(s2).equals(s) mustEqual true
//  }

}
