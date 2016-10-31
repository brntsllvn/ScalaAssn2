package com.persist.uw.examples

import com.persist.uw.examples.FSet.EmptyFSet
import org.specs2._

class TestFSet extends mutable.Specification {

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
    (s.size mustEqual 2) and
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

  "delete second element" >> {
    val s = FSet().add(1).add(2)
    val sot = s.delete(1)
    (sot.size mustEqual 1) and
      (sot.contains(1) mustEqual false) and
      (sot.contains(2) mustEqual true)
  }

  "delete" >> {
    val s = FSet().add(1).add(2).delete(1)
    val s0 = s.delete(2)
    (s.size mustEqual 1) and
      (s.contains(1) mustEqual false) and
      (s.contains(2) mustEqual true) and
      (s0.size mustEqual 0)
  }

  "delete element buried inside" >> {
    val s = FSet().add(1).add(2).add(42).add(12)
    val sot = s.delete(42)
    (sot.size mustEqual 3) and
      (sot.contains(42) mustEqual false) and
      (sot.contains(1) mustEqual true) and
      (sot.contains(2) mustEqual true) and
      (sot.contains(12) mustEqual true)
  }

  "single element in sets" >> {
    val s1 = FSet().add(1)
    val s2 = FSet().add(1)
    val doTheyEqual = s1.equals(s2)
    doTheyEqual mustEqual true
  }

  "single element in sets not equal" >> {
    val s1 = FSet().add(1)
    val s2 = FSet().add(2)
    val doTheyEqual = s1.equals(s2)
    doTheyEqual mustEqual false
  }

  "unequal set sizes do not equal" >> {
    val s1 = FSet().add(1)
    val s2 = FSet().add(2).add(3).add(4)
    val doTheyEqual = s1.equals(s2)
    doTheyEqual mustEqual false
  }

  "equals" >> {
    val s1 = FSet().add(1).add(2)
    val s2 = FSet().add(2).add(1).add(2)
    val s3 = FSet().add(1).add(2).add(3)
    val s1Eqs2 = s1.equals(s2)
    (s1Eqs2 mustEqual true) and
      (s1.equals(s3) mustEqual false)
  }

  "equals with several elements" >> {
    val s1 = FSet().add(1).add(2).add(3)
    val s2 = FSet().add(2).add(1).add(3)
    s1.equals(s2) mustEqual true
  }

  "unequal with several elements" >> {
    val s1 = FSet().add(3).add(2).add(1)
    val s2 = FSet().add(4).add(2).add(1)
    val eql = s1.equals(s2)
    eql mustEqual false
  }

  "set is subset of itself" >> {
    val s1 = FSet().add(1)
    val isSubset = s1.subset(s1)
    isSubset mustEqual true
  }

  "set is not subset" >> {
    val s1 = FSet().add(1)
    val s2 = FSet().add(2)
    s1.subset(s2) mustEqual false
  }

  "empty set is subset of every set" >> {
    val s1 = FSet().add(1)
    val isSubset = EmptyFSet.subset(s1)
    isSubset mustEqual true
  }

  "subset test3" >> {
    val s1 = FSet().add(1)
    val s2 = FSet().add(1)
    val isSubset = s1.subset(s2)
    isSubset mustEqual true
  }

  "subset" >> {
    val s1 = FSet().add(1).add(2)
    val s2 = s1.add(3)
    (s1.subset(s1) mustEqual true) and
      (s1.subset(s2) mustEqual true) and
      (s2.subset(s1) mustEqual false)
  }

  "intersect with same" >> {
    val s1 = FSet().add(1)
    val interSet = s1.intersect(s1)
    interSet.equals(s1) mustEqual true
  }

  "intersect with another nonempty" >> {
    val s1 = FSet().add(1).add(2).add(3)
    val s2 = FSet().add(2).add(3).add(4)
    val s3 = FSet().add(2).add(3)
    val intersection = s1.intersect(s2)
    intersection.equals(s3) mustEqual true
  }

  "intersect" >> {
    val s1 = FSet().add(1).add(2).add(3)
    val s2 = FSet().add(4).add(3).add(2)
    val s = FSet().add(2).add(3)
    s1.intersect(s2).equals(s) mustEqual true
  }

  "union with empty set" >> {
    val s1 = FSet().add(1)
    val union = s1.union(EmptyFSet)
    union.equals(s1) mustEqual true
  }

  "union with same set" >> {
    val s1 = FSet().add(1)
    val union = s1.union(s1)
    union.equals(s1) mustEqual true
  }

  "union with subset" >> {
    val s1 = FSet().add(1)
    val s2 = s1.add(2)
    val union = s2.union(s1)
    union.equals(s2) mustEqual true
  }

  "union with superset" >> {
    val s1 = FSet().add(1).add(2)
    val s2 = s1.delete(2)
    val union = s2.union(s1)
    union.equals(s1) mustEqual true
  }

  "union" >> {
    val s1 = FSet().add(1).add(2).add(3)
    val s2 = FSet().add(4).add(3).add(2)
    val s = FSet().add(1).add(2).add(3).add(4)
    s1.union(s2).equals(s) mustEqual true
  }
}
