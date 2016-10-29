package com.persist.uw.examples

import com.persist.uw.examples.FQueue.NonEmptyFQueue

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

// make it similar to FQueue
// tests must pass
// use only immutable data
// if using recursion make sure its tail recursive
// use case classes and objects, not Scala collection types
// correctness not performance is the goal

object FSet {

  case class NonEmptyFSet(val leadInt: Int, val existingSet: FSet) extends FSet

  case object EmptyFSet extends FSet

  def apply(): FSet = EmptyFSet
}

sealed trait FSet {

  import FSet._

  def size: Int = {
    @tailrec
    def size1(myFSet: FSet, accum: Int = 0): Int = {
      myFSet match {
        case NonEmptyFSet(i, next) => size1(next, accum + 1)
        case EmptyFSet => accum
      }
    }
    size1(this)
  }

  def add(newInt: Int): FSet = {
    def add1(myFSet: FSet): FSet = {
      myFSet match {
        case EmptyFSet => NonEmptyFSet(newInt, EmptyFSet)
        case NonEmptyFSet(someInt, _) =>
          if(this.contains(newInt)) this
          else NonEmptyFSet(newInt, this)
      }
    }
    add1(this)
  }

  def contains(searchTerm: Int): Boolean = {
    @tailrec
    def contains1(myFSet: FSet): Boolean = {
      myFSet match {
        case NonEmptyFSet(leadInt, next) => searchTerm == leadInt || contains1(next)
        case EmptyFSet => false
      }
    }
    contains1(this)
  }

  def delete(intToDelete: Int): FSet = {
    @tailrec
    def delete1(myFSet: FSet, accum: FSet = EmptyFSet): FSet = {
      myFSet match {
        case EmptyFSet => accum
        case NonEmptyFSet(leadInt, EmptyFSet) =>
          if(intToDelete == leadInt) delete1(EmptyFSet, accum)
          else delete1(EmptyFSet, accum.add(leadInt))
        case NonEmptyFSet(leadInt, NonEmptyFSet(someInt, anotherFSet)) =>
          if(intToDelete == leadInt) delete1(NonEmptyFSet(someInt, anotherFSet), accum)
          else delete1(NonEmptyFSet(someInt, anotherFSet), accum.add(leadInt))
      }
    }
    delete1(this)
  }

  def union(set1: FSet): FSet = ???

  def intersect(set1: FSet): FSet = ???

  def subset(set1: FSet): Boolean = ???

  def equals(set1: FSet): Boolean = ???
}
