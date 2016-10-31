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
    def delete1(mySet: FSet, accum: FSet = EmptyFSet): FSet = {
      mySet match {
        case EmptyFSet => accum
        case NonEmptyFSet(leadInt, next) =>
          if(intToDelete == leadInt) delete1(next, accum)
          else delete1(next, accum.add(leadInt))
      }
    }
    delete1(this)
  }

  def equals(comparisonSet: FSet): Boolean = {
    this.subset(comparisonSet) && comparisonSet.subset(this)
  }

  def subset(comparisonSet: FSet): Boolean = {
    @tailrec
    def isSubsetOf(thisSet: FSet): Boolean = {
      val setsTogether = (thisSet, comparisonSet)
      setsTogether match {
        case (EmptyFSet,NonEmptyFSet(someInt, next)) => true
        case (NonEmptyFSet(thisSetLeadInt,next), NonEmptyFSet(_,_)) =>
          if(!comparisonSet.contains(thisSetLeadInt)) false
          else isSubsetOf(next)
      }
    }
    isSubsetOf(this)
  }

  def intersect(anotherSet: FSet): FSet = {
    @tailrec
    def intersectWith(anotherSet: FSet, accum: FSet): FSet = {
      anotherSet match {
        case EmptyFSet => accum
        case NonEmptyFSet(leadInt, next) =>
          if (this.contains(leadInt)) intersectWith(next, accum.add(leadInt))
          else intersectWith(next, accum)
      }
    }
    intersectWith(anotherSet, EmptyFSet)
  }

  def union(anotherSet: FSet): FSet = {
    @tailrec
      def anotherSetUnionWith(anotherSet: FSet, accum: FSet): FSet = {
        anotherSet match {
          case EmptyFSet => accum
          case NonEmptyFSet(leadInt, next) => anotherSetUnionWith(next, accum.add(leadInt))
        }
      }
    anotherSetUnionWith(anotherSet, this)
  }
}
