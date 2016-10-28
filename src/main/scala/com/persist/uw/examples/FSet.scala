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

  case class NonEmptyFSet(leadInt: Int, existingSet: FSet) extends FSet

  private case object EmptyFSet extends FSet

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

  def add(newInt: Int): FSet = NonEmptyFSet(newInt, this)

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
    def delete1(myFSet: FSet): FSet = {
      myFSet match {
        case NonEmptyFSet(leadInt, next) =>
//          if(intToDelete == leadInt) NonEmptyFSet(next.leadInt, next)
//          else delete1(next)
        case EmptyFSet => EmptyFSet
      }
    }
    delete1(this)
  }

  def union(set1: FSet): FSet = ???

  def intersect(set1: FSet): FSet = ???

  def subset(set1: FSet): Boolean = ???

  def equals(set1: FSet): Boolean = ???
}
