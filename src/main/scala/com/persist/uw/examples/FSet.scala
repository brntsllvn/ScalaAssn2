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

  private case class NonEmptyFSet(existingSet: FSet) extends FSet

  private case object EmptyFSet extends FSet

  def apply(): FSet = EmptyFSet
}

sealed trait FSet {

  import FSet._

//  def size: Int = {
////    @tailrec
//    def size1(nonEmptySet: NonEmptyFSet, accum: Int = 0): Int = {
//      mySet match {
//        case NonEmptyFSet(nonEmptySet.existingSet) => size1(NonEmptyFSet(nonEmptySet.existingSet.tail), accum + 1)
//        case EmptyFSet => accum
//      }
//    }
//    size1(this)
//  }

  def add(newInt: Int): FSet = NonEmptyFSet(newInt :: this)

//  def insert(i: Int): FQueue = NonEmptyFQueue(i, this)

  def contains(i: Int): Boolean = ???

  def delete(i: Int): FSet = ???

  def union(set1: FSet): FSet = ???

  def intersect(set1: FSet): FSet = ???

  def subset(set1: FSet): Boolean = ???

  def equals(set1: FSet): Boolean = ???
}
