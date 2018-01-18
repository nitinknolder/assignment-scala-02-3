package edu.knoldus

import org.apache.log4j.Logger

class ListMethods {

  def findLengthOfList[A] (newList: List[A]): Int = {
    newList.foldRight (0)((_, totalLength) => totalLength + 1)
  }

  def concatenationOfList[A] (firstList: List[A], secondList: List[A]): List[A] = {

    firstList ++ secondList
  }

  def findSubSequences[A] (newList: List[A], subList: List[A]): Boolean = {

    def subSequence[A] (newList: List[A], subList: List[A]): Boolean = subList match {
      case head :: tail if head == newList.head => subSequence (newList.tail, tail)
      case head :: tail if head != newList.head => false
      case _ => true
    }

    newList match {
      case head :: tail if head == subList.head => {
        subSequence (tail, subList.tail) match {
          case true => true
          case false => findSubSequences (tail, subList)
        }
      }
      case head :: tail if head != subList.head => findSubSequences (tail, subList)
      case head :: Nil => true
      case _ => false
    }
  }

/*  def splitList[A] (originalList: List[A], f: A => Boolean): (List[A], List[A]) = {

    def listPartition[A] (originalList: List[A], firstPartition: List[A], secondPartition: List[A]): List[A] = {

      originalList match {

        case firstnum :: restNum if f (firstnum) => listPartition (restNum, firstnum :: firstPartition, secondPartition)
        case firstnum :: restNum if f (firstnum) => listPartition (restNum, firstPartition, firstnum :: secondPartition)
        case Nil => firstPartition
      }
    }

  }*/
}


object listMethod1 {

  def main (args: Array[String]) {

    val newObj = new ListMethods
    val log = Logger.getLogger (getClass)
    val val1 = 20
    val val2 = 30
    val val3 = 40
    val val4 = 50

    val newList = List (val1, val2, val3, val4,7,8) //for Integers
    val str1 = "a"
    val str2 = "b"
    val newList1 = List (str1, str2) //for Strings
    val val6 = 60
    val subList = List (val3,val4)
    val updatedList = List ()
    val originalList = List (val1, val2, val3, val4)
    log.info ("\n" + newObj.findLengthOfList (newList))
    log.info ("\n" + newObj.findLengthOfList (newList1))
    val firstList = List (val1, val2,43,45)
    val secondList = List (val1, val2)
    log.info ("\n" + newObj.concatenationOfList (firstList, secondList))
    log.info ("\n" + newObj.findSubSequences (newList, subList))
    //  log.info("\n" + newObj.splitList(originalList,(function: Int) => function > 30))
  }
}




