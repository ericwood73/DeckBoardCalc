package deckcalc

import scala.collection.immutable.Nil
import deckcalc.Types._

/**
  * Created by ericwood on 8/6/16.
  */
object RequiredDeckboardsEstimator {

  // place cut in next available board, adding a new board if it doesn't fit in any existing board
  def placeCut(cutLength: Double, currentCuts: List[BoardCuts], maxBoardLength: Double): List[BoardCuts] = {
    def addCutToNewBoard(): List[BoardCuts] = {
      (cutLength :: Nil, maxBoardLength - cutLength) :: currentCuts
    }
    // Find the first board with remaining greater than board length
    currentCuts.zipWithIndex.find(_._1._2 >= cutLength + .02 * maxBoardLength) match
    {
      case None => addCutToNewBoard()
      case Some(((cuts, remaining), index)) => currentCuts.updated(index, (cutLength :: cuts, remaining - cutLength))
    }
  }

  def getBestFitSolution(boardLengths: BoardLengths, maximumBoardLength: Double): List[BoardCuts] = {
    // Use best fit decreasing to get the upper bound on the number of boards
    def cutsAcc(boardLengths: BoardLengths, currentCuts: List[BoardCuts]): List[BoardCuts] = boardLengths match {
      case Nil => currentCuts
      case bl :: bls => cutsAcc(bls, placeCut(bl, currentCuts.sortBy(_._2), maximumBoardLength))
    }

    cutsAcc(boardLengths.sorted(Ordering[Double].reverse), Nil)
  }

  def getBiggestCutFirstSolution(boardLengths: BoardLengths, maximumBoardLength: Double): List[BoardCuts] = {
    def cutsAcc(boardLengths: BoardLengths, currentCuts: List[BoardCuts]): List[BoardCuts] = boardLengths match {
      case Nil => currentCuts
      case bl :: bls => cutsAcc(bls, placeCut(bl, currentCuts.sortBy(_._2), maximumBoardLength))
    }

    cutsAcc(boardLengths, Nil)
  }

}
