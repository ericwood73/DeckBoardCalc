/**
  * Created by ericwood on 5/21/16.
  */
package deckcalc

object Main extends App {
  import deckcalc.DeckBoardLengthCalculator._
  import deckcalc.RequiredDeckboardsEstimator._
  import deckcalc.DeckVisualizer._
  import deckcalc.Types._

  import java.io._

  val DECKING_WIDTH = 5.25 // inches
  val DECK_BOARD_SPACING = .125 // inches

  val deckPoints = (0.0, 10.625) :: (60.375, 10.625) :: (60.375, 0.0) :: (111.5, 0.0) :: (111.5, 91.25) :: (0.0, 91.25) :: Nil
//  val deckPoints = (0.0,0.0) :: (0.0,-91.25) :: (-111.5,-91.25) :: (-111.5,-10.625) :: (-51.125, -10.625) :: (-51.125, 0.0) :: Nil
  val deck2Points = (0.0,0.0) :: (0.0,-98.0) :: (-59.0,-147.0) :: (-123.0,-147.0):: (-123.0,0.0) :: Nil

  println ("Deck 1 board lengths")
  val deck1Slope = 1.0
  val deck1Boards = deckBoards(deckPoints, deck1Slope, 5.25, 0.125)
  val deck1BoardLengths = boardLengths(deck1Boards)
  println(deck1BoardLengths)
  val html = generateDeckHtml(deckPoints, deck1Slope, DECKING_WIDTH, DECK_BOARD_SPACING, deck1Boards).toString
  val pw = new PrintWriter(new File("deck1.html" ))
  pw.write(html)
  pw.close

//  println ("Deck 2 board lengths")
//  val deck2BoardLengths = deckBoardLengths(deckPoints,-1.0, 5.375)
//  println(deck2BoardLengths)
//  val deck3BoardLengths = deckBoardLengths(deck2Points,-1.0, 5.375)
//  println ("Deck 3 board lengths")
//  println(deck3BoardLengths)
  // Get one list of all deck boards
  val allDeckBoards = deck1BoardLengths //::: deck2BoardLengths ::: deck3BoardLengths
//  val allDeckBoards = deck2BoardLengths ::: deck3BoardLengths
  printMetrics(allDeckBoards, 20.0)
  printMetrics(allDeckBoards, 16.0)
//  val worstCase = getBestFitSolution(allDeckBoards, 16 * 12)
//  println("Worst case number of boards needed: " + worstCase.length)
//  val totalBoardsLength = allDeckBoards.sum
//  println("Best case number of boards needed: " + totalBoardsLength/(16 * 12))
//  worstCase.sortBy(_._2)
//  println(worstCase)
//  val maxWaste = worstCase.maxBy(_._2)._2
//  val minWaste = worstCase.minBy(_._2)._2
//  val totalWaste = worstCase.foldLeft(0.0)(_ + _._2)
//  val averageWaste = totalWaste/allDeckBoards.length
//  println("Waste - Max: " + maxWaste + " Min: " + minWaste + " Total: " + totalWaste + " Average: " + averageWaste)

  def printMetrics(boardLengths: BoardLengths, maxBoardLengthFeet: Double): Unit = {
    println("Metrics for " + maxBoardLengthFeet + " foot deck boards")
    val worstCase = getBestFitSolution(allDeckBoards, maxBoardLengthFeet * 12.0)
    println("Worst case number of boards needed: " + worstCase.length)
    val totalBoardsLength = boardLengths.sum
    println("Best case number of boards needed: " + totalBoardsLength/(maxBoardLengthFeet * 12.0))
    worstCase.sortBy(_._2)
    println("Cuts: " + worstCase)
    val maxWaste = worstCase.maxBy(_._2)._2
    val minWaste = worstCase.minBy(_._2)._2
    val totalWaste = worstCase.foldLeft(0.0)(_ + _._2)
    val averageWaste = totalWaste/boardLengths.length
    println("Waste - Max: " + maxWaste + " Min: " + minWaste + " Total: " + totalWaste + " Average: " + averageWaste)
  }


}
