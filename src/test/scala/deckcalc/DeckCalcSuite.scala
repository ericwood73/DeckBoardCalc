package deckcalc

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
  * Created by ericwood on 5/21/16.
  */
@RunWith(classOf[JUnitRunner])
class DeckCalcSuite extends FunSuite {

  val deck1Pts = (0.0,0.0) :: (0.0,-10.0) :: (-10.0,-10.0) :: (-10.0,0.0) :: Nil
  val deck2Pts = (0.0,0.0) :: (0.0,-10.0) :: (-10.0,-5.0) :: (-10.0,0.0) :: Nil
  val verticalLineSeg = ((0.0,0.0), (0.0,-10.0))
  val horizontalLineSeg = ((0.0,0.0), (-10.0,0.0))
  val slopedLineSeg = ((0.0,-10.0), (-10.0,-5.0))

  test("parallel lines") {
    assert(DeckCalc.intersectionOfLineWithSegment((0.0,-1.0), horizontalLineSeg) === None)
  }

  test("intersection of perpendicular lines - segment start is intersection") {
    assert(DeckCalc.intersectionOfLineWithSegment((Double.PositiveInfinity,0.0), horizontalLineSeg) === Some(0.0,0.0))
  }

  test("intersection of perpendicular lines - segment end is intersection") {
    assert(DeckCalc.intersectionOfLineWithSegment((Double.PositiveInfinity,-10.0), horizontalLineSeg) === Some(-10.0,0.0))
  }

  test("intersection of perpendicular lines - inside segment") {
    assert(DeckCalc.intersectionOfLineWithSegment((Double.PositiveInfinity,-1.0), horizontalLineSeg) === Some(-1.0,0.0))
  }

  test("intersection of perpendicular lines - outside segment") {
    assert(DeckCalc.intersectionOfLineWithSegment((Double.PositiveInfinity,-11.0), horizontalLineSeg) === None)
  }

  test("intersection of perpendicular lines - horizontal line with vertical line segment start") {
   assert(DeckCalc.intersectionOfLineWithSegment((0.0,0.0), verticalLineSeg) === Some(0.0,0.0))
  }

  test("intersection of perpendicular lines - horizontal line with vertical line segment end") {
    assert(DeckCalc.intersectionOfLineWithSegment((0.0,-10.0), verticalLineSeg) === Some(0.0,-10.0))
  }

  test("intersection of perpendicular lines - horizontal line outside vertical line segment") {
    assert(DeckCalc.intersectionOfLineWithSegment((0.0,-11.0), verticalLineSeg) === None)
  }

  test("intersection of nonzero slope with vertical - intersect at 0,-1.0") {
    assert(DeckCalc.intersectionOfLineWithSegment((1.0,-1.0), verticalLineSeg) === Some(0.0,-1.0))
  }

  test("intersection of non-perpendicular lines, non-zero y intercept for 1 - intersect at 1,2") {
    assert(DeckCalc.intersectionOfLineWithSegment((1.0,1.0),((1.0,3.0), (1.0, 0.0))) === Some(1.0,2.0))
  }

  test("intersection of non-perpendicular lines, both lines non-zero slope - intersect at 1,0") {
    assert(DeckCalc.intersectionOfLineWithSegment((1.0,-1.0),((1.0, -1.0), (-1.0, -1.0))) === Some(0.0,-1.0))
  }

  test("deck edges - square") {
    val edges = DeckCalc.getDeckEdges(deck1Pts)
    assert(edges.length === 4)
  }

  test("place cuts - empty list") {
    assert(DeckCalc.placeCut(5.0, List(), 20.0) === List((List(5.0), 15.0)))
  }

  test("place cuts - insufficient remaining") {
    assert(DeckCalc.placeCut(5.0, List((List(16.0), 4.0)), 20.0) === List((List(5.0), 15.0), (List(16.0), 4.0)))
  }

  test("place cuts - sufficient remaining") {
    assert(DeckCalc.placeCut(5.0, List((List(14.0), 6.0)), 20.0) === List((List(5.0, 14.0), 1.0)))
  }

  test("place cuts - 2 boards one sufficient and one not") {
    assert(DeckCalc.placeCut(5.0, List((List(5.0, 14.0), 1.0), (List(10.0), 10.0)), 20.0) === List((List(5.0, 14.0), 1.0), (List(5.0, 10.0), 5.0)))
  }

  test("place cuts - 2 boards both sufficient") {
    assert(DeckCalc.placeCut(5.0, List((List(5.0, 6.0), 9.0), (List(10.0), 10.0)), 20.0) === List((List(5.0, 5.0, 6.0), 4.0), (List(10.0), 10.0)))
  }

}
