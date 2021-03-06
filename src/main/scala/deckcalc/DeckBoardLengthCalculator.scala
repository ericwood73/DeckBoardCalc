package deckcalc

import scala.collection.immutable.Nil
import deckcalc.Types._
import deckcalc.Geometry._

/**
  * Created by ericwood on 8/6/16.
  */
object DeckBoardLengthCalculator {

  /**
    * get a list of lengths for the deck boards needed to cover a deck.  Assumes a Cartesian coordinate system with the
    * origin being the "back" or "top" left extent of the deck perimeter, +y pointing towards the "front" or "bottom" of
    * the deck and +x pointing to the right of the deck when looking towards the "back" or "top" of the deck.  All
    * measurements should be in the same units.  The length returned will be in those units as well.  This function will
    * always return the maximum board lengths needed to cover a run.  In other words if both edges of the deck board
    * intersect more than 2 deck edges (such as might happen with a cutout or stair step geometry), the calculated board
    * length will be the maximum length as determine by the deck edge intersections that are farthest apart.
    *
    * @param deckEdgeVertices - vertices of deck edges in the coordinate system defined above.  These points must define a convex
    *                 polygon (no notches or L shapes currently).  The first point should be the one with the smallest
    *                 x and y value and subsequent points should be defined in a clockwise fashion.
    * @param deckingSlope - angle of the decking WRT the coordinate system defined above.  +1.0 would be 45 deg running
    *                       from front left to back right and -1.0 would be 45 deg running from front right to back left
    * @param deckingWidth - width of deck including any gap between boards
    * @return A list of deck board lengths assuming no butt joints
    */
  def deckBoards(deckEdgeVertices: Seq[Coordinate], deckingSlope: Double, deckingWidth: Double, deckBoardSpacing: Double,
                       overrideStartOffset: Option[Double] = None): List[Board] = {

    def getDeckEdges(points: Seq[Coordinate]): List[LineSegment] = {
      def deckEdgesAcc(startingPoint: Coordinate, points: Seq[Coordinate], edges: List[LineSegment]): List[LineSegment] = points match {
        case Nil => edges
        case p :: Nil => deckEdgesAcc(startingPoint, Nil, (p, startingPoint) :: edges)
        case p :: ps => deckEdgesAcc(startingPoint, ps, (p, ps.head) :: edges)
      }
      deckEdgesAcc(points.head, points,Nil).reverse
    }

    // First get the deck edges as line segments
    val deckEdges = getDeckEdges(deckEdgeVertices)

    def nextBoardEdge(previousBoardEdge: LineEquation, offset: Double): LineEquation =  {
      // Get equation for a line parallel to the previous edge offset by the thickness of the board in a direction
      // normal to the board slope
      val (previousSlope, previousYIntercept) = previousBoardEdge
      // The sign on distance should be opposite of the sign on slope.  I.e. the x2 should be < x1 when slope is
      // positive and > x1 when slope is negative based on how we chose the starting point
      val sign = -1 * Math.abs(deckingSlope)/deckingSlope
      val (x2, y2) = pointAtDistancePerpendicular((0.0, previousYIntercept), previousBoardEdge, sign * offset)
      // now given x2, y2 find the next edge's y-intercept
      val nextYIntercept = y2 - previousSlope * x2
      println("Next deck board edge: (" + previousSlope + ", " + nextYIntercept + ")")
      (previousSlope,nextYIntercept)
    }

    // Get all intersection of the boardEdges with deck edges sorted by increasing y then x (top points first)
    def findIntersections(boardEdge: LineEquation): List[DeckEdgeIntersection] = {
      (for {
        deckEdge <- deckEdges
        deckEdgeIntersection = intersectionOfLineWithSegment(boardEdge, deckEdge)
        if (deckEdgeIntersection.isDefined)
      } yield (deckEdge, deckEdgeIntersection.get)).sortBy{case (s, p) => (p._2, p._1)}
    }

    // Return a point from the points list using the specified x and y comparison functions
    // The x comparison will be used first and the y comparison will be used if the x comparison returns false
    def getPoint(points: Seq[Coordinate], xCompare: Compare, yCompare: Compare) = {

      def findPointUsingComparator (acc: Coordinate, p: Coordinate): Coordinate = {
        if (xCompare(p._1, acc._1)) p
        else if (acc._1 == p._1 && yCompare(p._2, acc._2)) p
        else acc
      }

      points.reduceLeft(findPointUsingComparator)

    }

    def getBoardLength(boardLeftEdge: Option[LineSegment], boardRightEdge: Option[LineSegment],
                       topIntersectingEdges: (Option[LineSegment], Option[LineSegment]),
                       bottomIntersectingEdges: (Option[LineSegment], Option[LineSegment])): Double = {

      // Find the deck point in common between two edges.  Assuming edges are adjacent, last point for one edge should
      // be starting point for the other
      def getCommonPoint(edge1: LineSegment, edge2: LineSegment): Coordinate = (edge1, edge2) match {
        case (_, (edge1._2, _)) => edge1._2
        case (_,(_, edge1._1)) => edge1._1
        case _ => throw new Exception("Board intersects non-adjacent deck edges.  It is likely that this application does " +
          "not support this deck geometry.")
      }

      def getBoardEndPoints(leftPoint: Coordinate, rightPoint: Coordinate,
                            edges: (Option[LineSegment], Option[LineSegment])): List[Coordinate] = edges match {
        case (None, Some(_)) | (Some(_), None) => // Edges are the same, top point is on the left or right edge
          List(leftPoint, rightPoint)
        case (Some(e1), Some(e2)) if e1 == e2 => // Edges are the same, top point is on the left or right edge
          List(leftPoint, rightPoint)
        case (Some(e1), Some(e2)) if e1 != e2 =>
          // Edges are different.  Need to consider the deck point in common between the edges.
          List(leftPoint, rightPoint, getCommonPoint(e1, e2))
      }

      val topPoint = (boardLeftEdge, boardRightEdge) match {
        case (Some(e), None) => e._1
        case (None, Some(e)) => e._1
        case (Some(e1), Some(e2)) =>
          getPoint(getBoardEndPoints(e1._1, e2._1, topIntersectingEdges),
            if (deckingSlope >= 0) (_ < _) else (_ > _), (_ < _))
      }

      val bottomPoint = (boardLeftEdge, boardRightEdge) match {
        case (Some(e), None) => e._2
        case (None, Some(e)) => e._2
        case (Some(e1), Some(e2)) =>
          getPoint(getBoardEndPoints(e1._2, e2._2, bottomIntersectingEdges),
            if (deckingSlope >= 0) (_ > _) else (_ < _), (_ > _))
      }

      // Board length is the perpendicular distance between the top and bottom points
      // find the equation of the line perpendicular to slope passing through the top point
      val yIntTopLine = lineEquationFromPointAndSlope(topPoint,-1.0/deckingSlope)._2
      val yIntBottomLine = lineEquationFromPointAndSlope(bottomPoint, -1.0/deckingSlope)._2

      // distance between two lines is then given as abs(b2-b1)/(sqrt(1/m^2 +1)
      math.abs(yIntTopLine - yIntBottomLine)/(math.sqrt(1/math.pow(deckingSlope, 2.0) + 1))
    }


    /** Get the next board, if there is one
      *
      * @param previousBoard
      * @return
      */
    def getNextBoard(previousBoard: Board): Option[Board] = {
      // Determine which edge of the previous board to start from.  If the slope is positive, then the staritng edge
      // will be the right edge, otherwise use the left
      val boardEdgeEqs: Option[(LineEquation, LineEquation)] = deckingSlope match {
        case slope if slope >=0 => previousBoard._1._1 match {
          case Some(previousEdge) => {
            val rightEdge = (nextBoardEdge(lineEquationFromSegment(previousEdge), deckBoardSpacing))
            val leftEdge = nextBoardEdge(rightEdge, deckingWidth)
            Some((leftEdge, rightEdge))
          }
          case None => None
        }
        case _  => previousBoard._1._2 match {
          case Some(previousEdge) => {
            val leftEdge = (nextBoardEdge(lineEquationFromSegment(previousEdge), deckBoardSpacing))
            val rightEdge = (nextBoardEdge(leftEdge, deckingWidth))
            Some((leftEdge, rightEdge))
          }
          case None => None
        }
      }

      // Now get the intersection of the board edge equations with the decks edges and use the intersections to
      // get the board edges and intersecting deck edges
      boardEdgeEqs match {
        case None => None // If there are no more edges, then there are no more boards
        case Some((leftEdgeEq, rightEdgeEq)) =>  {
          // Now get the intersection for each edge
          val leftEdgeAllIntersections = findIntersections(leftEdgeEq)
          val rightEdgeAllIntersections = findIntersections(rightEdgeEq)

          val topEdges: (Option[LineSegment], Option[LineSegment]) =
            (leftEdgeAllIntersections, rightEdgeAllIntersections) match {
              case (Nil, i :: is)  => (None, Some(i._1))
              case (i :: is, Nil)  => (Some(i._1), None)
              case (i1 :: _, i2 :: _)  => (Some(i1._1), Some(i2._1))
            }

          val bottomEdges: (Option[LineSegment], Option[LineSegment]) =
            (leftEdgeAllIntersections, rightEdgeAllIntersections) match {
              case (Nil, i :: is)  => (None, Some(is.last._1))
              case (i :: is, Nil)  => (Some(is.last._1), None)
              case (_ :: is1, _ :: is2)  => (Some(is1.last._1), Some(is2.last._1))
            }

          def getBoardEdgeFromIntersections(intersections: List[DeckEdgeIntersection]): Option[LineSegment] =
            intersections match {
              case Nil => None
              case i :: is => Some((i._2, is.last._2))
            }

          val boardLeftEdge = getBoardEdgeFromIntersections(leftEdgeAllIntersections)
          val boardRightEdge = getBoardEdgeFromIntersections(rightEdgeAllIntersections)

          Some(((boardLeftEdge, boardRightEdge), getBoardLength(boardLeftEdge, boardRightEdge,
            topEdges, bottomEdges)))

        }
      }

    }

    def getStartingPoint = {
      // TODO - this should probably be done by getting the point furthest from a line through the middle or
      // possibly by computing a line through a point and then seeing if their are any points above that line
      getPoint(deckEdgeVertices, if (deckingSlope >= 0) (_ > _) else (_ < _), (_ < _))
    }

    def getEndingPoint = {
      getPoint(deckEdgeVertices, if (deckingSlope >= 0)  (_ < _) else (_ > _), (_ > _))
    }

    // Correct the starting point to ensure equal start and end board widths
    def correctStartPoint(startPoint: Coordinate): Coordinate = {
      // Get the starting and ending deck points
      val startingPoint = getStartingPoint;
      val endingPoint = getEndingPoint;
      println(s"Satring point: $startingPoint Ending point: $endingPoint")
      val startOffset = overrideStartOffset match {
        case Some(offset) => offset
        case None => {
          // Get a line perpendicular to slope through the starting point
          val perpLine = lineEquationFromPointAndSlope(startingPoint, -1.0/deckingSlope)
          // Get the intersection of that line with a line through the endpoint
          val boardRunEnd = intersection(perpLine, lineEquationFromPointAndSlope(endingPoint, deckingSlope))
          // Get the perpendicular distance to the endpoint
          val totalBoardRunLength = distance(startingPoint, boardRunEnd.get)
          // Get the fraction of a board needed to span the distance
          val boardWidthFraction = totalBoardRunLength % (deckingWidth + deckBoardSpacing)
          println(s"Total run width: $totalBoardRunLength Board width fraction: $boardWidthFraction")
          // Divide the fraction in half and offset the start point that amount to get the first board edge.
          // Sign will be the opposite of the slope
          -1 * Math.abs(deckingSlope)/deckingSlope * (boardWidthFraction/2.0)
        }
      }
      pointAtDistancePerpendicular(startingPoint, lineEquationFromPointAndSlope(startingPoint,deckingSlope), startOffset)

    }

    def getFirstBoard: Board = {
      val startingBoardEdgeEq = lineEquationFromPointAndSlope(correctStartPoint(getStartingPoint), deckingSlope)
      val deckIntersections = findIntersections(startingBoardEdgeEq)
      val startingBoardEdge = (deckIntersections.head._2, deckIntersections.last._2)
      val (boardLeftEdge, boardRightEdge, topDeckEdges, bottomDeckEdges) = deckingSlope match {
        case slope if slope >= 0 => (Some(startingBoardEdge), None,
                                    (Some(deckIntersections.head._1), None), (Some(deckIntersections.last._1), None))
        case _ => (None, Some(startingBoardEdge),
                  (None, Some(deckIntersections.head._1)), (None, Some(deckIntersections.last._1)))
      }

      ((boardLeftEdge, boardRightEdge), getBoardLength(boardLeftEdge, boardRightEdge, topDeckEdges, bottomDeckEdges))

    }

    def boardsAcc(previousBoard: Board, boards: List[Board]): List[Board] = {
      // Get the next board
      getNextBoard(previousBoard) match {
        case None => boards
        case Some(board) => {
          boardsAcc(board, board :: boards)
        }
      }
    }

    val firstBoard = getFirstBoard
    boardsAcc(firstBoard, firstBoard :: Nil)
  }

  def boardLengths(boards: List[Board]): BoardLengths = boards.map(b => b._2).reverse

}
