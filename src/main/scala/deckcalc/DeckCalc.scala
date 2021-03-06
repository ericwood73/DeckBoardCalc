package deckcalc

import scala.collection.immutable.Nil
import deckcalc.Types._

/**
  * Created by ericwood on 5/21/16.
  */
object DeckCalc {

  /** an x-y coordinate */
  type Coordinate = (Double,Double)

  /** a line representing a deck edge or board edge described by 2 points */
  type LineSegment = (Coordinate, Coordinate)

  /** equation of a line represented by slope and intercept.  If slope is infinite the intercept is the x intercept */
  type LineEquation = (Double, Double)

  /** a set of x-y coordinates that define the deck */
  type DeckPerimeter = List[Coordinate]

  /** A board may have 2 edges made up of line segments and has an overall length. One of the edges will be none if that
    * edge does not intersect any deck edges
    * First edge is left edge and second is right */
  type Board = ((Option[LineSegment], Option[LineSegment]), Double)

  /** Intersection of a board with the deck edge */
  type DeckEdgeIntersection = (LineSegment, Coordinate)



  type Compare = (Double, Double) => Boolean



  def intersection(line1: LineEquation, line2: LineEquation): Option[Coordinate] = (line1, line2) match {
    case (_, (line1._1, _)) => None // Slopes are equal, lines do not intersect
    case (_, (Double.PositiveInfinity, _)) => Some((line2._2, line1._1 * line2._2 + line1._2))
    case ((Double.PositiveInfinity, _), _) =>  Some((line1._2, line2._1 * line1._2 + line2._2))
    case ((slope1, yIntercept1), (slope2, yIntercept2)) => {
      val xIntersection = (yIntercept2 - yIntercept1) / (slope1 - slope2)
      val yIntersection = (slope1 * xIntersection) + yIntercept1
      Some((xIntersection, yIntersection))
    }
  }

  /** get the Coordinate of the intersection of a line described by the equation with the line segment */
  def intersectionOfLineWithSegment(line : LineEquation, lineSegment : LineSegment): Option[Coordinate] = {
    def pointInSegment(p: Coordinate, lineSegment: LineSegment): Option[Coordinate] = (p, lineSegment) match {
      case (_,(`p`, _)) => Some(p) // point is the same as the segment start
      case (_, (_, `p`)) => Some(p) // point is the same as the segment end
      case (_, _) => {
        val ((x1, y1), (x2, y2)) = lineSegment
        if ((math.min(x1, x2) <= p._1 && math.max(x1, x2) >= p._1) &&
          (math.min(y1, y2) <= p._2 && math.max(y1, y2) >= p._2)) {
          Some(p)
        }
        else {
          None
        }
      }
    }

    val lineSegEq = lineEquationFromSegment(lineSegment)
    intersection(line, lineSegEq) match {
      case None => None
      case Some(p) => pointInSegment(p, lineSegment)
    }
  }

//  def intersectionWithVertical(xIntercept: Double, line: LineSegment) : Option[Coordinate] = {
//    val (slope, yIntercept) = lineEquationFromSegment(line)
//    Some((xIntercept, slope * xIntercept + yIntercept))
//  }

  def getDeckEdges(points: Seq[Coordinate]): List[LineSegment] = {
    def deckEdgesAcc(startingPoint: Coordinate, points: Seq[Coordinate], edges: List[LineSegment]): List[LineSegment] = points match {
      case Nil => edges
      case p :: Nil => deckEdgesAcc(startingPoint, Nil, (p, startingPoint) :: edges)
      case p :: ps => deckEdgesAcc(startingPoint, ps, (p, ps.head) :: edges)
    }
    deckEdgesAcc(points.head, points,Nil).reverse
  }

  def lineEquationFromSegment(line: LineSegment): LineEquation = {
    val (p1, p2) = line
    (p1,p2) match {
      case ((_,_), (p1._1,_)) => (Double.PositiveInfinity, p1._1)
      case (_, _) => {
        val (x1, y1) = p1
        val (x2, y2) = p2
        val slope = (y2 - y1) / (x2 - x1)
        (slope, y1 - slope * x1)
      }
    }
  }

  def lineEquationFromPointAndSlope(point: Coordinate, slope: Double): LineEquation = {
    (slope, point._2 - slope * point._1)
  }

  def distance(p1: Coordinate, p2: Coordinate): Double = {
    // calc the distance between p1 and p2
    math.sqrt(math.pow(p2._2 - p1._2, 2.0) + math.pow(p2._1 - p1._1, 2.0))
  }

  /**
    * Get the point which lies a given distance along the line perpendicular to the given line.  The direction from
    * the line is given by the sign on distance.  The distance should be positive when the returned point should
    * have a greater x value and negative when it's x value should be less.
    *
    * @param startPoint
    * @param line
    * @param distance
    * @return
    */
  def pointAtDistancePerpendicular(startPoint: Coordinate, line: LineEquation, distance: Double): Coordinate = {
    val (slope, _) = line
    val perpSlope = -1/slope
    // Get the direction for the distance.  Positive distsance is in the direction of positive x and negative distance is
    // in the direction of negative x
    val sign = math.abs(distance)/distance
    // Get the new x
    val x2 = startPoint._1 + sign * math.sqrt(math.pow(distance, 2.0)/(1 + math.pow(perpSlope, 2.0)))
    // Now find the y of the point using the slope realtionship between x1, x2, y1 and y2
    val y2 = perpSlope * (x2 - startPoint._1) + startPoint._2
    (x2, y2)
  }

//  def getBoardLength(boardLeftEdge: (((Double, Double), (Double, Double)), (Double, Double)), boardRightEdge: (((Double, Double), (Double, Double)), (Double, Double)), leftEdgeIntersections: ((((Double, Double), (Double, Double)), (Double, Double)), (((Double, Double), (Double, Double)), (Double, Double))), rightEdgeIntersections: ((((Double, Double), (Double, Double)), (Double, Double)), (((Double, Double), (Double, Double)), (Double, Double)))) = ???

  /**
    * get a list of lengths for the deck boards needed to cover a deck.  Assumes a Cartesian coordinate system with the
    * origin being the back right extent of the deck perimeter, +y pointing towards the "back" or "top" of the deck and +x
    * pointing to the right of the deck when looking towards the "back" or "top" of the deck.  All measurements should be in the
    * same units.  The length returned will be in those units as well.  This function will always return the maximum
    * board lengths needed to cover a run.  In other words if both edges of the deck board intersect more than 2 deck
    * edges (such as might happen with a cutout or stair step geometry), the calculated board length will be the
    * maximum length as determine by the deck edge intersections that are farthest apart.
    *
    * @param deckEdgeVertices - vertices of deck edges in the coordinate system defined above.  These points must define a convex
    *                 polygon (no notches or L shapes currently).  The first point should by the one with the largest
    *                 x and y value and subsequent points should be defeined in a clockwise fashion.
    * @param deckingSlope - angle of the decking WRT the coordinate system defined above.  +1.0 would be 45 deg running
    *                       from front left to back right and -1.0 would be 45 deg running from front right to back left
    * @param deckingWidth - width of deck including any gap between boards
    * @return A list of deck board lengths assuming no butt joints
    */
  def deckBoardLengths(deckEdgeVertices: Seq[Coordinate], deckingSlope: Double, deckingWidth: Double, deckBoardSpacing: Double,
                       overrideStartOffset: Option[Double] = None): BoardLengths = {
    // First get the deck edges as line segments
    val deckEdges = getDeckEdges(deckEdgeVertices)

    def nextBoardEdge(previousBoardEdge: LineEquation, offset: Double): LineEquation =  {
      // Get equation for a line parallel to the previous edge offset by the thickness of the board in a direction
      // normal to the board slope
      val (previousSlope, previousYIntercept) = previousBoardEdge
      // The sign on distance is the same as the sign on slope.  I.e. the x2 should be > x1 when slope is positive and
      // < x1 when slope is negative based on how we chose the starting point
      val sign = Math.abs(deckingSlope)/deckingSlope
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
      } yield (deckEdge, deckEdgeIntersection.get)).sortBy{case (s, p) => (p._2, p._1)}(Ordering[(Double, Double)].reverse)
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
                   if (deckingSlope >= 0) (_ > _) else (_ < _), (_ > _))
      }

      val bottomPoint = (boardLeftEdge, boardRightEdge) match {
        case (Some(e), None) => e._2
        case (None, Some(e)) => e._2
        case (Some(e1), Some(e2)) =>
          getPoint(getBoardEndPoints(e1._2, e2._2, bottomIntersectingEdges),
                   if (deckingSlope >= 0) (_ < _) else (_ > _), (_ < _))
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
        case slope if slope >=0 => previousBoard._1._2 match {
          case Some(previousEdge) => {
            val leftEdge = (nextBoardEdge(lineEquationFromSegment(previousEdge), deckBoardSpacing))
            val rightEdge = nextBoardEdge(leftEdge, deckingWidth)
            Some((leftEdge, rightEdge))
          }
          case None => None
        }
        case _  => previousBoard._1._1 match {
          case Some(previousEdge) => {
            val rightEdge = (nextBoardEdge(lineEquationFromSegment(previousEdge), deckBoardSpacing))
            val leftEdge = (nextBoardEdge(rightEdge, deckingWidth))
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
      getPoint(deckEdgeVertices, if (deckingSlope >= 0) (_ < _) else (_ > _), (_ > _))
    }

    def getEndingPoint = {
      getPoint(deckEdgeVertices, if (deckingSlope >= 0)  (_ > _) else (_ < _), (_ < _))
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
          // Divide the fraction in half and take that amount from the starting point.  Sign will be the opposite of the slope
          -1 * Math.abs(deckingSlope)/deckingSlope * (deckingWidth - boardWidthFraction/2.0)
        }
      }
      pointAtDistancePerpendicular(startingPoint, lineEquationFromPointAndSlope(startingPoint,deckingSlope), startOffset)

    }

    def getFirstBoard: Board = {
      val startingBoardEdgeEq = nextBoardEdge(lineEquationFromPointAndSlope(correctStartPoint(getStartingPoint),
                                                                            deckingSlope), deckingWidth)
      val deckIntersections = findIntersections(startingBoardEdgeEq)
      val startingBoardEdge = (deckIntersections.head._2, deckIntersections.last._2)
      val (boardLeftEdge, boardRightEdge, topDeckEdges, bottomDeckEdges) = deckingSlope match {
        case slope if slope >= 0 => (None, Some(startingBoardEdge),
                                    (None, Some(deckIntersections.head._1)), (None, Some(deckIntersections.last._1)))
        case _ => (Some(startingBoardEdge), None,
                  (Some(deckIntersections.head._1), None), (Some(deckIntersections.last._1), None))
      }

      ((boardLeftEdge, boardRightEdge), getBoardLength(boardLeftEdge, boardRightEdge, topDeckEdges, bottomDeckEdges))

    }

    def boardLengthsAcc(previousBoard: Board,
                        boardLengths: BoardLengths): BoardLengths = {
      // Get the next board
      getNextBoard(previousBoard) match {
        case None => boardLengths
        case Some(board) => {
          boardLengthsAcc(board, board._2 :: boardLengths)
        }
      }
    }

    boardLengthsAcc(getFirstBoard, Nil).reverse

  }

//  def placeCut(cutLength: Double, currentCuts: List[BoardCuts], maxBoardLength: Double): List[List[BoardCuts]] = {
//    // returns a list of baord cuts with the specified cut added to a new board
//    def addCutToNewBoard(cutLength: Double, currentCuts: List[BoardCuts]): List[BoardCuts] = {
//      (cutLength :: Nil, maxBoardLength - cutLength) :: currentCuts
//    }
//
//    currentCuts match {
////      case Nil => ((cutLength :: Nil, maxBoardLength - cutLength) :: currentCuts) :: Nil
//      case Nil => addCutToNewBoard(cutLength, Nil) :: Nil // board added to list with the board length cut
//      case _ =>  {
//        val newCuts = for {
//          ((cuts, remaining), index) <- currentCuts.zipWithIndex
//          if (remaining > (cutLength +.02 * maxBoardLength))
//        }
//        yield {
//          currentCuts.updated(index, (cutLength :: cuts, remaining - cutLength))
//        }
//
//        addCutToNewBoard(cutLength, currentCuts) :: newCuts
//      }
//    }
//  }

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



//  def getBoardCuts(boardLengths: List[Double], maxBoardLength: Double): List[BoardCuts] = {
//    def boardCutsAcc(remBoardLengths: BoardLengths, currentCutsList: List[List[BoardCuts]]): List[List[BoardCuts]] = remBoardLengths match {
//      case Nil => currentCutsList
//      case bl :: bls =>
//        // Get all possible cuts for the next board length
//        println("Placing cut for board length: " + bl + "...")
//        val allCuts = currentCutsList.flatMap(cuts => placeCut(bl, cuts, maxBoardLength))
////          for {
////          cuts <- currentCutsList
////        }
////        yield {
////          placeCut(bl, cuts, maxBoardLength)
////        }.flatten
////        println("Possible board cuts: " + allCuts)
//        boardCutsAcc(bls, allCuts)
//    }
//
//    val allSolutions = boardCutsAcc(boardLengths.tail, placeCut(boardLengths.head, Nil, maxBoardLength))
//    allSolutions.reduceLeft((acc, l) => if (l.length < acc.length) l else acc)
//
////  }


//    boardLengths match {}
//    case Nil => currentCuts
//    case bl :: bls => {
//      for {
//        boardLength => boardLengths
//
//      }
//    }
//  }

}
