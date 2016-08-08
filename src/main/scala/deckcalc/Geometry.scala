package deckcalc

import deckcalc.Types._

/**
  * Created by ericwood on 8/7/16.
  */
object Geometry {

  /** geometry functions */
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
      case (_, ((x1, y1), (x2, y2))) if x1 == x2 && y1 != y2 => // vertical segment.  Only need to check point in between y's
        if (math.min(y1, y2) <= p._2 && math.max(y1, y2) >= p._2) Some(p) else None
      case (_, ((x1, y1), (x2, y2))) if y1 == y2 => // horizontal segment.  Only need to check point in between x's
        if (math.min(x1, x2) <= p._1 && math.max(x1, x2) >= p._1) Some(p) else None
      case _ => {
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

}
