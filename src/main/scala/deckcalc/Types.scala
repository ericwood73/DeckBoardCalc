package deckcalc

/**
  * Created by ericwood on 8/6/16.
  */
object Types {

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

  /** a list of board lengths */
  type BoardLengths = List[Double]

  /** a board with cut lengths and leftover */
  type BoardCuts = (List[Double],Double)

}
