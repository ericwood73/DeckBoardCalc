package deckcalc

import deckcalc.Types._
import deckcalc.Geometry._

import scala.xml

//import scalatags.Text.all._
//import scalatags.Text.svgTags._
//import scalatags.Text.svgAttrs.{points, fill, stroke, strokeWidth, viewBox, preserveAspectRatio}

/**
  * Created by ericwood on 8/6/16.
  */
object DeckVisualizer {

  def generateDeckHtml(deckVertices: Seq[Coordinate], deckingSlope: Double, deckingWidth: Double,
                   deckBoardSpacing: Double, deckBoards: List[Board]) = {
    def deckEdgePointsString: String = {
      val sb: StringBuilder = new StringBuilder
      deckVertices.foreach(p => sb.append(s"${p._1}, ${p._2} "))
      sb.result()
    }

    def boardEdgePointsString(boardEdge: LineSegment) = s"${boardEdge._1._1}, ${boardEdge._1._2} ${boardEdge._2._1}, ${boardEdge._2._2}"

    def boardTextLocation(board: Board): Coordinate = board._1._2 match {
      case Some(ls) => {
        val bottomRightPoint = ls._2
        val rightEdgeMidpointX = bottomRightPoint._1 - (board._2/2.0)/math.sqrt(1.0 + math.pow(deckingSlope,2.0))
        val rightEdgeMidpointY = bottomRightPoint._2 - deckingSlope*(bottomRightPoint._1 - rightEdgeMidpointX)
        pointAtDistancePerpendicular((rightEdgeMidpointX, rightEdgeMidpointY), lineEquationFromSegment(ls), -1*deckingWidth/1.5)
      }
      case None => {
        val leftEdge = board._1._1.get
        val bottomLeftPoint = leftEdge._2
        val leftEdgeMidpointX = bottomLeftPoint._1 - (board._2/2.0)/math.sqrt(1.0 + math.pow(deckingSlope,2.0))
        val leftEdgeMidpointY = bottomLeftPoint._2 - deckingSlope*(bottomLeftPoint._1 - leftEdgeMidpointX)
        pointAtDistancePerpendicular((leftEdgeMidpointX, leftEdgeMidpointY), lineEquationFromSegment(leftEdge), deckingWidth/2.5)
      }
    }

    def deckBoardEdges: List[LineSegment] =
      (for {
        deckBoard <- deckBoards
        edge1 = deckBoard._1._1
        edge2 = deckBoard._1._2
      }
      yield {
        var edges = edge1 match {
          case Some(ls) => List(ls)
          case None => Nil
        }
        edges = edge2 match {
          case Some(ls) => ls :: edges
          case None => edges
        }
        edges
      }).flatten

    val deckParameters =
      <p>{s"Decking slope:      $deckingSlope"}</p>
      <p>{s"Decking width:      $deckingWidth"}</p>
      <p>{s"Deck board spacing: $deckBoardSpacing"}</p>

    val deckDisplay =
      <div>
        <svg height="600" width="800" viewbox="0 -5 120 110" preserveAspectRatio="xMidYMid">
          <polygon points={deckEdgePointsString} fill="none" stroke="black" stroke-width=".5"/>
          {deckBoardEdges.map(edge => <polyline points={boardEdgePointsString(edge)} stroke="blue" stroke-width=".1"/>)}
          {deckBoards.map(b => {
            val textPoint = boardTextLocation(b)
            <text x={textPoint._1.toString} y={textPoint._2.toString} font-size="3" transform={s"rotate(45 ${textPoint._1}, ${textPoint._2})"}>{f"${b._2}%.3f"}</text>
          })}
        </svg>
      </div>


    val pageHeading =
      (<h1>Decking Visualization</h1>
      <div>{deckParameters}</div>)

//    val content = {pageHeading}
//    {deckDisplay}

    val page = <html>
                 <body>
                   {pageHeading}
                   {deckDisplay}
                 </body>
               </html>









//    html(
//      body(
//        h1("Decking Visualization"),
//        div(
//          p(s"Decking slope:      $deckingSlope"),
//          p(s"Decking width:      $deckingWidth"),
//          p(s"Deck board spacing: $deckBoardSpacing")
//        ),
//        div(
//          svg(height := 400, width := 600, viewBox := "0 0 120, 100", preserveAspectRatio := "xMidYMid")(
//            polygon(points := deckEdgePointsString, fill := "none", stroke := "black", strokeWidth := "2"),
//            for (deckBoardEdge <- deckBoardEdges) yield polyline(points := boardEdgePoints(deckBoardEdge))
//          )
//        )
//      )
//    )
    page
  }

}
