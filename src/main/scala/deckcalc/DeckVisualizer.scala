package deckcalc

import deckcalc.Types._

import scalatags.Text.all._
import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs.{points, fill, stroke, strokeWidth}

/**
  * Created by ericwood on 8/6/16.
  */
object DeckVisualizer {

  def generateDeckHtml(deckVertices: Seq[Coordinate], deckingSlope: Double, deckingWidth: Double,
                   deckBoardSpacing: Double):Frag = {
    val deckEdgePointsString = "0,0 0,91.5"

    html(
      body(
        h1("Decking Visualization"),
        div(
          p(s"Decking slope:      $deckingSlope"),
          p(s"Decking width:      $deckingWidth"),
          p(s"Deck board spacing: $deckBoardSpacing")
        ),
        div(
          svg(height := 400, width := 400)(
            polyline(points := deckEdgePointsString, fill := "none", stroke := "black", strokeWidth := "3")
          )
        )
      )
    )
  }

}
