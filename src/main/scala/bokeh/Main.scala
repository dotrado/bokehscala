package geotrellis.bokeh

import io.continuum.bokeh.{Line => BokehLine, _}
import io.continuum.bokeh.Tools._

import scala.collection.immutable.{IndexedSeq, NumericRange}
import math.{sin, Pi => pi}

/**
  * Created by sjzx on 2016/7/29.
  * http://bokeh.pydata.org/en/latest/docs/user_guide/quickstart.html
  */
object Main extends App {

  val xdr = new DataRange1d()
  val ydr = new DataRange1d()

  object source extends ColumnDataSource {
    val x: ColumnDataSource#Column[IndexedSeq, Double] = column(-2 * pi to 2 * pi by 0.1)
    val x0 = column(IndexedSeq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    val y = column(x.value.map(sin))
    val z = column(x.value.map(Math.pow(2, _)))
    val p = column(x.value.map(Math.pow(3, _)))
    val xs = column(IndexedSeq[Double](50, 40, 65, 10, 25, 37, 80, 60))
    val ys = column(IndexedSeq[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    val lat = column(IndexedSeq(39.91, 36.2, 31.28))
    val lon = column(IndexedSeq(116.39, 103.8, 121.46))

    val left = column(IndexedSeq[Double](1, 2, 3, 4, 5))
    val right = column(left.value.map(_ + 1))
    val top = column(IndexedSeq[Double](1, 10, 4, 5, 6))
    val text = column(IndexedSeq("q1", "q2", "q3", "q4", "q5"))

    val x_pathes = column(IndexedSeq[List[Double]](List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10), List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10), List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10)))
    val y_pathes = column(IndexedSeq[List[Double]](List(0, 8, 9, 8.8, 8.6, 8.5, 8, 9, 8.9, 9.8, 8.5, 0), List(0, 3, 5, 6, 7, 5, 4, 3, 3.6, 4.5, 5.5, 0), List(0, 1, 2, 1, 3, 2.5, 2.8, 3, 1.9, 2, 3, 0)))
    val colors = column(IndexedSeq[Color](Color.Chocolate, Color.Aqua, Color.Red))

    val startAngle = column(IndexedSeq[Double](1.385997, 0.616398, 0.146798, -0.577199, -1.92400))
    val endAngle = column(startAngle.value.map(_ + 0.3))

    val gear_x = column(IndexedSeq[Double](0, 10, 20))
    val gear_y = column(IndexedSeq[Double](0, 10, 20))
    val gear_teeth = column(IndexedSeq[Int](5, 6, 7))
    val gear_module = column(IndexedSeq[Double](3, 2, 1))
    val gear_shaftSize = column(IndexedSeq[Double](0.3, 0.2, 0.1))
  }

  import source._

  //  val plot = plotOne("全图")
  //  BokehHelper.save2Document(plot = plot)

  //  val plot = plotMulitple()
  //  BokehHelper.save2Document(plot)
  //  plotMap()

  plot

  def plot = {
    val plot = BokehHelper.getPlot(xdr, ydr, Pan | WheelZoom | Crosshair, width = 720, height = 720)
    BokehHelper.plotBasic(plot)
    //    plotPatches(plot)
    //    plotQuad(plot)
    //    plotAnnularWidge(plot)
    //    plotCsv(plot)
    //    plotSegment(plot)
    //    plotRect(plot)
    plotGear(plot)

    BokehHelper.save2Document(plot)
  }

  def plotGear(plot: Plot): Unit = {
    BokehHelper.setGearGlyph(plot, gear_x, gear_y, gear_teeth, gear_module, gear_shaftSize, x0, source)
  }

  def plotAnnularWidge(plot: Plot) {
    plot.x_range(xdr.start(-10).end(10))
    plot.y_range(xdr.start(-10).end(10))
    BokehHelper.setAnnularWedgeGlyph(plot, 3, 6, startAngle, endAngle, source)
    val textAngle = column(startAngle.value.map(_ + 0.3 / 2))
    val text_x = column(textAngle.value.map(6 * Math.cos(_)))
    val text_y = column(textAngle.value.map(6 * Math.sin(_)))
    println(text_x.value)
    println(text_y.value)
    BokehHelper.setTextGlyph(plot, text_x, text_y, text, source, angle = textAngle)
  }

  def plotPatches(plot: Plot) = {
    BokehHelper.setPatchesGlyph(plot, x_pathes, y_pathes, source, fill_Color = colors)
  }

  def plotQuad(plot: Plot) = {
    BokehHelper.setQuadGlyph(plot, left, right, x0, top, source)
    val textPosition = column(left.value.map(_ + 0.4))
    BokehHelper.setTextGlyph(plot, textPosition, top, text, source)
  }

  def plotSegment(plot: Plot) = {
    val plot = BokehHelper.getPlot(xdr, ydr, Pan | WheelZoom | Crosshair)
    BokehHelper.plotBasic(plot)
    val segemntGlyph = BokehHelper.setSegmentGlyph(plot, x0, ys, xs, ys, source)
    BokehHelper.save2Document(plot)
  }

  def plotRect(plot: Plot) = {
    val hover_tool = new HoverTool().tooltips(Tooltip("value" -> "@x0", "name" -> "@text"))
    plot.tools := hover_tool :: new PanTool :: new WheelZoomTool :: Nil
    BokehHelper.setRectGlyph(plot, x, y, source, 0.1, 0.01)
  }

  def plotMap() = {
    val plot = BokehHelper.getPlot(xdr, ydr, Pan | WheelZoom | Crosshair | BoxSelect, plot = new GMapPlot, width = 1200, height = 800)
    val map_options = new GMapOptions()
      .lat(35)
      .lng(110)
      .zoom(6)
      .map_type(MapType.Roadmap)
    plot.map_options(map_options)

    BokehHelper.setCircleGlyph(plot, lon, lat, source)
    BokehHelper.save2Document(plot)
  }

  def plotMulitple() = {
    val plot1 = plotOne("1")
    val plot2 = plotOne("2")
    val plot3 = plotOne("3")
    val plot4 = plotOne("4")
    BokehHelper.multiplePlots(List(List(plot1, plot2), List(plot3, plot4)), "all chart")
  }

  def plotOne(title: String = ""): Plot = {
    val plot = BokehHelper.getPlot(xdr, ydr, Pan | WheelZoom | Crosshair)
    BokehHelper.plotBasic(plot)
    val legend = plotContent(plot)
    plotLegend(plot, legend)
    plot.title(title)
  }

  def plotContent(plot: Plot) = {
    val circleGlyph = BokehHelper.setCircleGlyph(plot, x, y, source)
    val lineGlyph = BokehHelper.setLineGlyph(plot, x, z, source)
    val lineGlyph2 = BokehHelper.setLineGlyph(plot, x, y, source)
    val patchGlyph = BokehHelper.setPatchGlyph(plot, x, p, source)
    val circleCrossGlyph = BokehHelper.setCircleCrossGlyph(plot, x, p, source)
    val diamondGlyph = BokehHelper.setDiamondGlyph(plot, x, z, source)
    //    val textGlyph = BokehHelper.setTextGlyph(plot, x, z, source)
    List("y = sin(x)" -> List(circleGlyph, lineGlyph2), "y = x^2" -> List(lineGlyph, diamondGlyph), "y = x^3" -> List(circleCrossGlyph, patchGlyph))
  }

  def plotLegend(plot: Plot, legends: List[(String, List[GlyphRenderer])]) = {
    BokehHelper.getLegends(plot, legends)
  }
}
