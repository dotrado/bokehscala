package geotrellis.bokeh

import io.continuum.bokeh
import io.continuum.bokeh.{Line => BokehLine, _}

import scala.collection.immutable.{IndexedSeq, NumericRange}

/**
  * Created by sjzx on 2016/7/30.
  */
object BokehHelper {

  def getPlot(xdr: Range, ydr: Range, tools: List[Tool], width: Int = 800, height: Int = 400, plot: Plot = new Plot()) = {
    plot.x_range(xdr).y_range(ydr).tools(tools).width(width).height(height)
  }

  def plotBasic(plot: Plot) = {
    val xaxis = BokehHelper.getLinearAxis(plot, Location.Below)
    BokehHelper.setAxisLabel(xaxis, "x")
    val yaxis = BokehHelper.getLinearAxis(plot, Location.Right)
    BokehHelper.setAxisLabel(yaxis, "y")

    val xgrid = BokehHelper.getGrid(plot, xaxis, 0)
    val ygrid = BokehHelper.getGrid(plot, yaxis, 1)
  }

  def getLinearAxis(plot: Plot, position: Location): ContinuousAxis = {
    getAxis(plot, new LinearAxis, position)
  }

  /**
    * get datetime axis
    *
    * @param plot
    * @param position
    * @param formatter eg. new DatetimeTickFormatter().formats(Map(DatetimeUnits.Months -> List("%b %Y")))
    * @return
    */
  def getDatetimeAxis(plot: Plot, position: Location, formatter: DatetimeTickFormatter = new DatetimeTickFormatter().formats(Map(DatetimeUnits.Months -> List("%b %Y")))): ContinuousAxis = {
    getAxis(plot, new DatetimeAxis().formatter(formatter), position)
  }

  def getAxis(plot: Plot, axisType: ContinuousAxis, position: Location): ContinuousAxis = {
    val axis = axisType.plot(plot).location(position)
    setPlotAxis(plot, axis, position)
    setRenderer(plot, axis)
    axis
  }

  def setAxisLabel(axis: ContinuousAxis, axisLabel: String) = {
    axis.axis_label(axisLabel)
  }

  def setPlotAxis(plot: Plot, axis: ContinuousAxis, position: Location) {
    position match {
      case Location.Left => plot.left <<= (axis :: _)
      case Location.Above => plot.above <<= (axis :: _)
      case Location.Below => plot.below <<= (axis :: _)
      case Location.Right => plot.right <<= (axis :: _)
      case _ =>
    }
  }

  def getCircleGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circle = new Circle().x(column_x).y(column_y).size(size).fill_color(fill_Color).line_color(line_Color)
    getGlyphRenderer(value, circle)
  }
  def setCircleGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleGlyph = getCircleGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, circleGlyph).asInstanceOf[GlyphRenderer]
  }

  def getLineGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, line_Color: Color = Color.Black) = {
    val line = new BokehLine().x(column_x).y(column_y).line_width(width).line_color(line_Color)
    getGlyphRenderer(value, line)
  }

  def setLineGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, line_Color: Color = Color.Black) = {
    val lineGlyph = getLineGlyph(column_x, column_y, value, width, line_Color)
    setRenderer(plot, lineGlyph).asInstanceOf[GlyphRenderer]
  }

  def getPatchGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val patch = new Patch().x(column_x).y(column_y).line_width(width).line_color(line_Color).fill_color(fill_Color)
    getGlyphRenderer(value, patch)
  }

  def setPatchGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val patchGlyph = getPatchGlyph(column_x, column_y, value, width, fill_Color, line_Color)
    setRenderer(plot, patchGlyph).asInstanceOf[GlyphRenderer]
  }

  /**
    * 绘制多个上色区域，这里xs和ys表示x、y坐标，示例如下，每一个x List对应一个y List，必须严格对应，List个数要相等，每个List中个数也要相对应
    * 且最好每一个List中，x值起始结束均重复，y值起始结束赋固定值（eg. 0）,这样区域会以一条水平线包围（当然水平线可能在上部也可能在下部）
    * 最好将y值小（或者大，取决于水平线的位置）的放在后面，起到的作用是区域叠压的时候小的区域在上部，不会被压盖住看不见
    * 可以将alpha（透明度）设置位0.8
    * @param xs column(IndexedSeq[List[Double]](List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10), List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10), List(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10)))
    * @param ys column(IndexedSeq[List[Double]](List(0, 8, 9, 8.8, 8.6, 8.5, 8, 9, 8.9, 9.8, 8.5, 0), List(0, 3, 5, 6, 7, 5, 4, 3, 3.6, 4.5, 5.5, 0), List(0, 1, 2, 1, 3, 2.5, 2.8, 3, 1.9, 2, 3, 0)))
    * @param value
    * @param width
    * @param fill_Color
    * @param line_Color
    * @return
    */
  def getPatchesGlyph(xs: ColumnDataSource#Column[IndexedSeq, List[Double]], ys: ColumnDataSource#Column[IndexedSeq, List[Double]], value: DataSource, fill_Color: ColumnDataSource#Column[IndexedSeq, Color], width: Int = 3, line_Color: Color = Color.Black) = {
    val patches = new Patches().xs(xs).ys(ys).line_width(width).line_color(line_Color).fill_color(fill_Color).fill_alpha(0.8)
    getGlyphRenderer(value, patches)
  }

  def setPatchesGlyph(plot: Plot, xs: ColumnDataSource#Column[IndexedSeq, List[Double]], ys: ColumnDataSource#Column[IndexedSeq, List[Double]], value: DataSource, fill_Color: ColumnDataSource#Column[IndexedSeq, Color], width: Int = 3, line_Color: Color = Color.Black) = {
    val pathesGlyph = getPatchesGlyph(xs, ys, value, fill_Color, width, line_Color)
    setRenderer(plot, pathesGlyph)
  }

  def getCircleCrossGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleCross = new CircleCross().x(column_x).y(column_y).size(size).fill_color(fill_Color).line_color(line_Color)
    new Segment
    getGlyphRenderer(value, circleCross)
  }

  def setCircleCrossGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleCrossGlyph = getCircleCrossGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, circleCrossGlyph).asInstanceOf[GlyphRenderer]
  }

  def getTextGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], t: ColumnDataSource#Column[IndexedSeq, String], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black, angle: ColumnDataSource#Column[IndexedSeq, Double] = null) = {
    val text = new Text().x(column_x).y(column_y).text(t)
    if(angle != null)
      text.angle(angle)
    getGlyphRenderer(value, text)
  }

  def setTextGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], text: ColumnDataSource#Column[IndexedSeq, String], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black, angle: ColumnDataSource#Column[IndexedSeq, Double] = null) = {
    val textGlyph = getTextGlyph(column_x, column_y, text, value, size, fill_Color, line_Color, angle)
    setRenderer(plot, textGlyph).asInstanceOf[GlyphRenderer]
  }

  def getSegmentGlyph(x0: ColumnDataSource#Column[IndexedSeq, Double], y0: ColumnDataSource#Column[IndexedSeq, Double], x1: ColumnDataSource#Column[IndexedSeq, Double], y1: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, line_Color: Color = Color.Blue) = {
    val segment = new Segment().x0(x0).y0(y0).x1(x1).y1(y1).line_color(line_Color)
    getGlyphRenderer(value, segment)
  }

  def setSegmentGlyph(plot: Plot, x0: ColumnDataSource#Column[IndexedSeq, Double], y0: ColumnDataSource#Column[IndexedSeq, Double], x1: ColumnDataSource#Column[IndexedSeq, Double], y1: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, line_Color: Color = Color.Blue) = {
    val segmentGlyph = getSegmentGlyph(x0, y0, x1, y1, value, line_Color)
    setRenderer(plot, segmentGlyph).asInstanceOf[GlyphRenderer]
  }

  def getRectGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Double = 1, height: Double = 1) = {
    //when you create a rect object, you can add the Hover tool to show the data in the rect
    val rect = new Rect().x(column_x).y(column_y).width(width).height(height)
    getGlyphRenderer(value, rect)
  }

  def setRectGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Double = 1, height: Double = 1) = {
    val rectGlyph = getRectGlyph(column_x, column_y, value, width, height)
    setRenderer(plot, rectGlyph).asInstanceOf[GlyphRenderer]
  }

  //柱状图
  def getQuadGlyph(left: ColumnDataSource#Column[IndexedSeq, Double], right: ColumnDataSource#Column[IndexedSeq, Double], bottom: ColumnDataSource#Column[IndexedSeq, Double], top: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val quad = new Quad().left(left).right(right).bottom(bottom).top(top).fill_color(fill_Color).line_color(line_Color)
    getGlyphRenderer(value, quad)
  }

  def setQuadGlyph(plot: Plot, left: ColumnDataSource#Column[IndexedSeq, Double], right: ColumnDataSource#Column[IndexedSeq, Double], bottom: ColumnDataSource#Column[IndexedSeq, Double], top: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val quadGlyph = getQuadGlyph(left, right, bottom, top, value, fill_Color, line_Color)
    setRenderer(plot, quadGlyph).asInstanceOf[GlyphRenderer]
  }

  /**
    * 饼状图
    * 此处需要注意的是起始角度和结束角度均是序列值，并且0位置为水平向右，最大值为2π，最小值为-2π，且可以设置direction类改变饼状图的方向，默认为逆时针，即角度增大的方向。
    * 实际测试direction有BUG，不能设置为Direction.AntiClock，即默认值不能再设置
    * @param innerRadius 内圆半径
    * @param outerRadius 外圆半径
    * @param startAngle 起始角度
    * @param endAngle 结束角度
    * @param value
    * @param x
    * @param y
    * @return
    */
  def getAnnularWedgeGlyph(innerRadius: Double, outerRadius: Double, startAngle: ColumnDataSource#Column[IndexedSeq, Double], endAngle: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, x: Double = 0, y: Double = 0) = {
    val annularWedge = new AnnularWedge().x(x).y(y).inner_radius(innerRadius).outer_radius(outerRadius).start_angle(startAngle).end_angle(endAngle).fill_color(Color.Blue).line_color(Color.Red)//.direction(Direction.Clock)
    getGlyphRenderer(value, annularWedge)
  }

  def setAnnularWedgeGlyph(plot: Plot, innerRadius: Double, outerRadius: Double, startAngle: ColumnDataSource#Column[IndexedSeq, Double], endAngle: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, x: Double = 0, y: Double = 0) = {
    val annularWedgeGlyph = getAnnularWedgeGlyph(innerRadius, outerRadius, startAngle, endAngle, value, x, y)
    setRenderer(plot, annularWedgeGlyph).asInstanceOf[GlyphRenderer]
  }

  def getDiamondGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val diamond = new Diamond().x(column_x).y(column_y).size(size).fill_color(fill_Color).line_color(line_Color)
    getGlyphRenderer(value, diamond)
  }

  def setDiamondGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val diamondGlyph = getDiamondGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, diamondGlyph).asInstanceOf[GlyphRenderer]
  }

  def getGlyphRenderer(value: DataSource, glyph: Glyph) = {
    new GlyphRenderer().data_source(value).glyph(glyph)
  }

  /**
    *
    * @param legends eg.  val legends = List("y = sin(x)" -> List(lineGlyph, circleGlyph))
    */
  def getLegends(plot: Plot, legends: List[(String, List[GlyphRenderer])]): Legend = {
    val legend = new Legend().plot(plot).legends(legends)
    setRenderer(plot, legend)
    legend
  }

  def getLegends(plot: Plot, name: String, glyphList: List[GlyphRenderer]): Legend = {
    getLegends(plot, List(name -> glyphList))
  }

  def getLegends(plot: Plot, name: String, glyph: GlyphRenderer): Legend = {
    getLegends(plot, List(name -> List(glyph)))
  }

  /**
    *
    * @param plot
    * @param axis
    * @param dimension 0 means x and 1 means y
    * @return
    */
  def getGrid(plot: Plot, axis: ContinuousAxis, dimension: Int) = {
    val grid = new Grid().plot(plot).dimension(dimension).axis(axis)
    setRenderer(plot, grid)
    grid
  }

  def setRenderers(plot: Plot, renderers: List[Renderer] => List[Renderer]) = {
    plot.renderers <<= renderers
  }

  def setRenderer(plot: Plot, renderer: Renderer) = {
    val renderers: (List[Renderer] => List[Renderer]) = (renderer :: _)
    setRenderers(plot, renderers)
    renderer
  }

  /**
    * use gridplot to Multiple plots in the document
    *
    * @param children every child List is one row   eg. val children = List(List(microsoftPlot, bofaPlot), List(caterPillarPlot, mmmPlot))
    * @return
    */
  def multiplePlots(children: List[List[Plot]], title: String = ""): Plot = {
    new GridPlot().children(children).title(title)
  }

  def save2Document(plot: Plot, path: String = "sample.html"): Unit = {
    val document = new Document(plot)
    val html = document.save(path)
    println(s"Wrote ${html.file}. Open ${html.url} in a web browser.")
    html.view()
  }
}
