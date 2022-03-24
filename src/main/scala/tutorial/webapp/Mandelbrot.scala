package tutorial.webapp

import org.scalajs.dom.html
import org.scalajs.dom
import org.scalajs.dom.{document, window}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.collection.mutable.{Map => MMap}

case class Complex(r: Double, i: Double) {
  def +(c: Complex) = Complex(r + c.r, i + c.i)
  def squared = Complex(r * r - i * i, 2 * r * i )
  def isDiverged: Boolean = r * r + i * i > 4
}

@JSExportTopLevel("Mandelbrot")
object Mandelbrot {

  var pixelSize: Double = 0
  var lBound: Double = -2
  var tBound: Double = 1.2
  var cWidth: Int = 0
  var cHeight: Int = 0
  var maxIter: Int = 1000

  var mouseOffsetX: Int = 0
  var mouseOffsetY: Int = 0

  def divergeFn(c: Complex, z: Complex): Complex = {
    z.squared + c
  }

  def setupUI(): Unit = {
    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    canvas.width = window.innerWidth.toInt
    canvas.height = window.innerHeight.toInt
    cWidth = canvas.width
    cHeight = canvas.height
    canvas.id = "canvas"
    ctx.fillStyle = "red"
    pixelSize = 4.0 / window.innerWidth

    val hueMap: Map[(Int,Int), Double] = getHueMap()
    for(x <- 0 to cWidth; y <- 0 to cHeight) {
      val hue = hueMap((x,y))
      ctx.fillStyle = getColor(hue)
      ctx.fillRect(x,y,1,1)
    }
    document.body.appendChild(canvas)
  }

  def getColor(hue: Double): String = {
    hue match {
      case h if h < 0.01 => "#0732A2"
      case h if h < 0.02 => "#EB4D5C"
      case h if h < 0.03 => "#73CF42"
      case h if h < 0.04 => "#EB4D5C"
      case h if h < 0.05 => "#FF9600"
      case h if h < 0.06 => "#0732A2"
      case h if h < 0.07 => "#EFEFF4"
      case h if h < 0.08 => "#0732A2"
      case h if h < 0.2 => "#73CF42"
      case h if h < 0.5 => "#0732A2"
      case _ => "black"
    }
  }


  def updateBrot(): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    canvas.width = cWidth
    canvas.height = cHeight

    val hueMap: Map[(Int,Int), Double] = getHueMap()
    for(x <- 0 to cWidth; y <- 0 to cHeight) {
      val hue = hueMap((x,y))
      ctx.fillStyle = getColor(hue)
      ctx.fillRect(x,y,1,1)
    }
  }

  def pixelToComplex(x: Int, y: Int): Complex = {
    Complex(lBound + (x.toDouble * pixelSize), tBound - (y.toDouble * pixelSize))
  }

  def getHueMap(): Map[(Int,Int), Double] = {
    val iterMap = Map.newBuilder[(Int,Int), Int]
    val iterPixelArr: Array[Int] = new Array(maxIter + 1)
    var total: Int = 0
    for(x <- 0 to cWidth;
        y <- 0 to cHeight) {
      val numIter = inSet(pixelToComplex(x,y))
      iterMap += ((x,y) -> numIter)
      if(numIter < maxIter) {
        iterPixelArr(numIter) += 1
        total += 1
      }
    }
    val allIters: Map[(Int,Int), Int] = iterMap.result()
    val hueMap = Map.newBuilder[(Int,Int), Double]
    for(x <- 0 to cWidth;
        y <- 0 to cHeight) {
      val curIter = allIters((x,y))
      if(curIter == maxIter) {
        hueMap += ((x,y) -> 1)
      } else {
        for(i <- 0 to curIter) {
          hueMap += ((x,y) -> iterPixelArr(i).toDouble / total.toDouble)
        }
      }
    }
    hueMap.result()
  }



  def docLoadedListener(e: dom.Event): Unit = {
    setupUI()
    val canvas = document.getElementById("canvas")
    canvas.addEventListener("mousedown", {(ce: dom.MouseEvent) =>
                              mouseOffsetX = ce.clientX.toInt
                              mouseOffsetY = ce.clientY.toInt
                            })
    canvas.addEventListener("mouseup", {(ce: dom.MouseEvent) =>

                              val topLeft = pixelToComplex(mouseOffsetX, mouseOffsetY)
                              val bottomRight = pixelToComplex(ce.clientX.toInt, ce.clientY.toInt)
                              lBound = topLeft.r
                              tBound = topLeft.i
                              pixelSize =  (bottomRight.r - lBound) / cWidth
                              mouseOffsetX = 0
                              mouseOffsetY = 0
                              updateBrot()
                            })
  }

  // left 37
  // right 39
  // up 38
  // down 40
  // z 90
  // x 88
  @JSExport
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", docLoadedListener)
    window.addEventListener("resize", {(e: dom.Event) =>
                              cWidth = window.innerWidth.toInt
                              cHeight = window.innerHeight.toInt
                              updateBrot()
                            })
    document.addEventListener("keydown", {(e: dom.KeyboardEvent) =>
                                var handledChar = true
                                e.keyCode match {
                                  case 37 => lBound = lBound + lBound / 20
                                  case 39 => lBound = lBound - lBound / 20
                                  case 38 => tBound = tBound + tBound / 20
                                  case 40 => tBound = tBound - tBound / 20
                                  case 90 => pixelSize = pixelSize - pixelSize / 20
                                  case 88 => pixelSize = pixelSize + pixelSize / 20
                                  case _ => handledChar = false
                                }
                                if(handledChar) {
                                  e.preventDefault()
                                  updateBrot()
                                }
                                println(e.keyCode)
                              })


  }

  def inSet(c: Complex): Int = {
    var iteration = 0
    var fz = divergeFn(c, Complex(0,0))
    while(!fz.isDiverged && iteration < maxIter) {
      fz = divergeFn(c, fz)
      iteration += 1
    }
    iteration
  }
}
