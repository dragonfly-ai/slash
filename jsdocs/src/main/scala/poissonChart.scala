package livechart

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import viz.vega.plots.Histogram
import narr.*
import viz.PlotTargets.doNothing
import viz.LaminarViz
import viz.vega.facades.VegaView
import viz.vega.facades.Helpers.*
import slash.vector.Vec
import com.raquo.airstream.core.Signal
import narr.native.NArr

import scala.scalajs.js.typedarray.Float64Array
import io.circe.Encoder
import io.circe.Decoder
import io.circe.parser.decode
import slash.Random
import slash.stats.probability.distributions.Poisson

case class HistogramBin(
  bin0: Double,
  bin1: Double,
  count: Int
) derives Decoder, Encoder.AsObject

def poissonChart(): Div = {
  val rand = Random.defaultRandom
  val λ_ = Var[Double](5.0)
  val n_ = Var[Integer](1000)
  val poissonDist: Signal[Poisson] = λ_.signal.map(Poisson(_))
  val sampled : Signal[Vec[Int]] = poissonDist.combineWith(n_.signal).map(
    (dist: Poisson, n: Integer) => {
      val poissonSample:NArray[Long] = dist.sample(n, rand)
      NArray.tabulate[Double](n)((i:Int) => poissonSample(i).toDouble).asInstanceOf[Vec[Int]]
    }
  )

  def bin0(in: js.UndefOr[js.Dynamic]): Option[HistogramBin] =
    if in == js.undefined then None
    else decode[HistogramBin](js.JSON.stringify(in.asInstanceOf[js.Object])).toOption
  end bin0

  extension (in: Double)
    def print3DP = f"$in%,.3f"

  // https://vega.github.io/vega/examples/histogram/
  val chart = Histogram(
    List(
      viz.Utils.fillDiv,
      spec =>
        val _ = spec("data")(0).obj.remove("url")
        (),
//      spec => spec("data")(0)("values") = ujson.Arr(1.0) ,
      spec => spec("data")(1)("transform")(0)("field") = "data",
      spec => spec("scales")(0)("domain") = ujson.Obj("signal" -> "extent"),
      spec => spec("data")(1)("transform")(0)("extent") = ujson.Obj("signal" -> "extent"),
      spec => spec("data")(1)("transform") = ujson.Obj("type"->"extent", "field"->"data", "signal" -> "extent") +: spec("data")(1)("transform").arr ,
      spec =>
        val _ = spec("signals")(1).obj.remove("bind")
        (),
      spec =>
        val _ = spec("signals")(0).obj.remove("bind")
        (),
      spec => spec("signals")(1)("value") = 1,
      spec => spec("signals")(0 )("value") = -0.5,
      spec => spec("marks")(1)("encode")("enter")("x")("field") = "data",
      spec =>
        spec("signals") = spec("signals").arr :+
        ujson.Obj(
          "name" -> "tooltip",
          "value" -> ujson.Obj(),
          "on" -> ujson.Arr(
            ujson.Obj("events" -> "rect:mouseover", "update" -> "datum"),
            ujson.Obj("events" -> "rect:mouseout", "update" -> "{}" )
          )
        )

    )
  )

  val (chartDiv : Div, viewOpt: Signal[Option[VegaView]]) =
        LaminarViz.viewEmbed(chart)
  val (hoverBus, hoverCallback) = LaminarViz.signalBus
  val binStream = hoverBus.map(b => bin0(b))

  div(
    div(
      h2(
        child.text <-- λ_.signal.map(λ => s"Histogram of sample from Poisson(λ = $λ)"),
      ),
      span(
        "λ:",
        input(
          tpe := "number",
          controlled(
            value <-- λ_.signal.map(_.toString),
            onInput.mapToValue.map(_.toDouble) --> λ_.writer
          )
        ),

      ),

    span(
      "sample:",
      input(
        tpe := "number",
        controlled(
          value <-- n_.signal.map(_.toString),
          onInput.mapToValue.map( i => Integer.valueOf(i) ) --> n_.writer
        )
      ),
    ),
    ),

    chartDiv.amend(
      viewOpt.map{ (vvOpt : Option[VegaView]) =>
        vvOpt.foreach(_.safeAddSignalListener("tooltip", hoverCallback))
      } --> Observer{_ => ()},
      viewOpt.combineWith(sampled) --> Observer[(Option[VegaView], Vec[Int])]{ view_data =>
        view_data._1.foreach( view =>
          //println(view_data._2.asInstanceOf[Float64Array])
          view.data("points", js.Array.from( view_data._2.asInstanceOf[js.Array[Float]] ) )
          view.runAsync()
        )
        ()
      }
    ),
    table(
      tr(
        th("Statistic"),
        th("Calculated"),
        th("Sampled")
      ),
      tr(
        td("SampledMean"), td(child.text <-- poissonDist.map(_.mean.print3DP)), td(child.text <-- sampled.map(_.mean.print3DP))
      ),
      tr(
        td("SampledVariance"), td(child.text <-- poissonDist.map(_.`σ²`.print3DP)), td(child.text <-- sampled.map(_.variance.print3DP))
      ),
      tr(
        td( child.text <-- binStream.map{ b => b match
            case Some(b) => s"Prob(${b.bin0.ceil})"
            case None => "Prob(?)"
        }
        ),
        td(child.maybe <-- binStream.toWeakSignal.combineWith(poissonDist).map((b,dist) => b.flatten.map( c => dist.p(c.bin0.ceil.toLong).print3DP ))),
        td( child.maybe <-- binStream.toWeakSignal.combineWith(n_.signal).map((b,n) => b.flatten.map( c => (c.count.toDouble / n.toDouble).print3DP)))
      )
    )
  )
}