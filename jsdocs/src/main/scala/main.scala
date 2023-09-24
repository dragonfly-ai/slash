package livechart

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main
def LiveChart(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("node"),
    poissonChart()
  )
end LiveChart