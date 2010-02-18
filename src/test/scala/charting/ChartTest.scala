package charting

import org.jfree.chart.plot.PiePlot
import org.jfree.chart.{ChartPanel, ChartFactory}
import org.jfree.data.general.{DefaultPieDataset, PieDataset}
import org.jfree.ui.{RefineryUtilities, ApplicationFrame}
import org.scalatest.Suite

class ChartTest extends Suite {

   def testCreateShowBarChart {
      
   }

   def testCreateShowPieChart {
      new ApplicationFrame("Pie Chart Demo") {
         setContentPane(
            new ChartPanel(
               createPieChart("Pie Chart",
                  pieData(
                     ("One", 43.2D),
                     ("Two", 10D),
                     ("Three", 27.5D),
                     ("Four", 17.5D),
                     ("Five", 11D),
                     ("Six", 19.3D)))))
         pack
         RefineryUtilities.centerFrameOnScreen(this)
         setVisible(true)
      }
   }

   def createPieChart(name: String, dataset: PieDataset) = {
      val chart = ChartFactory.createPieChart("Pie Chart", dataset, true, true, false)
      val plot = chart.getPlot.asInstanceOf[PiePlot]
      plot.setSectionOutlinesVisible(false)
      chart
   }

   def pieData(values: (String, Double)*) = {
      val dataset = new DefaultPieDataset
      for(value <- values) dataset.setValue(value._1, value._2)
      dataset
   }
}