package imagechart

import java.io.File
import org.jfree.chart.{ChartUtilities, JFreeChart, ChartPanel, ChartFactory}
import org.jfree.ui.{ApplicationFrame, RefineryUtilities}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation
import org.scalatest.Suite
import simage.io.SImageIO._
import simage.structs.GrayScaleImage

class ReportBarChartsTest extends Suite {

   def testLoadImageShowHistogram {
      val dataset = new DefaultCategoryDataset
      dataset.setValue(8, "Java", 1)
      dataset.setValue(398, "Groovy", 2)
      dataset.setValue(1071, "JRuby", 3)
      dataset.setValue(18, "Scala", 4)
      dataset.setValue(186, "Clojure", 5)

      val chart = ChartFactory.createBarChart(
         null, null, null, dataset, PlotOrientation.VERTICAL, false, true, true)

      val axis = chart.getCategoryPlot.getDomainAxis
      axis.setLowerMargin(0.0)
      axis.setUpperMargin(0.0)
      axis.setTickLabelsVisible(false)

      ChartUtilities.saveChartAsPNG(new File("/tmp/langcomp.png"), chart, 500, 500)

      createShowWindow(
         "Barchart Window",
         chart)
   }

   def createShowWindow(title: String, chart: JFreeChart) {
      new ApplicationFrame(title) {
         setContentPane(new ChartPanel(chart))
         pack
         RefineryUtilities.centerFrameOnScreen(this)
         setVisible(true)
      }
   }
}