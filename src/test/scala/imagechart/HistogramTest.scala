package imagechart

import java.io.File
import org.jfree.chart.{ChartUtilities, JFreeChart, ChartPanel, ChartFactory}
import org.jfree.data.category.{CategoryDataset, DefaultCategoryDataset}
import org.jfree.ui.{ApplicationFrame, RefineryUtilities}
import org.jfree.chart.plot.PlotOrientation
import org.scalatest.Suite
import simage.io.SImageIO._
import simage.structs.GrayScaleImage

class HistogramTest extends Suite {

   def testLoadImageShowHistogram {
      val img = loadImageCP("/numbers.png")

      implicit def img2DS(img: GrayScaleImage) = {
         val dataset = new DefaultCategoryDataset
         val imgData = img.data.toList
         for(i <- img.min to img.max) {
            dataset.setValue(imgData.count(_ == i), "img", i)
         }
         dataset
      }

      val chart = createHistogram("Image histogram", img)

      ChartUtilities.saveChartAsPNG(new File("/tmp/hist.png"), chart, 500, 500)

      createShowWindow(
         "Histogram Window",
         chart)
   }

   def createHistogram(title: String, ds: CategoryDataset) = {
      var chart = ChartFactory.createAreaChart(
         null, null, null, ds, PlotOrientation.VERTICAL, false, true, true)

      val axis = chart.getCategoryPlot.getDomainAxis
      axis.setLowerMargin(0.0)
      axis.setUpperMargin(0.0)
      axis.setTickLabelsVisible(false)
      chart
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