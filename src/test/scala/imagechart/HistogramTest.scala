package imagechart

import org.jfree.chart.{JFreeChart, ChartPanel, ChartFactory}
import org.jfree.ui.{ApplicationFrame, RefineryUtilities}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation
import org.scalatest.Suite
import simage.io.SImageIO._
import simage.structs.GrayScaleImage

class HistogramTest extends Suite {

   def testLoadImageShowHistogram {
      val img = loadImageCP("/numbers.png")
      println(img)

      implicit def img2DS(img: GrayScaleImage) = {
         val dataset = new DefaultCategoryDataset
         val imgData = img.data.toList
         for(i <- img.min to img.max) {
            dataset.setValue(imgData.count(_ == i), "img", i)
         }
         dataset
      }

      createShowWindow(
         "Line Chart Demo",
         ChartFactory.createAreaChart(
            "Test Line Chart", "Value", "Number", img, PlotOrientation.VERTICAL, false, true, true))
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