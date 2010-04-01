package charting

import org.jfree.chart.plot.{PlotOrientation, PiePlot}
import org.jfree.chart.{JFreeChart, ChartPanel, ChartFactory}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.general.{DefaultPieDataset, PieDataset}
import org.jfree.data.xy.{DefaultIntervalXYDataset}
import org.jfree.ui.{RefineryUtilities, ApplicationFrame}
import org.scalatest.Suite

class ChartTest extends Suite {

   def atestCreateShowHistogram {
      val dataset = new DefaultIntervalXYDataset
      val arr = Array(
         Array(1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
         Array(5.0, 3.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
         Array(6.0, 4.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
         Array(1.0, 5.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
         Array(8.0, 6.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
         Array(2.0, 7.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0))
      dataset.addSeries(1, arr)
      createShowWindow(
         "Histogram Demo",
         ChartFactory.createHistogram(
            "Test Histogram", "label x", "label y", dataset, PlotOrientation.VERTICAL, true, true, true))
   }

   def testCreateShowAreaChart {
      val dataset = new DefaultCategoryDataset
      dataset.setValue(2.0, "cell.jpg", 0)
      dataset.setValue(2.0, "cell.jpg", 1)
      dataset.setValue(4.0, "cell.jpg", 2)
      dataset.setValue(5.0, "cell.jpg", 3)
      dataset.setValue(1.0, "cell.jpg", 4)
      dataset.setValue(9.0, "cell.jpg", 5)
      dataset.setValue(20.0, "cell.jpg", 6)
      dataset.setValue(2.0, "cell.jpg", 7)
      createShowWindow(
         "Line Chart Demo",
         ChartFactory.createAreaChart(
            "Test Line Chart", "Value", "Number", dataset, PlotOrientation.VERTICAL, false, true, true))
   }

   def atestCreateShowPieChart {
      createShowWindow(
         "Pie Chart Demo",
         createPieChart("Pie Chart",
            pieData(
               ("One", 43.2D),
               ("Two", 10D),
               ("Three", 27.5D),
               ("Four", 17.5D),
               ("Five", 11D),
               ("Six", 19.3D))))
   }

   def createShowWindow(title: String, chart: JFreeChart) {
      new ApplicationFrame(title) {
         setContentPane(new ChartPanel(chart))
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