package data.dataset

import scala.collection.immutable.HashMap
import data.*

case class RegressionDataset(features: Vector[Feature], values: Vector[HashMap[Feature, Value]], regressionFeature: NumericalFeature) extends Dataset {

  private def average: Double = {
    1.0 * values.map(m => m(regressionFeature).asInstanceOf[NumericalValue].v).sum / values.length
  }

  private def sse: Double = {
    values.map(m => math.pow(m(regressionFeature).asInstanceOf[NumericalValue].v - average, 2)).sum
  }

  override def gain(feature: CategoricalFeature): Double = {
    sse - group(feature)
      .map(kv =>
        kv._2.asInstanceOf[RegressionDataset].sse
      )
      .sum
  }

  override def gain(feature: NumericalFeature, border: Double): Double = {
    val (lower, upper) = group(feature, border)
    sse - lower.asInstanceOf[RegressionDataset].sse - upper.asInstanceOf[RegressionDataset].sse
  }

  override def mostCommonValue: NumericalValue = {
    NumericalValue(average)
  }

  override def getValue(i: Int): Value = values(i)(regressionFeature)

  override def toString(): String = {
    (features :+ regressionFeature).mkString(", ") + "\n" +
    values.map(m => (features.map(k => m(k)) :+ m(regressionFeature)).mkString(", ")).mkString("\n")
  }

}
