package data.dataset

import scala.collection.immutable.HashMap
import data.*

case class ClassificationDataset(features: Vector[Feature], values: Vector[HashMap[Feature, Value]], classFeature: CategoricalFeature) extends Dataset {

  private def entropy: Double = {
    - group(classFeature)
      .map(kv => 
        val n = 1.0 * kv._2.length / length
        1.0 * n * math.log(n) / math.log(2)
      )
      .sum
  }

  override def gain(feature: CategoricalFeature): Double = {
    entropy - group(feature)
      .map(kv =>
        1.0 * kv._2.length / length * kv._2.asInstanceOf[ClassificationDataset].entropy
      )
      .sum
  }

  override def gain(feature: NumericalFeature, border: Double): Double = {
    val (lower, upper) = group(feature, border)
    entropy 
      - 1.0 * lower.length / length * lower.asInstanceOf[ClassificationDataset].entropy 
      - 1.0 * upper.length / length * upper.asInstanceOf[ClassificationDataset].entropy
  }

  override def mostCommonValue: CategoricalValue = {
    group(classFeature).maxBy(kv => kv._2.length)._1.asInstanceOf[CategoricalValue]
  }

  override def getValue(i: Int): Value = values(i)(classFeature)

  override def toString(): String = {
    (features :+ classFeature).mkString(", ") + "\n" +
    values.map(m => (features.map(k => m(k)) :+ m(classFeature)).mkString(", ")).mkString("\n")
  }

}
