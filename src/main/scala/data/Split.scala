package data

trait Split
case class CategoricalSplit(feature: CategoricalFeature) extends Split
case class NumericalSplit(feature: NumericalFeature, border: Double) extends Split
