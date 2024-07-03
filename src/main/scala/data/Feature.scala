package data

trait Feature {
  def name: String
  override def toString(): String = name
}
case class NumericalFeature(name: String) extends Feature
case class CategoricalFeature(name: String) extends Feature
