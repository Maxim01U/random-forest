package data

trait Value
case class NumericalValue(v: Double) extends Value {
  override def toString(): String = v.toString()
}
case class CategoricalValue(v: String) extends Value {
  override def toString(): String = v
}
