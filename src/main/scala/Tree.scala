
import scala.collection.immutable.{HashMap, HashSet}
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import sys.process._
import data.*
import data.dataset.*

sealed trait Tree {
  def predict(row: HashMap[Feature, Value]): Value
}
case class Leaf(value: Value) extends Tree {
  override def predict(row: HashMap[Feature, Value]): Value = 
    value
}
case class ClassBranch(feature: CategoricalFeature, children: HashMap[CategoricalValue, Tree]) extends Tree {
  override def predict(row: HashMap[Feature, Value]): Value = 
    children(row(feature).asInstanceOf[CategoricalValue]).predict(row)
}
case class RegressionBranch(feature: NumericalFeature, border: Double, left: Tree, right: Tree) extends Tree {
  override def predict(row: HashMap[Feature, Value]): Value = 
    if (row(feature).asInstanceOf[NumericalValue].v < border)
      left.predict(row)
    else
      right.predict(row)
}

object Tree {

  val threshold = 0.1
  
  def apply(initialDataset: Dataset): Tree = {

    def helper(dataset: Dataset): Tree = {
      lazy val splitOption = dataset.optimalSplit

      if (1.0 * dataset.length / initialDataset.length < threshold || !splitOption.isDefined) {
        Leaf(dataset.mostCommonValue)
      }
      else if (dataset match {case cd@ClassificationDataset(features, values, classFeature) => cd.group(classFeature).size == 1 case _ => false})
        Leaf(dataset.mostCommonValue)
      else {
        splitOption.get match {
          case CategoricalSplit(feature) => 
            ClassBranch(feature, dataset.group(feature).map(kv => (kv._1.asInstanceOf[CategoricalValue], helper(kv._2))))
          case NumericalSplit(feature, border) => 
            val (lower, upper) = dataset.group(feature, border)
            RegressionBranch(feature, border, helper(lower), helper(upper))
        }
      }
    }

    helper(initialDataset)
  }

  def toDot(tree: Tree): String = {

    def helper(tree: Tree, acc: String, i: Int): (String, Int) = {
      tree match {
        case ClassBranch(feature, children) => 
          val updated = acc + "  " + i + " [label=\"" + feature + "\"];\n"
          children.foldLeft((updated, i + 1))((t, c) =>
            val updated = t._1 + "  " + i + " -> " + t._2 + " [label=\"" + c._1 + "\"];\n";
            helper(c._2, updated, t._2)
          )
        case RegressionBranch(feature, border, left, right) => 
          val updated = acc + "  " + i + " [label=\"" + feature + "\"];\n"
          List((border, left, "<"), (border, right, ">=")).foldLeft((updated, i + 1))((t, c) =>
            val updated = t._1 + "  " + i + " -> " + t._2 + " [label=\"" + c._3 + " " + c._1 + "\"];\n";
            helper(c._2, updated, t._2)
          )
        case Leaf(value) => 
          (acc + "  " + i + " [label=\"" + value + "\"];\n", i + 1)
      }
    }

    "digraph DecisionTree {\n" +
    helper(tree, "", 0)._1 + "}\n"
  }

}
