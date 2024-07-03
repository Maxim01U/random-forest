import data.*
import data.dataset.*

case class RandomForest(trees: Vector[Tree]) {

  def predict(dataset: Dataset): Int = {
    val predictions = (0 until dataset.length).map(i =>
      val votes = trees.map(t =>
        t.predict(dataset(i))
      )
      dataset match {
        case ClassificationDataset(features, values, classFeature) => 
          votes.groupBy(v => v).maxBy(_._2.length)._1
        case RegressionDataset(features, values, regressionFeature) => 
          NumericalValue(1.0 * votes.map(v => v.asInstanceOf[NumericalValue].v).sum / votes.length)
      }
    )

    (0 until dataset.length).foldLeft(0)((acc, i) =>
      predictions(i) match {
        case cv@CategoricalValue(v) if dataset.getValue(i).asInstanceOf[CategoricalValue] == cv => 
          acc + 1
        case nv@NumericalValue(v) if math.abs(dataset.getValue(i).asInstanceOf[NumericalValue].v - v) < RandomForest.precision => 
          acc + 1
        case _ =>
          acc
      }
    )
  }

}

object RandomForest {

  val precision = 0.01

  def apply(dataset: Dataset, trees: Int): RandomForest = {
    println("Building Random Forest")
    RandomForest(
      (1 to trees)
        .map(i => 
          println("built " + i + "/" + trees + " trees")
          Tree(dataset.randomize)
        )
        .toVector
    )
  }

}
