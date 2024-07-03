package data.dataset

import scala.collection.immutable.HashMap
import data.*

trait Dataset {
  def features: Vector[Feature]
  def values: Vector[HashMap[Feature, Value]]
  def gain(feature: CategoricalFeature): Double
  def gain(feature: NumericalFeature, border: Double): Double

  def makeCopy(newValues: Vector[HashMap[Feature, Value]]): Dataset = {
    this match {
      case cd@ClassificationDataset(features, values, classFeature) => 
        cd.copy(values = newValues)
      case rd@RegressionDataset(features, values, regressionFeature) => 
        rd.copy(values = newValues)
    }
  }

  def group(feature: CategoricalFeature): HashMap[Value, Dataset] = {
    HashMap.from(
      values
        .groupBy(m => m(feature))
        .map(kv => (kv._1, makeCopy(kv._2)))
    )
  }

  def group(feature: NumericalFeature, border: Double): (Dataset, Dataset) = {
    val (lower, upper) = values.partition(map => map(feature).asInstanceOf[NumericalValue].v < border)
    (
      makeCopy(lower),
      makeCopy(upper)
    )
  }

  def mostCommonValue: Value

  def length: Int = values.length

  def apply(i: Int): HashMap[Feature, Value] = values(i)

  def getValue(i: Int): Value

  def randomize: Dataset = 
    makeCopy((0 until length).map(i => values(i)).toVector)

  def getBorders(feature: NumericalFeature): Vector[Double] = {
    values
      .map(m => m(feature).asInstanceOf[NumericalValue].v)
      .sortWith(_ < _)
      .sliding(2)
      .map(v => 1.0 * v.sum / v.length)
      .toVector
  }

  def optimalSplit: Option[Split] = {
    val (split, maxGain) = 
      features
        .map(f =>
          f match {
            case cf@CategoricalFeature(name) => 
              Vector((CategoricalSplit(cf), gain(cf)))
            case nf@NumericalFeature(name) => 
              getBorders(nf).map(border => (NumericalSplit(nf, border), gain(nf, border)))
          }
        )
        .flatten
        .maxBy(_._2)

    if (maxGain > 0)
      Some(split)
    else
      None
  }

}

object Dataset {

  /*
    featureTypes assigns types to dataset features
      - True - CategoricalFeature
      - False - NumericalFeature
  */
  def apply(path: String, featureTypes: Vector[Boolean]): Either[String, Dataset] = {

    @annotation.tailrec
    def helper(features: Vector[(String, Boolean)], values: List[Vector[String]], acc: Vector[HashMap[Feature, Value]], line: Int): 
    Either[String, Vector[HashMap[Feature, Value]]] = {

      values match {
        case head :: next => 
          lazy val v: Vector[Option[(Feature, Value)]] = head.zip(features).map(t => 
            if (t._2._2) 
              Some((CategoricalFeature(t._2._1), CategoricalValue(t._1)))
            else if (t._1.toDoubleOption.isDefined)
              Some((NumericalFeature(t._2._1), NumericalValue(t._1.toDouble)))
            else
              None
          )
          if (head.length != features.length)
            Left("expected: " + features.length + " values; got: " + head.length + " values; at line: " + line)
          else if (v.contains(None))
            val i = v.indexOf(None)
            Left("unable to parse value at column: " + (i + 1) + "; line: " + line)
          else
            helper(features, next, acc :+ HashMap.from(v.map(o => o.get)), line + 1)
        case Nil => 
          Right(acc)
      }
    }

    val source = scala.io.Source.fromFile(path)
    val lines = try source.getLines.toList finally source.close() 
    lines match {
      case head :: tail =>
        val featureNames = head.split(",").toVector
        val values = tail.map(s => s.split(",").toVector).toList
        
        if (featureNames.length != featureTypes.length)
          Left("expected: " + featureTypes.length + " features; got: " + featureNames.length + " features; at line 1")
        else
          val features = featureNames.zip(featureTypes)
          helper(features, values, Vector(), 2).flatMap(vec =>
            val f = features.map(t =>
              if (t._2)
                CategoricalFeature(t._1)
              else
                NumericalFeature(t._1)
            )
            val shuffled = scala.util.Random.shuffle(vec)
            f.last match {
              case cf@CategoricalFeature(name) => 
                Right(ClassificationDataset(f.take(f.length - 1), shuffled, cf))
              case nf@NumericalFeature(name) => 
                Right(RegressionDataset(f.take(f.length - 1), shuffled, nf))
            }
          )
      case Nil =>
        Left("file is empty\n")
    }
  }

}
