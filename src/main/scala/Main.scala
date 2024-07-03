
import data.dataset.Dataset
import data.NumericalFeature

@main def main(): Unit = {
  /*
    Create a dataset from file "dry-bean.csv"
    As it is indicated by "(0 until 16).map(i => false).toVector :+ true" -
      first 16 features of the dataset are numerical (false) and the last feature is categorical (true)
  */
  Dataset("dry-bean.csv", (0 until 16).map(i => false).toVector :+ true)
    match {
      case Right(dataset) => 
        // First 500 lines are training data.
        val trainingData = dataset.makeCopy(dataset.values.take(500))
        
        // The rest is test data.
        val testData = dataset.makeCopy(dataset.values.drop(500))
        
        // Create Random Forest with 50 trees.
        val rf = RandomForest(trainingData, 50)

        // make a prediction using Random Forest and calculate the number of correct answers
        val correct = rf.predict(testData)
        println("precision: " + correct + "/" + testData.length + " (" + 1.0 * correct / testData.length + "%)\n")

        // Print one of the trees to console in .dot format.
        // Go to https://dreampuf.github.io/GraphvizOnline/ and paste printed code to get an image of the tree.
        println("Example of the Random Forest tree: (paste the code to https://dreampuf.github.io/GraphvizOnline/)\n" + Tree.toDot(rf.trees.head))
      case Left(message) => 
        println(message)
    }
}