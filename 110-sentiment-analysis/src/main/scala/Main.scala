// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._


object Main {

	type Embedding = (String, Array[Double])
	type ParsedReview = (Integer, String, Double)
	type TReview = (Integer, String, Double, Array[String])
	type MRReview = (Integer, Integer,String)
	type JReview = (Integer,Integer, Array[Double])
	type TempReview = (Integer,Integer, Array[Double],Integer)
	type AReview = (Integer,Integer,Array[Double])
	type FinalReview = (Integer,Integer,org.apache.spark.ml.linalg.Vector)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]

  // Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

	def tokenizeReviews(set: Dataset[ParsedReview]): Dataset[TReview] = {
			new Tokenizer().setInputCol("text").setOutputCol("words").transform(set).as[TReview]
	}

	def formatReviews(set: Dataset[TReview]): Dataset[MRReview] = {
		set.flatMap(r => r._4.map(w => (r._1, normalizeRating(r._3), w))).toDF("id", "overall", "word").as[MRReview]
	}

	def normalizeRating(x: Double): Int = x match {
		case 1 | 2 => 0
		case 3 => 1
		case _ => 2
	}

	def joinVector(ds1: Dataset[MRReview],ds2: Dataset[Embedding]): Dataset[JReview] = {
		ds1.join(ds2, "word").select("id", "overall", "vec").as[JReview]
	}

	def calcAverageVector(set: Dataset[JReview]): Dataset[AReview] = {
		val tmp = set.map(r => (r._1, r._2, r._3, 1)).toDF("id", "overall", "vec", "iterator").as[TempReview]

		val sum = tmp.groupByKey(_._1).reduceGroups((a, b) => (a._1, a._2, a._3.zip(b._3).map(x => x._1 + x._2), a._4 + b._4))
		sum.map(x => (x._2._1, x._2._2, x._2._3.map(y => y / x._2._4))).toDF("id","overall","vec").as[AReview]
	}

	def convertToVec(set: Dataset[AReview]): Dataset[FinalReview] = {
		set.map(r => (r._1, r._2, Vectors.dense(r._3))).toDF("id", "label", "features").as[FinalReview]
	}

	def analyzeData (dataset: Dataset[FinalReview]) : List[Double] =
	{
		// Split the data into 10 pieces
		val splits = dataset.randomSplit(Array(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1), seed = 1234L)
		val results = (0 to 9).map(x => analyze(x, splits))
		results.toList
	}

	def analyze[T] (iterator: Int, arrayOfSets: Array[Dataset[T]]) : Double =
	{
		val test = arrayOfSets(iterator)
		val temp = arrayOfSets.filter(x => x == test)
		val train = temp.reduce((x,y) => x.union(y))
		// specify layers for the neural network
		val layers = Array[Int](50, 5, 4, 3)
		// create the trainer and set its parameters
		val trainer = new MultilayerPerceptronClassifier()
			.setLayers(layers)
			.setBlockSize(128)
			.setSeed(1234L)
			.setMaxIter(100)
		// train the model
		val model = trainer.fit(train)
		// compute accuracy on the test set
		val result = model.transform(test)
		val predictionAndLabels = result.select("prediction", "label")
		val evaluator = new MulticlassClassificationEvaluator().setMetricName("accuracy")
		evaluator.evaluate(predictionAndLabels)
	}


	def main(args: Array[String]) = {

    val glove  = loadGlove ("/Users/vongrad/Downloads/reviews/glove.6B.50d.txt")
    val reviews = loadReviews ("/Users/vongrad/Downloads/reviews/reviews_Automotive_5.json")

		val tokenizedReviews = tokenizeReviews(reviews)
		val formattedReviews = formatReviews(tokenizedReviews)
		val joinedVec = joinVector(formattedReviews, glove)

		val avgVec = calcAverageVector(joinedVec)

		val finalSet = convertToVec(avgVec)

		val results = analyzeData(finalSet)
		val averageResult = results.foldRight(0.0)(_+_) / 10

		results.map(x => println("Accuracy: " + x))

		spark.stop
  }

}
