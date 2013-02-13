import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.collection.mutable

import java.io.File

import scala.util.Random
import scala.math.log

object Dict {
  var dict = mutable.Map.empty[String,Int]; //maps words to their index in the eventual matrix
  var wordCount = 0;
  val stopWords = Array("a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount", "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as", "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the");
}

object Classifier {
  
  def stringToTokens(str : String) : Array[String] = {
    val lowerStr = str.toLowerCase();
    val lowerStrWOPunctuation = lowerStr.replaceAll("[^\\sa-z0-9']", "");
    val ls = lowerStrWOPunctuation.split("\\s+");
    return ls; 
  }
  
  def fileToFrequencyDict(file : File) : mutable.Map[Int,Int] = {
    var fileDict = mutable.Map.empty[Int,Int]; //maps words to frequency in curr file
    var lines = scala.io.Source.fromFile(file).getLines();
    for (line <- lines) {
      for (word <- stringToTokens(line)) {
        //if we haven't seen the word yet, add it to our dictionary
        if (!Dict.dict.contains(word)) {
          Dict.dict += (word -> Dict.wordCount);
          Dict.wordCount += 1;
        }
        var index = Dict.dict(word);
        //if we've seen this word in this file, increment in fileDict
        if (fileDict.contains(index)){
          fileDict(index)+=1;
        }
        //if we haven't seen this word in this file, add it to fileDict
        else{
          fileDict += (index -> 1);
        }
      }
    }
    return fileDict;
  }
  
  def filesToMatrices(files : Array[File]) : (BIDMat.SMat, BIDMat.SMat) = {
    var matrixEntries = mutable.ListBuffer.empty[(Int,Int,Int)]; //a record for each file-word pair, eventual matrix entries
    var fileCounter = 0;
    
    for (file <- files){
        val fileDict = fileToFrequencyDict(file); //maps words to frequency in curr file
	    //add the items from this file's fileDict to our matrix entries
	    fileDict foreach {
	      case (wordIndex, frequency) =>
	        matrixEntries.append((wordIndex,fileCounter,frequency));
	    }
	    fileCounter += 1;
    }
    
    val wordIndices = matrixEntries.map(x => x._1).toList;
    val fileIndices = matrixEntries.map(x => x._2).toList;
    val frequencyValues = matrixEntries.map(x => x._3).toList; //for freq matrix
    val len = wordIndices.length;
    val presenceValues = List.fill(len){1}; //for presence matrix
    
    val wordIndicesCol = icol(wordIndices);
    val fileIndicesCol = icol(fileIndices);
    val frequencyValuesCol = icol(frequencyValues);
    val presenceValuesCol = icol(presenceValues);
    
    val frequencyMatrix = sparse(wordIndicesCol,fileIndicesCol,frequencyValuesCol);
    val presenceMatrix = sparse(wordIndicesCol,fileIndicesCol,presenceValuesCol);
    
    return (frequencyMatrix,presenceMatrix);
  }
  
  def trainClassifier(freqMatrix : BIDMat.SMat) : BIDMat.SMat = {
    val wordCountVector = sum(freqMatrix,2); //creates column vector of sum of each row
    val wordCountVectorSize = wordCountVector.size;
    val wordCountTotal = sum(wordCountVector)(0); //sum of all elements in the vector
    val vocabularySize = Dict.dict.size;
    
    var vectorEntries = mutable.ListBuffer.empty[(Int,Double)]; //an entry for each word in vocab
    var zeroCountProb = log(1/(wordCountTotal+vocabularySize));
    for(wordIndex <- 0 to (vocabularySize-1)){
      if ((wordIndex < wordCountVectorSize) && (wordCountVector(wordIndex) != 0)){
	    val prob = log((wordCountVector(wordIndex)+1)/(wordCountTotal+vocabularySize));
	    vectorEntries.append((wordIndex,prob));
      }
      else{
        vectorEntries.append((wordIndex,zeroCountProb));
      }
    }
    val wordIndicesCol = icol(vectorEntries.map(x => x._1).toList);
    val colIndexCol = icol(List.fill(vectorEntries.size){0});
    val probsCol = col(vectorEntries.map(x => x._2).toList);
    val probVector = sparse(wordIndicesCol,colIndexCol,probsCol);
    return probVector;
  }
  
  def selectRandom(fullLength: Int, targetLength : Int) : (BIDMat.IMat, BIDMat.IMat) = {
    var r = (new Range(0,fullLength,1)).toList;
    r = Random.shuffle(r);
    val trainingColIndices = icol(r.slice(0,targetLength));
    val testColIndices = icol(r.slice(targetLength,fullLength));
    return (trainingColIndices,testColIndices);
  }
  
  def numPosNumNeg(posClassifier : BIDMat.SMat, negClassifier : BIDMat.SMat, testCols : BIDMat.SMat) : (Int,Int) = {
    var numRows = testCols.nrows;
    var numCols = testCols.ncols;
    val posClassifierSized = posClassifier(0 to (numRows-1), ?);
    val negClassifierSized = negClassifier(0 to (numRows-1), ?);
    println(testCols.nrows, testCols.ncols);
    println(posClassifier.nrows, posClassifier.ncols);
    println(posClassifierSized.nrows, posClassifierSized.ncols);
    println(negClassifier.nrows, negClassifier.ncols);
    println(negClassifierSized.nrows, negClassifierSized.ncols);
    
    var numPos = 0;
    var numNeg = 0;
    for (i <- 0 to (numCols-1)){
      val file = testCols(?,i);
      val posProbTerms = file *@ posClassifierSized;
      val posScore = sum(posProbTerms,1)(0); //sum of the column
      val negProbTerms = file *@ negClassifierSized;
      val negScore = sum(negProbTerms,1)(0); //sum of the column
      if (posScore > negScore){
        numPos += 1;
      }
      else {
        numNeg +=1;
      }
    }
    return (numPos,numNeg);
  }
  
  def precision (truePositives : Int, falsePositives : Int) : Float = {
    return truePositives/(truePositives+falsePositives);
  }
  
  def recall (truePositives : Int, falseNegatives : Int) : Float = {
    return truePositives/(truePositives+falseNegatives);
  }
  
  def f1(truePositives : Int, falsePositives :Int, falseNegatives :Int) : Float = {
    val p = precision(truePositives,falsePositives);
    val r = recall(truePositives,falseNegatives);
    return (2*p*r)/(p+r);
  }
  
  def testClassifier(posClassifier : BIDMat.SMat, negClassifier : BIDMat.SMat, posTestCols : BIDMat.SMat, negTestCols : BIDMat.SMat) : Float = {
	val posResults = numPosNumNeg(posClassifier,negClassifier,posTestCols);
	val negResults = numPosNumNeg(posClassifier,negClassifier,negTestCols);
	val truePositives = posResults._1; //positive reviews classifed as positive
	val falsePositives = negResults._1; //negative reviews classified as positive
	val falseNegatives = posResults._2; //positive reviews classiifed as negative
    return f1(truePositives,falsePositives,falseNegatives);
  }
  
  def main(args : Array[String]) : Unit = {
    val negDir = new java.io.File("resources/txt_sentoken/neg");
    val negFiles = negDir.listFiles();
    val posDir = new java.io.File("resources/txt_sentoken/pos");
    val posFiles = posDir.listFiles();
    
    val posMatrices = filesToMatrices(posFiles);
    val negMatrices = filesToMatrices(negFiles);
    
    val posFrequencyMatrix = posMatrices._1;
    val posPresenceMatrix = posMatrices._2;
    val negFrequencyMatrix = negMatrices._1;
    val negPresenceMatrix = negMatrices._2;
    
    for (i <- 0 to 10){
      val posColIndices = selectRandom(1000,900);
      val posTrainingCols = posFrequencyMatrix(?, posColIndices._1);
      val posTestCols = posFrequencyMatrix(?, posColIndices._2);
      val posClassifier = trainClassifier(posTrainingCols);
      
      val negColIndices = selectRandom(1000,900);
      val negTrainingCols = negFrequencyMatrix(?, negColIndices._1);
      val negTestCols = negFrequencyMatrix(?, negColIndices._2);
      val negClassifier = trainClassifier(negTrainingCols);
      
      val score = testClassifier(posClassifier,negClassifier,posTestCols,negTestCols);
      println(score);
    }
    
  }
}