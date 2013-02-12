import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.collection.mutable

import java.io.File

object Dict {
  var dict = mutable.Map.empty[String,Int]; //maps words to their index in the eventual matrix
  var wordCount = 0;
}

object Classifier {
  
  def stringToTokens(str : String) : Array[String] = {
    val lowerStr = str.toLowerCase();
    val lowerStrWOPunctuation = lowerStr.replaceAll("[^\\sa-z0-9']", "");
    val ls = lowerStrWOPunctuation.split("\\s+");
    return ls; 
  }
  
  def filesToMatrix(files : Array[File]) : BIDMat.SMat = {
    var matrixEntries = mutable.ListBuffer.empty[(Int,Int)]; //a record for each file-word pair, eventual matrix entries
    var fileCounter = 0;
    for (file <- files){
        var lines = scala.io.Source.fromFile(file).getLines();
	    for (line <- lines) {
	      for (word <- stringToTokens(line)) {
	        if (!Dict.dict.contains(word)) {
	          Dict.wordCount += 1;
	          Dict.dict += (word -> Dict.wordCount);
	        }
	        matrixEntries.append((Dict.dict(word),fileCounter));
	      }
	    }
	    fileCounter += 1;
    }
    matrixEntries = matrixEntries.distinct;
    
    val wordIndices = matrixEntries.map(x => x._1).toList;
    val fileIndices = matrixEntries.map(x => x._2).toList;
    val len = wordIndices.length;
    val values = List.fill(len){1};
    
    val wordIndicesCol = icol(wordIndices);
    val fileIndicesCol = icol(fileIndices);
    val valuesCol = icol(values);
    
    val matrix = sparse(wordIndicesCol,fileIndicesCol,valuesCol);
    return matrix;
  }
  
  def main(args : Array[String]) : Unit = {
    val negDir = new java.io.File("resources/txt_sentoken/neg");
    val negFiles = negDir.listFiles();
    val posDir = new java.io.File("resources/txt_sentoken/pos");
    val posFiles = posDir.listFiles();
    
    val posMatrix = filesToMatrix(posFiles);
    val negMatrix = filesToMatrix(negFiles);
    
    println(Dict.dict.size);
    println(posMatrix);
    println(negMatrix);
  }
}