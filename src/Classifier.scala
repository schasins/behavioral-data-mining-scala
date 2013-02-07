import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import scala.collection.mutable

import java.io.File

object Classifier {
  
  def stringToTokens(str : String) : Array[String] = {
    val lowerStr = str.toLowerCase();
    val lowerStrWOPunctuation = lowerStr.replaceAll("[^\\sa-z0-9']", "");
    val ls = lowerStrWOPunctuation.split("\\s+");
    return ls; 
  }
  
  def main(args : Array[String]) : Unit = {
    val negDir = new java.io.File("resources/txt_sentoken/neg");
    val negFiles = negDir.listFiles();
    val posDir = new java.io.File("resources/txt_sentoken/pos");
    val posFiles = posDir.listFiles();
    
    var dict = mutable.Map.empty[String,Int]; //maps words to their index in the eventual matrix
    var matrixEntries = mutable.ListBuffer.empty[(Int,Int)]; //a record for each file-word pair, eventual matrix entries
    var wordCounter = 0;
    var fileCounter = 0;
    for (file <- negFiles++posFiles){
        //println("new file", file.getCanonicalPath());
        val lines = scala.io.Source.fromFile(file).getLines();
	    for (line <- lines) {
	      for (word <- stringToTokens(line)) {
	        if (!dict.contains(word)) {
	          wordCounter += 1;
	          dict = dict + (word -> wordCounter);
	        }
	        matrixEntries.append((dict(word),fileCounter));
	      }
	    }
	    fileCounter += 1;
    }
    
    println(dict.size);
    println(matrixEntries.size);
    matrixEntries = matrixEntries.distinct;
    println(matrixEntries.size);
    
    val wordIndices = matrixEntries.map(x => x._1).toList;
    val fileIndices = matrixEntries.map(x => x._2).toList;
    val len = wordIndices.length;
    val values = List.fill(len){1};
    
    val wordIndicesCol = icol(wordIndices);
    val fileIndicesCol = icol(fileIndices);
    val valuesCol = icol(values);
    
    val matrix = sparse(wordIndicesCol,fileIndicesCol,valuesCol);
    println(matrix);
  }
}