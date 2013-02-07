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
    
    var dict = mutable.Map.empty[String,Int];
    for (file <- negFiles++posFiles){
        println("new file", file.getCanonicalPath());
        val lines = scala.io.Source.fromFile(file).getLines();
	    for (line <- lines) {
	      for (word <- stringToTokens(line)) {
	        if (!dict.contains(word)) {
	          dict = dict + (word -> 1);
	        }
	      }
	    }
    }
    
    println(dict.size);
  }
}