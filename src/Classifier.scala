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
  
  def fileToFrequencyDict(file : File) : mutable.Map[Int,Int] = {
    var fileDict = mutable.Map.empty[Int,Int]; //maps words to frequency in curr file
    var lines = scala.io.Source.fromFile(file).getLines();
    for (line <- lines) {
      for (word <- stringToTokens(line)) {
        //if we haven't seen the word yet, add it to our dictionary
        if (!Dict.dict.contains(word)) {
          Dict.wordCount += 1;
          Dict.dict += (word -> Dict.wordCount);
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
  }
}