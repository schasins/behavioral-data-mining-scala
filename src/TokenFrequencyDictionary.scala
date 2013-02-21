import scala.collection.mutable

object TokenFrequencyDictionary {
  val filterStopWords = true;
  val ngramLimit = 3;
  var dict = mutable.Map.empty[String,Int]; //maps words to their index in the eventual matrix
  val stopWordsMap = makeMap();
  
  def makeMap() : mutable.Map[String,Boolean] = {
      val stopWords = Array("a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount", "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as", "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the");
	  val stopWordsMap = mutable.Map.empty[String,Boolean];
	  for (word <- stopWords){
	    stopWordsMap += (word -> true);
	  }
	  return stopWordsMap;
  }
  
  def stringToTokens(str : String) : Array[String] = {
    val lowerStr = str.toLowerCase();
    val lowerStrWOPunctuation = lowerStr.replaceAll("[^\\sa-z0-9']", "");
    val ls = lowerStrWOPunctuation.split("\\s+");
    return ls; 
  }
  
  def stringToNgrams(str : String, n : Int) : Array [String] = {
    var ngrams = Array.empty[String];
    val lowerStr = str.toLowerCase();
    val sentences = lowerStr.split(".");
    for (sentence <- sentences){
      val lowerSentence = sentence.replaceAll("[^\\sa-z0-9']", "");
      val ls = lowerSentence.split("\\s+");
      for (i <- 0 to ls.length-n){
    	  ngrams = ngrams :+ ls.slice(i,i+n).deep.mkString(" ");
      }
    }
    return ngrams;
  }
  
  def processString(str : String) : Unit = {
	  //words may be either a list of words or a list of both words and bigrams
	  var words = stringToTokens(str);
	  for (n <- (1 to ngramLimit)){
	    words = words ++ stringToNgrams(str,n);
	  }
	  for (word <- words) {
	    //if the word is in stop words, ignore it, if we're filtering out stop words
	    if (!filterStopWords || !stopWordsMap.contains(word)){
	        //if we haven't seen the word yet, add it to our dictionary
	        if (!dict.contains(word)) {
	          dict += (word -> 1);
	        }
	        //otherwise just increment count
	        else{
	          dict(word) += 1;
	        }
	    }
	  }
  }

}