# Unique Word Collector
# this script imports text, removes all punctuation, and creates a list of unique words

# Andrew Johnson

########################################################################################################
########################################################################################################

setwd("C:/Users/Andrew/Desktop/R Data Files/")

WordCollector <- function(fileName){
  # imports text file
  textFile <- readLines(fileName, encoding = "UTF-8")
  
  # parses text into individual strings
  textFile <- str_split(textFile, " ")
  
  # removes punctuation
  textFile2 <- gsub('[[:punct:] ]+' , ' ' , textFile)
  
  # reparses strings
  textFile3 <- str_split(textFile2, " ")
  
  # separate strings into elements
  textFile4 <- c(textFile3)
  
  # converts the list to a vector
  textFile5 <- unlist(textFile4)
  
  # remove NULL elements
  textFile6 <- textFile5[textFile5 != '']
  
  # deduplicate
  textFile7 <- toupper(textFile6)
  textFile7 <- unique(textFile7)
  
  # removes all numeric elements 
  textFile8 <- textFile7[!(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",
                                 textFile7))]
  
  return(textFile8)
  
}

nameVector <- c(list.files(pattern = ".txt"))

TotalSet <- c()
for(i in 1:length(nameVector)){
  uniqueWords <- WordCollector(nameVector[i])
  TotalSet <- c(TotalSet, uniqueWords)
}

TotalSet <- unique(TotalSet)

