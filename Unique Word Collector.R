# Unique Word Collector
# this script imports text, removes all punctuation, and creates a list of unique words

# Andrew Johnson

########################################################################################################
########################################################################################################

# sets working directory
setwd("C:/Users/Andrew/Desktop/R Data Files/")

# function that receives a file name (should be a text file) and returns a vector of unique words from
# that file
WordCollector <- function(fileName){
  
  # reads text file
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
  
  # returns final text vector
  return(textFile8)
  
}

# produces a vector of all the file names in the working directory
nameVector <- c(list.files(pattern = ".txt"))

# produces a blank vector intended to take on a new unique word vector from each file
TotalSet <- c()

# this loop cycles through the working directory, reading each file and adding the new vector of 
# unique words to the TotalSet vector
for(i in 1:length(nameVector)){
  
  # sends a file name to the WordCollector function 
  uniqueWords <- WordCollector(nameVector[i])
  
  # adds the new vector to the primary vector, TotalSet
  TotalSet <- c(TotalSet, uniqueWords)
}

# removes all repeats from TotalSet
TotalSet <- unique(TotalSet)

# used to print the new word bank to the working directory
#write(TotalSet, file = "Word Bank.txt")

