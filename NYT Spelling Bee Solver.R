# NYT Spelling Bee Solver
# this script finds all words comprised of a set of letters and outputs them in order of string length

# Andrew Johnson

########################################################################################################
###########   ENTER LETTERS AND I WILL GIVE YOU EVERY WORD COMPRISED OF ONLY THOSE LETTERS  ############
#####################  ENTER A FOCAL LETTER AND EVERY WORD WILL CONTAIN THAT LETTER   ##################
keyLetters <- "hndiarb"
focalLetter <- 'b'

minimumWordLength <- 4

########################################################################################################
########################################################################################################

# generates a full alphabet vector
alphabet <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
              'w','x','y','z')

# variable for number of key letters
keyLetterLength <- nchar(keyLetters)

# variable for the number of letters not included in the key letters (# of total letters - # of key letters )
complementLength <- length(alphabet) - keyLetterLength

# separates the string of user input (i.e. letters of interest) into individual characters
funcLetters <- unlist(strsplit(keyLetters, split=""))

# initially just a copy of the alphabet vector
keyLetterComplement <- alphabet

# removes each key letter from the alphabet set, producing the complement of the key letter set
for(i in 1:keyLetterLength){
  keyLetterComplement <- keyLetterComplement[!(grepl(funcLetters[i], keyLetterComplement))]
}

########################################################################################################
#   import dataset of words and convert it to a vector
########################################################################################################

# imports a text bank to reference
textDataAlpha <- read.table("C:/Users/Andrew/Desktop/R Data Files/Dictonary Word Bank.txt")

# produces a vector with a string as each element
textVector <- as.vector(textDataAlpha$V1)

# removes all words smaller than a predetermined length from the word bank
textData <- textVector[nchar(textVector) >= minimumWordLength]

# removes all duplicated words from the word bank
textData <- unique(textData)

########################################################################################################

# creates a variable to manipulate the original text bank
textOutput <- textData

# removes all words that contain each letter of the complement set from textOutput
for(j in 1:complementLength){
  
  textOutput <- textOutput[!(grepl(keyLetterComplement[j], textOutput, ignore.case = TRUE))]
  
}

# removes all words that do not contain the focal letter from textOutput 
textOutput <- textOutput[grepl((focalLetter), textOutput, ignore.case = TRUE)]

# orders textOutput vector by word length in decending order
textOutput <- textOutput[order(nchar(textOutput), textOutput, decreasing = TRUE)]

# prints textOutput to the consol (hopefully to be used to solve the NYT Spelling Bee puzzle)
textOutput

