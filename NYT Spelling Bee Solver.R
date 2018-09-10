# NYT Spelling Bee Solver
# this script finds all words comprised of a set of letters and outputs them in order of string length

# Andrew Johnson

########################################################################################################
###########   ENTER LETTERS AND I WILL GIVE YOU EVERY WORD COMPRISED OF ONLY THOSE LETTERS  ############
########################################################################################################
keyLetters <- "hbgutor"
focalLetter <- 'r'

minimumWordLength <- 4

########################################################################################################
########################################################################################################
########################################################################################################
alphabet <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
              'w','x','y','z')

keyLetterLength <- nchar(keyLetters)
complementLength <- length(alphabet) - keyLetterLength

funcLeters <- unlist(strsplit(keyLetters, split=""))

keyLetterComplement <- alphabet

for(i in 1:keyLetterLength){
  keyLetterComplement <- keyLetterComplement[!(grepl(funcLeters[i], keyLetterComplement))]
}

########################################################################################################
#   import dataset of words and convert it to a vector
########################################################################################################

textDataAlpha <- read.table("C:/Users/Andrew/Desktop/R Data Files/AndrewsWordBank.txt")

textVector <- as.vector(textDataAlpha$V1)

textData <- textVector[nchar(textVector) >= minimumWordLength]

textData <- unique(textData)

########################################################################################################
########################################################################################################


textOutput <- textData

for(j in 1:complementLength){
  textOutput <- textOutput[!(grepl(keyLetterComplement[j], textOutput, ignore.case = TRUE))]
}

textOutput <- textOutput[grepl((focalLetter), textOutput, ignore.case = TRUE)]

textOutput <- textOutput[order(nchar(textOutput), textOutput, decreasing = TRUE)]

textOutput

