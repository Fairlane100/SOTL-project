#setwd("~/Documents/Research Projects/Git Code/SOTL-project")
newset <- read.csv("Missing compiled cleaned 8-15-16.csv", 
                 stringsAsFactors = FALSE)

source("SOTLutils.R")
library(mallet)

infile <- "Missing compiled cleaned 8-15-16.csv"
stopfile <- "SOTL-stoplist.csv"
equivfile <- "SOTL-equivalencies.csv"
termConcatfile <- "SOTL-termConcat.csv"

performCleaning(infile,stopfile,equivfile,termConcatfile)
newwords <- read.csv("Missing compiled cleaned 8-15-16 CLEANED_CULLED.csv")
newwordsabstracts <-newwords$abstract
newwordsabstracts <- iconv(newwordsabstracts, "UTF-8", "UTF-8",sub=' ')
newwordsabstracts.v <- paste(newwordsabstracts, collapse = " ")  #ADDED THIS##
newwordsabstracts.l <- strsplit(newwordsabstracts.v, "\\s+")  ## AND THEN RE-RAN THIS
# SEE JOCKERS PAGE 11 FOR COMMANDS, FOLLOWING CHAPTER.FREQS.T EXAMPLE
                        
# make into list of chr's, was list of array of chr's, where each was one word
                        # LINES 21-23 ARE NOT WORKING AND I CANT FIND ORIGINAL REFERENCE
                        str(ids.v)
                        str(chunks.v)
                        summary(nchar(chunks.v))
                        
                        
                        NUMTOPICS = 12
                        mallet.instances <- mallet.import(ids.v, chunks.v,
                        "SOTL-stoplist primary 7-18-16.csv",
                        FALSE,
                        token.regexp = "[\\p{L}']+")
topic.model <- MalletLDA(num.topics = NUMTOPICS)
topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
length(vocabulary)
new.word.freqs <- mallet.word.freqs(topic.model = topic.model)
head(new.word.freqs)

topic.model$train(400)

topic.words.m <- mallet.topic.words(topic.model,
smoothed = TRUE,
normalized = TRUE)
dim(topic.words.m)
colnames(topic.words.m) <- vocabulary
head(topic.words.m)