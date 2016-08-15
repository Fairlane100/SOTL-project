setwd("~/Documents/Research Projects/Git Code/SOTL-project")
newset <- read.csv("Missing compiled cleaned 8-15-16.csv", 
                 stringsAsFactors = FALSE)

source("SOTLutils.R")

infile <- "Missing compiled cleaned 8-15-16.csv"
stopfile <- "SOTL-stoplist.csv"
equivfile <- "SOTL-equivalencies.csv"
termConcatfile <- "SOTL-termConcat.csv"

performCleaning(infile,stopfile,equivfile,termConcatfile)

newset <- read.csv(infile,
                 stringsAsFactors = FALSE)
SOTLstopwords <- read.csv(stopfile, 
                          stringsAsFactors = FALSE, header = F)$V1
SOTLequivalentWords <- read.csv(equivfile, 
                                stringsAsFactors = FALSE, header = F)
SOTLPhrasesTerms <- read.csv(termConcatfile, 
                             stringsAsFactors = FALSE, header = F)
# defaulted to data.frame.  needs to be in vector form.
SOTLPhrasesTerms <- as.vector(as.matrix(SOTLPhrasesTerms))

c <- newset
# Clean the abstract data before processing
c$abstract <- cleanText(c$abstract, 
                        SOTLstopwords, 
                        SOTLequivalentWords,
                        SOTLPhrasesTerms)
#THIS IS NOT WORKING. GOT ERROR FOR TOLOWER.
c$abstract <- iconv(c$abstract, "UTF-8", "UTF-8",sub=' ')
# remove short words
c$abstract <- rm_nchar_words(c$abstract, "1,2")
#GOT ERROR BECAUSE I DON'T HAVE THE RM_NCHAR FUNCTION
#I LOOKED IT UP AND FOUND IT. TRIED TO ADD WITH CALL BELOW, BUT GOT AN ERROR.
install.packages("rm_nchar_words")
rm_nchar_words::install_github("qdapRegex/R/rm_nchar_words.R")

newwords <- nd$abstract
words.lower <- tolower(words)  # Still didn't work - I'm stuck.
#I'm going to continue to get rid of punctuation.
words.nopunct <- gsub("[^[:alnum:][:space:]']", " ", words)
                        words.l <- strsplit(words.nopunct, "\\s+")
                        words.lower <-tolower(words.l)   #Hmm, it worked this time. No error message. 
                        
                        # make into list of chr's, was list of array of chr's, where each was one word
                        chunks.v <- unlist(lapply(words.lower, paste, collapse = " "))
                        str(ids.v)
                        str(chunks.v)
                        summary(nchar(chunks.v))
                        
                        library(mallet)
                        NUMTOPICS = 12
                        source("SOTLutils.R")
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