# loading in new cleaned file and running Mallett

# read in the whole csv file for processing
setwd("~/Documents/Research Projects/Git Code/SOTL-project")
newdata <- read.csv("Combined clean  8-1-16.csv", stringsAsFactors = FALSE)
dim(newdata)

# remove rows where abstract is NA
# note that I already removed the empty abstracts, so this should not make a difference (running as test)
setwd("~/Documents/Research Projects/Git Code/SOTL-project")
#getting error on tolower "invalid multibyte string 3"
#will try to remove foreign characters and see it this helps

nd <- gsub('(f|ht)tp\\S+\\s*', "", nd)
       # remove funky chars that gsub doesnt seem to be able to deal with
old1 <- "äó_„ŽñîÑñîñ"
new1 <- paste(rep(" ",nchar(old1)),collapse = "")
nd <- chartr(old1,new1,nd)    # It seems to have worked? Try tolower again

words <- nd$abstract
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