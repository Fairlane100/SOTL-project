library(mallet)
source("SOTLutils.R")
orig <- read.csv("Combined clean  6-13-16 CLEANED_CULLED.csv",
                 stringsAsFactors = FALSE)
splitorig <- orig

# split each abstract into words
ids.v <- paste(splitorig$journal,splitorig$year,splitorig$Volume.number,splitorig$Issue,splitorig$page,sep="_")
ids.v <- gsub(" ","",ids.v)
words <- splitorig$abstract
words.l <- strsplit(words, "\\s+")

# make into list of chr's, was list of array of chr's, where each was one word
chunks.v <- unlist(lapply(words.l, paste, collapse = " "))
# running mallet with stoplist, equivalencies, and concat  DIDNT WORK WITH EXTRA FILES
mallet.instances <- mallet.import(ids.v, chunks.v,
                                  "SOTL-stoplist primary 7-18-16.csv", "SOTL-equivalencies.csv", "SOTL-termConcat.csv"
                                  FALSE,
                                  token.regexp = "[\\p{L}']+")
# removing equivalencies and concat - IT RUNS NOW, BUT NEED TO FIGURE OUT HOW TO HANDLE CONCAT AND EQUIV
mallet.instances <- mallet.import(ids.v, chunks.v,
                                  "SOTL-stoplist primary 7-18-16.csv",
                                  FALSE,
                                  token.regexp = "[\\p{L}']+")

topic.model <- MalletLDA(num.topics = NUMTOPICS)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
#length(vocabulary)
word.freqs <- mallet.word.freqs(topic.model = topic.model)
#head(word.freqs)

topic.model$train(400)

topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed = TRUE,
                                    normalized = TRUE)
dim(topic.words.m)
colnames(topic.words.m) <- vocabulary
#topic.words.m[,1:6]
for (i in 1:NUMTOPICS) {
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 100)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,0.8), rot.per=0, random.order = F)