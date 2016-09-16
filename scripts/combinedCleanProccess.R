library(mallet)
library(wordcloud)

NUMTOPICS <- 10

# read in the whole csv file for processing
c <- read.csv("Combined clean  5-12-16.csv", stringsAsFactors = FALSE)
#dim(c)
# [1] 6775   27

# remove rows where abstract is NA
c <- c[which(c$abstract != ""),]
#dim(c)
# [1] 5625   27
s <- c
#s <- c[1:5,]
# split each abstract into words
ids.v <- paste(s$journal,s$year,s$Volume.number,s$Issue,s$page,sep="_")
ids.v <- gsub(" ","",ids.v)
words <- s$abstract
words.lower <- tolower(words)
# get rid of punctuation
words.nopunct <- gsub("[^[:alnum:][:space:]']", " ", words.lower)
words.l <- strsplit(words.nopunct, "\\s+")
# make into list of chr's, was list of array of chr's, where each was one word
chunks.v <- unlist(lapply(words.l, paste, collapse = " "))
#str(ids.v)
#str(chunks.v)
#summary(nchar(chunks.v))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#9.0   642.0   803.0   843.2  1021.0  4455.0 

mallet.instances <- mallet.import(ids.v, chunks.v,
                                  "stoplist.csv",
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
}
