library(mallet)
source("SOTLutils.R")
orig <- read.csv("Combined clean  6-13-16 CLEANED_CULLED.csv",
                 stringsAsFactors = FALSE)
NUMTOPICS <- 20
topout <- trainTopicModel(orig, NUMTOPICS)
topic.words.df <- topout[[1]]
doc.topics <- round(topout[[2]])
doc.topics <- doc.topics/rowSums(doc.topics)
topic.freqs <- printFrequentTerms(topic.words.df,5)
