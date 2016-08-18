# SOTL-TermFreqFromMallet.R
source("SOTLutils.R")
library(mallet)
# Read in a cleaned, culled SOTL file, and create term frequency matrixes
#    based on the LDA from Mallet (ignores Disc column of the input file)

# Note: Run 'SOTL-CreateCleanAbstracts.R' first to create the file to read in here.
# Make sure you are in the right Folder before sourcing this R file!!

inputFile <- "Combined clean  8-17-16 CLEANED_CULLED.csv"
NUMTOPICS <- 15

CleanInput <- read.csv(inputFile, stringsAsFactors = FALSE)

topout <- trainTopicModel(c, NUMTOPICS)
topic.words.df <- topout[[1]]
doc.topics <- round(topout[[2]])
doc.topics <- doc.topics/rowSums(doc.topics)
doc.topics[doc.topics < .1] <- 0
doc.topics <- doc.topics/rowSums(doc.topics)
doc.topics <- signif(doc.topics,digits=2)
topnames <- NULL
for (i in 1:NUMTOPICS) {
    topnames <- c(topnames,paste("Topic",i))
}
colnames(doc.topics) <- topnames

d <- cbind(as.data.frame(doc.topics),c)
write.csv(d, "topicsAndDisciplinesOut.csv")

# the 5 is only for print, the freqs returned contains all words
topic.freqs <- printFrequentTerms(topic.words.df,5)