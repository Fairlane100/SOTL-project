library(tm)
library(qdap)
library(cluster)

source("SOTLutils.R")

NUMTOPICS <- 13

# read in csv files, 
c <- read.csv("Combined clean  6-13-16 CLEANED_CULLED.csv", 
                 stringsAsFactors = FALSE)

# word count statistics of remaining abstracts
abstractWords <- unlist(lapply(strsplit(c$abstract, "\\s+"), length))
summary(abstractWords)

z <- c$abstract

corpus <- VCorpus(VectorSource(z))
dtm <- DocumentTermMatrix(corpus)
dtm

# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/
#
#k means algorithm, NUMTOPICS clusters, 100 starting configurations
#kfit <- kmeans(dtm, NUMTOPICS, nstart=100)
# Too much data.  must reduce number of terms
reducedTerms <- 500

dtm <- dtm[, names(tail(sort(colSums(as.matrix(dtm))), reducedTerms))]
#normalize term freq's
dtm <- dtm/rowSums(as.matrix(dtm))
dtm

kfit <- kmeans(as.matrix(dtm), 13, nstart=100)

#clusplot(as.matrix(dtm), kfit$cluster, color=T, shade=T, labels=2, lines=0)

pca <- prcomp(dtm)
plot(pca)
plot(cumsum(pca$sdev))