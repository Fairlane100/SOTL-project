library(tm)
library(lda)
library(LDAvis)
library("slam")
library("topicmodels")
library(servr)
source("../scripts/SOTLutils.R")
orig <- read.csv("../working/Combined clean  9-27-16 CLEANED_CULLED.csv",
                 stringsAsFactors = FALSE)
a <- orig$abstract
corpus <- Corpus(VectorSource(a))

dtm <- DocumentTermMatrix(corpus)
#dtm$dimnames$Docs <- as.character(c(1:(dtm$nrow)))
dtm$dimnames$Docs <- orig$title
dim(dtm)
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[ord]
write.csv(freq[ord],"word_freq_beforetfidf.csv")

summary(col_sums(dtm))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.00     1.00     2.00    20.16     8.00 15100.00 

term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
  log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03408 0.17690 0.22270 0.27290 0.30040 2.34900 

dtm <- dtm[,term_tfidf >= 0.23]
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[ord]
write.csv(freq[ord],"word_freq_aftertfidf.csv")
# save a copy of orig with empty abstracts removed...
orig0removed <- orig[row_sums(dtm) > 0,]
dtm <- dtm[row_sums(dtm) > 0,]
summary(col_sums(dtm))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.000    1.000    2.000    5.922    4.000 2262.000 
dim(dtm)
#[1] 7448 9250

k <- 13
SEED <- 2010
#TM <- VEM_fixed = LDA(dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED))
TM <- LDA(dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED))
#TM("alpha")
TM@alpha
#Terms <- terms(TM[["VEM_fixed"]], 30)
#Terms[,1:k]
#Terms <- as.data.frame(terms(TM[["VEM_fixed"]], 30))
Terms <- as.data.frame(terms(TM, 30))
Terms[,1:k]
write.csv(Terms,paste("../working/topicterms ", k,".csv"))
# trying to write the file in the working directory
#write(json_VEMf, "../scripts/vis_app/json_VEMf")
                      
### How to find term frequency of terms in specific topics
# 1) assign each document to its most likely topic
# 2) add up all term freqs for terms in each doc of its likely topic
TMtopics <- topics(TM)
#length(TMtopics)
tfdf <- data.frame(1:30)
for (t in 1:k) {
    # sum term freqs for docs that belong to the topic
    dtmt <- dtm[which(TMtopics==t),]
    myTopicTermFreq <- col_sums(dtmt)
    mySorted <- sort(myTopicTermFreq,decreasing = T)
    topic <- names(mySorted[1:30])
    freq <- as.vector(mySorted[1:30])
    tfdf <- cbind(tfdf,topic)
    tfdf <- cbind(tfdf, freq)
}
tfdf <- tfdf[,-1]
colnames(tfdf) <- rep(1:k,each=2)
write.csv(tfdf,paste("../working/topicsTermFreqs ", k,".csv"))
