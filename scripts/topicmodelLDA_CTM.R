# topicmodels for LDA and CTM
#https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
library(tm)
library(LDAvis)
source("../scripts/SOTLutils.R")
orig <- read.csv("Combined clean  8-25-16 CLEANED_CULLED.csv",
                 stringsAsFactors = FALSE)
a <- orig$abstract
corpus <- Corpus(VectorSource(a))

dtm <- DocumentTermMatrix(corpus)
dtm$dimnames$Docs <- as.character(c(1:(dtm$nrow)))
dim(dtm)
#[1]  7741 19662

# The mean term frequency-inverse document frequency (tf-idf) over documents containing
# this term is used to select the vocabulary. This measure allows to omit terms which have low
# frequency as well as those occurring in many documents. We only include terms which have
# a tf-idf value of at least .23 which is a bit more than the median and ensures that the very
# frequent terms are omitted.
library("slam")
summary(col_sums(dtm))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.00     1.00     2.00    20.16     8.00 15100.00 

term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * 
    log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03408 0.17690 0.22270 0.27290 0.30040 2.34900 

dtm <- dtm[,term_tfidf >= 0.23]
df <- orig[row_sums(dtm) > 0,] # for later mallet

dtm <- dtm[row_sums(dtm) > 0,]
summary(col_sums(dtm))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.000    1.000    2.000    5.922    4.000 2262.000 
dim(dtm)
#[1] 7448 9250
library("topicmodels")
k <- 13
SEED <- 2010
TM <- list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
           VEM_fixed = LDA(dtm, k = k,
                        control = list(estimate.alpha = FALSE, seed = SEED)),
           Gibbs = LDA(dtm, k = k, method = "Gibbs",
                        control = list(seed = SEED, burnin = 1000,
                        thin = 100, iter = 1000)),
           CTM = CTM(dtm, k = k,
                        control = list(seed = SEED,
                        var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(TM[1:2], slot, "alpha")
# VEM  VEM_fixed 
# 0.06503094 3.84615385 
# We see that if α is estimated it is set to a value much smaller than the default.
# This indicates that in this case the Dirichlet distribution has more mass at the 
# corners and hence, documents consist only of few topics.  
# The lower α the higher is the percentage of documents
#  which are assigned to one single topic with a high probability.

sapply(TM, function(x) mean(apply(posterior(x)$topics,
                                  1, function(z) - sum(z * log(z)))))
# VEM VEM_fixed     Gibbs       CTM 
# 1.103838  2.517957  2.530559  1.295639 
# Higher values indicate that the topic distributions are more evenly spread 
#  over the topics.  What this means i haven't a clue.

# Topic <- topics(TM[["VEM"]], 1)
# Terms <- terms(TM[["VEM"]], 5)
# Terms[,1:k]
# Terms <- terms(TM[["VEM_fixed"]], 5)
# Terms[,1:k]
# Terms <- terms(TM[["Gibbs"]], 5)
# Terms[,1:k]
# Terms <- terms(TM[["CTM"]], 5)
# Terms[,1:k]

# try LDAvis
# have to make a corpus of the dtm that was reduced from tfidf reduction
## Convert tdm to a list of text
dtm2list <- apply(dtm, 1, function(x) {
    paste(rep(names(x), x), collapse=" ")
})

## convert to a Corpus
myCorp <- VCorpus(VectorSource(dtm2list))
#http://rstatistics.net/text-mining-with-lda/
# do a Mallet LDA also to compare all together.
# TODO: create a Mallet LDA
# add names to corpus from dtm. Not getting into corpus for some reason.
# Convert the model to LDAVis format using above link
# NB: there is a reference there also about finding optimal number of topics.
df$abstract <- dtm2list
topout <- trainTopicModel(df, k)
# phi is topic.words as probabilities
phi <- as.matrix(topout[[1]])
# term.frequency is vector of word counts of vocab
# This is a workaround for a bug in term.freq from mallet.
# https://github.com/mimno/RMallet/issues/2
term.frequency <- topout[[3]]
term.frequency$NEWterm.freq <- colSums(phi)
phiNorm <- phi/rowSums(phi)
# theta is doc.topics as probabilities
theta <- as.matrix(topout[[2]])
theta <- theta/rowSums(theta)
# doc.length is num terms in each doc
doc.length <- sapply(dtm2list,function(x) length(unlist(strsplit(x," "))))
# vocab is a vector of terms
vocab <- colnames(phi)

json_lda_mallet <- LDAvis::createJSON(phi = phiNorm, theta = theta,
                               vocab = vocab,
                               doc.length = doc.length,
                               term.frequency = term.frequency$NEWterm.freq)

#inspect(myCorp)
library(servr)
json_VEM <- topicmodels_json_ldavis(fitted = TM[["VEM"]], corpus = myCorp, doc_term = dtm)
json_VEMf <- topicmodels_json_ldavis(fitted = TM[["VEM_fixed"]], corpus = myCorp, doc_term = dtm)
json_Gib <- topicmodels_json_ldavis(fitted = TM[["Gibbs"]], corpus = myCorp, doc_term = dtm)
json_CTM <- topicmodels_json_ldavis(fitted = TM[["CTM"]], corpus = myCorp, doc_term = dtm)
write(json_VEM, "../scripts/vis_app/json_VEM")
write(json_VEMf, "../scripts/vis_app/json_VEMf")
write(json_Gib, "../scripts/vis_app/json_Gib")
write(json_CTM, "../scripts/vis_app/json_CTM")
write(json_lda_mallet, "../scripts/vis_app/json_lda_mallet")

#serVis(json_lda_mallet)
#serVis(json_VEM)
#serVis(json_VEMf)
#serVis(json_Gib)
#serVis(json_CTM)

#### Get top topics for each doc from CTM, and line up the discipline for each doc
# then connect them and visualize in a graph
# just get the most likely topic for each document
TopicsCTM <- topics(TM[["CTM"]],1)
docinx <- names(TopicsCTM)
Discs <- orig$Disc[as.numeric(docinx)]
discnodes <- paste0("D",sort(unique(Discs)))
topicnodes <- paste0("T",sort(unique(TopicsCTM)))
allnodes <- c(discnodes,topicnodes)
allnodeswithids <- data.frame(c(1:length(allnodes)),allnodes)
colnames(allnodeswithids) <- c("id","label")
alledges <- data.frame(source=Discs,target=TopicsCTM+length(discnodes))

library(rgexf)
path <- paste0(getwd(),"/gephi_CTM_Disc_Topic.gexf")
write.gexf(allnodeswithids,alledges,output=path)
gexfFile <- write.gexf(allnodeswithids,alledges)
