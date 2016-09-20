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
#inspect(myCorp)
library(servr)
json_VEM <- topicmodels_json_ldavis(fitted = TM[["VEM"]], corpus = myCorp, doc_term = dtm)
json_VEMf <- topicmodels_json_ldavis(fitted = TM[["VEM_fixed"]], corpus = myCorp, doc_term = dtm)
json_Gib <- topicmodels_json_ldavis(fitted = TM[["Gibbs"]], corpus = myCorp, doc_term = dtm)
json_CTM <- topicmodels_json_ldavis(fitted = TM[["CTM"]], corpus = myCorp, doc_term = dtm)
#serVis(json_VEM)
serVis(json_VEMf)
#serVis(json_Gib)
#serVis(json_CTM)


