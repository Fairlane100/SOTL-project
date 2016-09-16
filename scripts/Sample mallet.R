library(mallet)
source("../scripts/SOTLutils.R")
orig <- read.csv("Combined clean  8-25-16 CLEANED_CULLED.csv",
                 stringsAsFactors = FALSE)
NUMTOPICS <- 13

topout <- trainTopicModel(orig, NUMTOPICS)
topic.words.df <- topout[[1]]
doc.topics <- round(topout[[2]])
doc.topics <- doc.topics/rowSums(doc.topics)
topic.freqs <- printFrequentTerms(topic.words.df,5)

topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)

# reduce matrix size by removing sparse words
dim(topic.words.df) # 20 rows of topics, 19456 cols of words
head(colSums(topic.words.df))
# sort cols (words) by total frequency across all topics
topic.words.df <- topic.words.df[,order(colSums(topic.words.df),decreasing = T)]
head(colSums(topic.words.df))
tail(colSums(topic.words.df))
# take top 1000 words
topic.words.df.1000 <- topic.words.df[,1:1000]

# find correlation matrix of topic.words.df
topic.words.df.norm <- topic.words.df.1000/rowSums(topic.words.df.1000)
cor.matrix <- cor(topic.words.df.norm, use="complete.obs",method="pearson")
# output is numwords rows and numwords cols (19456 in this case), and shows
# the correlaion bewttn
pca <- princomp(cor.matrix)
x <- predict(pca)[,1]
y <- predict(pca)[,2]
topicwords <- colnames(topic.words.df.1000)

# make a df of topics rows, and n cols of top freq words in each topic
n <- 5
topic.topwords <- NULL
wordsInARow <- NULL
for(i in 1:length(topic.freqs)) {
    topwords <- t(as.data.frame(names(topic.freqs[[i]][1:n])))
    rownames(topwords) <- paste("topic",i)
    topic.topwords <- rbind(topic.topwords,topwords)
    colnames(topic.topwords) <- c(1:n)
    wordsInARow <- c(wordsInARow,as.character(topwords))
}
mycolPalette <- c("red","dark red","blue","light blue","dark blue",
              "green", "light green","dark green","gray","black",
              "purple","orange","dark orange","brown1",
              "cyan","dark cyan","light cyan","yellow","violet","aquamarine")
library(RColorBrewer)
n <- 5
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

mycolsN <- rep(mycolPalette[1:NUMTOPICS],each=n)
# for each word in wordsInARow, find x and y, then assign colors to each topic, then plot.
xp <- sapply(wordsInARow,function(a) x[which(topicwords==a)])
yp <- sapply(wordsInARow,function(a) y[which(topicwords==a)])
plot(xp,yp,col="white",xlim=c(-8,9))
text(xp,yp,wordsInARow,col=mycolsN)
