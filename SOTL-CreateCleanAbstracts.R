library(tm)
library(qdap)

source("SOTLutils.R")

NUMTOPICS <- 13

# read in csv files, 
#orig <- read.csv("Combined clean  5-31-16.csv", 
infile <- "Combined clean  6-13-16"


orig <- read.csv(paste(infile,".csv", sep=""),
                 stringsAsFactors = FALSE)
SOTLstopwords <- read.csv("SOTL-stoplist.csv", 
                          stringsAsFactors = FALSE, header = F)$V1
SOTLequivalentWords <- read.csv("SOTL-equivalencies.csv", 
                                stringsAsFactors = FALSE, header = F)
SOTLPhrasesTerms <- read.csv("SOTL-termConcat.csv", 
                             stringsAsFactors = FALSE, header = F)
# defaulted to data.frame.  needs to be in vector form.
SOTLPhrasesTerms <- as.vector(as.matrix(SOTLPhrasesTerms))
# keep the original for comparison
c <- orig
# Clean the abstract data before processing
c$abstract <- cleanText(c$abstract, 
                        SOTLstopwords, 
                        SOTLequivalentWords,
                        SOTLPhrasesTerms)
# do not know why, but must do this stuff outside of a function or doesnt work.
#d$abstract <- cleanText2(c$abstract)
old1 <- "òäó_„ŽîÑîñõð"
new1 <- paste(rep(" ",nchar(old1)),collapse = "")
c$abstract <- chartr(old1,new1,c$abstract)
# remove short words
c$abstract <- rm_nchar_words(c$abstract, "1,2")

outfile <- paste(infile,"CLEANED.csv")
outfileReduced <- paste(infile, "CLEANED_CULLED.csv")

write.csv(c,file=outfile,row.names = FALSE)

# remove abstracts which are < 8 words (including empty ones)
# DONT do this here because want same rows as input.
rowsToRemove <- getBadRows(c$abstract, 8)
c <- c[-rowsToRemove,]

# word count statistics of remaining abstracts
abstractWords <- unlist(lapply(strsplit(c$abstract, "\\s+"), length))
summary(abstractWords)

write.csv(c,file=outfileReduced,row.names = FALSE)
