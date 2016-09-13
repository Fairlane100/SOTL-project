# SOTL-TermFreqFromDisc.R
source("SOTLutils.R")

# Read in a cleaned, culled SOTL file, and create term frequency matrixes
#    based on the Discipline column in the input ('Disc')

# Note: To create input file, run 'SOTL-CreateCleanAbstracts.R' 
# Make sure you are in the right Folder before sourcing this R file!!

inputFile <- "Combined clean  8-25-16 CLEANED_CULLED.csv"


CleanInput <- read.csv(inputFile, stringsAsFactors = FALSE)

z <- split(CleanInput$abstract,CleanInput$Disc)
# treat disciplines as 'document'
# dont collapse if want to treat abstracts like docs instead of disciplines.
z <- lapply(z, paste, collapse = " ") 
corpus <- VCorpus(VectorSource(z))
dtm <- DocumentTermMatrix(corpus)

disc.words.m <- as.matrix(dtm)
disc.words.df <- as.data.frame(disc.words.m)

disc.freqs  <- printFrequentTerms(disc.words.df,5)

for (i in 1:length(disc.freqs)) {
  tmp <- disc.freqs[[i]]
  tmp <- t(data.frame(tmp[,1:200]))
  write.csv(tmp, paste("discfreq",i,".csv"))
}
write.csv (disc.freqs, file ="sorted.disc.freqs.csv",)
