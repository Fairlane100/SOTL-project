# SOTL-TermFreqFromDisc.R
source("SOTLutils.R")

# Read in a cleaned, culled SOTL file, and create term frequency matrixes
#    based on the Discipline column in the input ('Disc')

# Note: To create input file, run 'SOTL-CreateCleanAbstracts.R' 
# Make sure you are in the right Folder before sourcing this R file!!

inputFile <- "Combined clean  8-17-16 CLEANED_CULLED.csv"


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
# TRIED TO ADD SOME COMMANDS HERE:
# I want to see the whole list of disc.freqs in descending order
# with the item count included
# I have tried to get the structure of disc.freqs, and it seems to only have the words 
# (no counts)
# I also tried to run it without the parameter so that I could get a complete list. 
# Won't run without the parameter

# Next I tried to sort the list with this command. It didn't work
# I need the command that calculates frequencies
sort.disc.freqs <- sort(disc.freqs, decreasing = TRUE)

write.csv (disc.freqs, file ="sorted.disc.freqs.csv",)
#This wrote a file that just shows all the words, no frequency counts

disc.freqs.t <- table(disc.words.df)
# ran this and it never came back with results - did it choke? 
# did not receive an error message
# When I did this previously, I used Mallet.