# findRowsThatMatchAnyTerm.R

# input:
# termFile, fileToSearch, columnName in target file
# assuming we are in 'working' to start with, or go there...

termFile <- "IABSsearchterms.csv"
fileToSearch <- "Combined clean  9-27-16 CLEANED_CULLED.csv"
columnName <- "abstract"

terms <- read.csv(termFile, stringsAsFactors = FALSE,header = FALSE)
terms <- terms$V1 # needs to be a char vector else all heck breaks loose
toSearch <- read.csv(fileToSearch, stringsAsFactors = FALSE)
theDocs <- toSearch[,columnName]

termsOneString <- paste(terms,collapse="|")
matches <- grep(termsOneString,theDocs)

result <- toSearch[matches,]
write.csv(result, "rowsThatMatch.csv")