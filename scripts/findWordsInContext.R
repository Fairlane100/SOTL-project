# findWordsInContext.R

# read in the cleaned abstracts file
c <- read.csv("Combined clean  8-25-16 CLEANED_CULLED.csv",stringsAsFactors = FALSE)

# for the given word (THEWORD), find all occurances of the word, 
#    and print out N (N) words before and after (if they exist)


THEWORD <- "journal"
N <- 3
DISC <- 5 # make DISC <- 0 to match all disciplines

### mkNlist makes a list of indices to print out.
mkNlist <- function(inx, support, len) {
    s <- (inx-support):(inx+support)
    s <- s[s>0]
    s <- s[s<=len]
    s
}
### loops through all abstracts, finds the word, prints the +-N words around it.
resultDf <- NULL
for (i in 1:length(c$abstract)) {
    a <- c$abstract[i]
    d <- c$Disc[i]
    aa <- unlist(strsplit(a," "))
    tmpword <- paste0("\\b",THEWORD,"\\b")
    matches <- NULL
    if (DISC == 0 || d == DISC) {
        matches <- grep(tmpword,aa)
    }
    for (match in matches) {
        string <- paste(aa[mkNlist(match, N, length(aa))],collapse = " ")
        resultDf <- rbind(resultDf, data.frame(i,d,string))
        #print(string)
    }
}
if (!is.null(resultDf)){
    colnames(resultDf) <- c("Abstract","Disc","Match")
    write.csv(resultDf,paste0("wordsInContextResult_Word_",THEWORD,"_Context_",N,"_Discipline_",DISC,".csv"))
}