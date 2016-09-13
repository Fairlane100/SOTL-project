# findWordsInContext.R

# read in the cleaned abstracts file
c <- read.csv("Combined clean  8-25-16 CLEANED_CULLED.csv",stringsAsFactors = FALSE)

# for the given word (THEWORD), find all occurances of the word, 
#    and print out N (N) words before and after (if they exist)

THEWORD <- "graphical"
N <- 5

### mkNlist makes a list of indices to print out.
mkNlist <- function(inx, support, len) {
    s <- (inx-support):(inx+support)
    s <- s[s>0]
    s <- s[s<=len]
    s
}
### loops through all abstracts, finds the word, prints the +-N words around it.
for (i in 1:length(c$abstract)) {
    a <- c$abstract[i]
    aa <- unlist(strsplit(a," "))
    matches <- grep(THEWORD,aa)
    if (length(matches) > 0) {
        print(paste("Abstract",i))
    }
    for (match in matches) {
        string <- paste(aa[mkNlist(match, N, length(aa))],collapse = " ")
        print(string)
    }
}