# utilities to support the SOTL processing
# SOTLutils.R
 

newchartr <- function(old,charvec) {
    n <- 1
    print(old)
    for (i in tolower(substring(old,seq(1,nchar(old),n),seq(n,nchar(old),n)))) {
        print(i)
        charvec <- gsub(i," ",charvec)
    }
}

performCleaning <- function(infile,stopfile,equivfile,termConcatfile) {
    infileNoExtension <- sub(".csv","",infile)
    outfile <- paste(infileNoExtension,"CLEANED.csv")
    outfileReduced <- paste(infileNoExtension, "CLEANED_CULLED.csv")
    
    orig <- read.csv(infile,
                     stringsAsFactors = FALSE)
    SOTLstopwords <- read.csv(stopfile, 
                              stringsAsFactors = FALSE, header = F)$V1
    SOTLequivalentWords <- read.csv(equivfile, 
                                    stringsAsFactors = FALSE, header = F)
    SOTLPhrasesTerms <- read.csv(termConcatfile, 
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
    #old1 <- "òäó_„Žîñõð"
    #print(old1)
    #c$abstract <- newchartr(old1,c$abstract)
    # give up on custom code. iconv works well.
    c$abstract <- iconv(c$abstract, "UTF-8", "UTF-8",sub=' ')
    # remove short words
    c$abstract <- rm_nchar_words(c$abstract, "1,2")
    
    write.csv(c,file=outfile,row.names = FALSE)
    
    # remove abstracts which are < 8 words (including empty ones)
    # DONT do this here because want same rows as input.
    rowsToRemove <- getBadRows(c$abstract, 8)
    c <- c[-rowsToRemove,]
    
    # word count statistics of remaining abstracts
    abstractWords <- unlist(lapply(strsplit(c$abstract, "\\s+"), length))
    summary(abstractWords)
    
    write.csv(c,file=outfileReduced,row.names = FALSE)
}

cleanText <- function(txtVec, stops, equivwords, concatTerms) {
    tmpVec <- txtVec
    
    # make all lower case
    tmpVec <- tolower(tmpVec)
    stops <- tolower(stops)
    SOTLequivalentWords[[1]] <- tolower(SOTLequivalentWords[[1]])
    SOTLequivalentWords[[2]] <- tolower(SOTLequivalentWords[[2]])
    concatTerms <- tolower(concatTerms)
    # remove URLs
    tmpVec <- gsub('(f|ht)tp\\S+\\s*', "", tmpVec)
#     # remove funky chars that gsub doesnt seem to be able to deal with
#     old1 <- "äó_„ŽñîÑñîñ"
#     new1 <- paste(rep(" ",nchar(old1)),collapse = "")
#     tmpVec <- chartr(old1,new1,tmpVec)
    # remove punctuation
    tmpVec <- gsub("[^[:alnum:][:space:]]", " ", tmpVec)
    # remove numbers
    tmpVec <- gsub("\\d+", "", tmpVec)
    # remove stop words
    tmpVec <- removeWords(tmpVec, stopwords("english"))
    # replace equivalencies (NB has to before stopword remove)
    for (i in 1:dim(equivwords)[1]) {
        patt <- paste0('\\b', equivwords[i,2], '\\b')
        repl <- equivwords[i,1]
        #print(paste("repl",patt,"with",repl))
        tmpVec <-gsub(patt, repl, tmpVec)
    }
    #tmpVec <- mgsub(equivwords[,2],equivwords[,1],tmpVec)
    # remove SOTL stopwords
    #tmpVec <- mgsub(stops,rep("",length(stops)),tmpVec)
    for (i in 1:length(stops)) {
        patt <- paste0('\\b', stops[i], '\\b')
        #print(paste("repl",patt,"with",""))
        tmpVec <-gsub(patt, "", tmpVec)
    }
    # concatenate phrases to create 'terms'
    for (i in 1:length(concatTerms)) {
        patt <- paste0('\\b', concatTerms[i], '\\b')
        cpat <- gsub(" ","",concatTerms[i])
        #print(paste("repl",patt,"with",""))
        tmpVec <-gsub(patt, cpat, tmpVec)
    }
    # remove short words
    # tmpVec <- rm_nchar_words(tmpVec, "1,2")
    # Remove numbers
    tmpVec <- removeNumbers(tmpVec)
    # Remove punct
    tmpVec <- removePunctuation(tmpVec)
    # remove extra white space
    tmpVec <- stripWhitespace(tmpVec)
    # remove leading and trailing spaces
    tmpVec <- gsub("^\\s+|\\s+$", "", tmpVec)
    
    tmpVec
}

getBadRows <- function(txtVec, minWordCount) {
    # if abstract is NULL, wordcount will be 0
    #rows <- which(txtVec == "")
    statwords <- strsplit(txtVec, "\\s+")
    chunks.v <- unlist(lapply(statwords, length))
    rows <- which(chunks.v < minWordCount)
    
    rows
}

trainTopicModel <- function(df, numtopics) {
    ids.v <- paste(df$journal,df$year,df$Volume.number,df$Issue,df$page,sep="_")
    ids.v <- gsub(" ","",ids.v)
    chunks.v <- df$abstract
    mallet.instances <- mallet.import(ids.v, chunks.v,
                                      "stoplist.csv",
                                      FALSE,
                                      token.regexp = "[\\p{L}]+")
    topic.model <- MalletLDA(num.topics = numtopics)
    topic.model$model$setRandomSeed(42L) # for repeatability
    topic.model$loadDocuments(mallet.instances)
    
    topic.model$train(400)
    
    topic.words.notNormed.m <- mallet.topic.words(topic.model,
                                                  smoothed = TRUE,
                                                  normalized = FALSE)
    topic.words.notNormed.m <- round(topic.words.notNormed.m)
    vocabulary <- topic.model$getVocabulary()
    colnames(topic.words.notNormed.m) <- vocabulary
    
    out.df <- as.data.frame(topic.words.notNormed.m)
    doc.topics <- mallet.doc.topics(topic.model, F, T)
    
    return(list(out.df,doc.topics))
}


printFrequentTerms <- function(m, num) {
    freqs <- list()
    for(i in 1:dim(m)[1]) {
        tempF <- sort(m[i,], decreasing = TRUE)
        freqs[[i]] <- tempF
        # output top 'num' from each
        txt <- paste(i, " Top 5 words, and their word count")
        writeLines(txt)
        a <- freqs[[i]][1:num]
        print(a)
        writeLines("")
    }
    freqs
}

topones <- function(x,n) {
    return(x[1:n])
}

getIntersectionMatrix <- function(topics, discs, numFreqs,nums) {
    intersections <- data.frame(stringsAsFactors = FALSE)
    commonwords   <- data.frame(stringsAsFactors = FALSE)
    
    # subset based on top numFreqs
    toptopics <- lapply(topics,topones, numFreqs)
    topdiscs  <- lapply(discs, topones, numFreqs)
    for(i in 1:length(toptopics)) {
        for (j in 1:length(topdiscs)) {
            #print(paste(i," ",j))
            thistop <- toptopics[[i]]
            thisdisc <- topdiscs[[j]]
            cwords <- intersect(names(thistop), names(thisdisc))
            numIntersects <- length(cwords)
            intersections[i,j] <- numIntersects
            if (numIntersects > 0) {
                tmp <- data.frame(i,j,numIntersects)
                tmp <- cbind(tmp, cwords)
                commonwords <- rbind(commonwords, tmp)
            }
        }
    }
    colnames(commonwords) <- c("Topic", "Discipline", "Intersects", "Terms")
    colnames(intersections) <- 1:nums
    return(list(intersections, commonwords))
}



