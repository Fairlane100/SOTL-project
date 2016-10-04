# utilities to support the SOTL processing
# SOTLutils.R
library(qdap)
library(tm)
library(mallet)

# none of the char replacement code works.
# Seems because the special chars appear to magically change
# to other special chars when within a function or called.
# something to do with char encoding, but not sure what.
# newchartr <- function(old,charvec) {
#     n <- 1
#     print(old)
#     for (i in tolower(substring(old,seq(1,nchar(old),n),seq(n,nchar(old),n)))) {
#         print(i)
#         charvec <- gsub(i," ",charvec)
#     }
# }

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
    # concat 'non' with the word after it and remove the 'non' word
    c$abstract <- removeNons(c$abstract)
    
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

removeNons <- function(absVec) {
    tmpVec <- absVec
    for (i in 1:length(absVec)) {
        a <- absVec[i]
        aa <- unlist(strsplit(a," "))
        nons <- which(aa=="non")
        if (length(nons) != 0) {
            aa[nons+1] <- paste0("non",aa[nons+1])
            aa <- aa[-nons]
            tmpVec[i] <- paste(aa,collapse=" ")
        }
    }
    tmpVec
}

cleanText <- function(txtVec, stops, equivwords, concatTerms) {
    tmpVec <- txtVec
    
    #to fix non-ASCII
    tmpVec <- iconv(tmpVec, "UTF-8", "ASCII",sub=' ')
    #tmpVec <- str_replace_all(tmpVec,"[^[:graph:]]", " ") 
    # make all lower case
    tmpVec <- tolower(tmpVec)
    stops <- iconv(stops, "UTF-8", "UTF-8",sub=' ')
    stops <- tolower(stops)
    equivwords[[1]] <- iconv(equivwords[[1]], "UTF-8", "UTF-8",sub=' ')
    equivwords[[1]] <- tolower(equivwords[[1]])
    equivwords[[2]] <- iconv(equivwords[[2]], "UTF-8", "UTF-8",sub=' ')
    equivwords[[2]] <- tolower(equivwords[[2]])
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
    #topic.words.notNormed.m <- round(topic.words.notNormed.m) # breaks LDAvis, dont do
    vocabulary <- topic.model$getVocabulary()
    colnames(topic.words.notNormed.m) <- vocabulary
    
    out.df <- as.data.frame(topic.words.notNormed.m)
    doc.topics <- mallet.doc.topics(topic.model, F, T)
    word.freqs <- mallet.word.freqs(topic.model = topic.model)
    return(list(out.df, doc.topics, word.freqs))
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

#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's 
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)
    
    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)
    
    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = doc_length,
                                   term.frequency = freq_matrix$Freq)
    
    return(json_lda)
}

