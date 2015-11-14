# utility functions for preprocessing

# input is a 'link' which is an element of the header that is returned
# from a call to https://api.mendeley.com/documents
# we want to extract the type of link, and if it is a 'next' return TRUE, else FALSE
is.nextLink <- function(link) {
    rel <- sub("^.*>;","",link) 
    rel <- sub(" rel=\"","",rel)
    rel <- sub("\"","",rel)
    if (rel == "next") return(TRUE)
    else return(FALSE)
}

# input is a 'link' which is an element of the header that is returned
# from a call to https://api.mendeley.com/documents
# we want to extract the url to send next - it is paged, and we need next page
getUrlFromLink <- function(link) {
    # assume input is correct!
    url <- sub("<","",documents$headers$link)
    url <- sub(">.*$","",url)
    url
}

# input a list of documents from Mendeley API
# Walk through the list, create a data frame of interesting parts
# each row is a document
# cols are title, authors, year, source, id, keywords, abstract
# lists are collapsed to strings for authors and keywords
doclistToDataframe <- function(docs.l) {
    newdocs.df <- data.frame()
    for (i in 1:length(docs.l)) {
        newdf <- data.frame(title=NA,authors=NA,year=NA,src=NA,id=NA,
                            keywords=NA,abstract=NA,stringsAsFactors = FALSE)
        if(!is.null(docs.l[[i]]$title)) newdf$title=docs.l[[i]]$title
        if(!is.null(docs.l[[i]]$authors)) newdf$authors=paste(unlist(docs.l[[i]]$authors),collapse = " ")
        if(!is.null(docs.l[[i]]$year)) newdf$year=docs.l[[i]]$year
        if(!is.null(docs.l[[i]]$source)) newdf$src=docs.l[[i]]$source
        if(!is.null(docs.l[[i]]$id)) newdf$id=docs.l[[i]]$id
        if(!is.null(docs.l[[i]]$keywords)) newdf$keywords=paste(docs.l[[i]]$keywords,collapse=" ")
        if(!is.null(docs.l[[i]]$abstract)) newdf$abstract=docs.l[[i]]$abstract
                    
        newdocs.df <- rbind(newdocs.df, newdf)
    }
    newdocs.df
}