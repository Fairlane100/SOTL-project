## Calculate stats of the documentMetaData.csv contents.
# report stats in some form.

#doc.df <- read.csv(file = "documentMetaData.csv", stringsAsFactors = FALSE)
count <- dim(doc.df)[1]
tna <- length(which(is.na(doc.df$title)))
ana <- length(which(is.na(doc.df$authors)))
yna <- length(which(is.na(doc.df$year)))
sna <- length(which(is.na(doc.df$src)))
ina <- length(which(is.na(doc.df$id)))
kna <- length(which(is.na(doc.df$keywords)))
bna <- length(which(is.na(doc.df$abstract)))

str <- paste("number of documents:",count,"\n",
      "missing titles:",tna,"\n",
      "missing authors:",ana,"\n",
      "missing year:",yna, "\n",
      "missing source:",sna,"\n",
      "missing id:",ina,"\n",
      "missing keywords:",kna,"\n",
      "missing abstract:",bna)
cat(str)