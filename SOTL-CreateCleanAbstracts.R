library(tm)
library(qdap)

source("SOTLutils.R")

infile <- "Combined clean  6-13-16.csv"
stopfile <- "SOTL-stoplist.csv"
equivfile <- "SOTL-equivalencies.csv"
termConcatfile <- "SOTL-termConcat.csv"

performCleaning(infile,stopfile,equivfile,termConcatfile)