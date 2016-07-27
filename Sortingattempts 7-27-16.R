#sorting corpus based on word frequencies
# need to have word.freqs file loaded
#do two sorts based on term frequency and doc frequency

str(word.freqs)
str(termfreqs)
sorted.wordfreqs.termfreq <- word.freqs[order(-word.freqs$term.freq), ]
write.csv (sorted.wordfreqs.termfreq, file ="sorted.wordfreqs.termfreq",)

sorted.wordfreqs.termfreq <- word.freqs[order(-word.freqs$doc.freq), ]
write.csv (sorted.wordfreqs.termfreq, file ="sorted.wordfreqs.docfreq.csv",)