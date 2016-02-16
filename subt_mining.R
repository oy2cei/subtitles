#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
library(SnowballC)   
library(tm)   

first2500 <- read.csv("words.csv",header=F)$V1
first2500 <- as.character(first2500)
cname <- file.path("./sub/")  
cname   
dir(cname)  

docs <- Corpus(DirSource(cname, pattern = "*.srt"))   
#summary(docs) 
#inspect(docs[1])

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("<.*>", " ", docs[[j]])
  docs[[j]] <- gsub("$y.", "y ", docs[[j]])
  docs[[j]] <- gsub("$e.", "e ", docs[[j]])   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
 }
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, first2500)
docs <- tm_map(docs, removePunctuation) 

docs <- tm_map(docs, stripWhitespace)   
#docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, PlainTextDocument)  
##==================================================
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   
freq <- colSums(as.matrix(dtm))   
#length(freq)  
#ord <- order(freq)   
#m <- as.matrix(dtm)   
#dim(m)   
#write.csv2(m, file="dtm.csv")   
write.csv2(freq, "freq.csv")







#=====================================остальное не так важно


dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  


freq <- colSums(as.matrix(dtms))   
freq  


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 50)  #кол-во слов? 
write.csv2(freq, "freq.csv")

findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.


wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

library(ggplot2)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 


findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   

findAssocs(dtms, "contrast", corlimit=0.90) # specifying a correlation limit of 0.95   


library(wordcloud)   
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)   


set.seed(142)   
wordcloud(names(freq), freq, max.words=100)  

set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   










#Clustering by Term Similarity