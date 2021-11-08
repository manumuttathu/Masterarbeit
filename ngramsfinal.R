# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(tm)
library(textmineR)
library(RWeka)
library(wordcloud)
rt <- read.csv("/Volumes/GoogleDrive/Meine Ablage/Manu Master/Datasetforlabelling/final/Orient/finalOrientsurprise.csv")
rt$text <- sapply(rt$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

#convert some_txt into a corpus and remove stopwords and punctuations
corp=VCorpus(VectorSource(rt$text))
corp=tm_map(corp, removeWords, stopwords('english'))
corp <- tm_map(corp, removePunctuation) 

Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
  gsub("@\\w+", "", x)  #only removes the '@'
  gsub('"', '', x)
  gsub('\" \"', '', x)
  gsub('\\n\n', '', x)
  gsub('\\n', '', x)
  gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b","",x)#email format
  
}
mystopwords <- c(stopwords("english"),"backhand","index","rt","íí","get","like","just","yes","know","will","good","day","people")

#remove stopwords
corp <- tm_map(corp,removeWords,mystopwords)
corp <- tm_map(corp,Textprocessing)
corp

minfrq <-100
#create the bigrams and Term document matrix
BigramTokenizer <- function(y) NGramTokenizer(y, Weka_control(min = 2, max = 2))
#tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
#m= inspect(tdm)
token_delim <- " \\t\\r\\n.!,?;()"
bitoken = NGramTokenizer(corp, Weka_control(min=2,max=2,delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <-two_word[order(two_word$Freq,decreasing = TRUE),]
#sort_three <-three_word[order(three_word$Freq,decreasing = TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq, max.words = 500,scale=c(2,0.5),min.freq = minfrq,rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8, "Dark2") )
summary(warnings())
