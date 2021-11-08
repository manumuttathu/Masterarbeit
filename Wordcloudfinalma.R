library(corpus)
library(tm)

tweets <- read.csv("/Volumes/GoogleDrive/Meine Ablage/Manu Master/Datasetforlabelling/final/Orient/finalOrienttrust.csv",encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)

# build a corpus, and specify the source to be character vectors
myCorpus <- VCorpus(VectorSource(tweets$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
removewhitespace <- function(x) gsub("^[[:space:]]*","",x) 
removetrailing <- function(x) gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
removeextra <- function(x)gsub(' +',' ',x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??

myCorpus <- tm_map(myCorpus, stripWhitespace)

# add two extra stop words: 'available' and 'via'
#myStopwords <- c(stopwords("english"), "doses","dose", "covid","vaccins", "vaccin", "astrazeneneca", "pfizer",'get', 'can', 'covid', 'amp','will','pfizer', 'vaccine', 'moderna', 'biontech', 'covaxin', 'covishield', 'sputnik', 'sputnikV', 'sinopharm', 'astrazeneca', 'johnsonjohnson', "dose","countri","vaccin", "sinovac", "covidvaccin", "pfizerbiontech", "first", "shot", "second", "vaccin","covidvaccin", "sputnikv")
myStopwords <- c(stopwords("english"),"background","image","color","tone","indices","none","skin","urls","red_heart","false","n","str","id_str","hand_medium","background_image","folded_hands","flexed_biceps","url","tongue","sticking","tongue_sticking","e_tongue","url","url_e","url_profile","vaccinated","vaccination","first","second","get","got","doses","dose","covidvaccines","million","covidvaccine","coronavirus","sinovac", "biontech","covishield","sputnikv","vaccines","covid","pfizer","dose","dose ","amp","moderna","vaccine","vaccin","pfizerbiontech","sinopharm","index","covaxin","covidvaccination","astrazeneca","covidvaccin")


# remove 'r' and 'big' from stopwords
#myStopwords <- setdiff(myStopwords, c("r", "big","vaccin","dose"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words

library(wordcloud)
require(dplyr)
#dtm <- DocumentTermMatrix(myCorpus)

dtm <- TermDocumentMatrix(myCorpusCopy)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)

library(wordcloud2)
#colour <- "#E43054-anger;#F2993A-anticipation; #FADB4D-joy;#99C933-trust;#35A450-fear;#9F78BA-disgust;#729DC9-sadness; #3FA5C0-surprise"
#colour <- "#E43054" #anger
#colour <- "#F2993A" #anticipation
#colour <- "#FADB4D" #joy
colour <- "#99C933" #trust
#colour <- "#35A450" #fear
#colour <- "#9F78BA" #disgust
#colour <- "#729DC9" #sadness
#colour <- "#3FA5C0" #surprise

#figPath <- "twitter.jpg"
wordcloud2(data=d,size=1.5,color=colour,backgroundColor="white")