#tweets <- read.csv("AA+.csv",encoding = "UTF-8", header = TRUE,stringsAsFactors = FALSE)
tweets <- read_excel("/Volumes/GoogleDrive/Meine Ablage/Manu Master/Datasetforlabelling/SS-.xlsx")

library(tm)
library(textclean)
library(stringr)
# build textcleaner function

textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    #replace_emoji() %>% # replace emoji to words
    #replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim()%>% # removes whitespace from start and end of string
    replace_non_ascii(remove.nonconverted = TRUE)%>%
    replace_white()
  mystopwords <- c(stopwords("english"))
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, mystopwords)%>%
    tm_map(removeWords, c("tone","indices","none","skin","urls","red_heart","false","n","str","id_str","hand_medium","background_image","folded_hands","flexed_biceps","url","tongue","sticking","tongue_sticking","e_tongue","url","url_e","url_profile","vaccinated","vaccination","first","second","get","got","doses","dose","covidvaccines","million","covidvaccine","coronavirus","sinovac", "biontech","covishield","sputnikv","vaccines","covid","pfizer","dose","dose ","amp","moderna","vaccine","vaccin","pfizerbiontech","sinopharm","index","covaxin","covidvaccination","astrazeneca","covidvaccin"))
  
  # convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
  
}

dtm_5 <- textcleaner(tweets$text)



freqterm_5 <- findFreqTerms(dtm_5,5)
freqterm_5
dtm_5 <- dtm_5[,freqterm_5]
rownum_5 <- apply(dtm_5,1,sum)
dtm_5 <- dtm_5[rownum_5>0,]
lda_5 <- LDA(dtm_5,k = 6,control = list(seed = 1502))
library(tidytext)
library(tidyverse)

topic_5 <- tidy(lda_5,matrix = "beta")
top_terms_5 <- topic_5 %>%
  group_by(topic) %>%
  top_n(5,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_5 <- top_terms_5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_5


textcleaner_2 <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    #replace_emoji() %>% # replace emoji to words
    #replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    #replace_date(replacement = "") %>% # remove date
    #replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim()%>% # removes whitespace from start and end of string
    replace_non_ascii(remove.nonconverted = TRUE)%>%
    replace_white()
  return(as.data.frame(x))
  
}

# apply textcleaner function. note: we only clean the text without convert it to dtm
clean_5 <- textcleaner_2(tweets$text)
clean_5 <- clean_5 %>% mutate(id = rownames(clean_5))

# crete dtm
library(textmineR)
set.seed(1502)
dtm_r_5 <- CreateDtm(doc_vec = clean_5$x,
                     doc_names = clean_5$id,
                     ngram_window = c(1,2),
                     stopword_vec = c(stopwords("en"), "tone","indices","none","skin","urls","red_heart","false","n","str","id_str","hand_medium","background_image","folded_hands","flexed_biceps","url","tongue","sticking","tongue_sticking","e_tongue","url","url_e","url_profile","vaccinated","vaccination","first","second","get","got","doses","dose","covidvaccines","million","covidvaccine","coronavirus","sinovac", "biontech","covishield","sputnikv","vaccines","covid","pfizer","dose","dose ","amp","moderna","vaccine","vaccin","pfizerbiontech","sinopharm","index","covaxin","covidvaccination","astrazeneca","covidvaccin"),
                     lower = TRUE,
                     verbose = F)

dtm_r_5 <- dtm_r_5[,colSums(dtm_r_5)>2]

set.seed(1502)
mod_lda_5 <- FitLdaModel(dtm = dtm_r_5,
                         k = 20, # number of topic
                         iterations = 500,
                         burnin = 180,
                         alpha = 0.1,beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

mod_lda_5$r2
plot(mod_lda_5$log_likelihood,type = "l")

mod_lda_5$top_terms <- GetTopTerms(phi = mod_lda_5$phi,M = 15)
data.frame(mod_lda_5$top_terms)

mod_lda_5$coherence

mod_lda_5$prevalence <- colSums(mod_lda_5$theta)/sum(mod_lda_5$theta)*100
mod_lda_5$prevalence

mod_lda_5$summary <- data.frame(topic = rownames(mod_lda_5$phi),
                                coherence = round(mod_lda_5$coherence,3),
                                prevalence = round(mod_lda_5$prevalence,3),
                                top_terms = apply(mod_lda_5$top_terms,2,function(x){paste(x,collapse = ", ")}))

modsum_5 <- mod_lda_5$summary %>%
  `rownames<-`(NULL)
modsum_5


modsum_5 %>% pivot_longer(cols = c(coherence,prevalence)) %>%
  ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(title = "Die besten Topics anhand des Kohärenz -und Prävalenzwertes",subtitle = "Tweets aus SS mit negativer Valenz", 
       x = "Topics", y = "Wert")+ theme(plot.title=element_text(hjust = 0.5,face = "bold"))+theme(plot.subtitle=element_text(hjust = 0.5,face = "bold"))


mod_lda_5$linguistic <- CalcHellingerDist(mod_lda_5$phi)
mod_lda_5$hclust <- hclust(as.dist(mod_lda_5$linguistic),"ward.D")
mod_lda_5$hclust$labels <- paste(mod_lda_5$hclust$labels, mod_lda_5$labels[,1])
plot(mod_lda_5$hclust)

y=modsum_5 %>% 
  arrange(desc(coherence)) %>%
  slice(1:3)

library(writexl)
write_xlsx(y,"/Volumes/GoogleDrive/Meine Ablage/Manu Master/Datasetforlabelling/SS-topic.xlsx")
#print(xtable(y,type="latex"),)
#write.table(tab,america.txt)