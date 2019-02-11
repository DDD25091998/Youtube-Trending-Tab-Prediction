#Importing all the libraries
library(corrplot)
library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(Hmisc)
library(sentimentr)
library(shuffle)
library(stringr)
library(syuzhet)
# defining the dataframes 
df1 = na.omit(read_csv("GBvideos.csv"))
df2 = na.omit(read_csv("GBcomments.csv"))

attach(df1)
attach(df2)
#we have to create a subsample for df2 because it is much to large 
set.seed(1)
rows = sample(nrow(df2),1000,replace = T)
df2sample = df2[rows,]

#dropping useless colums 
df1 = df1[1:9]

# Creating a few new features to better quantify the data
feature_creation = function(df){
  df['like_rate'] = df$likes/(df$likes+df$dislikes +1)
  df['dislike_rate'] = df$dislikes/(df$likes+df$dislikes +1)
  df['percent_reacted'] = (df$likes + df$dislikes)/(df$views+1)
  df['percent_commented'] = (df$comment_total)/(df$views+1)
  
  return(df)
}

df1 = feature_creation(df1)

#Performing some initial data visualization to identify outliers: 
boxplot(df1[6:8])
boxplot(df1[10:11])
boxplot(df2[3:4])


# Try to identify basic correlations : 
cor_matrix = cor(df1[6:13])
corrplot(cor_matrix)



# Lets look into this with more detail ( Jamies' hate drives views )

#Lets do some more visualisation with wordclouds 

#We must first clean the 
cleaner <- function(x){
  x = Corpus(VectorSource(x))
  transformed= x %>%
    tm_map(removePunctuation)%>%
    tm_map(removePunctuation)%>%
    tm_map(stripWhitespace)%>%
    tm_map(removeNumbers)%>% 
    tm_map(removeWords,stopwords("en"))%>% 
    tm_map(removeWords, toupper(stopwords("en")))%>% 
    tm_map(removeWords, capitalize(stopwords("en"))) 
  return(transformed)
}

counter_wordcloud <- function(x){
  cleaned = cleaner(x)
  wordcloud(cleaned,scale=c(2,.5), max.words = 50,random.order = F,colors = brewer.pal(8,"Dark2"))
  
  data_tdm = TermDocumentMatrix(cleaned,control = list(tolower = F))
  m = as.matrix(data_tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  print(head(d,20))
  
}
counter <- function(corp) {
  tdm = TermDocumentMatrix(corp,control = list(tolower = F))
  m = as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(head(d,20))
  
}

counter_wordcloud(df1$title)
counter_wordcloud(df1$tags)
counter_wordcloud(df1$channel_title)
#df2 is too large, we need to sample it : 


sampled = cleaner(df2sample$comment_text)
wordcloud(sampled,scale=c(2,.5), max.words = 50,random.order = F,colors = brewer.pal(8,"Dark2"))
counter(sampled)


#If we do some sentiment analysis:
Sentiment <- function(Thing,string) {
  S = get_nrc_sentiment(Thing)
  head(S)
  TotalSentiment <- data.frame(colSums(S[,c(1:10)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
  rownames(TotalSentiment) <- NULL
  
  #total sentiment score of all texts
  ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Total Count") + ggtitle(string)
}

#A function to turn the cleaned corpus into a data frame to be used in sentiment analysis
Data_framer <- function(corpus) {
  df = data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
  
}


clean_titles = cleaner(df1$title)
clean_titles = Data_framer(clean_titles)
Sentiment(clean_titles$text,"GB titles sentiment analysis")

clean_tags = cleaner(df1$tags)
clean_tags = Data_framer(clean_tags)
Sentiment(clean_tags$text,"GB tags sentiment analysis")

clean_channeltitles = cleaner(df1$channel_title)
clean_channeltitles = Data_framer(clean_channeltitles)
Sentiment(clean_channeltitles$text,"GB channel titles sentiment analysis")


clean_comments = cleaner(df2sample$comment_text)
clean_comments = Data_framer(clean_comments)
Sentiment(clean_comments$text,"GB comments sentiment analysis")



#Polarity analysis 
Sentences = get_sentences(clean_titles$text)
S = sentiment(Sentences)

df1 = data.frame(df1,S$sentiment)

clean_comments_full = cleaner(df2$comment_text)
clean_comments_full = Data_framer(clean_comments_full)
Sentences = get_sentences(clean_comments_full$text)
S = sentiment(Sentences)

df2 = data.frame(df2,S$sentiment)


#lets try some regression : 
summary(df1$S.sentiment)

summary(df2$S.sentiment)

# First we can see that the scores for both are very different : 
# the polarity expressed in comments is much larger-ranging than the one expressed in titles 



mod1a = glm(likes~. -video_id -title -channel_title -category_id -tags -likes -dislikes -comment_total, family = gaussian, data = df1)
summary(mod1a)#Sentiment polarity is a good predictor of the number of likes suggesting that 

mod1b = glm(percent_commented~. -video_id -title -channel_title -category_id -tags -likes -dislikes -comment_total, family = gaussian, data = df1)
summary(mod1b)




mod2 = glm(likes~. -video_id -comment_text, family = gaussian, data = df2)
summary(mod2)
mod3 = glm(replies~. -video_id -comment_text, family = gaussian, data = df2)
summary(mod3)
mod4 = glm(S.sentiment~. -video_id -comment_text, family = gaussian, data = df2)
summary(mod4)

#sentiment seems to be a very good predictor for the number of likes 

detach(df1)
detach(df2)






