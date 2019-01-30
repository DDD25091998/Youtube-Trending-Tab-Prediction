library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(Hmisc)
library(shuffle)
library(stringr)



#Loading dataset and cleaning 
GBcomments = na.omit(read_csv("GBcomments.csv"))
GBvideos = na.omit(read_csv("GBvideos.csv"))

names(GBcomments)[1]<- "comment_ID"
names(GBcomments)[3] = "comment_likes"

sum(is.na(GBvideos))
sum(is.na(GBcomments))


#


attach(GBcomments)
attach(GBvideos)


cor(comment_total,likes)
cor(comment_total,dislikes)

#likes_n_dislikes = data.frame(likes+dislikes)
#GBvideos = data.frame(GBvideos[1:8],likes_n_dislikes,GBvideos[9:11])

like_ratio = data.frame(likes/(likes + dislikes+1))
outspoken_public = data.frame((comment_total)/(likes + dislikes+1))#ratio of comments to likes and dislikes 
controversy_likes = data.frame(views/(likes + dislikes+1))#proportion of viewership that voted 
controversy_comments = data.frame(views/(comment_total+1))#proportion of viewership that commented

cor(outspoken_public,views)
cor(controversy_likes,controversy_comments)#the more people vote, the more people tend to comment, vice versa ? 

newGB = data.frame(GBvideos,controversy_comments,controversy_likes,like_ratio,outspoken_public)
z = c("controversy_comments","controversy_likes","like_ratio","outspoken_public")


names(newGB)[12] = z[1]
names(newGB)[13] = z[2]
names(newGB)[14] = z[3]
names(newGB)[15] = z[4]


attach(newGB)

naivemod = glm(views~. -video_id -title -channel_title -category_id -tags -thumbnail_link, family = gaussian, data = newGB)
summary(naivemod)#simple linear regression, likes ratio quite effective

mod1 = glm(views~. -comment_total - likes -dislikes - controversy_comments -date -outspoken_public -controversy_likes -video_id -title -channel_title -category_id -tags -thumbnail_link, family = gaussian, data = newGB)
summary(mod1)#likes ratio is not very significant as a predictor


mod2 = glm(like_ratio~. -like_ratio -likes -dislikes -video_id -title -channel_title -category_id -tags -thumbnail_link, family = gaussian, data = newGB)
summary(mod2)#likes ratio seems to be pushed up by the number of views ? are more viewed videos more appreciated ??

#text analysis 
# I. visualizing most used words 

#lets define a function 
#a transformer that takes text as an input and converts it into a  corpus, removes stopwords, capital letters, numbers and whitespace: 

set.seed(1)
transformer <- function(text){
  corpus<- Corpus(VectorSource(text))
  
  transformer<- corpus %>% #tm_map(tolower) %>% 
    tm_map(stripWhitespace)%>% 
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers)%>%  
    tm_map(removeWords,stopwords("en"))%>%
    tm_map(removeWords, toupper(stopwords("en")))%>%
    tm_%>%
    tm_map(removeWords, capitalize(stopwords("en"))) #%>%tm_map(removeWords, capitalize(c("official","video","trailer","makeup","new","first","live","halloween","music","day")))
    
}

# The two most used words appear much more often in ALL CAPS than in lower case or just wifth normal capitalization : suggesting that indeed video attractiveness is bazsed on "sight effect" 
#naive official : 707
# non cap official : 689
#capped official : 50
#OFFICIAL 657 --> 1300% more • VIDEO : 422 --> 1200% more 
#official 18 • video : 16
#Official 32 • Video : 19 
#How to show this : base way with calculus 
#Stupid way with regex : about 23% more all caps than non all caps --> this would suggest that the most used words are also disproportionately more likelly to be all capped 
sum(str_count(GBvideos$title,"\\b[a-z]+\\b"))
sum(str_count(GBvideos$title,"\\b[A-Z]+\\b"))


#Counter of the number of occurences of most used strings
counter <- function(corp) {
  tdm = TermDocumentMatrix(corp)
  m = as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d,20)

}


title_classification = transformer(newGB$title)
wordcloud(title_classification,scale=c(2,.5), max.words = 50,random.order = F,colors = brewer.pal(8,"Dark2"))
counter(title_classification)

tag_classification = transformer(newGB$tags)
wordcloud(tag_classification,scale=c(2,.5), max.words = 50,random.order = TRUE,colors = brewer.pal(8,"Dark2"))
counter(tag_classification)


channel_classification = transformer(newGB$channel_title)
wordcloud(tag_classification,scale=c(2,.5), max.words = 50,random.order = TRUE,colors = brewer.pal(8,"Dark2"))
counter(channel_classification)

#WE HAVE TO SHUFFLE THE DATASET BECAUSE IT IS TOO BIG FOR THE COMPUTER TO COMPUTE EFFICIENTLY
set.seed(1)
rows = sample(nrow(GBcomments),200000,replace = T)
GB_sample = GBvideos[rows,]
GB_sample_classification = transformer(GB_sample)

wordcloud(GB_sample_classification,scale=c(2,.5), max.words = 50,random.order = TRUE,colors = brewer.pal(8,"Dark2"))
counter(GB_sample_classification)
###########################################


detach(GBcomments)
detach(GBvideos)
detach(newGB)






