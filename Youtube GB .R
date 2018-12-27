library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(wordcloud2)
GBcomments = read_csv("GBcomments.csv")
GBvideos = read_csv("GBvideos.csv")

names(GBcomments)[1]<- "comment_ID"
names(GBcomments)[3] = "comment_likes"

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
#first a transformer : 

set.seed(1)
transformer <- function(text){
  corpus<- Corpus(VectorSource(text))
  
  transformer<- corpus %>% tm_map(tolower) %>% 
    tm_map(stripWhitespace)%>% 
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers) %>% 
    tm_map(removeWords,stopwords('english'))
    
}
title_classification = transformer(newGB$title)
wordcloud(title_classification,min.freq = 100,scale=c(2,.5), max.words = 100,random.order = TRUE,colors = brewer.pal(8,"Dark2"))

comment_classification = transformer(GBcomments$comment_text)
wordcloud(comment_classification,min.freq = 100,scale=c(2,.5), max.words = 100,random.order = TRUE,colors = brewer.pal(8,"Dark2"))







detach(GBcomments)
detach(GBvideos)
detach(newGB)



