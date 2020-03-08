---
  title: "Youtube Topic and Comment Analysis </br> The Tonight Show "
author: "Doris Kuo"
date: "2/27/2020"
output: ioslides_presentation
---
  

library(rvest)
library(tidyverse)
library(tidytext)
library(tesseract)
library(pdftools)
library(wordcloud2)
library(tm)
library(stringr)
library(textstem)
library(dplyr)
library(sentimentr)
library(ggplot2)
library(gridExtra)
library(textstem)
library(stm)


## Agenda
#1. Webscrapping:  Get information of the latest 100 Video
#Video Link, Title, Description, Views, Likes, Dislikes, Comments

#2. Word Cloud: Most popular topic of title/discription and comments

#3. Text Sentiment: Whether the sentiment of title/discription is related to views?
  
#  4. Topic Model: Highly discussed topics in comments of the most liked/disliked videos?
  
  # 1. Webscrapping 
  
  ## 
  
#1. Get the latest 100 Video Links ( R, rvest package)
#2. Get Title, Description, Views, Likes, Dislikes, Comments by the links (Python, BeautifulSoup & Selenium)

# read url of the YT channel
YoutubeHTML <- read_html("https://www.youtube.com/user/latenight/videos") 
YoutubeHTML 

#title
title <- YoutubeHTML %>% 
  html_nodes("h3 a") %>% 
  html_text()

#link
link <- YoutubeHTML %>% 
  html_nodes("h3 a") %>% 
  html_attr("href")

#combine title and link to dataframe
df <- data.frame(matrix(ncol = 2, nrow = 30))
x <- c('Title','Link')
colnames(df) <- x

df$Title <- title
df$Link <- link
df<-df%>%
  mutate(Link = paste0("https://www.youtube.com",Link))

head(df)
write.csv(df, "tempdf.csv")


```{python, echo= FALSE, eval = FALSE}
import urllib.request
import urllib.parse
import urllib.error
from bs4 import BeautifulSoup
import ssl
import json
import ast
import json
import os
from urllib.request import Request, urlopen
import pandas as pd
df = pd.read_csv("tempdf.csv",encoding = "ISO-8859-1")
df2 = pd.DataFrame(columns=["LINK",'TITLE','NUMBER_OF_VIEWS','LIKES','DISLIKES','COMMENTS'])
df2["LINK"] = df["Link"]
# For ignoring SSL certificate errors

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE
# Creating a BeautifulSoup object of the html page for easy extraction of data.
# Input from user

for i in range(100):
  url = df2.iloc[i,0]
# Making the website believe that you are accessing it using a mozilla browser

req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
webpage = urlopen(req).read()
soup = BeautifulSoup(webpage, 'html.parser')
html = soup.prettify('utf-8')
video_details = {}

for span in soup.findAll('span',attrs={'class': 'watch-title'}):
  df2.iloc[i,1] = span.text.strip()

#for span in soup.findAll('span',attrs={'class': 'style-scope ytd-video-primary-info-renderer'}):
#df2.iloc[i,2] = span.text.strip()    

#for script in soup.findAll('script',attrs={'type': 'application/ld+json'}):
#channelDesctiption = json.loads(script.text.strip())
#df2.iloc[i,3] = channelDesctiption['itemListElement'][0]['item']['name']

for div in soup.findAll('div',attrs={'class': 'watch-view-count'}):
  df2.iloc[i,2] = div.text.strip()

for button in soup.findAll('button',attrs={'title': 'I like this'}):
  df2.iloc[i,3] = button.text.strip()

for button in soup.findAll('button',attrs={'title': 'I dislike this'}):
  df2.iloc[i,4] = button.text.strip()
import time
from selenium.webdriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
for i in range(100):
  url = df2.iloc[i,0]
commentlist = []
with Chrome() as driver:
  
  wait = WebDriverWait(driver,20)
driver.get(url)

for item in range(3): #by increasing the highest range you can get more content
  wait.until(EC.visibility_of_element_located((By.TAG_NAME, "body"))).send_keys(Keys.END)
time.sleep(3)

for comment in wait.until(EC.presence_of_all_elements_located((By.CSS_SELECTOR, "#comment #content-text"))):
  commentlist.append(comment.text)
df2.iloc[i,6] =  wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"div#description yt-formatted-string"))).text
df2.iloc[i,5] = commentlist
df2.to_csv("YT_50.csv")
```



knitr::include_graphics('df.jpg')




YT <- read.csv("YT_100.csv")
head(YT)



# 2. Word Cloud: </br> Most popular topic of title/discription and comments



## Title and Description
#Chatting, Peforming, Music, Dance


YT <- read.csv("YT_100.csv")

YT<- YT[-c(3,4,5,6,7), ]  
YT_title <- YT$TITLE
YT_descipt <- YT$DESCRIPTION

#get all list in the writtenstatement together
YT_titledescipt<-paste(YT_title,YT_descipt,collapse = "")

YT_titledescipt<-YT_titledescipt%>%
  str_replace_all("Â","")%>%
  str_replace_all("Subscribe NOW to The Tonight Show Starring Jimmy Fallon:","")%>%
  str_replace_all("The Tonight Show Starring Jimmy Fallon","")%>%
  str_replace_all("Jimmy","")%>%
  gsub("([a-z])([A-Z])", "\\1 \\2",.)%>%
  tolower(.)%>%
  str_replace_all("â|ÿ|ã|¢|???|T","")%>%
  str_replace_all("tonight","")%>%
  str_squish(.)%>%
  removeWords(c("'", stopwords(kind = "en")))%>%
  removePunctuation(.)%>%
  removeNumbers(.)%>%
  lemmatize_strings(.)

#get the tokens of the document
tokens = data_frame(text = YT_titledescipt) %>% 
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)
tokens%>%
  filter(n > 2) %>% 
  na.omit() %>% 
  wordcloud2(shape = "cardioid")


## Comments
#Express love, Mention Jimmy Fallon, Discuss songs/ movie


YT_comment <- YT$COMMENTS

#get all list in the writtenstatement together
YT_comment<-paste(YT_comment,collapse = "")

YT_comment<-YT_comment%>%
  str_replace_all("\\[","")%>%
  gsub("([a-z])([A-Z])", "\\1 \\2",.)%>%
  gsub("[^A-Za-z0-9 ]","",.)%>%
  tolower(.)%>%
  str_replace_all("jimmy","")%>%
  str_replace_all("tonight","")%>%
  str_squish(.)%>%
  removeWords(c("'", stopwords(kind = "en")))%>%
  removePunctuation(.)%>%
  removeNumbers(.)%>%
  lemmatize_strings(.)

#get the tokens of the document
tokens2 = data_frame(text = YT_comment) %>% 
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

tokens2%>%
  filter(n > 30) %>% 
  na.omit() %>% 
  wordcloud2(shape = "cardioid")

knitr::include_graphics('cloud.jpg')


# 3. Text Sentiment: </br> Whether the sentiment of title/discription is related to views?


#get positive and negative words separately
nrcWord <- textdata::lexicon_nrc()
nrcWord


## Choose Sentiment

#Positive: positive, trust, joy
#  Negative: negative, anger, sadness, disgust


nrcWord_positive <- nrcWord%>%
  filter(sentiment=="trust" |sentiment=="positive"|sentiment=="joy")
nrcValues <- lexicon::hash_sentiment_nrc
positive_score <- nrcValues[nrcValues$x %in% nrcWord_positive$word,]

nrcWord_negative <- nrcWord%>%
  filter(sentiment=="negative" |sentiment=="anger"|
           sentiment=="sadness"|sentiment=="disgust")
negative_score <- nrcValues[nrcValues$x %in% nrcWord_negative$word,]

YT$titledescipt<-paste0(YT_title,YT_descipt,sep = "-")

YT$titledescipt<-YT$titledescipt%>%
  str_replace_all("Â","")%>%
  str_replace_all("Subscribe NOW to The Tonight Show Starring Jimmy Fallon:","")%>%
  str_replace_all("The Tonight Show Starring Jimmy Fallon","")%>%
  str_replace_all("Jimmy","")%>%
  gsub("([a-z])([A-Z])", "\\1 \\2",.)%>%
  tolower(.)%>%
  str_replace_all("â|ÿ|ã|¢|???|T","")%>%
  str_replace_all("tonight","")%>%
  str_squish(.)%>%
  removeWords(c("'", stopwords(kind = "en")))%>%
  removePunctuation(.)%>%
  removeNumbers(.)%>%
  lemmatize_strings(.)

positive_score_Sentiment <- sentiment(get_sentences(YT$titledescipt), 
                                      polarity_dt = positive_score) %>% 
  group_by(element_id) %>% 
  summarize(nrc_meanpositivescore = mean(sentiment))
head(positive_score_Sentiment)


negative_score_Sentiment <- sentiment(get_sentences(YT$titledescipt), 
                                      polarity_dt = negative_score) %>% 
  group_by(element_id) %>% 
  summarize(nrc_meannegativescore = mean(sentiment))
head(negative_score_Sentiment)

nrc_score_df= merge(positive_score_Sentiment,negative_score_Sentiment,by="element_id")
nrc_score_df<- nrc_score_df%>%
  mutate(nrc_totalscore = nrc_meanpositivescore+nrc_meannegativescore)
nrc_score_df

#add score the original df
YT_score <- cbind(YT, nrc_score_df)
YT_score<-YT_score%>%
  select(-element_id)%>%
  mutate(NUMBER_OF_VIEWS = str_replace_all(NUMBER_OF_VIEWS, " views|,",""),
         NUMBER_OF_VIEWS = as.numeric(NUMBER_OF_VIEWS))



## Use positive titles/discriptions more. 


YT_score %>%
  ggplot(aes(nrc_totalscore, NUMBER_OF_VIEWS)) + 
  geom_point(size = 4, color = "#F6C200", alpha=0.3)+
  theme_minimal()



## But Negative titles/discriptions will slightly drive more views.


YT_score %>%
  ggplot(aes(nrc_totalscore, NUMBER_OF_VIEWS)) + 
  geom_point(size = 4, color = "#F6C200", alpha=0.3)+
  geom_smooth(method = "lm", color="#D99900")+
  theme_minimal()



# 4. Topic Model: </br> Highly discussed topics in comments of the most liked/disliked videos?

## Get top 10 liked video



YT_score <- YT_score%>%
  mutate(LIKES = str_replace_all(LIKES, ",","")
         ,LIKES = as.numeric(LIKES))%>%
  mutate(DISLIKES = str_replace_all(DISLIKES, ",","")
         ,DISLIKES = as.numeric(DISLIKES))%>%
  mutate(LIKES_PROP = LIKES/NUMBER_OF_VIEWS)%>%
  mutate(DISLIKES_PROP = DISLIKES/NUMBER_OF_VIEWS)


YT_score$COMMENTS<-YT_score$COMMENTS%>%
  gsub("([a-z])([A-Z])", "\\1 \\2",.)%>%
  gsub("[^A-Za-z0-9 ]","",.)%>%
  tolower(.)%>%
  str_replace_all("â|ÿ|ã|¢|???|T","")%>%
  str_replace_all("jimmy|jimin|jin","")%>%
  str_squish(.)%>%
  removeWords(c("'", stopwords(kind = "en")))%>%
  removePunctuation(.)%>%
  removeNumbers(.)%>%
  lemmatize_strings(.)


topten_likes <- YT_score%>%
  arrange(desc(LIKES_PROP))%>%
  select(COMMENTS,LIKES_PROP)%>%
  head(10)

topten_dislikes <- YT_score%>%
  arrange(desc(DISLIKES_PROP))%>%
  select(COMMENTS,DISLIKES_PROP)%>%
  head(10)


knitr::include_graphics('likes.jpg')




## Choose #topic = 2



#Topic Model
#Likes
set.seed(1001)


LikesCommentText = textProcessor(documents = topten_likes$COMMENTS, 
                                 metadata = topten_likes, 
                                 stem = FALSE)
LikesCommentPrep = prepDocuments(documents = LikesCommentText$documents, 
                                 vocab = LikesCommentText$vocab,
                                 meta = LikesCommentText$meta)
kTest = searchK(documents = LikesCommentPrep$documents, 
                vocab = LikesCommentPrep$vocab, 
                K = c(2,3, 4, 5, 10), verbose = FALSE)

plot(kTest)


## Topics


likes_topics2 = stm(documents = LikesCommentPrep$documents, 
                    vocab = LikesCommentPrep$vocab, seed = 1001,
                    K = 2, verbose = FALSE)

plot(likes_topics2)


## BTS Dance Party / energetic / dance / songs / he's my bias(husband)


labelTopics(likes_topics2)




## Get top 10 disliked video


knitr::include_graphics('dislikes.jpg')

## Choose #topic = 2


set.seed(1001)


DisLikesCommentText = textProcessor(documents = topten_dislikes$COMMENTS, 
                                    metadata = topten_dislikes, 
                                    stem = FALSE)
DisLikesCommentPrep = prepDocuments(documents = DisLikesCommentText$documents, 
                                    vocab = DisLikesCommentText$vocab,
                                    meta = DisLikesCommentText$meta)
kTest = searchK(documents = DisLikesCommentPrep$documents, 
                vocab = DisLikesCommentPrep$vocab, 
                K = c(2,3, 4, 5, 10), verbose = FALSE)

plot(kTest)


## Topics


dislikes_topics2 = stm(documents = DisLikesCommentPrep$documents, 
                       vocab = DisLikesCommentPrep$vocab, seed = 1001,
                       K = 2, verbose = FALSE)

plot(dislikes_topics2)




## Comments' Sentiment is not highly relevant to the dislike rate. Probably the reason is more about the topic itself.


labelTopics(dislikes_topics2)



# Thanks

