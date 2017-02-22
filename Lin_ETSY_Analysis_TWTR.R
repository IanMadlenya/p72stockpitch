#Nathan Lin
#McIntire Investment Institute Point72 Stock Pitch Competition
#Etsy (ETSY - L)
#TWitter Analysis

#Libraries Needed
library(RTextTools)
library(twitteR)
library(base64enc)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

## Authenticate with the Twitter API

key='bGC6x5vBs5P49MlFgoouP8624'
secret='KUz7MOrF9ek61XsjKQuxynZVwclRgTjZSaatg9JkLQzONFPfsV'
setwd("~/Desktop/DS4559/text_mining_and_web_scraping")
access_token='788807806564573184-rqtUh6kgaceeCh5q2A94NGViNKEY3DJ'
access_token_secret='NWE2fwtk5VOA2Yybo7MOPby5BquSMWMXEnYG791xbf3Nb'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="~/Desktop/text_mining_and_web_scraping/cacert.pem",
              method="curl")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, access_token, access_token_secret)
save(authenticate, file="twitter authentication.Rdata")

# harvest some tweets (8,000 randomly selected Tweets with the keyword 'Etsy')
some_tweets = searchTwitter("Etsy", n=8000, lang="en", retryOnRateLimit=120)

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

names(some_txt) = NULL


col=brewer.pal(6,"Dark2")
wordcloud(some_txt, min.freq=20, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=50, random.order=F,colors=col)

####End of R Code####