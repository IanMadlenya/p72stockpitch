#Nathan Lin
#McIntire Investment Institute Point72 Stock Pitch Competition
#Etsy (ETSY - L)
#TWitter Analysis

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

## Authenticate

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

# harvest some tweets
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

## Here we perform some sentiment analysis:

## First we inidicate lists of positive and negative words. These are located in your 
##"Day 14" Resources folder

positives= readLines("D:/Second Year/DS 4559 - Data Science/Sentiment Analysis/negative_words.txt")
negatives= readLines("D:/Second Year/DS 4559 - Data Science/Sentiment Analysis/positive_words.txt")

## Below is a function that scores sentiment on a scale of -5 to 5 (-5 being the most negative
## and 5 being the most positive).  A score is determined for each tweet based on its correlation
## with the positive words and the negative words.  This is original code written by a veteran R
##user that functions as part of an old package called "sentiment" that is no longer available.

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

## Now apply the function to our actual data.  

Score <- score.sentiment(some_txt,positives,negatives,.progress='none')

## Score has two fields: score and text.  We 
## are interested in score at this point, but we can look at a few of the tweets' text and
## the associated score first.

head(Score)

## Let’s plot a histogram of the sentiment score:

hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have 'Date' in Cville ",
     border="black",col="skyblue")

## We can calculate overall sentiment by adding together all of the scores:

sum(Score$score)


## Let's take a look at the emotional content of the tweets:

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("D:/Second Year/DS 4559 - Data Science/Sentiment Analysis/emotions.csv.gz",header=FALSE)
  ##lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("D:/Second Year/DS 4559 - Data Science/Sentiment Analysis/subjectivity.csv.gz",header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
library(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion Categories", y="Tweets") + ggtitle("Emotional Categories of Tweets")


## Make a plot of the overall polarity of the tweets:

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) + ggtitle("Polarity of Tweets") + xlab("Polarity") + ylab("Count")

## Finally, you can convert to a document term matrix and look for frequencies and associations:

text.corpus <- Corpus(VectorSource(some_txt))
dtm <- DocumentTermMatrix(text.corpus)