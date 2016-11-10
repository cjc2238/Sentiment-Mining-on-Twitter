############
# Libraries 
############

require(wordcloud)
require(tm)
require(RCurl)
require(twitteR)
require(ROAuth)
require(SnowballC)
require(ggplot2)
require(dplyr)
require(tidyr)
require(lubridate)

############################
# Authenticate Twitter API
###########################

source('~/GitHub/Searching Twitter Authentication.R', echo=TRUE)

setup_twitter_oauth(Consumer_Key, Consumer_Secret, Access_Token, Access_Secret)

setwd("~/GitHub/Sentiment Mining on Twitter")

#########################
# Pull and load the data
#########################
hrc <- searchTwitteR('Hillary Clinton + President', resultType = "recent", n=5000)
hrc <- twListToDF(hrc)
hrc <- hrc[, order(names(hrc))]
hrc$created <- strftime(hrc$created, '%Y-%m-%d')
hrc$created <- as.POSIXct(hrc$created, tz = "EST")
hrc$day = as.numeric(format(hrc$created, format = "%d"))


#####################
# Process the corpus
#####################
D1 <- Corpus(VectorSource(hrc$text))

D2 <- sapply(D1,function(row) iconv(row, "latin1", "ASCII", sub=""))

## Remove Websites
D3 <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+@", "", D2)
## Remove spammed values by only selecting unique tweets
D5 <- Corpus(VectorSource(D3))

#########################
## Clean the corpus data 
#########################

## Remove puncuation
D6 <- tm_map(D5, removePunctuation)
## Convert all characters to lower case
D6 <- tm_map(D5, content_transformer(tolower))
## Remove all stop words ("and" "the" "etc").
D6 <- tm_map(D5, removeWords, stopwords("english"))
## Remove numbers
D6 <- tm_map(D5, removeNumbers)
## Remove all the white space
D6 <- tm_map(D5, stripWhitespace)

## Convert corpus to a term document matrix - so each word can be analyzed individuallly

tdm.corpus <- TermDocumentMatrix(D6)

###################################################################
# Match words in corpus to lexicons of positive and negative words
###################################################################

## Upload positive and negative word lexicons

positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")

## Search for matches between each word and the two lexicons
hrc$positive <- tm_term_score(tdm.corpus, positive)
hrc$negative <- tm_term_score(tdm.corpus, negative)

hrc$score <- hrc$positive - hrc$negative

#########################
# Pull and load the data
#########################

djt <- searchTwitteR('Donald Trump + President', resultType = "recent", n=5000)
djt <- twListToDF(djt)
djt <- djt[, order(names(djt))]
djt$created <- strftime(djt$created, '%Y-%m-%d')
djt$created <- as.POSIXct(djt$created, tz = "EST")
djt$day = as.numeric(format(djt$created, format = "%d"))


#####################
# Process the corpus
#####################
D1 <- Corpus(VectorSource(djt$text))

D2 <- sapply(D1,function(row) iconv(row, "latin1", "ASCII", sub=""))

## Remove Websites
D3 <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+@", "", D2)
## Remove spammed values by only selecting unique tweets
D5 <- Corpus(VectorSource(D3))

#########################
## Clean the corpus data 
#########################

## Remove puncuation
D6 <- tm_map(D5, removePunctuation)
## Convert all characters to lower case
D6 <- tm_map(D5, content_transformer(tolower))
## Remove all stop words ("and" "the" "etc").
D6 <- tm_map(D5, removeWords, stopwords("english"))
## Remove numbers
D6 <- tm_map(D5, removeNumbers)
## Remove all the white space
D6 <- tm_map(D5, stripWhitespace)

## Convert corpus to a term document matrix - so each word can be analyzed individuallly

tdm.corpus <- TermDocumentMatrix(D6)

###################################################################
# Match words in corpus to lexicons of positive and negative words
###################################################################

## Upload positive and negative word lexicons

positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")

## Search for matches between each word and the two lexicons
djt$positive <- tm_term_score(tdm.corpus, positive)
djt$negative <- tm_term_score(tdm.corpus, negative)

djt$score <- djt$positive - djt$negative

#########################################
# Generate density plot for sentiment
#########################################
mu <- as.data.frame(c(mean(hrc$score),mean(djt$score)))
names(mu) <- "mean"

p_djt <- ggplot(djt, aes(score)) + geom_density(color="red", size = 1, fill = "#FF6666", alpha = .5)
p_both <- p_djt + geom_density(data = hrc, aes(score), color="blue", size = 1, fill = "#56B4E9", alpha = .5)

p_both + geom_vline(data = mu, aes(xintercept=mean),
                   color=c("blue","red"), linetype="dashed", size=.8) + labs(title="Sentiment of Twitter Users Regarding Presidential Candidates",x="Sentiment Score", y = "Density")

