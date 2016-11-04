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
source('~/GitHub/Sentiment Mining on Twitter/Pull tweets by day_HRC.R', echo=TRUE)

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

##########################
# generate the word cloud
##########################

wordcloud(D6, max.words = 100, random.order = F, col= rainbow(20), scale = c(6, .5))

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

#########################################
# Generate graph for sentiment over time
#########################################

## Create data frame to graph

hrc2 <- hrc %>% group_by(created) %>% summarise(mean(score))
names(hrc2) <- c("Date", "Score")
hrc3 <- hrc %>% group_by(created) %>% summarise(mean(positive))
names(hrc3) <- c("Date", "Positive")
hrc4 <- hrc %>% group_by(created) %>% summarise(mean(negative))
names(hrc4) <- c("Date","Negative")

## Plot a line plot using ggplot
ggplot(hrc2, aes(Date, Score)) + geom_line() + xlab("Date") + ylab("Mean Sentiment Score")

hrc_plot <- ggplot() + 
  geom_line(aes(Date, Score, colour="Average"), size = 2, hrc2) +
  geom_line(aes(Date, Positive, colour="Positive"), size = 2, hrc3) +
  geom_line(aes(Date, Negative, colour="Negative"), size = 2, hrc4) + labs(title = "Sentiment of Hilary Related Clinton Tweets")

