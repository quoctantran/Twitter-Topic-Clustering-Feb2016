# ========================================
# MODULE 6 - SENTIMENT ANALYSIS
# ========================================

# This method simply count the positive and negative words from dictionary
# Run on the downloaded and clean text from previous steps

# ----------------------------------------
# Load library
# ----------------------------------------

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

# ----------------------------------------
# Load file, text was clean already
# ----------------------------------------

path <- c("D:/twitter_us-election-2016/tweets/combined/")

# Read full data set
trump.df <- read.csv(paste0(path, "trump_clean3.csv"))
clinton.df <- read.csv(paste0(path, "clinton_clean3.csv"))
sanders.df <- read.csv(paste0(path, "sanders_clean3.csv"))
cruz.df <- read.csv(paste0(path, "cruz_clean3.csv"))
rubio.df <- read.csv(paste0(path, "rubio_clean3.csv"))

# Create prototype
p <- 0.1 # Portion of data in prototype

trump.ptt <- trump.df[sample(nrow(trump.df), ceiling(nrow(trump.df) * p)), ]
clinton.ptt <- clinton.df[sample(nrow(clinton.df), ceiling(nrow(clinton.df) * p)), ]
sanders.ptt <- sanders.df[sample(nrow(sanders.df), ceiling(nrow(sanders.df) * p)), ]
cruz.ptt <- cruz.df[sample(nrow(cruz.df), ceiling(nrow(cruz.df) * p)), ]
rubio.ptt <- rubio.df[sample(nrow(rubio.df), ceiling(nrow(rubio.df) * p)), ]

# ----------------------------------------
# Sentiment analysis
# ----------------------------------------

# Evaluation tweets function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none') {
  
  # Check the library
  require(plyr)
  require(stringr)
  
  # Sentiment scoring function
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words){
                    
                    # Clean up the tweets
                    #sentence <- gsub('[[:punct:]]', "", sentence)
                    #sentence <- gsub('[[:cntrl:]]', "", sentence)
                    sentence <- gsub('\\d+', "", sentence)
                    #sentence <- tolower(sentence)
                    
                    # Split a string into words
                    word.list <- str_split(sentence, '\\s+')
                    words <- unlist(word.list)
      
                    # Evaluate sentiment score based on positve and negative words
                    pos.matches <- match(words, pos.words)
                    neg.matches <- match(words, neg.words)
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    score <- sum(pos.matches) - sum(neg.matches)
      
                    return(score)
                  }, pos.words, neg.words, .progress=.progress)
  
  # Create a data framce to output the results  
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

# Dictionary of positive and negative words
pos <- scan('D:/twitter_us-election-2016/R/positive-words.txt', what='character', comment.char=';')
neg <- scan('D:/twitter_us-election-2016/R/negative-words.txt', what='character', comment.char=';')

# Add some more customized words
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

# Run sentiment analysis on the clean text
stack <- clinton.ptt # trump.ptt clinton.ptt sanders.ptt cruz.ptt rubio.ptt
searchterm <- "Hillary Clinton"

Dataset <- stack
Dataset$text <- as.factor(Dataset$clean3)
scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')

write.csv(scores, file=paste(searchterm, 'D:/twitter_us-election-2016/R/scores.csv'), row.names = T)
  
# Total evaluation: positive / negative / neutral
stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())

write.csv(by.tweet, file=paste(searchterm, 'D:/twitter_us-election-2016/R/opin.csv'), row.names = T)

# ----------------------------------------
# Ploting sentiment analysis
# ----------------------------------------
ggplot(by.tweet, aes(created, number)) +
  geom_line(aes(group = tweet, color = tweet), size = 2) +
  geom_point(aes(group=tweet, color=tweet), size = 4) +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 1))
ggtitle(searchterm)
