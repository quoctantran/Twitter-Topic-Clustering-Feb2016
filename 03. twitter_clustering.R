# =================================================
# MODULE 4 - TOPIC CLUSTERING
# =================================================

# -------------------------------------------------
# Install library
# -------------------------------------------------
library(data.table)
library(NLP)
library(tm)
library(ngram)

# -------------------------------------------------
# Import data from CSV files
# -------------------------------------------------

path <- c("D:/twitter_us-election-2016/tweets/combined/")

trump.df <- read.csv(paste0(path, "trump_clean3.csv"))
clinton.df <- read.csv(paste0(path, "clinton_clean3.csv"))
sanders.df <- read.csv(paste0(path, "sanders_clean3.csv"))
cruz.df <- read.csv(paste0(path, "cruz_clean3.csv"))
rubio.df <- read.csv(paste0(path, "rubio_clean3.csv"))

# -------------------------------------------------
# Remove some other useless words
# -------------------------------------------------

rm.words <- c("amp", "donald", "trump",
              "hillary", "clinton",
              "sanders", "bernie",
              "ted", "cruz", "tedcruz",
              "marco", "rubio",
              "bush", "jeb", "ben", "carson",
              "chris", "christie", "john", "kasich")

CleanUp <- function(tweet.df) {
  tweet.df$clean3 <- paste0(" ", tweet.df$clean2, " ")
  for (i in 1:length(rm.words)) {
    word <- paste0(" ", rm.words[i], " ")
    tweet.df$clean3 <- gsub(word, "", tweet.df$clean3, fixed = T)
  }
  tweet.df$clean3 <- gsub("^\\s+|\\s+$", "", tweet.df$clean3)
  return(tweet.df)
}

trump.df <- CleanUp(trump.df)
clinton.df <- CleanUp(clinton.df)
sanders.df <- CleanUp(sanders.df)
cruz.df <- CleanUp(cruz.df)
rubio.df <- CleanUp(rubio.df)

var.keep <- c("id", "created", "weekday", "screenName", "text",
              "clean", "clean2", "clean3", "stem")

setcolorder(trump.df, var.keep)
setcolorder(clinton.df, var.keep)
setcolorder(sanders.df, var.keep)
setcolorder(cruz.df, var.keep)
setcolorder(rubio.df, var.keep)

# Export to CSV files for next loading (backup)
write.csv(trump.df, paste0(path, "trump_clean3.csv"), row.names = FALSE)
write.csv(clinton.df, paste0(path, "clinton_clean3.csv"), row.names = FALSE)
write.csv(sanders.df, paste0(path, "sanders_clean3.csv"), row.names = FALSE)
write.csv(cruz.df, paste0(path, "cruz_clean3.csv"), row.names = FALSE)
write.csv(rubio.df, paste0(path, "rubio_clean3.csv"), row.names = FALSE)

# -------------------------------------------------
# Create traiing & testing data sets
# Training data set: tweets on Mon, Tue, Thu, Fri and Sat
# Testing data set: tweets on Wed and Sun
# -------------------------------------------------

trump.df$weekday <- weekdays(as.Date(trump.df$created), abbreviate = TRUE)
clinton.df$weekday <- weekdays(as.Date(clinton.df$created), abbreviate = TRUE)
sanders.df$weekday <- weekdays(as.Date(sanders.df$created), abbreviate = TRUE)
cruz.df$weekday <- weekdays(as.Date(cruz.df$created), abbreviate = TRUE)
rubio.df$weekday <- weekdays(as.Date(rubio.df$created), abbreviate = TRUE)

# Training data set
train.set <- c("Mon", "Tue", "Thu", "Fri", "Sat")

trump.train <- subset(trump.df, weekday %in% train.set)
clinton.train <- subset(clinton.df, weekday %in% train.set)
sanders.train <- subset(sanders.df, weekday %in% train.set)
cruz.train <- subset(cruz.df, weekday %in% train.set)
rubio.train <- subset(rubio.df, weekday %in% train.set)

# Testing data set
test.set <- c("Wed", "Sun")

trump.test <- subset(trump.df, weekday %in% test.set)
clinton.test <- subset(clinton.df, weekday %in% test.set)
sanders.test <- subset(sanders.df, weekday %in% test.set)
cruz.test <- subset(cruz.df, weekday %in% test.set)
rubio.test <- subset(rubio.df, weekday %in% test.set)

# Export to CSV files for next loading
write.csv(trump.train, paste0(path, "trump_train.csv"), row.names = FALSE)
write.csv(clinton.train, paste0(path, "clinton_train.csv"), row.names = FALSE)
write.csv(sanders.train, paste0(path, "sanders_train.csv"), row.names = FALSE)
write.csv(cruz.train, paste0(path, "cruz_train.csv"), row.names = FALSE)
write.csv(rubio.train, paste0(path, "rubio_train.csv"), row.names = FALSE)

write.csv(trump.test, paste0(path, "trump_test.csv"), row.names = FALSE)
write.csv(clinton.test, paste0(path, "clinton_test.csv"), row.names = FALSE)
write.csv(sanders.test, paste0(path, "sanders_test.csv"), row.names = FALSE)
write.csv(cruz.test, paste0(path, "cruz_test.csv"), row.names = FALSE)
write.csv(rubio.test, paste0(path, "rubio_test.csv"), row.names = FALSE)


# =================================================
# MODULE 4.1 - WORK ON TRAINING DATA SET
# =================================================

# -------------------------------------------------
# Document term matrix, TF-IDf and Ngram (n = 2)
# -------------------------------------------------

# Import training and test data sets
path <- c("D:/twitter_us-election-2016/tweets/combined/")

trump.train <- read.csv(paste0(path, "trump_train.csv"))
clinton.train <- read.csv(paste0(path, "clinton_train.csv"))
sanders.train <- read.csv(paste0(path, "sanders_train.csv"))
cruz.train <- read.csv(paste0(path, "cruz_train.csv"))
rubio.train <- read.csv(paste0(path, "rubio_train.csv"))

# trump.test <- read.csv(paste0(path, "trump_test.csv"))
# clinton.test <- read.csv(paste0(path, "clinton_test.csv"))
# sanders.test <- read.csv(paste0(path, "sanders_test.csv"))
# cruz.test <- read.csv(paste0(path, "cruz_test.csv"))
# rubio.test <- read.csv(paste0(path, "rubio_test.csv"))

# Convert text in column "Clean3" to corpus
trump.cp <- VCorpus(VectorSource(trump.train$clean3))
clinton.cp <- VCorpus(VectorSource(clinton.train$clean3))
sanders.cp <- VCorpus(VectorSource(sanders.train$clean3))
cruz.cp <- VCorpus(VectorSource(cruz.train$clean3))
rubio.cp <- VCorpus(VectorSource(rubio.train$clean3))

# Convert text in column "Clean2" to corpus
# trump.cp <- VCorpus(VectorSource(trump.test$clean3))
# clinton.cp <- VCorpus(VectorSource(clinton.test$clean3))
# sanders.cp <- VCorpus(VectorSource(sanders.test$clean3))
# cruz.cp <- VCorpus(VectorSource(cruz.train$clean3))
# rubio.cp <- VCorpus(VectorSource(rubio.train$clean3))

# Ngram TF-IDF
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))

trump.tfidf.dtm <- DocumentTermMatrix(trump.cp, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
clinton.tfidf.dtm <- DocumentTermMatrix(clinton.cp, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
sanders.tfidf.dtm <- DocumentTermMatrix(sanders.cp, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
cruz.tfidf.dtm <- DocumentTermMatrix(cruz.cp, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
rubio.tfidf.dtm <- DocumentTermMatrix(rubio.cp, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# Ngram Frequency
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
# trump.dtm <- DocumentTermMatrix(trump.cp, control = list(tokenize = BigramTokenizer))
# clinton.dtm <- DocumentTermMatrix(clinton.cp, control = list(tokenize = BigramTokenizer))
# sanders.dtm <- DocumentTermMatrix(sanders.cp, control = list(tokenize = BigramTokenizer))
# cruz.dtm <- DocumentTermMatrix(cruz.cp, control = list(tokenize = BigramTokenizer))
# rubio.dtm <- DocumentTermMatrix(rubio.cp, control = list(tokenize = BigramTokenizer))

# Remove the sparsity
trump.tfidf.dtm2 <- removeSparseTerms(trump.tfidf.dtm, 0.99)
clinton.tfidf.dtm2 <- removeSparseTerms(clinton.tfidf.dtm, 0.99)
sanders.tfidf.dtm2 <- removeSparseTerms(sanders.tfidf.dtm, 0.99)
cruz.tfidf.dtm2 <- removeSparseTerms(cruz.tfidf.dtm, 0.99)
rubio.tfidf.dtm2 <- removeSparseTerms(rubio.tfidf.dtm, 0.99)

# -------------------------------------------------
# k-means clustering 
# -------------------------------------------------

# -------------------------------------------------
# TRUMP - TRAINING SET
# -------------------------------------------------

# Elbow method to determine number of clusters
trump.k.max <- 15
trump.wss <- sapply(1:trump.k.max, function(k) {kmeans(trump.tfidf.dtm2, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:trump.k.max, trump.wss, type="b", pch = 19, frame = FALSE, main = "Trump - Training Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
trump.n.cluster <- 3 # Based on elbow
abline(v = trump.n.cluster, lty = 2)
trump.kmeans <- kmeans(trump.tfidf.dtm2, trump.n.cluster)

# finding out what the clusters are about
for (i in 1:trump.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(trump.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * trump.kmeans$size[i] / sum(trump.kmeans$size), "\n")
}

# -------------------------------------------------
# CLINTON - TRAINING SET
# -------------------------------------------------

clinton.k.max <- 15
clinton.wss <- sapply(1:clinton.k.max, function(k) {kmeans(clinton.tfidf.dtm2, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:clinton.k.max, clinton.wss, type="b", pch = 19, frame = FALSE, main = "Clinton - Training Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
clinton.n.cluster <- 2 # Based on elbow
abline(v = clinton.n.cluster, lty = 2)
clinton.kmeans <- kmeans(clinton.tfidf.dtm2, clinton.n.cluster)

# finding out what the clusters are about
for (i in 1:clinton.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(clinton.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * clinton.kmeans$size[i] / sum(clinton.kmeans$size), "\n")
}

# -------------------------------------------------
# SANDERS - TRAINING SET
# -------------------------------------------------

sanders.k.max <- 15
sanders.wss <- sapply(1:sanders.k.max, function(k) {kmeans(sanders.tfidf.dtm2, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:sanders.k.max, sanders.wss, type="b", pch = 19, frame = FALSE, main = "Sanders - Training Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
sanders.n.cluster <- 13 # Based on elbow
abline(v = sanders.n.cluster, lty = 2)
sanders.kmeans <- kmeans(sanders.tfidf.dtm2, sanders.n.cluster)

# finding out what the clusters are about
for (i in 1:sanders.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(sanders.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * sanders.kmeans$size[i] / sum(sanders.kmeans$size), "\n")
}

# -------------------------------------------------
# CRUZ - TRAINING SET
# -------------------------------------------------

cruz.k.max <- 15
cruz.wss <- sapply(1:cruz.k.max, function(k) {kmeans(cruz.tfidf.dtm2, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:cruz.k.max, cruz.wss, type="b", pch = 19, frame = FALSE, main = "Cruz - Training Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
cruz.n.cluster <- 10 # Based on elbow
abline(v = cruz.n.cluster, lty = 2)
cruz.kmeans <- kmeans(cruz.tfidf.dtm2, cruz.n.cluster)

# finding out what the clusters are about
for (i in 1:cruz.n.cluster) {
  cat(paste(" Cluster ", i,": ", sep =""))
  s <- sort(cruz.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * cruz.kmeans$size[i] / sum(cruz.kmeans$size), "\n")
}

# -------------------------------------------------
# RUBIO - TRAINING SET
# -------------------------------------------------

rubio.k.max <- 15
rubio.wss <- sapply(1:rubio.k.max, function(k) {kmeans(rubio.tfidf.dtm2, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:rubio.k.max, rubio.wss, type="b", pch = 19, frame = FALSE, main = "Rubio - Training Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
rubio.n.cluster <- 4 # Based on elbow
abline(v = rubio.n.cluster, lty = 2)
rubio.kmeans <- kmeans(rubio.tfidf.dtm2, rubio.n.cluster)

# finding out what the clusters are about
for (i in 1:rubio.n.cluster) {
  cat(paste(" Cluster ", i,": ", sep =""))
  s <- sort(rubio.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * rubio.kmeans$size[i] / sum(rubio.kmeans$size), "\n")
}


# =================================================
# MODULE 4.2 - WORK ON TESTING DATA SET
# =================================================

# -------------------------------------------------
# Document term matrix, TF-IDf and Ngram (n = 2)
# -------------------------------------------------

# Import testing data sets
path <- c("D:/twitter_us-election-2016/tweets/combined/")

trump.test <- read.csv(paste0(path, "trump_test.csv"))
clinton.test <- read.csv(paste0(path, "clinton_test.csv"))
sanders.test <- read.csv(paste0(path, "sanders_test.csv"))
cruz.test <- read.csv(paste0(path, "cruz_test.csv"))
rubio.test <- read.csv(paste0(path, "rubio_test.csv"))

# Convert text in column "Clean3" to corpus
trump.cp.test <- VCorpus(VectorSource(trump.test$clean3))
clinton.cp.test <- VCorpus(VectorSource(clinton.test$clean3))
sanders.cp.test <- VCorpus(VectorSource(sanders.test$clean3))
cruz.cp.test <- VCorpus(VectorSource(cruz.train$clean3))
rubio.cp.test <- VCorpus(VectorSource(rubio.train$clean3))

# Ngram TF-IDF
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))

trump.tfidf.dtm3 <- DocumentTermMatrix(trump.cp.test, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
clinton.tfidf.dtm3 <- DocumentTermMatrix(clinton.cp.test, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
sanders.tfidf.dtm3 <- DocumentTermMatrix(sanders.cp.test, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
cruz.tfidf.dtm3 <- DocumentTermMatrix(cruz.cp.test, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
rubio.tfidf.dtm3 <- DocumentTermMatrix(rubio.cp.test, control = list(tokenize = BigramTokenizer, weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# Ngram Frequency
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
# trump.dtm <- DocumentTermMatrix(trump.cp, control = list(tokenize = BigramTokenizer))
# clinton.dtm <- DocumentTermMatrix(clinton.cp, control = list(tokenize = BigramTokenizer))
# sanders.dtm <- DocumentTermMatrix(sanders.cp, control = list(tokenize = BigramTokenizer))
# cruz.dtm <- DocumentTermMatrix(cruz.cp, control = list(tokenize = BigramTokenizer))
# rubio.dtm <- DocumentTermMatrix(rubio.cp, control = list(tokenize = BigramTokenizer))

# Remove the sparsity
trump.tfidf.dtm4 <- removeSparseTerms(trump.tfidf.dtm3, 0.99)
clinton.tfidf.dtm4 <- removeSparseTerms(clinton.tfidf.dtm3, 0.99)
sanders.tfidf.dtm4 <- removeSparseTerms(sanders.tfidf.dtm3, 0.99)
cruz.tfidf.dtm4 <- removeSparseTerms(cruz.tfidf.dtm3, 0.99)
rubio.tfidf.dtm4 <- removeSparseTerms(rubio.tfidf.dtm3, 0.99)

# -------------------------------------------------
# k-means clustering 
# -------------------------------------------------

# -------------------------------------------------
# TRUMP - TESTING SET
# -------------------------------------------------

# Elbow method to determine number of clusters
trump.k.max <- 15
trump.wss.test <- sapply(1:trump.k.max, function(k) {kmeans(trump.tfidf.dtm4, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:trump.k.max, trump.wss.test, type="b", pch = 19, frame = FALSE, main = "Trump - Testing Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
trump.n.cluster <- 3 # Based on elbow
abline(v = trump.n.cluster, lty = 2)
trump.kmeans <- kmeans(trump.tfidf.dtm4, trump.n.cluster)

# finding out what the clusters are about
for (i in 1:trump.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(trump.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * trump.kmeans$size[i] / sum(trump.kmeans$size), "\n")
}

# -------------------------------------------------
# CLINTON - TESTING SET
# -------------------------------------------------

clinton.k.max <- 15
clinton.wss.test <- sapply(1:clinton.k.max, function(k) {kmeans(clinton.tfidf.dtm4, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:clinton.k.max, clinton.wss.test, type="b", pch = 19, frame = FALSE, main = "Clinton - Testing Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
clinton.n.cluster <- 2 # Based on elbow
abline(v = clinton.n.cluster, lty = 2)
clinton.kmeans <- kmeans(clinton.tfidf.dtm4, clinton.n.cluster)

# finding out what the clusters are about
for (i in 1:clinton.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(clinton.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * clinton.kmeans$size[i] / sum(clinton.kmeans$size), "\n")
}

# -------------------------------------------------
# SANDERS - TESTING SET
# -------------------------------------------------

sanders.k.max <- 15
sanders.wss.test <- sapply(1:sanders.k.max, function(k) {kmeans(sanders.tfidf.dtm4, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:sanders.k.max, sanders.wss.test, type="b", pch = 19, frame = FALSE, main = "Sanders - Testing Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
sanders.n.cluster <- 13 # Based on elbow
abline(v = sanders.n.cluster, lty = 2)
sanders.kmeans <- kmeans(sanders.tfidf.dtm4, sanders.n.cluster)

# finding out what the clusters are about
for (i in 1:sanders.n.cluster) {
  cat(paste("Cluster ", i,": ", sep =""))
  s <- sort(sanders.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * sanders.kmeans$size[i] / sum(sanders.kmeans$size), "\n")
}

# -------------------------------------------------
# CRUZ - TESTING SET
# -------------------------------------------------

cruz.k.max <- 15
cruz.wss.test <- sapply(1:cruz.k.max, function(k) {kmeans(cruz.tfidf.dtm4, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:cruz.k.max, cruz.wss.test, type="b", pch = 19, frame = FALSE, main = "Cruz - Testing Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
cruz.n.cluster <- 10 # Based on elbow
abline(v = cruz.n.cluster, lty = 2)
cruz.kmeans <- kmeans(cruz.tfidf.dtm4, cruz.n.cluster)

# finding out what the clusters are about
for (i in 1:cruz.n.cluster) {
  cat(paste(" Cluster ", i,": ", sep =""))
  s <- sort(cruz.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * cruz.kmeans$size[i] / sum(cruz.kmeans$size), "\n")
}

# -------------------------------------------------
# RUBIO - TESTING SET
# -------------------------------------------------

rubio.k.max <- 15
rubio.wss.test <- sapply(1:rubio.k.max, function(k) {kmeans(rubio.tfidf.dtm4, k, nstart = 10)$tot.withinss})

# Plot to see the elbow
plot(1:rubio.k.max, rubio.wss.test, type="b", pch = 19, frame = FALSE, main = "Rubio - Testing Set",
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Create the clusters
rubio.n.cluster <- 4 # Based on elbow
abline(v = rubio.n.cluster, lty = 2)
rubio.kmeans <- kmeans(rubio.tfidf.dtm4, rubio.n.cluster)

# finding out what the clusters are about
for (i in 1:rubio.n.cluster) {
  cat(paste(" Cluster ", i,": ", sep =""))
  s <- sort(rubio.kmeans$centers[i,], decreasing = T)
  cat(names(s)[1:5], " ", 100 * rubio.kmeans$size[i] / sum(rubio.kmeans$size), "\n")
}


# =================================================
# MODULE 5 - WORD CLOUD
# =================================================

library(RColorBrewer)
library(wordcloud)

draw.wordcloud <- function(tfidf.dtm) {
  tfidf.dtm3 <- removeSparseTerms(tfidf.dtm, 0.995)
  dtm.mt <- as.matrix(tfidf.dtm3)

  word.freqs <- sort(colSums(dtm.mt), decreasing = TRUE)
  dtm.df <- data.frame(word = names(word.freqs), freq = word.freqs)

  wordcloud(dtm.df$word, dtm.df$freq, scale = c(5,0.5), max.word = dim(dtm.df)[1],
            random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}

draw.wordcloud(trump.tfidf.dtm)
draw.wordcloud(clinton.tfidf.dtm)
draw.wordcloud(sanders.tfidf.dtm)
draw.wordcloud(cruz.tfidf.dtm)
draw.wordcloud(rubio.tfidf.dtm)