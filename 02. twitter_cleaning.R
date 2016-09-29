# =================================================
# MODULE 2 - MERGING TWEETS
# =================================================

# -------------------------------------------------
# Install library
# -------------------------------------------------

library(data.table)
library(NLP)
library(tm)

# -------------------------------------------------
# Import and stack all data files into data.table
# -------------------------------------------------

# Create a list of CSV files to be read
path <- c("D:/twitter_us-election-2016/tweets/")
file.list <- list.files(path = path, pattern = "*.csv")

# List of files per candidates
file.trump <- file.list[grep("trump", file.list, ignore.case = TRUE)]
file.clinton <- file.list[grep("clinton", file.list, ignore.case = TRUE)]
file.sanders <- file.list[grep("sanders", file.list, ignore.case = TRUE)]
file.cruz <- file.list[grep("cruz", file.list, ignore.case = TRUE)]
file.rubio <- file.list[grep("rubio", file.list, ignore.case = TRUE)]

# Import tweets and stack them together
tweet.import <- function(file.list, path) {
  # Add full path to files
  for (i in 1:length(file.list)) file.list[i] <- paste0(path, file.list[i])
  
  # Read all files in the list
  tweet <- read.csv(file.list[1])
  for (i in 2:length(file.list))
    tweet <- merge(tweet, read.csv(file.list[i]), all.x = TRUE, all.y = TRUE)
  
  tweet <- data.table(tweet)
  return(tweet)
}

tweet.trump <- tweet.import(file.trump, path)
tweet.clinton <- tweet.import(file.clinton, path)
tweet.sanders <- tweet.import(file.sanders, path)
tweet.cruz <- tweet.import(file.cruz, path)
tweet.rubio <- tweet.import(file.rubio, path)

# -------------------------------------------------
# Clean up the data structure
# -------------------------------------------------

# Keep only important fields
var.keep <- c("id", "created", "screenName", "text")
var.drop <- setdiff(names(tweet.trump), var.keep)

tweet.trump <- tweet.trump[, -var.drop, with = FALSE]
tweet.clinton <- tweet.clinton[, -var.drop, with = FALSE]
tweet.sanders <- tweet.sanders[, -var.drop, with = FALSE]
tweet.cruz <- tweet.cruz[, -var.drop, with = FALSE]
tweet.rubio <- tweet.rubio[, -var.drop, with = FALSE]

# Set the columns order fo the clean database
setcolorder(tweet.trump, var.keep)
setcolorder(tweet.clinton, var.keep)
setcolorder(tweet.sanders, var.keep)
setcolorder(tweet.cruz, var.keep)
setcolorder(tweet.rubio, var.keep)

# Remove duplicate
setkey(tweet.trump, "id")
setkey(tweet.clinton, "id")
setkey(tweet.sanders, "id")
setkey(tweet.cruz, "id")
setkey(tweet.rubio, "id")

tweet.trump <- unique(tweet.trump)
tweet.clinton <- unique(tweet.clinton)
tweet.sanders <- unique(tweet.sanders)
tweet.cruz <- unique(tweet.cruz)
tweet.rubio <- unique(tweet.rubio)

# Sort tweets by ID then by created date
tweet.trump <- tweet.trump[order(id, created)]
tweet.clinton <- tweet.clinton[order(id, created)]
tweet.sanders <- tweet.sanders[order(id, created)]
tweet.cruz <- tweet.cruz[order(id, created)]
tweet.rubio <- tweet.rubio[order(id, created)]

# Export to CSV files for next loading (for backup)
# write.csv(tweet.trump, paste0(path, "combined/", "trump_clean.csv"), row.names = FALSE)
# write.csv(tweet.clinton, paste0(path, "combined/", "clinton_clean.csv"), row.names = FALSE)
# write.csv(tweet.sanders, paste0(path, "combined/", "sanders_clean.csv"), row.names = FALSE)
# write.csv(tweet.cruz, paste0(path, "combined/", "cruz_clean.csv"), row.names = FALSE)
# write.csv(tweet.rubio, paste0(path, "combined/", "rubio_clean.csv"), row.names = FALSE)


# =================================================
# MODULE 3 - CLEAN UP TWEETS CONTENTS
# =================================================

# -------------------------------------------------
# Clean up tweet using data tables
# -------------------------------------------------

# Remove these following things
# "RT" and "via"; "<ed>" and "<U+...>"; hyper-links;
# Tabs; Extra spaces;
clean.dt <- function(tweet.dt) {
  tweet.dt[, clean := gsub("^\\s+|\\s+$", "",
                      gsub("[ \t]{2,}", "",
                      gsub("http[^[:space:]]*", "", 
                      gsub("<.*?>", "",      
                      gsub("<ed>", "",
                      gsub("#","hash-tag-convert",
                      gsub("u.s.","united states",  
                      tolower(gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text))))))))),
           by = .(text)]
  return(tweet.dt)
}

trump.dt <- clean.dt(tweet.trump)
clinton.dt <- clean.dt(tweet.clinton)
sanders.dt <- clean.dt(tweet.sanders)
cruz.dt <- clean.dt(tweet.cruz)
rubio.dt <- clean.dt(tweet.rubio)

# -------------------------------------------------
# Clean up tweet using text corpus
# -------------------------------------------------

# Convert data table to corpus
trump.cp <- VCorpus(VectorSource(trump.dt$clean))
clinton.cp <- VCorpus(VectorSource(clinton.dt$clean))
sanders.cp <- VCorpus(VectorSource(sanders.dt$clean))
cruz.cp <- VCorpus(VectorSource(cruz.dt$clean))
rubio.cp <- VCorpus(VectorSource(rubio.dt$clean))

# Full list of stopwords and other words (i.e. 322 words)
sw.full <- c("i've", "a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "can't", "co", "con", "could", "couldn't", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fifty", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasn't", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its","it's", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "your's","yours", "yourself", "yourselves", "the")
sw.full <- c(sw.full, c("youtube", "watch"))

# Function to clean tweets in corpus
# WARNING: Takes around 4 hours !!!
clean.cp <- function(tweet.cp, word.rm) {
  tweet.cp <- tm_map(tweet.cp, removeWords, c(word.rm, sw.full, stopwords("english"), stopwords("SMART")))
  tweet.cp <- tm_map(tweet.cp, stripWhitespace)
  tweet.cp <- tm_map(tweet.cp, removeNumbers)
  tweet.cp <- tm_map(tweet.cp, removePunctuation, preserve_intra_word_dashes = TRUE)
  tweet.cp <- tm_map(tweet.cp, content_transformer(function(x) gsub("hash-tag-convert", "#", x)))
  return(tweet.cp)
}

trump.cp <- clean.cp(trump.cp, word.rm = c("donald", "trump", "realdonaldtrump"))
clinton.cp <- clean.cp(clinton.cp, word.rm = c("hillary", "clinton", "hillaryclinton"))
sanders.cp <- clean.cp(sanders.cp, word.rm = c("sanders", "bernie", "sensanders", "sen"))
cruz.cp <- clean.cp(cruz.cp, word.rm = c("ted", "cruz", "tedcruz", "sentedcruz", "sen"))
rubio.cp <- clean.cp(rubio.cp, word.rm = c("marco", "rubio", "marcorubio"))

# Add the result columns to previous data tables
trump.clean2 <- data.frame(text= unlist(sapply(trump.cp, `[`, "content")), stringsAsFactors = FALSE)
clinton.clean2 <- data.frame(text= unlist(sapply(clinton.cp, `[`, "content")), stringsAsFactors = FALSE)
sanders.clean2 <- data.frame(text= unlist(sapply(sanders.cp, `[`, "content")), stringsAsFactors = FALSE)
cruz.clean2 <- data.frame(text= unlist(sapply(cruz.cp, `[`, "content")), stringsAsFactors = FALSE)
rubio.clean2 <- data.frame(text= unlist(sapply(rubio.cp, `[`, "content")), stringsAsFactors = FALSE)

trump.dt <- trump.dt[, clean2 := trump.clean2]
clinton.dt <- clinton.dt[, clean2 := clinton.clean2]
sanders.dt <- sanders.dt[, clean2 := sanders.clean2]
cruz.dt <- cruz.dt[, clean2 := cruz.clean2]
rubio.dt <- rubio.dt[, clean2 := rubio.clean2]

# Export to CSV files for next loading (for backup)
# write.csv(trump.dt, paste0(path, "combined/", "trump_clean2.csv"), row.names = FALSE)
# write.csv(clinton.dt, paste0(path, "combined/", "clinton_clean2.csv"), row.names = FALSE)
# write.csv(sanders.dt, paste0(path, "combined/", "sanders_clean2.csv"), row.names = FALSE)
# write.csv(cruz.dt, paste0(path, "combined/", "cruz_clean2.csv"), row.names = FALSE)
# write.csv(rubio.dt, paste0(path, "combined/", "rubio_clean2.csv"), row.names = FALSE)

# -------------------------------------------------
# Stemming document
# -------------------------------------------------

trump.cp.stem <- tm_map(trump.cp, stemDocument, language = "english")
clinton.cp.stem <- tm_map(clinton.cp, stemDocument, language = "english")
sanders.cp.stem <- tm_map(sanders.cp, stemDocument, language = "english")
cruz.cp.stem <- tm_map(cruz.cp, stemDocument, language = "english")
rubio.cp.stem <- tm_map(rubio.cp, stemDocument, language = "english")

# Add the result columns to previous data tables
trump.stem <- data.frame(text= unlist(sapply(trump.cp.stem, `[`, "content")), stringsAsFactors = FALSE)
clinton.stem <- data.frame(text= unlist(sapply(clinton.cp.stem, `[`, "content")), stringsAsFactors = FALSE)
sanders.stem <- data.frame(text= unlist(sapply(sanders.cp.stem, `[`, "content")), stringsAsFactors = FALSE)
cruz.stem <- data.frame(text= unlist(sapply(cruz.cp.stem, `[`, "content")), stringsAsFactors = FALSE)
rubio.stem <- data.frame(text= unlist(sapply(rubio.cp.stem, `[`, "content")), stringsAsFactors = FALSE)

trump.dt <- trump.dt[, stem := trump.stem]
clinton.dt <- clinton.dt[, stem := clinton.stem]
sanders.dt <- sanders.dt[, stem := sanders.stem]
cruz.dt <- cruz.dt[, stem := cruz.stem]
rubio.dt <- rubio.dt[, stem := rubio.stem]

# Export to CSV files for next loading (final results)
write.csv(trump.dt, paste0(path, "combined/", "trump_clean3.csv"), row.names = FALSE)
write.csv(clinton.dt, paste0(path, "combined/", "clinton_clean3.csv"), row.names = FALSE)
write.csv(sanders.dt, paste0(path, "combined/", "sanders_clean3.csv"), row.names = FALSE)
write.csv(cruz.dt, paste0(path, "combined/", "cruz_clean3.csv"), row.names = FALSE)
write.csv(rubio.dt, paste0(path, "combined/", "rubio_clean3.csv"), row.names = FALSE)