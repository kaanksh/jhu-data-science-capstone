library(quanteda)
library(ggplot2)
#library(doParallel)
library(data.table)
library(stringr)

source("model/global.R")







# Get vector of swear words to remove
# File downloaded from http://www.bannedwordlist.com/lists/swearWords.csv
getSwearWords <- function() {
  swearWordsFile <- "data/swearWords.csv"
  
  swearWords <- readLines(swearWordsFile, skipNul = TRUE, warn = FALSE) # warning because of missing "end of line" character
  swearWords <- unlist(strsplit(swearWords, ",", fixed = TRUE))
  swearWords
}

# Sentence tokenization function
tokenizeSentences <- function(x) {
  # Remove twitter characters here manually because the tokenize function
  # only removes complete features (i.e. sentences here not characters)
  # Remove underscores too because they are used as a ngram concatenator by Quanteda
  patternRm <- "#|@|_"
  
  if(is(x, "corpus")) {
    # Corpus modified directly to avoid copy
    texts(x) <- sapply(texts(x), gsub, pattern = patternRm, replacement = "" )
    xClean <- x
  } else {
    xClean <- gsub(patternRm, "", x)
  }
  
  tokens <- tokenize(xClean, what = "sentence", removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE
                     , removeSeparators = TRUE, removeTwitter = FALSE, removeHyphen = TRUE, removeURL = TRUE)
  
 
  # Add #s# marker at beginning of sentences
  unlist(lapply(tokens, function(y) paste('#s#', toLower(y))))
  #unlist(lapply(tokens, toLower))
}

# N-grams tokenization function
# Default is unigram if n is not set
# removeTwitter needs to be FALSE in case start and end of sentence markers used (# character)
tokenizeNgrams <- function(x, n = 1L) {
  # Keep removeTwitter to FALSE in cas special start and end of sentences are used
  tokenize(x, what = "word", removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE
           , removeSeparators = TRUE, removeTwitter = FALSE, removeHyphen = TRUE, removeURL = TRUE
           , ngrams = n, simplify = TRUE)
}

# DFM creation function
# Can be used with a corpus or tokens
generateDfm <- function(x, n = 1L, ignoredFeatures = character(0)) {
  # Keep removeTwitter to FALSE in cas special start and end of sentences are used
  dfm(x, what = "word", removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE
      , removeSeparators = TRUE, removeTwitter = FALSE, removeHyphen = TRUE, removeURL = TRUE
      , ngrams = n, ignoredFeatures=ignoredFeatures)
}

# NEW FUNCTIONS
getNgramPrefix <- function(string, sep = "_") {
  if(length(string) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  splitNgram <- strsplit(string, "_", fixed = TRUE)[[1]]
  paste0(splitNgram[1:length(splitNgram) - 1], collapse = "_")
}

getNgramLastWord <- function(string, sep = "_") {
  if(length(string) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  splitNgram <- strsplit(string, "_", fixed = TRUE)[[1]]
  splitNgram[length(splitNgram)]
}

splitNgramPrefix <- function(string, sep = "_") {
  if(length(string) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  splitNgram <- strsplit(string, "_", fixed = TRUE)[[1]]
  n <- length(splitNgram) 
  c(paste0(splitNgram[1:n- 1], collapse = "_"), splitNgram[n])
}


# Generates a n-gram with the data.table format
generateNgramDt <- function(tokens, n = 1, ignoredFeatures = character(0)) {
  
  dfm <- generateDfm(tokens, n, ignoredFeatures)
  # Convert to data.table
  if(n == 1) {
    dtDfm <- data.table(prefix = "", lastWord = features(dfm), count = as.integer(colSums(dfm)), key = "count")
  } else {
    tempSplitPrefix <- t(vapply(features(dfm), splitNgramPrefix, c("", "")))
    dtDfm <- data.table(prefix = tempSplitPrefix[, 1], lastWord = tempSplitPrefix[ ,2]
                        , count = as.integer(colSums(dfm)), key = c("prefix", "count"))
  }
}
