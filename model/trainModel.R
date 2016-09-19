library(quanteda)
library(ggplot2)
#library(doParallel)
library(data.table)
library(stringr)


# Get vector of swear words to remove
# File downloaded from http://www.bannedwordlist.com/lists/swearWords.csv
getSwearWords <- function() {
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
  
  # Add start and end of sentences symbols
  # #bos# = beginning of sentence
  # #eos# = end of sentence (like EOF symbol for "end of file")
  
  # Symbols added because of the Ngrams chapter in the book Speech and Language Processing
  # by Daniel Jurafsky and James Martin
  # Not sure if these symbols will be used in the prediction model
  # WARNING : these symbols cannot use punctuation that would be removed in the n-grams tokenization
  
  # ************************************************************************************
  # TODO : see if start and end of sentences useful for final model.
  # => If used : 
  #       - set removeTwitter to FALSE in the tokenizeNgrams function
  #       - Remove Twitter content with regexp manually after the ngram tokenization
  # ************************************************************************************
  #unlist(lapply(tokens, function(y) paste('#bos#', toLower(y), '#eos#')))
  unlist(lapply(tokens, toLower))
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