library(data.table)
library(stringr)
library(quanteda)

MAX_NB_PREDICT <- 5 # maximum number of predictions
MAX_N_INPUT_NGRAM <- 3  # maximum number of words for an input ngram
SENT_START_MARKER <-"#s#"
MODEL_FILE <- "dfm_shiny_model.rds"
END_OF_SENT_CHAR <- c(".", "?", "!")

# DESCRIPTION
#   Returns n-grams containing the next-word candidates for the input n-gram
# INPUT
#   - inputNgram : n-gram with the format "W1_W2_W3" where WX are words. 1 to 3 words are acceptable
#   - ngramModel : list of 4 elements containing data.tables of the DFM for the model.
#       The data.tables in the list have 2 columns : ngram and count
#       The first element of the list is the 1-gram DFM, the second is the 2-grams DFM etc.#       
# OUTPUT
#   - data.table(ngram, count) :
#       Contains at most MAX_N_INPUT_NGRAM n-grams with their count number
#       If no match is found the output is empty
#       
getNgramMatch <- function(inputNgram, ngramModel) {
  if(length(inputNgram) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  # Default empty result
  match <- data.table(prefix = character(0), lastWord = character(0), count = integer(0))
  
  # Input n-gram level
  n <- length(strsplit(inputNgram, "_", fixed = TRUE)[[1]])
  
  if(n > MAX_N_INPUT_NGRAM) {
    errorMsg <- paste("The input n-gram", paste0("\"", inputNgram, "\""), "has more than", MAX_N_INPUT_NGRAM, "words.")
    stop(errorMsg)
  }
  
  # Return at most MAX_NB_PREDICT of the most frequent matches
  match <- head(ngramModel[[n+1]][prefix == inputNgram][order(count, decreasing = TRUE)], MAX_NB_PREDICT)
  match$n <- n+1
  
  # Less than MAX_NB_PREDICT matches found : try to match a lower-level ngram
  if(nrow(match) < MAX_NB_PREDICT) {
    if(n >=2) {
      # If at least 2 words, remove FIRST word and try to match this new ngram
      croppedNgram <- str_split_fixed(inputNgram, "_", 2)[2]
      # Add lower-level ngrams to the current level ngrams
      # fill = TRUE because of 1-grams which don't have a prefix
      match <- rbind(match, getNgramMatch(croppedNgram, ngramModel))
      # If not enough matches to reach MAX_NB_PREDICT : return most common unigrams to complete the set of predictions
    } else {
      # Return most frequent unigrams
      tempMatch <- head(ngramModel[[1]][order(count, decreasing = TRUE)], MAX_NB_PREDICT)
      tempMatch$n <- 1
      
      # Add lower-level ngrams to the current level ngrams
      match <- rbind(match, tempMatch)
    }
  }
  match
}

# DESCRIPTION
#   Returns the most likely next words based on the input n-gram.
#   Similar to predictNextWord() but does not compute the stupid backoff score to be faster
#   Most likely words are ranked by descending n-gram level and descending frequency count
# INPUT
#   - inputNgram : n-gram with the format "W1_W2_W3_W4" where WX are words. 1 to 4 words are acceptable
#   - ngramModel : list of 4 elements containing data.tables of the DFM for the model.
#       The data.tables in the list have 2 columns : ngram and count
#       The first element of the list is the 1-gram DFM, the second is the 2-grams DFM etc.#       
# OUTPUT
#   - data.frame with 2 columns : (nextWord, score)
#     Maximum number of rows = MAX_NB_PREDICT and rows ordered with the highest scores first
#  
predictNextWordFast <- function(inputNgram, ngramModel) {
  if(length(inputNgram) != 1) {
    stop("Only atomic vectors allowed")
  }
  # Replace empty string by start of sentence marker
  if(inputNgram == "") {
    ngramToFind <- SENT_START_MARKER
  } else {
    ngramToFind <- inputNgram
  }
  
  ngramMatches <- getNgramMatch(ngramToFind, ngramModel)
  
  # Remove duplicate next words from lower level n-grams
  ngramMatches <- ngramMatches[!duplicated(ngramMatches$lastWord)]
  
  # Returns only the MAX_NB_PREDICT n-grams with the highest scores
  head(ngramMatches[order(-n, -count)], MAX_NB_PREDICT)[, lastWord]
}

# x : user text input to be tokenized
# Output : n-gram with the format "X_Y_Z" with n between 1 and 3
tokenizeInput <- function(x) {
  sentTokens <- tokenizeSentences(x)
  lastSentence <- tail(sentTokens, 1)
  
  # If the last sentence token ends by and end of sentence marker
  # manually add a new SENT_START_MARKER at the end
  if(substr(lastSentence, nchar(lastSentence), nchar(lastSentence)) %in% END_OF_SENT_CHAR) {
    sentTokens[length(sentTokens)] <- paste(sentTokens[length(sentTokens)], SENT_START_MARKER)
  }
  
  ngram1 <- tokenizeNgrams(sentTokens, 1)
  
  # Concatenate the last 3 n-grams to create the input n-gram
  paste(tail(ngram1, 3), collapse = "_")
}


####################################################################################################
# Tokenizing functions (must be the same as the ones used for the model creation)
####################################################################################################
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
