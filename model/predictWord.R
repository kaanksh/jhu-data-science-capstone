library(quanteda)
library(ggplot2)
#library(doParallel)
library(data.table)
library(stringr)

# TODO : remove all checks (parameter values, size, etc.) after testing to improve performance ?

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
#   Returns the stupid backoff score of the input n-gram
# INPUT
#   - inputNgram : n-gram with the format "W1_W2_W3_W4" where WX are words. 1 to 4 words are acceptable
#   - ngramModel : list of 4 elements containing data.tables of the DFM for the model.
#       The data.tables in the list have 2 columns : ngram and count
#       The first element of the list is the 1-gram DFM, the second is the 2-grams DFM etc.#       
# OUTPUT
#   - Stupid backoff score for inputNgram (numeric)
#  
getSboScore <- function(inputNgram, ngramModel) {
  if(length(inputNgram) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  # Default result if ngram not found
  score <- 0
  
  # TODO : A LOT of perf optim to do because counts sometimes recalculated for nothing
  
  # Input n-gram level
  n <- length(strsplit(inputNgram, "_", fixed = TRUE)[[1]])
  
  # 0-word ngrams
  if(n <= 0) {
    score <- 0
    # At least 1 word
  } else {
    tempSplitPrefix <- splitNgramPrefix(inputNgram)
    inputNgramCount <- ngramModel[[n]][prefix == tempSplitPrefix[1] & lastWord == tempSplitPrefix[2]]$count
    
    # Stop with error if more than 1 match (duplicates problem)
    if((length(inputNgramCount) > 1)) {
      errorMsg <- paste("Duplicate rows found for ngram", inputNgram)
      stop(errorMsg)
    }
    
    # If ngram has at least 1 occurence (i.e. : 1 match in the ngram model)
    if(length(inputNgramCount) == 1) {
      # For unigrams, lower ngram count = number of observed unigrams
      if(n == 1) {
        lowerNgramCount <- nrow(ngramModel[[n]])
        # n >= 2
      } else {
        # Remove LAST word to get the lower level ngram i.e use the ngram prefix
        # Prefix is blank ("") for unigrams
        lowerNSplitPrefix <- splitNgramPrefix(tempSplitPrefix[1])
        lowerNgramCount <- ngramModel[[n - 1]][prefix == lowerNSplitPrefix[1] & lastWord == lowerNSplitPrefix[2]]$count
      }
      if(length(lowerNgramCount) >= 1 && lowerNgramCount != 0 && !is.na(lowerNgramCount)) {
        score <- inputNgramCount / lowerNgramCount
      } else {
        # This can happen if the lower level n-gram (without the last word) is not found in the lower level DFM
        # This should not happen and means there is a problem in the DFM
        
        # Only a warning is printed to avoid having the entire application crash. The default score is 0
        stop("lowerNgramCount has no value. This can happen if the lower \
                       level n-gram (without the last word) is not found in the lower level DFM")
      }
      
      # ngram has no occurence
    } else {
      if(n >= 2) {
        # Remove FIRST word to get the lower level ngram
        croppedNgram <- str_split_fixed(inputNgram, "_", 2)[2]
        score <- LAMBDA_SBO * getSboScore(croppedNgram, ngramModel)
      }
    } 
  }
  score
}

# DESCRIPTION
#   Returns the most likely next words (with the associated score) based on the input n-gram
#   ***WARNING*** : DEPRECATED funtion 
#     Because of the last optimizations and the choice to not use the stupid backoff score, this function 
#     is not to be used as is because it will return some errors linked ti the stupid backoff score.
#     Function only kept for reference.
# INPUT
#   - inputNgram : n-gram with the format "W1_W2_W3_W4" where WX are words. 1 to 4 words are acceptable
#   - ngramModel : list of 4 elements containing data.tables of the DFM for the model.
#       The data.tables in the list have 2 columns : ngram and count
#       The first element of the list is the 1-gram DFM, the second is the 2-grams DFM etc.#       
# OUTPUT
#   - data.frame with 2 columns : (nextWord, score)
#     Maximum number of rows = MAX_NB_PREDICT and rows ordered with the highest scores first
#  
predictNextWord <- function(inputNgram, ngramModel) {
  if(length(inputNgram) != 1) {
    stop("Only atomic vectors allowed")
  }
  
  ngramMatches <- getNgramMatch(inputNgram, ngramModel)
  
  fullNgram <- paste(ngramMatches$prefix, ngramMatches$lastWord, sep = "_")
  ngramMatches$score <- sapply(fullNgram, getSboScore, ngramModel)
  
  # Remove duplicate next words from lower level n-grams
  ngramMatches <- ngramMatches[!duplicated(ngramMatches$lastWord)]
  
  # Returns only the MAX_NB_PREDICT n-grams with the highest scores
  head(ngramMatches[, lastWord, score][order(score, decreasing = TRUE)], MAX_NB_PREDICT)
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
