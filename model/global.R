# PARAMETERS : modify if necessary
# If USE_SAMPLE_DATA is FALSE, all the data will be used
# If USE_SAMPLE_DATA is TRUE, only a random subset of FILE_SAMPLE_RATE will be used. 
USE_SAMPLE_DATA <- TRUE
# Global sample rate for the original files
#FILE_SAMPLE_RATE <- 0.01

# Parameters to build ngrams on small samples and merge them later
SUB_FILE_SAMPLE_RATE <- 0.001
NB_SUB_FILES <- 3
FILE_SAMPLE_RATE <- SUB_FILE_SAMPLE_RATE * NB_SUB_FILES

MAX_NB_PREDICT <- 5 # maximum number of predictions
MAX_N_INPUT_NGRAM <- 3  # maximum number of words for an input ngram
MAX_NGRAM_LEVEL <- 4
LAMBDA_SBO <- 0.4 # lambda parameter for stupid backoff
NB_CORES <- 4
TOP_NGRAM_LIMIT <- 50 # Number of ngrams to plot
# minimum number of occurences a ngram needs to have to be kept in the DFM
# For reference, the Google cut-off point was at least 40 occurences for 1,024,908,267,229 in total
MIN_NGRAM_COUNT <- 2
RDS_FILE_VERSION <- "v0.16"  # to know from which model a RSD file was created

SENT_START_MARKER <-"#s#"