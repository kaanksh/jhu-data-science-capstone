# The following objects are not necessary for the UI
# They are in the global.R file so that they are loaded before the UI
# This way when the UI appears to the user, the app is ready to be used

source("predictWordShiny.R")

# Load ngram model
dfmModel <<- readRDS(MODEL_FILE)