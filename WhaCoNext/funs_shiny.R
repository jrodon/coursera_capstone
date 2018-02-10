# funs_shiny.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Functions used in the Shiny application
# Javier A. Rodón
# 10.02.2018

## Tokenize ####
makeTokens <- function(input, n = 1L, save = T, filename = NULL) {
      output <- tokens(input, what = "word",
                       remove_separators = T,
                       remove_numbers = T,
                       remove_punct = T,
                       remove_symbols = T,
                       remove_twitter = T,
                       remove_hyphens = T,
                       remove_url = T,
                       verbose = T,
                       ngrams = n)
      if (save) save(output, file = filename, compress = T)
      return(output)
}

## Tokenize the input ####
funInput <- function(input) {
      input <- input %>% tolower() %>% 
            tokens(what = "word", ngrams = 1L, remove_punct = TRUE) %>% 
            unlist(use.names = F)
      return(input)
}

## Find ngrams from the input ####
findNgrams <- function(input, ngramLevel = 4) {
      # Initialize the vector
      hits <- NULL
      wordsFound <- NULL
      hitsCount <- length(hits)
      while(hitsCount <= 5 & ngramLevel > 1) {
            # Get the "history" of the proposed string, i.e., the last n-1 words of the input
            history <- paste0(tail(input, ngramLevel - 1), collapse = "_")
            # Find ngrams that start with the given history, adding to the ones already found
            hits <- rbindlist(
                  list(hits, 
                       ngramsCount[[ngramLevel]][ngram %like% paste0("^", history, "_")]
                  ), fill = TRUE
            )
            # Pick out the ngrams that end in the same word
            if(length(hits) > 0 & length(wordsFound) > 0) {
                  repeatedNgrams <- hits[(hitsCount+1):10
                                         ][sapply(ngram, 
                                                  function(x) tail(
                                                        funInput(
                                                              gsub("_", " ", x, fixed = T)),
                                                        1),
                                                  USE.NAMES = F) %in% wordsFound]$ngram
                  # Filter those repeated ngrams
                  hits <- hits[!ngram %in% repeatedNgrams]
            }
            # Count how many ngrams were found in this pass
            hitsCount <- nrow(hits) - hitsCount
            # print(paste("Found", hitsCount, "hits at the", ngramLevel, "gram level"))
            ngramLevel <- ngramLevel - 1
            # If there are still not 5 hits, get the list of endings found
            if(hitsCount < 5 & hitsCount > 0) {
                  wordsFound <- sapply(hits$ngram,
                                       function(x) tail(funInput(gsub("_", " ", x, fixed = T)), 1)) 
            }
      }
      if(hitsCount < 5 & ngramLevel == 1) {
            missWord <- 5 - nrow(hits)
            hits <- rbindlist(list(hits, ngramsCount[[1]][order(-count), .SD[1:missWord]]))
      }
      return(hits[order(-count), .SD[1:5, ngram]])
}

## Calculate the probablities ####
## This function takes a character string as an input, and a Ngram level to use.
funProb <- function(userInput, ngramLevel = 4) {
      x <- ngramsCount
      # Tokenize the input into words and get the (n-1) last words
      input <- funInput(userInput)
      ngramLevel <- ifelse(ngramLevel > length(input), ngramLevel <- length(input) + 1, ngramLevel)
      history <- tail(input, ngramLevel-1)
      # Find the n-grams that start with the (n-1) last words of the input.
      hits <- findNgrams(history, ngramLevel)
      # Find the actual length of the n-grams found
      actualNLevel <- sapply(hits, function(x) length(funInput(gsub("_", " ", x, fixed = T))))
      # Check wether we are within a single ngram level or not
      if(abs(max(actualNLevel) - min(actualNLevel)) < 1e-5) actualNLevel <- actualNLevel[1]
      # Check that wether we are at the 1-gram level
      if(any(actualNLevel > 1)) {
            # Get the history of the n-gram and its counts
            history <- sapply(actualNLevel, function(x) paste0(tail(input, x-1), collapse = "_"))
            baseCount <- NULL
            for(l in 1:length(actualNLevel)){
                  baseCount[l] <- x[[actualNLevel[l]-1]][ngram %in% history[l]]$count
            }
      } else {
            baseCount <- nrow(x[[1]])
      }
      res <- NULL
      if(length(actualNLevel) > 1) {
            levels <- unique(actualNLevel)
            for(lev in levels) {
                  for(hit in hits[actualNLevel == lev]) {
                        # Calculate the MLE and SBO score of each n-gram found 
                        # with the given history
                        res <- rbindlist(list(res, x[[lev]][ngram %in% hit, `:=` (
                              mle = count/baseCount[which(hits %in% hit)],
                              sbo = 0.4^(ngramLevel-lev) * count / baseCount[which(hits %in% hit)]
                        )][!is.na(sbo)]))
                  }
                  x[[lev]][,`:=` (mle = NULL, sbo = NULL)]
            }
      } else {
            for(hit in hits) {
                  res <- x[[actualNLevel]][ngram %in% hit, `:=` (
                        mle = count/baseCount,
                        sbo = 0.4^(ngramLevel-actualNLevel) * count/baseCount
                  )][!is.na(sbo)]
            }
            x[[actualNLevel]][,`:=` (mle = NULL, sbo = NULL)]
      }
      unique(res[order(-sbo)])
}
