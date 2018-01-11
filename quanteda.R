library(quanteda)
library(doParallel)
library(dplyr)
library(tidyr)
library(tibble)
## Download and inflate the files ####
webPath <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("coursera-swiftkey.zip")){
      download.file(webPath, destfile="coursera-swiftkey.zip")
      unzip("coursera-swiftkey.zip")}

## Load the provided files ####
filesPath <- paste(getwd(), "final", "en_US", sep = "/")

txtBlog <- readLines(paste(filesPath,"en_US.blogs.txt", sep = "/"), skipNul = TRUE)
txtNews <- readLines(paste(filesPath,"en_US.news.txt", sep = "/"), skipNul = TRUE)
txtTwtr <- readLines(paste(filesPath,"en_US.twitter.txt", sep = "/"), skipNul = TRUE)

## Sample 25% of each source ####
fraction <- 0.25

samBlog <- funSample(txtBlog, fraction, "blog") ; rm(txtBlog) ; gc()
samNews <- funSample(txtNews, fraction, "news") ; rm(txtNews) ; gc()
samTwtr <- funSample(txtTwtr, fraction, "twitter") ; rm(txtTwtr) ; gc()

## Create the corpus ####
corpusBlog <- corpus(samBlog, text_field = "value", docid_field = "source") ; rm(samBlog)
corpusNews <- corpus(samNews, text_field = "value", docid_field = "source") ; rm(samNews)
corpusTwtr <- corpus(samTwtr, text_field = "value", docid_field = "source") ; rm(samTwtr)
docvars(corpusBlog, "Source") <- "Blogs"
docvars(corpusNews, "Source") <- "News"
docvars(corpusTwtr, "Source") <- "Twitter"

## Inspect elements
# texts(corpusBlog[1])
# infoBlogs <- summary(corpusBlog)
# kwic(corpusBlog, "love")
# topfeatures(dfmBlog, 24)

## Join all the corpii (?) ####
corpusFull <- corpusBlog + corpusNews + corpusTwtr
save(corpusFull, file = "./corpus_full.Rds", compress = T)
rm(corpusBlog, corpusNews, corpusTwtr) ; gc()

## Tokenize in sentences ####
sentences <- makeParallel(makeSentences, corpusFull)
save(sentences, file = "./list_sentences.Rds", compress = T)
rm(corpusFull) ; gc()

## Engrams ####
## Tokenize 
engrams <- tokens(tolower(sentences), what = "word",
                  remove_separators = T,
                  remove_numbers = T,
                  remove_punct = T,
                  remove_symbols = T,
                  remove_twitter = T,
                  remove_hyphens = F,
                  remove_url = T, # Here we lose the source parameter, since
                  verbose = T,    # tolower() coerces the token object 
                  ngrams = 1L)    # (i.e., a list) to a character vector.
save(engrams, file = "./list_engrams.Rds", compress = T)

## Build document-frequency matrices
dfmEn <- dfm(engrams, verbose = TRUE)
save(dfmEn, file = "./dfm_engrams.Rds", compress = T)
rm(engrams) ; gc()
## Trim the dfm keeping the tokens that appear only once across all documents
dfmEn <- dfm_trim(dfmEn, min_count = 2, verbose = T)
save(dfmEn, file = "./dfm_engrams_trim.Rds", compress = T)

## Tabulate the token frequencies (i.e., counts across all document) 
freqEn <- textstat_frequency(dfmEn) %>% 
      mutate(prediction = feature) %>%
      select(prediction, frequency)
save(freqEn, file = "./count_engrams.Rds", compress = T)
rm(freqEn) ; rm(dfmEn) ; gc()

## Bigrams ####
bigrams <- tokens(tolower(sentences), what = "word",
                  remove_separators = T,
                  remove_numbers = T,
                  remove_punct = T,
                  remove_symbols = T,
                  remove_twitter = T,
                  remove_hyphens = F,
                  remove_url = T, # Here we lose the source parameter, since
                  verbose = T,    # tolower() coerces the token object 
                  ngrams = 2L)    # (i.e., a list) to a character vector.
save(bigrams, file = "./list_bigrams.Rds", compress = T)

## Build document-frequency matrices
dfmBi <- dfm(bigrams, verbose = TRUE)
save(dfmBi, file = "./dfm_bigrams.Rds", compress = T)
rm(bigrams) ; gc()
## Trim the dfm keeping the tokens that appear only once across all documents
dfmBi <- dfm_trim(dfmBi, min_count = 2, verbose = T)
save(dfmBi, file = "./dfm_bigrams_trim.Rds", compress = T)

## Tabulate the token frequencies (i.e., counts across all document) 
freqBi <- textstat_frequency(dfmBi) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "prediction"), sep = "_")
save(freqBi, file = "./count_bigrams.Rds", compress = T)
rm(freqBi) ; rm(dfmBi) ; gc()

## Trigrams ####
trigrams <- tokens(tolower(sentences), what = "word",
                   remove_separators = T,
                   remove_numbers = T,
                   remove_punct = T,
                   remove_symbols = T,
                   remove_twitter = T,
                   remove_hyphens = F,
                   remove_url = T, # Here we lose the source parameter, since
                   verbose = T,    # tolower() coerces the token object 
                   ngrams = 3L)    # (i.e., a list) to a character vector.
save(trigrams, file = "./list_trigrams.Rds", compress = T)

## Build document-frequency matrices
dfmTri <- dfm(trigrams, verbose = TRUE)
save(dfmTri, file = "./dfm_trigrams.Rds", compress = T)
rm(trigrams) ; gc()
## Trim the dfm keeping the tokens that appear only once across all documents
dfmTri <- dfm_trim(dfmTri, min_count = 2, verbose = T)
save(dfmTri, file = "./dfm_trigrams_trim.Rds", compress = T)

## Tabulate the token frequencies (i.e., counts across all document) 
freqTri <- textstat_frequency(dfmTri) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "word2", "prediction"), sep = "_")
save(freqTri, file = "./count_trigrams.Rds", compress = T)
rm(freqTri) ; rm(dfmTri) ; gc()

## Tetragrams ####
tetragrams <- tokens(tolower(sentences), what = "word",
                     remove_separators = T,
                     remove_numbers = T,
                     remove_punct = T,
                     remove_symbols = T,
                     remove_twitter = T,
                     remove_hyphens = F,
                     remove_url = T, # Here we lose the source parameter, since
                     verbose = T,    # tolower() coerces the token object 
                     ngrams = 4L)    # (i.e., a list) to a character vector.
save(tetragrams, file = "./list_tetragrams.Rds", compress = T)
rm(sentences) ; gc()

## Build document-frequency matrices
dfmTetra <- dfm(tetragrams, verbose = TRUE)
save(dfmTetra, file = "./dfm_tetragrams.Rds", compress = T)
rm(tetragrams) ; gc()
## Trim the dfm keeping the tokens that appear only once across all documents
dfmTetra <- dfm_trim(dfmTetra, min_count = 2, verbose = T)
save(dfmTetra, file = "./dfm_tetragrams_trim.Rds", compress = T)

## Tabulate the token frequencies (i.e., counts across all document) 
freqTetra <- textstat_frequency(dfmTetra) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "word2", "word3", "prediction"), sep = "_")
save(freqTetra, file = "./count_tetragrams.Rds", compress = T)
rm(dfmTetra) ; gc()

## Load all the frequency tables ####
load("./count_engrams.Rds") ; gc()
load("./count_bigrams.Rds") ; gc()
load("./count_trigrams.Rds") ; gc()

## Stupid Backoff algorithm ####
funInput <- function(x) {
      x <- tolower(tokens(x, what = "word", ngrams = 1L,
                          remove_punct = T))
      x <- data_frame(rowname = paste0("word", length(x):1),
                      input = x) %>% spread(rowname, input)
      if(ncol(x) < 3) {
            nCols <- ncol(x)
            while(nCols < 3) {
                  x[ ,paste0("word",nCols+1)] <- 0
                  nCols <- nCols + 1
            }
      } 
      return(x)
}

funSBO <- function(x, n = 5) {
      if(x$word3 %in% freqTetra$word1 &
         x$word2 %in% freqTetra$word2[freqTetra$word1 == x$word3] &
         x$word1 %in% freqTetra$word3[freqTetra$word1 == x$word3 &
                                          freqTetra$word2 == x$word2]) {
            countTetra <- freqTetra %>%
                  filter(word1 %in% x$word3 & word2 %in% x$word2 & word3 %in% x$word1)
            countTri <- freqTri %>% 
                  filter(word1 %in% x$word3 & word2 %in% x$word2 & prediction %in% x$word1)
            score <- countTetra %>% 
                  mutate(score = frequency / countTri$frequency) %>%
                  # select(prediction, score) %>%
                  arrange(desc(score))
      } else if(x$word2 %in% freqTri$word1 &
                x$word1 %in% freqTri$word2[freqTri$word1 == x$word2]) {
            countTri <- freqTri %>% 
                  filter(word1 %in% x$word2 & word2 %in% x$word1)
            countBi <- freqBi %>%
                  filter(word1 %in% x$word2 & prediction %in% x$word1)
            score <- countTri %>% 
                  mutate(score = 0.4 * frequency / countBi$frequency) %>%
                  # select(prediction, score) %>%
                  arrange(desc(score))
      } else if(x$word1 %in% freqBi$word1) {
            countBi <- freqBi %>%
                  filter(word1 %in% x$word1)
            countEn <- freqEn %>%
                  filter(prediction %in% x$word1)
            score <- countBi %>% 
                  mutate(score = 0.4^2 * frequency / countEn$frequency) %>%
                  # select(prediction, score) %>%
                  arrange(desc(score))
      } else {
            score <- freqEn %>% 
                  filter(prediction %in% x$word1) %>%
                  mutate(score = 0.4^3 * frequency / 3) %>% 
                  select(prediction, score) %>% 
                  arrange(desc(score))
      }
      return(score[1:n,])
}


## Calculate the relative tf (i.e., tf relative to the document length)
#tfPropBlogUni <- tf(dfmBlogUni, scheme = "prop") # Same as the follwing line
# tfPropBlogUni <- dfm_weight(dfmBlogUni, type = "relfreq")
## Calculate the df (i.e., the count of the number of documents 
## in which a term -or feature- occurs) 
# dfBlogUni <- docfreq(dfmBlogUni, scheme = "count")
# Calculate the idf, i.e., the logarithmic inverse of the fraction of 
# documents that contain a feature (log Tot_Docs/Docs_with_feature)
# idfBlogUni <- docfreq(dfmBlogUni, scheme = "inverse")
# Calulate the tf-idf (i.e., tf(t,d) * idf(t,D))
# tfidfBlogUni <- tfidf(dfmBlogUni)

## Build document-frequency matrices ####
# In general, separated by sources, each ngram added troughout the source
# dfmUni <- dfm(corpusFull, verbose = TRUE,
#               remove_numbers = T,
#               remove_punct = T,
#               remove_symbols = T,
#               remove_twitter = T,
#               remove_hyphens = F,
#               remove_url = T,
#               groups = "Source",
#               ngrams = 1L)
# For each sentence in the source
