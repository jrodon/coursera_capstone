library(quanteda)
library(dplyr)
library(tidyr)
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

## Sample 10% of each source ####
fraction <- 0.2

funSample <- function(x, fraction = 0.1, origin, seed = 1780) {
      require(dplyr)
      
      set.seed(seed)
      
      y <- x %>% as_data_frame() %>% sample_frac(size = fraction) 
      
      if(!missing(origin)) {
            origin <- as.character(origin)
            y <- y %>% mutate(source = paste(origin,seq(1:round(length(x)*fraction)),
                                             sep = "_"))
      }
      y
}

samBlog <- funSample(txtBlog, fraction, "blog") ; rm(txtBlog) ; gc()
samNews <- funSample(txtNews, fraction, "news") ; rm(txtNews) ; gc()
samTwtr <- funSample(txtTwtr, fraction, "twitter") ; rm(txtTwtr) ; gc()

## Create the corpus ####
corpusBlog <- corpus(samBlog, text_field = "value", docid_field = "source") ; rm(samBlog)
corpusNews <- corpus(samNews, text_field = "value", docid_field = "source")
corpusTwtr <- corpus(samTwtr, text_field = "value", docid_field = "source")
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

## Tokenize ####
sentencesBlog <- tokens(corpusBlog, what = "sentence",
                        remove_numbers = T,
                        remove_punct = F,
                        remove_symbols = T,
                        remove_hyphens = F,
                        remove_url = T,
                        verbose = T) 
rm(corpusBlog)

engramsBlog <- tokens(tolower(sentencesBlog), what = "word",
                       remove_separators = T,
                       remove_numbers = T,
                       remove_punct = T,
                       remove_symbols = T,
                       remove_twitter = T,
                       remove_hyphens = F,
                       remove_url = T, # Here we lose the source parameter, since
                       verbose = T,    # tolower() coerces the token object 
                       ngrams = 1L)    # (i.e., a list) to a character vector.
bigramsBlog <- tokens(tolower(sentencesBlog), what = "word",
                      remove_separators = T,
                      remove_numbers = T,
                      remove_punct = T,
                      remove_symbols = T,
                      remove_twitter = T,
                      remove_hyphens = F,
                      remove_url = T, # Here we lose the source parameter, since
                      verbose = T,    # tolower() coerces the token object 
                      ngrams = 2L)    # (i.e., a list) to a character vector.
trigramsBlog <- tokens(tolower(sentencesBlog), what = "word",
                       remove_separators = T,
                       remove_numbers = T,
                       remove_punct = T,
                       remove_symbols = T,
                       remove_twitter = T,
                       remove_hyphens = F,
                       remove_url = T, # Here we lose the source parameter, since
                       verbose = T,    # tolower() coerces the token object 
                       ngrams = 3L)    # (i.e., a list) to a character vector.
tetragramsBlog <- tokens(tolower(sentencesBlog), what = "word",
                         remove_separators = T,
                         remove_numbers = T,
                         remove_punct = T,
                         remove_symbols = T,
                         remove_twitter = T,
                         remove_hyphens = F,
                         remove_url = T, # Here we lose the source parameter, since
                         verbose = T,    # tolower() coerces the token object 
                         ngrams = 4L)    # (i.e., a list) to a character vector.

rm(sentencesBlog)
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
dfmBlogUni <- dfm(engramsBlog, verbose = TRUE)
rm(engramsBlog)

dfmBlogBi <- dfm(bigramsBlog, verbose = TRUE)
rm(bigramsBlog)

dfmBlogTri <- dfm(trigramsBlog, verbose = TRUE)
rm(trigramsBlog)

dfmBlogTetra <- dfm(tetragramsBlog, verbose = TRUE)
rm(tetragramsBlog)

## Trim the dfm keeping the tokens that appear more than 10 times across all documents ####
dfmBlogUni <- dfm_trim(dfmBlogUni, min_count = 10, verbose = T)
dfmBlogBi <- dfm_trim(dfmBlogBi, min_count = 10, verbose = T)
dfmBlogTri <- dfm_trim(dfmBlogTri, min_count = 10, verbose = T)
dfmBlogTetra <- dfm_trim(dfmBlogTetra, min_count = 10, verbose = T)

## Tabulate the token frequencies (i.e., counts across all document) ####
freqBlogEn <- textstat_frequency(dfmBlogUni) %>% 
      mutate(prediction = feature) %>%
      select(prediction, frequency)
rm(dfmBlogUni)

freqBlogBi <- textstat_frequency(dfmBlogBi) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "prediction"), sep = "_")
rm(dfmBlogBi)

freqBlogTri <- textstat_frequency(dfmBlogTri) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "word2", "prediction"), sep = "_")
rm(dfmBlogTri)

freqBlogTetra <- textstat_frequency(dfmBlogTetra) %>%
      mutate(prediction = feature) %>%
      select(prediction, frequency) %>%
      separate(prediction, c("word1", "word2", "word3", "prediction"), sep = "_")
rm(dfmBlogTetra)

## Save the frequency tables and clear memory ####
save(freqBlogEn, file = "./count_engrams.Rds", compress = T)
save(freqBlogBi, file = "./count_bigrams.Rds", compress = T)
save(freqBlogTri, file = "./count_trigrams.Rds", compress = T)
save(freqBlogTetra, file = "./count_tetragrams.Rds", compress = T)

## Stupid Backoff algorithm ####

funInput <- function(x) {
      x <- tolower(tokens(x, what = "word", ngrams = 1L,
                          remove_punct = T))
      x <- data_frame(rowname = paste0("word", length(x):1),
                      input = x) %>% spread(rowname, input)
      
      # if(ncol(x) >= 2) {
      #       x <- x %>% select(1:2)
      # }
      return(x)
}

funSBO <- function(x, n = 5) {
      # if(ncol(x) >= 3) {
            if(x$word3 %in% freqBlogTetra$word1 &
               x$word2 %in% freqBlogTetra$word2[freqBlogTetra$word1 == x$word3] &
               x$word1 %in% freqBlogTetra$word3[freqBlogTetra$word1 == x$word3 &
                                                freqBlogTetra$word2 == x$word2]) {
                  countTetra <- freqBlogTetra %>%
                        filter(word1 %in% x$word3 & word2 %in% x$word2 & word3 %in% x$word1)
                  countTri <- freqBlogTri %>% 
                        filter(word1 %in% x$word3 & word2 %in% x$word2 & prediction %in% x$word1)
                  score <- countTetra %>% 
                        mutate(score = frequency / countTri$frequency) %>%
                        # select(prediction, score) %>% 
                        top_n(n, score)
            } else if(x$word2 %in% freqBlogTri$word1 &
                      x$word1 %in% freqBlogTri$word2[freqBlogTri$word1 == x$word2]) {
                  countTri <- freqBlogTri %>% 
                        filter(word1 %in% x$word2 & word2 %in% x$word1)
                  countBi <- freqBlogBi %>%
                        filter(word1 %in% x$word2 & prediction %in% x$word1)
                  score <- countTri %>% 
                        mutate(score = 0.4 * frequency / countBi$frequency) %>%
                        # select(prediction, score) %>% 
                        top_n(n, score)
            } else if(x$word1 %in% freqBlogBi$word1) {
                  countBi <- freqBlogBi %>%
                        filter(word1 %in% x$word1)
                  countEn <- freqBlogEn %>%
                        filter(prediction %in% x$word1)
                  score <- countBi %>% 
                        mutate(score = 0.4^2 * frequency / countEn$frequency) %>%
                        # select(prediction, score) %>% 
                        top_n(n, score)
            } else {
                  score <- freqBlogEn %>% 
                        filter(prediction %in% x$word1) %>%
                        mutate(score = 0.4^3 * frequency / 3) %>% top_n(n, score)
            }
      # }
      return(score)
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
