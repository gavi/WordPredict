library(tm)
library(stringi)
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE,filter_words=c()) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  
  #' To avoid :: calls
  stri_split_boundaries <- stringi::stri_split_boundaries
  stri_join <- stringi::stri_join
  
  options <- stringi::stri_opts_brkiter(
    type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
  )
  
  #' Tokenizer
  #' 
  #' @param x character
  #' @return character vector with n-grams
  function(x) {    
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
    
    # If filter words provided, clean up
    #if (length(filter_words)>0){
    tokens <- tokens[!tokens %in% filter_words]  
    #}
    
    
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}


#Function that cleans the data and returns the corpus using tm package
cleanData <- function(content){
  doc.vec <- VectorSource(list(content))
  doc.corpus <- Corpus(doc.vec)
  doc.corpus <- tm_map(doc.corpus, removeNumbers) # remove numbers
  doc.corpus <- tm_map(doc.corpus, stripWhitespace) # remove whitespaces
  doc.corpus <- tm_map(doc.corpus, content_transformer(tolower)) #lowercase all content
  doc.corpus <- tm_map(doc.corpus, removePunctuation) # remove any punctuations
  doc.corpus
}

#Function to convert corpus back to Text
corpusToText <- function(corpus){
  unlist(sapply(corpus, `[`, "content"))
}

#Function to calculate N-Gram counts

topNGrams <- function(tokenizer,content,top=10){
  ngrams<-tokenizer(content)
  #Create a table
  data<-data.frame(word=rownames(head(sort(table(ngrams),decreasing=TRUE),top)),count=head(sort(table(ngrams),decreasing=TRUE),top))
}

setwd("~/work/R/capstone")
news=readLines("final/en_US/en_US.news.txt",n=50000)
tweets=readLines("final/en_US/en_US.twitter.txt",n=100000)
blogs=readLines("final/en_US/en_US.blogs.txt",n=50000)
profanity=readLines("final/en_US/bad-words.txt")
newscombined=paste(news,collapse=" ")
blogscombined=paste(blogs,collapse=" ")
tweetscombined=paste(tweets,collapse=" ")


#wordstoremove=c(stopwords("english"),profanity)
splitter<-ngram_tokenizer(n=1)
unigram<-ngram_tokenizer(n=1,filter=profanity)
bigram<-ngram_tokenizer(n=2,filter=profanity)
trigram<-ngram_tokenizer(n=3,filter=profanity)
quadgram<-ngram_tokenizer(n=4,filter=profanity)

news_cleaned<-cleanData(newscombined)
news_cleaned_content <-corpusToText(news_cleaned)
blogs_cleaned<-cleanData(blogscombined)
blogs_cleaned_content <-corpusToText(blogs_cleaned)
tweets_cleaned<-cleanData(tweetscombined)
tweets_cleaned_content <-corpusToText(tweets_cleaned)

all_data <- paste(c(news_cleaned_content,blogs_cleaned_content,tweets_cleaned_content),collapse=" ")
ugram_all<-unigram(all_data)
ugram<-table(ugram_all)
bgram<-table(bigram(all_data))
tgram<-table(trigram(all_data))
qgram<-table(quadgram(all_data))

write.table(ugram,"ugram.txt")
write.table(bgram,"bgram.txt")
write.table(tgram,"tgram.txt")
write.table(qgram,"qgram.txt")




