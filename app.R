library(tm)
library(stringi)

#Main Tokenizer Function

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

#Simple Splitter used for Input Splitting

splitter<-ngram_tokenizer(n=1)

#qgram_index<-read.csv("qgram_index.csv")
#str(qgram_index)
#saveRDS(qgram_index,"q.rds")
ugram<-readRDS("u.rds")
bgram<-readRDS("b.rds")
tgram<-readRDS("t.rds")
qgram<-readRDS("q.rds")

colnames(ugram)<-c("id","word","freq")
colnames(bgram)<-c("id1","id2","freq")
colnames(tgram)<-c("id1","id2","id3","freq")
colnames(qgram)<-c("id1","id2","id3","id4","freq")


lookup<-function(word){
  ugram[ugram$word==word,]$id
}

rlookup<-function(ids){
  as.character(sapply(ids,function(x)ugram[ugram$id==x,]$word))
}


get_ids<-function(str){
  words<-splitter(corpusToText(cleanData(str)))
  ret<-c()
  for(word in words){
    ret<-c(ret,lookup(word))
  }
  ret
}

lastN<-function(vec,n){
  if(length(vec)<n){
    return(vec)
  }
  a<-length(vec)-n+1
  b<-length(vec)
  ret<-vec[a:b]
  ret
}

findInQuadgram<-function(ids){
  ids<-lastN(ids,3)
    if(length(ids)==3){
    d<-qgram[qgram$id1==ids[1]&qgram$id2==ids[2]&qgram$id3==ids[3],]
    if(nrow(d)>0){
      ret<-head(d[order(d$freq,decreasing=T),],10)
      return(ret$id4)
    }
  }
}

findInTrigram<-function(ids){
  ids<-lastN(ids,2)
    if(length(ids)==2){
    d<-tgram[tgram$id1==ids[1]&tgram$id2==ids[2],]
    if(nrow(d)>0){
      ret<-head(d[order(d$freq,decreasing=T),],10)
      return(ret$id3)
    } 
  }
}

findInBigram<-function(ids){
  ids<-lastN(ids,1)
  if(length(ids)==1){
    d<-bgram[bgram$id1==ids[1],]
    if(nrow(d)>0){
      ret<-head(d[order(d$freq,decreasing=T),],10)
      return(ret$id2)
    }
  }
}


predict2<-function(str){
  rlookup(findInBigram(get_ids(str)))
}
predict3<-function(str){
  rlookup(findInTrigram(get_ids(str)))
}
predict4<-function(str){
  rlookup(findInQuadgram(get_ids(str)))
}

predict<-function(str){
  output<-predict4(str)
  if(length(output)==0){
    output<-predict3(str)
    if(length(output)==0){
      output<-predict2(str)
    }
  }
  data.frame(suggestions=output)
}


