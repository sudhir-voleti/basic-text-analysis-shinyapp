
puncts = c(",", "\\.", "!", "\\?");

text.clean = function(x,html_tags,numbers)                    # text data
{ #require("tm")
  x  =  tolower(x)
  if(html_tags){
    x  =  gsub("<.*?>", " ", x)             # regex for removing HTML tags
  }
  if(numbers){
    x  =  removeNumbers(x) 
  }# convert to lower case characters
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
   
  
                     # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

dtm.word.count <- function(dtm) {
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  tsum = ss.col
  # tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  return(tsum)
}


dtm.word.cloud <- function(count = count, max.words = 100,title = "Title"){
  
  if (class(count)[1] == "DocumentTermMatrix"|class(count)[1] == "simple_triplet_matrix")
  {
    tsum = dtm.word.count(count)
  } else {
    tsum = count
  }
  
  if (class(tsum) != "numeric") stop("Give input as wordcount or DocumentTermMatrix")
  
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(4, 1),     # range of word sizes
            min.freq = .01,                     # min.freq of words to consider
            max.words = max.words,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
}   

distill.cog.tcm = function(mat1, # input TCM or DTM MAT
                           mattype = "DTM", # "DTM" or TCM
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
  require(igraph)
  
  mat1 = as.matrix(mat1)
  
  if (mattype == "DTM"){
    mat1 = tcrossprod(t(mat1))
  }
  
  if (mattype == "TCM"){
    mat1 = as.matrix(mat1)
    mat1 = mat1 + t(mat1)
  }
  
  
  if (ncol(mat1) > 1000) {
    tst = round(ncol(mat1)/100)  # divide mat1's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(mat1))]
    b = c(0,b,ncol(mat1))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempmat1 = mat1[,(b[i]+1):(b[i+1])]
      su = colSums(as.matrix(tempmat1))
      ss.col = c(ss.col,su);rm(su)
    }
  } else {
    ss.col = colSums(as.matrix(mat1))
  }
  
  # a = colSums(mat1) # collect colsums into a vector obj a
  a = ss.col
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    #mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } 
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  
  
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  E(graph)$width <- E(graph)$weight/min(E(graph)$weight)  
  visIgraph(graph,layout = 'layout.kamada.kawai')
} 
dtm.tcm.creator <- function(text,id = "",
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional ,
                            bigram.encoding = TRUE,
                            bigram.min.freq = 5,
                            min.dtm.freq = 2,
                            skip.grams.window = 5,
                            html_tags = TRUE,
                            numbers = TRUE) {
  
  # if (class(text) != "character" | length(text) < 3){
  #   stop("data format Not correct. Make sure it's a character verctor of length above 3")
  # }
  
  stpw1 = readLines("data/stopwords.txt")# stopwords list from git
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  stpw3  = unique(gsub("'"," ",c(stpw1,stpw2)))
  
  if ((id == "")[1]){
    id = 1:length(text)
  }
  
  if (std.clean == TRUE) {
    # print("Performing Standard Text Cleaning")
    text = text.clean(text,html_tags,numbers)
  }
  
  if (std.stop.words == TRUE){
    # print("Removing Stop Words")
    stop.words.f = unique(c(stpw3,stop.words.additional))
    text = removeWords(text,stop.words.f)            # removing stopwords created above
    text = stripWhitespace(text)                  # removing white spacestop.words.additional
  }
  
  tok_fun = word_tokenizer  # using word & not space tokenizers
  
  if (bigram.encoding == TRUE){
    
    # data = data.frame(id = 1:length(text),text = text, stringsAsFactors = F)
    
    # print("finding bi-grams for encoding with selected criteria")
    
    it_0 = itoken( text,
                   tokenizer = tok_fun,
                   ids = id,
                   progressbar = F)
    
    vocab = create_vocabulary(it_0, ngram = c(2L, 2L))
    pruned_vocab = data.frame(prune_vocabulary(vocab, term_count_min = bigram.min.freq))
    
    replace_list = pruned_vocab$term[order(pruned_vocab$term_count, decreasing = T)]
    
    # Cut the bi-grams upto 200 words
    
    if (length(replace_list) > 200){
      replace_list = replace_list[1:200]
    }
    
    if (length(replace_list) > 1){     # 0, my edit 1
      text = paste("",text,"")
      
      pb <- txtProgressBar(min = 1, max = (length(replace_list)), style = 3) ; i = 0
      
      # print(paste("Encoding",length(replace_list),"bi-grams as unigram"))
      for (term in replace_list){
        i = i + 1
        focal.term = gsub("_", " ",term)        # in case dot was word-separator
        replacement.term = term
        text = gsub(paste("",focal.term,""),paste("",replacement.term,""), text)
        # setTxtProgressBar(pb, i)
      }                  
    } else {
      print("No bigram to encode with selected criteria")}
  }
  
  # print("Creating Document Term Matrix")
  # Create DTM
  it_m = itoken(text,
                tokenizer = tok_fun,
                ids = id,
                progressbar = F)
  
  vocab = create_vocabulary(it_m)
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = min.dtm.freq)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  
  # print("Creating Term Co-occurrence Matrix")
  
  # vectorizer = vocab_vectorizer(pruned_vocab,
  #                               grow_dtm = FALSE,
  #                               skip_grams_window = skip.grams.window)
  # 
  # tcm = create_tcm(it_m, vectorizer) # func to build a TCM
  
  # print("Done!!")
  
  out = list(dtm = dtm_m)#, tcm = tcm)
  return(out)
}

bigram.collocation <- function(text1){   # text1 from readLines() is input
  
  # require(magrittr)
  # require(tidytext)
  # require(dplyr)
  # require(tidyr)
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  # create words df
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>%
    anti_join(stop_words) %>% 
   dplyr::count(word, sort = TRUE) #%>% 
  text_df
  
  # create bigrams df
  bigram_df <- data_frame(text1) %>% 
    unnest_tokens(bigrams, text1, token = "ngrams", n = 2) %>%
   dplyr:: count(bigrams, sort = TRUE) %>%
    ungroup() %>%
    
    # separate & filter bigrams for stopwords
    separate(bigrams, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!(word1 %in% stop_words$word)) %>%
    dplyr::filter(!(word2 %in% stop_words$word)) #%>%
  
  bigram_df              
  
  # create a merged df
  new_df = bigram_df %>% mutate(k1 = 0) %>% mutate(k2 = 0) # %>%
  
  for (i1 in 1:nrow(bigram_df)){
    
    a0 = which(bigram_df$word1[i1] == text_df$word) 
    ifelse((length(a0) > 0), { new_df$k1[i1] = text_df$n[a0] }, next ) 
    
    a1 = which(bigram_df$word2[i1] == text_df$word) 
    ifelse((length(a1) > 0), { new_df$k2[i1] = text_df$n[a1] }, next ) 
    
  } # i1 loop ends
  
  new_df1 = new_df %>% filter(n > 1) %>% mutate(coll.ratio = (n*nrow(new_df))/(k1*k2)) %>%
    filter(coll.ratio >= 1) %>%
    unite(bigram_united, word1, word2) %>%
    arrange(desc(coll.ratio)) %>% 
    dplyr::select(bigram_united, n, coll.ratio) 
  new_df1 = data.frame(new_df1)
  return(new_df1)
}   # func ends

split_by_puncts <- function(puncts, test0){
  for (i0 in 1:length(puncts)){test0 = str_replace_all(test0, puncts[i0], "@@")}; test0
  a0 = tibble(phrases = str_split(test0, "@@"))
  return(a0)
} # util func ends

concordance.r <- function(text1,  # corpus
                          word1,  # focal word for whcih context is sought
                          k,
                          regx){     # context window length in words on either side
  
  require(magrittr)
  require(tidytext)
  require(dplyr)
  require(tidyr)
  require(stringi)
  
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>% 
    
    # build an index for word positions in the corpus
    mutate(index = 1) %>% mutate(wordnum = 1:sum(index)) %>% dplyr::select(-index) #%>%
  
  text_df
  
  if(regx==TRUE){
    a0 = which(stringr::str_detect(text_df$word,word1))
  }else{
    #a0 = which(stringr::str_detect(text_df$word,word1))
    # a0 = grep(text_df$word,pattern = word1)
    a0 = which(text_df$word == word1)
  }
  a1 = matrix(0, nrow = length(a0), ncol = 3)
  colnames(a1) = c("start", "focal", "stop")
  for (i1 in 1:nrow(a1)){a1[i1, 1] = max(0, a0[i1]-k) 
  a1[i1, 2] = a0[i1]
  a1[i1, 3] = min(nrow(text_df), a0[i1]+k)  }
  head(a1)
  list0 = vector("list", length = length(a0))
  if(regx==TRUE){
    for (i2 in 1:length(list0)){
      vec2join <- text_df$word[a1[i2,1]:a1[i2, 3]]
      vec2join <- append(vec2join,"<mark>",after = k)
      vec2join <- append(vec2join,"</mark>",after=k+2)
      list0[[i2]] = stri_join(vec2join, collapse=" ") 
      #list0[[i2]] = gsub(word1, paste0('<mark>', word1, '</mark>', collapse=""), list0[[i2]])   # gsub(pattern, replacement, x)
    } # i2 ends
  }else{
    for (i2 in 1:length(list0)){
      list0[[i2]] = stri_join(text_df$word[a1[i2,1]:a1[i2, 3]], collapse=" ") 
      list0[[i2]] = gsub(word1, paste0('<mark>', word1, '</mark>', collapse=""), list0[[i2]])   # gsub(pattern, replacement, x)
    } # i2 ends
    list0[[2]]
  }
  # creat a list to store the contexts or concordances of word1  
  
  
  
  
  # read list into dataframe for easier display of output  
  list_df = data.frame(NULL)
  for (i2 in 1:length(a0)){list_df[i2,1] = list0[[i2]]}
  colnames(list_df) = 'concordance'
  
  return(list_df) } # func ends




## define func to stopword-remove from raw_corpus
drop_stopwords_corpus <- function(raw_corpus, custom.stopwords=NULL, use.tidy.stopwords=FALSE){
  
  library(tidyverse)
  library(tidytext)
  library(stringr)
  
  # setup stop.words df first
  stop.words = data.frame(word = unique(c(custom.stopwords, c("the", "a", "an", "of"))), stringsAsFactors=FALSE)
  if (use.tidy.stopwords == "TRUE") {
    stop.words = data.frame(word = unique(c(unlist(stop.words), stop_words$word)), stringsAsFactors=FALSE)}
  
  ## piped workflow for stopword-removal from corpus
  a0 = raw_corpus %>% data_frame() %>% 
    mutate(docID = row_number()) %>% rename(text = ".") %>% select(docID, text) %>%
    
    # sentence-tokenize and build sentence layer
    unnest_tokens(sentence, text, token = "sentences") %>% 
    # group_by(docID) %>% 
    mutate(sentID=row_number()) %>% 
    select(docID, sentID, sentence) %>%
    
    # word-tokenize and drop stopwords
    unnest_tokens(word, sentence) %>% 
    anti_join(stop.words) #%>%
  
  # rebuilding corpus, first at sentence layer
  sent_corpus = data.frame(docID = numeric(), sentID = numeric(), 
                           sentence = character(), stringsAsFactors=FALSE)
  
  for (i1 in 1:max(a0$sentID)){
    a100 = a0[a0$sentID == i1,]   # for each sentence, collect all cleaned tokens
    sent_corpus[i1, 1] =  a100$docID[1]
    sent_corpus[i1, 2] =  a100$sentID[1]
    sent_corpus[i1, 3] = paste0(str_c(a100$word, collapse=" "), ".")   # using str_c()
  }   # i1 ends
  
  b0 = which(is.na(sent_corpus$docID))
  if (length(b0)>0) {sent_corpus = sent_corpus[-b0,]}
  
  # rebuilding corpus, now at doc layer
  doc_corpus = data.frame(docID = numeric(), text = character(), stringsAsFactors=FALSE)
  
  for (i2 in 1:max(sent_corpus$docID)){
    a200 = sent_corpus[sent_corpus$docID == i2,] 	
    doc_corpus[i2, 1] = a200$docID[1]	
    doc_corpus[i2, 2] = str_c(a200$sentence, collapse=" ")    }    # i2 ends
  
  return(doc_corpus) }    # drop_stopwords_corpus() func ends 

# # testing above func
# speech = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt')
# system.time({  new_corpus = drop_stopwords_corpus(speech, use.tidy.stopwords=TRUE)  })    # 0.26 secs
# 
# 
# a21 <- a2

## == define collect_terms() routine for calling inside replace_bigrams()
collect_terms <- function(a21){  # sentence has colms {docID, sentID, word1, word2, bigram1, out_colm}
  
  b100 = (is.na(a21$bigram1))
  a21$bigram1[b100] = a21$word1[b100]
  
  a21$word2[!(b100)] = ""
  bigram2 = c(a21$bigram1[2:nrow(a21)], a21$word2[nrow(a21)]); bigram2
  bigram2[!(b100)] = ""; bigram2
  bigram2 = c(a21$word1[1], 
              bigram2[1:(length(bigram2)-2)], 
              paste(bigram2[nrow(a21)-1], a21$word2[nrow(a21)], sep=" ")); bigram2
  
  a21$out_colm = bigram2
  # return(a21)  }
  return(a21)  }

# a21 = a2[a2$sentID == 2,] %>% mutate(out_colm = bigram1); a21; collect_terms(a21)


# raw_corpus <- speech
# 
# 

#raw_corpus <- Document
#stopw_list <- c("will","shall")

## == brew func to build bigrams.
replace_bigram <- function(raw_corpus, stopw_list, min_freq = 2){
  
  # first filter out stopwords.
  print(
    system.time({ 
      
      # textdf = drop_stopwords_corpus(raw_corpus, custom.stopwords=c("and", "to")) 
      #corpus = str_replace_all(tolower(raw_corpus), c(" of ", " the ", " and "), " ") 
      
      corpus = str_replace_all(tolower(raw_corpus[,2]), stopw_list, " ") 
      textdf = data.frame(docID=seq(1:length(corpus)),nick=raw_corpus[,1], text=corpus, stringsAsFactors=FALSE)
      a1 = textdf$text %>% split_by_puncts(puncts,.) #----New
      temp <-lapply(a1$phrases, function(x) str_c(x,collapse = ","))
      temp <- unlist(temp)
      textdf$text <- temp
      temp <- NULL
      
      
    })   ) # 0.26 secs for speech
  
  # create sentence layer and unnesting bigrams
  a0 = textdf %>% 	
    unnest_tokens(sentence, text, token = "sentences") %>% 
    dplyr::mutate(sentID=row_number()) %>% 
    dplyr::select(docID, sentID,nick, sentence) %>%
    
    # bigram-tokenize, count and filter by freq
    group_by(sentID) %>% unnest_tokens(ngram, sentence, token = "ngrams", n = 2) %>% ungroup() #%>%
  
  a0
  
  # creating frequent bigrams for replacement
  a1 = a0 %>% 
    dplyr::count(ngram, sort=TRUE) %>% dplyr::filter(n >= min_freq) %>% 
    separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>% 
    
    # drop all stopwords in the bigrams of interest
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%
    
    unite(bigram1, c("word1", "word2"), sep="_")	%>% 
    dplyr::select(ngram, bigram1)
  a1
  
  # merging the 2 above dfs
  a2 = dplyr::left_join(a0, a1, by=c("ngram" = "ngram")) %>%
    separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>%
    dplyr:: select(-ngram)
  a2
  
  
  a3 <- collect_terms(a2)
  #a3 <- a21
  # building bigram2
  # a3 = a2 %>% mutate(out_colm = bigram1) %>% 
  #   group_by(sentID) %>% 
  #   transmute(out_colm = collect_terms()) %>% 
  #   ungroup() # %>%
  # 
  # a3<- a21
  b0 = which(is.na(a3$docID))
  if (length(b0)>0) {a3 = a3[-b0,]}
  
  a3
  
  # rebuilding corpus, first at sentence layer
  sent_corpus = data.frame(docID = numeric(), sentID = numeric(), nick=character(),
                           sentence = character(), stringsAsFactors=FALSE)
  
  for (i1 in 1:max(a3$sentID)){
    a100 = a3[a3$sentID == i1,]   # for each sentence, collect all cleaned tokens
    sent_corpus[i1, 1] =  a100$docID[1]
    sent_corpus[i1, 2] =  a100$sentID[1]
    sent_corpus[i1,3] = a100$nick[1]
    sent_corpus[i1, 4] = paste0(str_c(a100$out_colm, collapse=" "), ".")   # using str_c()
  }   # i1 ends
  
  b0 = which(is.na(sent_corpus$docID))
  if (length(b0)>0) {sent_corpus = sent_corpus[-b0,]}
  
  # rebuilding corpus, now at doc layer
  doc_corpus = data.frame(docID = numeric(),nick=character(), text = character(), stringsAsFactors=FALSE)
  
  for (i2 in 1:max(sent_corpus$docID)){
    a200 = sent_corpus[sent_corpus$docID == i2,] 	
    doc_corpus[i2, 1] = a200$docID[1]	
    doc_corpus[i2,2] = a200$nick[1]
    doc_corpus[i2, 3] = str_c(a200$sentence, collapse=" ")    }    # i2 ends
  
  return(doc_corpus) }    # replace_bigrams() func ends 
# 
# # testing on speech
#speech = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt')


# stopw_list <- c("dear","countrymen")
# system.time({ bigrammed_corpus = replace_bigram(speech, min_freq = 2,stopw_list = stopw_list) }) # 0.66 secs
# bigrammed_corpus[1:3,]
# 









