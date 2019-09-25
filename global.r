
text.clean = function(x)                    # text data
{ #require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
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
                            skip.grams.window = 5) {
  
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
    text = text.clean(text)
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
    count(word, sort = TRUE) #%>% 
  text_df
  
  # create bigrams df
  bigram_df <- data_frame(text1) %>% 
    unnest_tokens(bigrams, text1, token = "ngrams", n = 2) %>%
    count(bigrams, sort = TRUE) %>%
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


concordance.r <- function(text1,  # corpus
                          word1,  # focal word for whcih context is sought
                          k){     # context window length in words on either side
  
  require(magrittr)
  require(tidytext)
  require(dplyr)
  require(tidyr)
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>% 
    
    # build an index for word positions in the corpus
    mutate(index = 1) %>% mutate(wordnum = 1:sum(index)) %>% dplyr::select(-index) #%>%
  
  text_df
  
  # locate context words for each instance of the focal word
  a0 = which(text_df$word == word1)
  a1 = matrix(0, nrow = length(a0), ncol = 3)
  colnames(a1) = c("start", "focal", "stop")
  for (i1 in 1:nrow(a1)){a1[i1, 1] = max(0, a0[i1]-k) 
  a1[i1, 2] = a0[i1]
  a1[i1, 3] = min(nrow(text_df), a0[i1]+k)  }
  head(a1)
  
  require(stringi)
  # creat a list to store the contexts or concordances of word1  
  list0 = vector("list", length = length(a0))
  for (i2 in 1:length(list0)){
    list0[[i2]] = stri_join(text_df$word[a1[i2,1]:a1[i2, 3]], collapse=" ") 
    list0[[i2]] = gsub(word1, paste0('*', word1, '*', collapse=""), list0[[i2]])   # gsub(pattern, replacement, x)
  } # i2 ends
  list0[[2]]
  
  # read list into dataframe for easier display of output  
  list_df = data.frame(NULL)
  for (i2 in 1:length(a0)){list_df[i2,1] = list0[[i2]]}
  colnames(list_df) = 'concordance'
  
  return(list_df) } # func ends
