#################################################
#               Basic Text Analysis             #
#################################################

library("shiny")
library("tm")
library("wordcloud")
library("qdap")
library("RWeka")
library("igraph")

shinyServer(function(input, output,session) {
  set.seed=2092014   
  
  len = function(x){
    if ( x == "-" && length(x) == 1)  {return (0)} 
    else {return(length(unlist(x)))}
  }
  
    dataset <- reactive({
    
    if (is.null(input$file)) {return(NULL)}
    else {
      
      Document = readLines(input$file$datapath)
      Doc.id=seq(1:length(Document))
      calib=data.frame(Doc.id,Document)
      return(calib)}
    
  })
  
  x0 <- reactive({
    
    if (is.null(input$file)) {return(NULL)}
    else {
    
    progress <- shiny::Progress$new(session, min=1, max=2)
    on.exit(progress$close())
    
    progress$set(message = 'Text Cleaning in progress',
                 detail = 'This may take a while...')
    
      calib = (dataset())
      x  = as.character(calib$Document)        
      x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
      x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
      x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
      x  =  tolower(x)                          # convert to lower case characters
      x  =  removePunctuation(x)                # removing punctuation marks
      x  =  removeNumbers(x)                    # removing numbers
      x  =  stripWhitespace(x)                  # removing white space
      x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
     
      text  =  x;  rm(x)
      
      stp_word1 = stopwords('english')
      stp_word2 = readLines("data/stopwords.txt")
      comn  = unique(c(stp_word1, stp_word2))
      stp_word = unique(c(gsub("'","",comn),comn))
      sto = unique(c(stp_word,unlist(strsplit(input$stopw,","))))
      
      myCorpus = Corpus(VectorSource(text))
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,c(sto))
      myCorpus = tm_map(myCorpus, stripWhitespace)   # removes white space
      myCorpus = as.character(unlist(myCorpus))
      
      x1 = Corpus(VectorSource(myCorpus))
      return(x1)
      
      progress$set(value = 2)
      # Sys.sleep(0.5)
      
    }
  })
      
    tdm = reactive({ 

      if (is.null(input$file)) {return(NULL)}
      else {
        
      progress <- shiny::Progress$new(session, min=3, max=5)
      on.exit(progress$close())
      progress$set(message = 'Bigram creation in progress',
                   detail = 'This may take a while...')
      progress$set(value = 3)
      

      x1 = x0()
        calib = (dataset())
      ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
      
      tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                    tolower = TRUE, 
                                                    removePunctuation = TRUE,
                                                    removeNumbers = TRUE,
                                                    stopwords = TRUE ))
      tdm = tdm0; rm('tdm0')
      a1 = apply(tdm, 1, sum)  
      a2 = ((a1 > 1))
      tdm.new = tdm[a2, ]
      rm('a1','a2','tdm')
      
      # remove blank documents (i.e. columns with zero sums)
      a0 = NULL; 
      for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
      length(a0)    # no. of empty docs in the corpus
      if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
      
      rm('a0','tdm.new')
      dim(tdm.new1)    # reduced tdm
      x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
      dim(x1mat);    # store[i1, 5] = ncol(x2mat);
      
      test = colnames(x1mat); 
      test1 = gsub(" ",".", test);  # replace spaces with dots
      colnames(x1mat) = test1
      
      a11 = apply(x1mat, 2, sum)
      a12 = order(a11, decreasing = T)
      a13 = as.matrix(a11[a12])
      
      #x1 = tm_map(x1, stripWhitespace)
      x1 = unlist(lapply(x1, content)) 
      for (i in 1:nrow(a13)){    
        focal.term = gsub("\\.", " ", rownames(a13)[i])
        replacement.term = gsub(" ", "-", focal.term)
        replacement.term=paste("",replacement.term,"")
        x1 = gsub(focal.term, replacement.term, x1)  
        
      }	# now, our x corpus has the top 400 bigrams encoded as unigrams
      
      progress$set(message = 'TDM creation in progress',
                   detail = 'This may take a while...')
      progress$set(value = 4)
      
      
      x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
      tdm = TermDocumentMatrix(x1)
      colnames(tdm) = calib$Doc.id
      
      if (input$ws == "tf"){
        # cat("Hi")
        tdm = weightTf(tdm)
      }
      else if (input$ws == "tf-idf"){
        
        tdm = weightTfIdf(tdm, normalize = F )
        
        }
      
      tdm = tdm[rowSums(as.matrix(tdm)) > 1,] 
      tdm = tdm[,colSums(as.matrix(tdm)) > 0] 
      
      return(tdm)
      }
      
      progress$set(value = 5)
  })
  
      output$wordword <- renderPlot({    
        
        # wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
        
        mat = as.matrix(tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),])
        mat = mat[1:input$max,]
        
        cmat  =  mat %*% t(mat)
        diag(cmat) = 0
        cmat[cmat <  quantile(cmat,.90)] = 0
        
        graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
        # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
        plot(  graph,			#the graph to be plotted
              layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
              #main='Organizational network example',	#specifies the title
              #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
              vertex.frame.color='blue', 		#the color of the border of the dots 
              vertex.label.color='black',		#the color of the name labels
              vertex.label.font=1,			#the font of the name labels
              vertex.size = .00001,
              # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
              vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
        )
      })
        
        
        ###################})
    output$dtmsummary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        
      cat("Weighing Scheme is ",input$ws,"\n\n")
      # tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),]
      inspect(tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),][1:10,1:10])}
      
    })

output$wordcloud <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      m  = as.matrix((tdm()))
      v = sort(rowSums(m), decreasing = TRUE)
      wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
    }
  })
  
output$dtmsummary1  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    
  # cat("Weights of Terms in  Wordcloud","\n\n")
  m  = as.matrix(tdm())
  v = sort(rowSums(m), decreasing = TRUE)
  z = data.frame(v[1:input$max]); colnames(z) = "Weights"
  z
  }
})


norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)

m_norm = reactive({
  m_norm <- norm_eucl(t(tdm()))
  return(m_norm)
})

data.pca <- reactive({prcomp(m_norm(),center = TRUE,scale. = TRUE)})

output$pcaplot <- renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {

plot(data.pca(), type = "l"); abline(h=1)
}
})

clust = reactive({
set.seed(19890201)
cl <- kmeans(m_norm(), input$seg)
return(cl)
})

da = reactive({
  da = data.frame(as.numeric(colnames(tdm())),clust()$cluster)  
  colnames(da) = c("Doc.id","Seg Membership")
  return(da)
})


output$summary  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    
  fit = clust()
  Segment.Membership = as.character(fit$cluster)
  # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
  Summary = list( Count = table(Segment.Membership),Segment.Membership = Segment.Membership )
  Summary
  }
})


output$segplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    
  plot_output_list <- lapply(1:input$seg, function(i) {
    plotname <- paste("plot", i, sep="")
    plotOutput(plotname, height = 700, width = 700)
  })
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
  }
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.
max_plots = 20

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      cl = clust()
      pct = round((cl$size/sum(cl$size))*100,2)
      
      stdm = t(t(tdm())[(cl$cluster == my_i),])
      m  = as.matrix(stdm)
      v = sort(rowSums(m), decreasing = TRUE)
      v = data.frame(v[v > 0 ])
      if (nrow(v) >= 40) {n = 40}
      else {n = nrow(v)}
      top_word = as.matrix(v[1:n,])
      row.names(top_word) = row.names(v)[1:n]
      
      # title(main =paste("Segment ",my_i))
      wordcloud(rownames(top_word), top_word,  scale=c(4,1), 1, random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
      mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
      
      box(lty = '11', col = 'black')
    })
  })
}

#-----------------------------------------

output$segcoocrplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    plot_output_list <- lapply(1:input$seg, function(i) {
      plotname <- paste("plot1", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.
max_plots = 20

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot1", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      cl = clust()
      pct = round((cl$size/sum(cl$size))*100,2)
      
      stdm = t(t(tdm())[(cl$cluster == my_i),])
      m  = as.matrix(stdm)
      v = sort(rowSums(m), decreasing = TRUE)
      v = data.frame(v[v > 0 ])
      if (nrow(v) >= 40) {n = 40}
      else {n = nrow(v)}
      
      mat = as.matrix(stdm[order(rowSums(as.matrix(stdm)),decreasing=T),][1:n,])
      
      cmat  =  mat %*% t(mat)
      diag(cmat) = 0
      cmat[cmat <  quantile(cmat,.90)] = 0
      
      graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
      # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
      plot(  graph,			#the graph to be plotted
             layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
             #main='Organizational network example',	#specifies the title
             #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
             vertex.frame.color='blue', 		#the color of the border of the dots 
             vertex.label.color='black',		#the color of the name labels
             vertex.label.font=1,			#the font of the name labels
             vertex.size = .00001,
             # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
             vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
             # title( main = paste("Segemnt ",my_i))
             )
      
      
      mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
      
      box(lty = '11', col = 'black')
    })
  })
}

#-----------------------------------------

p  = reactive({
  
  
  progress <- shiny::Progress$new(session, min=6, max=10)
  on.exit(progress$close())
  progress$set(message = 'Sentiment Analysis Started',
               detail = 'This may take a while...')
  progress$set(value = 6)
  
  # head(data$Document)
  
  data = dataset()
  data$Document = gsub("<.*?>", "", data$Document)
  data$Document = gsub("/", "", data$Document) 
  data$Document = gsub("\\\\", "", data$Document) 
  data$Document = gsub("\\)", "", data$Document)
  data$Document = gsub("\\(", "", data$Document)
  
  data$Document =  tolower(data$Document)
  
  progress$set(message = 'Calculating Sentiment Score',
               detail = 'This may take a while...')
  progress$set(value = 7)
  
  p = with(data, polarity(Document, Doc.id))
  
  
  progress$set(message = 'Identifying positive/negative words',
               detail = 'This may take a while...')
  progress$set(value = 8)
  
  pwords = gsub(" ","-",setdiff(unique(unlist(p$all$pos.words)),"-"))
  nwords = gsub(" ","-",setdiff(unique(unlist(p$all$neg.words)),"-"))
  
  repl = c(pwords[grep("-",pwords)],nwords[grep("-",nwords)])
  
  progress$set(message = 'Creating Positive/Negative TDMS',
               detail = 'This may take a while...')
  progress$set(value = 9)
  
  if (length(repl) != 0) {
  text = data$Document
  for (i in 1:length(repl)){
    fterm = gsub("-"," ",repl[i])
    rterm = repl[i]
    text = gsub(fterm,rterm,text)
  }
  }
  else {
    text = data$Document 
  }
  myCorpus = Corpus(VectorSource(text))
  tdm = TermDocumentMatrix(myCorpus)
  dim(tdm)
  pos.tdm = tdm[!is.na(match(row.names(tdm),pwords)),]
  neg.tdm = tdm[!is.na(match(row.names(tdm),nwords)),]
  out = list(p, pos.tdm,neg.tdm)
  
  progress$set(message = 'Storing Results',
               detail = 'This may take a while...')
  progress$set(value = 10)
  return(out)
  
  
    })

output$posplot <- renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
   p1 = p()
   pos.tdm = p1[[2]]
   m  = as.matrix(pos.tdm)
   v = sort(rowSums(m), decreasing = TRUE)
   wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
  }
})


output$negplot <- renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
  p1 = p()
  neg.tdm = p1[[3]]
  m  = as.matrix(neg.tdm)
  v = sort(rowSums(m), decreasing = TRUE)
  wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
  }
})


output$negplot <- renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
  p1 = p()
  neg.tdm = p1[[3]]
  m  = as.matrix(neg.tdm)
  v = sort(rowSums(m), decreasing = TRUE)
  wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
  }
})

output$flowplot <- renderPlot({
  p1 = p()
  plot(p1[[1]]$all$polarity, type = "l", ylab = "Polarity Score",xlab = "Document Number")
  abline(h=0)
})

  output$table <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      
    p1 = p()
    d = data.frame(p1[[1]]$all$polarity,dataset())
    d = d[,c(2,1,3)]; colnames(d) = c("Doc.id","Polarity Score","Document")
    
    test = merge(da(),d,by.x ="Doc.id", by.y= "Doc.id", all=T)
    
    test}
    
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  output$table0 <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      tb = da()
      test = merge(tb,dataset(),by.x ="Doc.id", by.y= "Doc.id", all=T)
      return(test)
      }
      }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  output$start = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      
    out = list(tdm(),p(),clust(),data.pca())
    cat("Calculation Completed")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  

})
