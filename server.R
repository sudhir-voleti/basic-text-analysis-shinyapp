#################################################
#               Basic Text Analysis             #
#################################################

shinyServer(function(input, output,session) {
  set.seed=2092014   

dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
      else {
        Document = readLines(input$file$datapath)
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        return(calib)}
      })

dtm_tcm =  reactive({
  
  textb = dataset()$Document
  ids = dataset()$Doc.id

  dtm.tcm = dtm.tcm.creator(text = textb,
                            id = ids,
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional = unlist(strsplit(input$stopw,",")),
                            bigram.encoding = TRUE,
                            # bigram.min.freq = 20,
                            min.dtm.freq = input$freq,
                            skip.grams.window = 10)
  if (input$ws == "weightTf") {
    dtm = as.matrix(dtm.tcm$dtm)  
  } 
  
  if (input$ws == "weightTfIdf"){
    model_tfidf = TfIdf$new()
    dtm = as.matrix(model_tfidf$fit_transform(dtm.tcm$dtm))
    
    tempd = dtm*0
    tempd[dtm > 0] = 1
    dtm = dtm + tempd
  }  
  
  # tcm = dtm.tcm$tcm
  dtm_tcm_obj = list(dtm = dtm)#, tcm = tcm)
})

wordcounts = reactive({
  
  return(dtm.word.count(dtm_tcm()$dtm))
  
}) 

output$wordcloud <- renderPlot({
  tsum = wordcounts()
  tsum = tsum[order(tsum, decreasing = T)]
  dtm.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
  
      })
      
output$cog.dtm <- renderVisNetwork({
  
  distill.cog.tcm(mat1=dtm_tcm()$dtm, # input TCM MAT
                  mattype = "DTM",
                  title = "COG from DTM Adjacency", # title for the graph
                  s=input$nodes,    # no. of central nodes
                  k1 = input$connection)  # No. of Connection with central Nodes
      })

# output$cog.tcm <- renderPlot({
#   
# distill.cog.tcm(mat1=dtm_tcm()$tcm, # input TCM MAT,
#                   mattype = "TCM",
#                   title = "TCM from glove algorithm - Graph ", # title for the graph
#                   s=input$nodes,    # no. of central nodes
#                   k1 = input$connection)  # No. of Connection with central Nodes
#   
#       })
        

output$dtmsummary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
  else {
    sortedobj = dtm_tcm()$dtm[,order(wordcounts(), decreasing = T)]
    (t(sortedobj[1:10,1:10]))
  }
      })

output$dtmsize  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    size = dim(t(dtm_tcm()$dtm))
    dtm_size = paste("Term Document Matrix (TDM) size is ", size[1]," X ", size[2],". Below are the first 10 docs X top 10 tokens")
    return(dtm_size)
  }
})

output$dtmsummary1  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
  }
})


output$concordance = renderPrint({
  a0 = concordance.r(dataset()$Document,input$concord.word, input$window)
  concordance = a0$concordance
  concordance
})

output$bi.grams = renderPrint({
  a0 = bigram.collocation(dataset()$Document)
  a0 = a0[order(a0$n, decreasing = T),]
  if (nrow(a0) > 100){
    a1 = a0[1:100,]
  } else {
    a1 = a0
  }
  a1
})

output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )

})
