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
    dtm = as.DocumentTermMatrix(dtm.tcm$dtm, weighting = weightTf)  
  } 
  
  if (input$ws == "weightTfIdf"){
    dtm = as.DocumentTermMatrix(dtm.tcm$dtm, weighting = weightTfIdf)
  }
  
  tcm = dtm.tcm$tcm
  dtm_tcm_obj = list(dtm = dtm, tcm = tcm)
})

wordcounts = reactive({
  
  return(dtm.word.count(dtm_tcm()$dtm))
  
}) 

output$wordcloud <- renderPlot({
  tsum = wordcounts()
  tsum = tsum[order(tsum, decreasing = T)]
  dtm.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
  
      })
      
output$cog.dtm <- renderPlot({
  
  distill.cog.tcm(mat1=dtm_tcm()$dtm, # input TCM MAT
                  mattype = "DTM",
                  title = "TCM from DTM Adjacency - Graph", # title for the graph
                  s=input$nodes,    # no. of central nodes
                  k1 = input$connection)  # No. of Connection with central Nodes
      })

output$cog.tcm <- renderPlot({
  
distill.cog.tcm(mat1=dtm_tcm()$tcm, # input TCM MAT,
                  mattype = "TCM",
                  title = "TCM from glove algorithm - Graph ", # title for the graph
                  s=input$nodes,    # no. of central nodes
                  k1 = input$connection)  # No. of Connection with central Nodes
  
      })
        

output$dtmsummary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
  else {
    sortedobj = dtm_tcm()$dtm[,order(wordcounts(), decreasing = T)]
    inspect(t(sortedobj[1:10,1:10]))
  }
      })

output$dtmsummary1  <- renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
  }
})

output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )

})
