#################################################
#               Basic Text Analysis             #
#################################################
library(shiny)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(visNetwork)

library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)
library(tools)
library(pdftools)

if (!require(shinyWidgets)) {install.packages("shinyWidgets")};  library(shinyWidgets)
if (!require(stringr)) {install.packages("stringr")};  library(stringr)
if (!require(DT)){install.packages('DT')}; library('DT')
if (!require(quanteda)){install.packages('quanteda', type="source")}; library('quanteda')

options(shiny.maxRequestSize = 50*1024^2) 

shinyServer(function(input, output,session) {
  set.seed=2092014   
  
  dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)
        #colnames(Document) <- c("Doc.id","Document")
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        print(input$file$name)
        return(calib)} else if(file_ext(input$file$datapath)=="pdf")
      {          
        pdf_text0 <- pdftools::pdf_text(input$file$datapath)                
        pdf_text1 <- str_replace_all(pdf_text0, 
                                     pattern = "([.!?])\n(\\w)", 
                                     replacement = "\\1\n\n\\2") 
  
        # Collapse multiple repetitions of newline into a paragraph break
        pdf_text1 <- gsub("\n{2,}", "\n\n", pdf_text1)
        pdf_text1 <- gsub("\n\\s{2,}", " ", pdf_text1)
  
        # Combine text from all pages while preserving line breaks
        pdf_text1 <- paste(pdf_text1, collapse = "\n\n")
        pdf_text2 <- str_split(pdf_text1, pattern = "\n\n")
        #Document = pdf_text2
          Doc.id <- seq(1, length(pdf_text2[[1]]))
          calib <- data.frame(Doc.id, pdf_text2)
          colnames(calib) <- c("Doc.id","Documents")
          print(input$file$name)
          return(calib)} else
      {
      Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      Document[,1] <- str_to_title(Document[,1])
      Document[,1] <- make.names(Document[,1], unique=TRUE)
      Document[,1] <- tolower(Document[,1])
      Document[,1] <- str_replace_all(Document[,1],"\\.","_")
      Document<-Document[complete.cases(Document), ]
      Document <- Document[!(duplicated(Document[,1])), ]
      rownames(Document) <- Document[,1]
      return(Document)
      }
      
    }
  })
  
  cols <- reactive({colnames(dataset())})
  output$pre_proc1 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    checkboxInput('html',"Remove HTML tags",value = TRUE)
      }
  })
  
  output$pre_proc2 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    checkboxInput('num',"Remove Numbers",value = TRUE)
  }
  })
  
  y_col <- reactive({
    x <- match(input$x,cols())
    y_col <- cols()[-x]
    return(y_col)
    })
  
  output$id_var <- renderUI({
    print(cols())
    selectInput("x","Select ID Column",choices = cols())
  })
  
  output$doc_var <- renderUI({
    selectInput("y","Select Text Column",choices = y_col())
  })
 
 output$up_size <- renderPrint({
    size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)")
  })
  
  text_summ <- reactive({summary(quanteda::corpus(dataset()[,input$y]))})
  quant_mod <- reactive({quanteda::corpus(dataset()[,input$y])})
    
    output$text <- renderUI({
        req(input$file$datapath)
        str1 <- paste("Total no of documets:", nrow(dataset()))
        str2 <- paste("Range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
        #str3 <- paste("Maximum number of sentence: ",)
        str4 <- paste("Average number of sentences per document: ",round(mean(text_summ()$Sentences),2))
        HTML(paste(str1, str2,str4, sep = '<br/>'))
    })

    output$text2 <- renderUI({
        req(input$file$datapath)
        str2 <- paste("Range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
        #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
        str4 <- paste("Average number of words: ",round(mean(text_summ()$Tokens),2))
        HTML(paste(str2,str4, sep = '<br/>'))
    })
  output$samp_data <- DT::renderDataTable({
    DT::datatable(dataset(),rownames = FALSE)
  })
  
  dtm_tcm =  eventReactive(input$apply,{
    textb = dataset()[,input$y]
    ids = dataset()[,input$x]
    dtm.tcm = dtm.tcm.creator(text = textb,
                              id = ids,
                              std.clean = TRUE,
                              std.stop.words = TRUE,
                              stop.words.additional = unlist(strsplit(input$stopw,",")),
                              bigram.encoding = TRUE,
                              # bigram.min.freq = 20,
                              min.dtm.freq = input$freq,
                              skip.grams.window = 10,
                              html_tags=input$html,
                              numbers = input$num)
      dtm = as.matrix(dtm.tcm$dtm)  
      dtm
      dtm_tcm_obj = list(dtm = dtm)#, tcm = tcm)
      dtm_tcm_obj
  })
  
dtm_idf =  eventReactive(input$apply,{
    textb = dataset()[,input$y]
    ids = dataset()[,input$x]
    dtm.tcm = dtm.tcm.creator(text = textb,
                              id = ids,
                              std.clean = TRUE,
                              std.stop.words = TRUE,
                              stop.words.additional = unlist(strsplit(input$stopw,",")),
                              bigram.encoding = TRUE,
                              # bigram.min.freq = 20,
                              min.dtm.freq = input$freq,
                              skip.grams.window = 10)

      model_tfidf = TfIdf$new()
      dtm = round(as.matrix(model_tfidf$fit_transform(dtm.tcm$dtm)),2)
      
      tempd = dtm*0
      tempd[dtm > 0] = 1
      dtm = dtm + tempd
    dtm_tcm_obj = list(dtm = dtm)#, tcm = tcm)
    dtm_tcm_obj
  })

  ordered_dtm_idf<- reactive({if (is.null(input$file)) {return(NULL)}
    else{
      mat1= dtm_idf()$dtm
      a = colSums(mat1)
      b = order(-a)     # nice syntax for ordering vector in decr order  
      mat2 = mat1[,b]
      return(mat2)
      
    }
    
  })
  
  ordered_dtm <- reactive({if (is.null(input$file)) {return(NULL)}
    else{
      mat1= dtm_tcm()$dtm
      a = colSums(mat1)
      b = order(-a)     # nice syntax for ordering vector in decr order  
      mat2 = mat1[,b]
      return(mat2)     }    
  })
  
  
  output$idf_table <- renderDataTable({
    temp <- ordered_dtm_idf()[1:10,1:10]
    #  temp <- tem[1:10,1:10]
    return(temp)
  })
  
  output$dtm_table <- renderDataTable({
    temp <- ordered_dtm()[1:10,1:10]
    #  temp <- tem[1:10,1:10]
    return(temp)

  })

  word1 <- reactive({
    if (is.null(input$wordl_t1)) {return(NULL)}
    else{
      return(unlist(strsplit(input$wordl_t1, ",")))
    }
  })

#  build_dtm01 <- function(corpus0){    
#    tidy_df = dplyr::tibble(text = corpus0) |>  
#      dplyr::mutate(doc_id = row_number()) |> # Add doc_id first
#      dplyr::rename(text = text) |>       
#      dplyr::select(doc_id, text) |> # Reorder columns      
#      unnest_tokens(word, text) |>
#      dplyr::anti_join(stop_words) |>
#      dplyr::group_by(doc_id) |>
#      dplyr::count(word, sort=TRUE) |>
#      dplyr::rename(value = n)
    #dtm = tidy_df |> cast_sparse(doc_id, word, value)  
    #return(dtm) }

  #corpus_dtm <- reactive({build_dtm01(dataset()[,input$y])})
  #output$custom_cog <- renderVisNetwork(Build_custom_cog(corpus_dtm(), title = "custom COG", word1()))
  output$custom_cog <- renderVisNetwork(Build_custom_cog(dtm_tcm()$dtm, title = "custom COG", word1()))
    
  idfwordcounts = reactive({ return(dtm.word.count(dtm_idf()$dtm))   }) 
  wordcounts = reactive({ return(dtm.word.count(dtm_tcm()$dtm))  })   
  
  output$idf_wordcloud <- renderPlot({  
    if (is.null(input$file)) {return(NULL)}
    else{
      tsum = idfwordcounts()
      tsum = tsum[order(tsum, decreasing = T)]
      dtm.word.cloud(count = tsum,max.words = input$max,title = 'TF-IDF Wordcloud')
    }
  })
  
  output$wordcloud <- renderPlot({    
    if (is.null(input$file)) {return(NULL)}
    else{
      tsum = wordcounts()
      tsum = tsum[order(tsum, decreasing = T)]
      dtm.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
    }
  })
  
#  output$cog.idf <- renderVisNetwork({
#    if (is.null(input$file)|input$apply==0) {return(NULL)}
#    else{
#      distill.cog.tcm(mat1=dtm_idf()$dtm, # input TCM MAT
#                      mattype = "DTM",
#                      title = "COG from TF-IDF Adjacency", # title for the graph
#                      s=input$nodes,    # no. of central nodes
#                      k1 = input$connection)  # No. of Connection with central Nodes
#    }
#  })
  
#  output$cog.dtm <- renderVisNetwork({
#    if (is.null(input$file)|input$apply==0) {return(NULL)}
#    else{
#      distill.cog.tcm(mat1=dtm_tcm()$dtm, # input TCM MAT
#                      mattype = "DTM",
#                      title = "COG from DTM Adjacency", # title for the graph
#                      s=input$nodes,    # no. of central nodes
#                      k1 = input$connection)  # No. of Connection with central Nodes
#    }
#  })

  output$idf_size  <- renderPrint({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      size = dim(t(dtm_idf()$dtm))
      dtm_size = paste("TF-IDF matrix size is ", size[2]," X ", size[1],". Below are the first 10 docs X top 10 tokens")
      return(dtm_size)
    }
  })

  output$dtmsize  <- renderPrint({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      size = dim(t(dtm_tcm()$dtm))
      dtm_size = paste("Document Token Matrix (DTM) size is ", size[2]," X ", size[1],". Below are the first 10 docs X top 10 tokens")
      return(dtm_size)
    }
  })
  
  output$dtmsummary2  <- renderDataTable({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      data.frame(score = idfwordcounts()[order(idfwordcounts(), decreasing = T)][1:input$max])
    }
  })
  
  output$dtmsummary1  <- renderDataTable({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
    }
  })
  
  
  # This is your reactive element.
  df_reactive <- eventReactive(input$apply,{
    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      a0 = concordance.r(dataset()[,input$y],input$concord.word, input$window,input$regx)
      a0  }    
  })
  
  output$concordance = renderDataTable({
    datatable(df_reactive(), escape = F, options = list(dom = "lt"))
  })
  
  bi_gram <- reactive({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      #a1 = dataset()[,input$y] %>% split_by_puncts(puncts,.) #N-------------
      a1 = dataset()[,input$y] |> split_by_puncts(puncts) 
      a2 = tibble(phrases = unlist(a1));
      a0 = bigram.collocation(a2)      
      #a0 = bigram.collocation(dataset()$Document)
      a0$coll.ratio <- round(a0$coll.ratio,2)
      a0 = a0[order(a0$n, decreasing = T),]
      if (nrow(a0) > 100){
        a1 = a0[1:100,]
      } else {
        a1 = a0
      }
      a1
    }
    return(a1)
  })
  
  output$bi.grams = renderDataTable({ bi_gram()  })
  output$bi_word_cloud <- renderPlot({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      wordcloud(bi_gram()$bigram_united, bi_gram()$n,     # words, their freqs 
                scale = c(4, 1),     # range of word sizes
                min.freq = .01,                     # min.freq of words to consider
                max.words = input$max,       # max #words
                colors = brewer.pal(8, "Dark2"))
    }
  })
  
  output$dtm_text <- renderText({
    size = dim(ordered_dtm())
    dtm_size = paste("DTM has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
  })
  
  output$tfidf_text <- renderText({
    size = dim(ordered_dtm_idf())
    dtm_size = paste("TF-IDF has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
  })
  
  output$bi_text <- renderText({
    size = dim(bigram_data())
    dtm_size = paste("Bi-gram corpus has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
  })
  
  output$download_tfidf <- downloadHandler(
    filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_tfidf.csv",collapse = "") },
    content = function(file) {      
      new_dtm <- ordered_dtm_idf()
      write.csv(new_dtm, file, row.names=T)
    }
  )
 
  output$download_dtm <- downloadHandler(
    filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_dtm.csv",collapse = "") },
    content = function(file) {
      new_dtm <- ordered_dtm()
      write.csv(new_dtm, file, row.names=T)
    }
  )
  
  
  bigram_data <- reactive({
                bigrammed_corpus = replace_bigram(dataset(),
                                                  min_freq = 2,
                                                  stopw_list=unlist(strsplit(input$stopw,","))
                                                  )
                return(bigrammed_corpus[,2:3])
                })

  output$download_bigram <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_bigram_corpus.csv",collapse = " ") },
    content = function(file) {
      write.csv(bigram_data(), file,row.names=FALSE)
    }
  )
  
 # output$downloadData1 <- downloadHandler(
 #   filename = function() { "Gen Ai in the mktg classroom CNS 2024.pdf" },
 #   content = function(file) {
 #     writeLines(readLines("data/Gen Ai in the mktg classroom CNS 2024.pdf"), file)
 #   }
 # )
  
#  output$downloadData2 <- downloadHandler(
#    filename = function() { "Indian_patent-act-1970.pdf" },
#    content = function(file) {
#      writeLines(readLines("data/Indian_patent-act-1970.pdf"), file)
#    }
#  )
  
 output$downloadData3 <- downloadHandler(
  filename = function() { "uber_reviews_itune.csv" },
  content = function(file) {
    write.csv(read.csv("data/uber_reviews_itune.csv"), file, row.names=F, col.names=F, fileEncoding = "UTF-8")
  }
)  
  
  output$downloadData4 <- downloadHandler(
    filename = function() { "GameOfThrones_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Game of Thrones IMDB reviews.txt"), file)
    }
  )  
output$downloadData5 <- downloadHandler(
    filename = function() { "Amazon-Q4_2023_Transcript.pdf" },
    content = function(file) {
      writeLines(readLines("data/Amazon-Q4_2023_Transcript.pdf"), file)
    }
  )  
  
})
