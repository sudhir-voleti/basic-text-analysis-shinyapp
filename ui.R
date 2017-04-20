#################################################
#               Basic Text Analysis             #
#################################################

library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(stringi)
library(magrittr)
library(tidytext)
library(dplyr)
library(tidyr)


shinyUI(fluidPage(
  
 titlePanel("Basic Text Analysis"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    selectInput("ws", "Weighing Scheme", 
                c("weightTf","weightTfIdf"), selected = "weightTf"), # weightTf, weightTfIdf, weightBin, and weightSMART.
    
    sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 0,  max = 100, value = 2),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 50),  
    
    numericInput("nodes", "Number of Central Nodes in co-occurrence graph", 4),
    numericInput("connection", "Number of Max Connection with Central Node", 5),
    
    textInput("concord.word",('Enter word for which you want to find concordance'),value = 'good'),
    sliderInput("window",'Concordance Window',min = 2,max = 100,5),
    
    submitButton(text = "Apply Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
        tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                          #, height = 280, width = 400
                         verbatimTextOutput("start"))
                ,
                tabPanel("Example dataset", h4(p("Download Sample text file")), 
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")),
                
                tabPanel("TDM & Word Cloud",h4("Term Document Matrix [1:10,1:10"),
                         verbatimTextOutput("dtmsummary"),
                         br(),
                         br(),
                         h4("Word Cloud"),
                         plotOutput("wordcloud",height = 700, width = 700),
                         h4("Weights Distribution of Wordcloud"),
                         verbatimTextOutput("dtmsummary1")),
                tabPanel("Term Co-occurrence",
                         plotOutput("cog.dtm",height = 700, width = 700)
                         ),
                tabPanel("Bigram",
                         verbatimTextOutput("bi.grams")
                         ),
                tabPanel("Concordance",
                         verbatimTextOutput("concordance"))
                )
           )
       )
    )

