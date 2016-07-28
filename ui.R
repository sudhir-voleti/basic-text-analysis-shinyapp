#################################################
#               Basic Text Analysis             #
#################################################

library("shiny")
library("tm")
library("wordcloud")
library("qdap")
library("RWeka")


shinyUI(fluidPage(
  
   tags$head(includeScript("google_analytics.js")),
  
  titlePanel("Basic Text Analysis"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    
    textInput("stopw", ("Enter stop words seperated by comma(,)"), value = "will,can"),
    
    selectInput("ws", "Weighing Scheme", 
                c("tf","tf-idf"), selected = "tf"),
    
    sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 1,  max = 50, value = 4),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 50),  
    
    numericInput("seg", "Number of Segments", 4),
    
#     numericInput("tdmfreq", "Minimum frequency of terms for Topic Model:", 2),
#     
#     h6(div(textOutput("caption1"),style = "color:Blue")),
#     
#     h6(div(textOutput("caption2"))),
#     
#     numericInput("topic", "Number of Topics to fit:", 2),
    
    submitButton(text = "Apply Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is seperated from another document with a new line character.
                           To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refeshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the cumputations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                         h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'Download Example file'),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png"), #, height = 280, width = 400
                         verbatimTextOutput("start"))
                ,
                
                tabPanel("TDM & Word Cloud",h4("Term Document Matrix [1:10,1:10"),
                         verbatimTextOutput("dtmsummary"),
                         br(),
                         br(),
                         h4("Word Cloud"),
                         plotOutput("wordcloud",height = 700, width = 700),
                         h4("Weights Distribution of wordcloud"),
                         verbatimTextOutput("dtmsummary1")),
                tabPanel("Term Co-occurrence",
                         h4("Co-occurrence (Top 50 Term)"),
                         plotOutput("wordword",height = 700, width = 700)),
                #                         
                tabPanel("Segmentation - Summary",
                         h4("Principal component plot"),
                         plotOutput("pcaplot",height = 600, width = 700),
                         h4("Summary"),
                         verbatimTextOutput("summary")),
               
                
                
                tabPanel("Segmentation - Word cloud",uiOutput("segplots")),
                tabPanel("Segmentation - Co-occurrence",uiOutput("segcoocrplots")),
                tabPanel("Segmentation - Data",dataTableOutput("table0")),
                
                tabPanel("Sentiment Analysis",
                         h4("Sentiment Score across Documents"),
                         plotOutput("flowplot",height = 600, width = 700),
                         h4("Postive Words word cloud"),
                         plotOutput("posplot",height = 600, width = 700),
                         h4("Negative Words word cloud"),
                         plotOutput("negplot",height = 600, width = 700)),
                
                tabPanel("Sentiment Score Data",dataTableOutput("table"))
                #                         
                         )
                )
  
)
)