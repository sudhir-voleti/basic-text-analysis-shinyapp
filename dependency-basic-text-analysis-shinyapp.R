try(require(shiny) || install.packages("shiny"))
try(require(text2vec) || install.packages("text2vec"))
try(require(tm) || install.packages("tm"))
try(require(tokenizers) || install.packages("tokenizers"))
try(require(wordcloud) || install.packages("wordcloud"))
try(require(slam) || install.packages("slam"))
if (!require(visNetwork)) {install.packages("visNetwork")}

try(require(stringi) || install.packages("stringi"))
try(require(magrittr) || install.packages("magrittr"))
try(require(tidytext) || install.packages("tidytext"))
try(require(dplyr) || install.packages("dplyr"))
try(require(tidyr) || install.packages("tidyr"))
try(require(igraph)|| install.packages("igraph"))


library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(visNetwork)
# Update test 123

library(stringi)
library(magrittr)
library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)

