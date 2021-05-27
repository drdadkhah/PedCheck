#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#Tutorial link and source of packages have been cited via comments or by library(package-name)
#MIT licence
#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(reshape2)
library(shinythemes)
# install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("shinyalert")
library(shinyalert)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("journal"),
  #shinythemes::themeSelector(), 
  useShinyalert(),
  
  #----------------
  
  setBackgroundImage(
    src = "http://127.0.0.1/background.jpg"
  ),
  
  #-------------
  
  # Application title
  titlePanel("An analysis on new Reddit posts about predatory journals"),
  h6("   Version 1.5, realease data 14 April 2021"),
  br(),
  br(),
  br(),
  br(),
  h4("Number of topics"),
  h5("Default value has been set to 8 based on analysis on the data"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("ktopics",
                  "Number of topics:",
                  min = 1,
                  max = 50,
                  value = 8),
      sliderInput("kwords",
                  "Number of words in each topic:",
                  min = 1,
                  max = 20,
                  value = 7),
      
      sliderInput("postsummary",
                  "Number of representive sentenses for posts:",
                  min = 1,
                  max = 200,
                  value = 3),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      h2("Topics in the posts"),
      plotOutput("distPlot1"),
      br(),
      h2("Top five post in each topic"),
      DT::dataTableOutput("mytable1"),
      br(),
      h2("The summary of posts"),
      DT::dataTableOutput("summary1"),
      br(),
      h2("The posts"),
      DT::dataTableOutput("mytable2"),
     
      
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  #-----------
  #https://cran.r-project.org/web/packages/shinyalert/vignettes/shinyalert.html
  library(shinyalert)
  shinyalert(title="Information",
             text="Hi, in every run, our online application retrieve new posts from Reddit, then analyse them. It means that we need some times to present result to you. The average time between openning the website till presentation of results is 5 minutes.",
             type = "info",
             showConfirmButton = TRUE,
             confirmButtonText = "OK",
             animation = TRUE,
  )
  
  showNotification("Hi, in every run, our online application retrieve new posts from Reddit, then analyse them. It means that we need some times to present result to you. The average time between openning the website till presentation of results is 5 minutes. Thank you for your patience!", action = NULL, duration = 20, closeButton = TRUE,
                   id = NULL, type = c("warning"),
                   session = getDefaultReactiveDomain())
  
  #-----------
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  #https://gist.github.com/sebleier/554280
  mystopwords<-c("i",
                 "me",
                 "my",
                 "myself",
                 "we",
                 "our",
                 "ours",
                 "ourselves",
                 "you",
                 "your",
                 "yours",
                 "yourself",
                 "yourselves",
                 "he",
                 "him",
                 "his",
                 "himself",
                 "she",
                 "her",
                 "hers",
                 "herself",
                 "it",
                 "its",
                 "itself",
                 "they",
                 "them",
                 "their",
                 "theirs",
                 "themselves",
                 "what",
                 "which",
                 "who",
                 "whom",
                 "this",
                 "that",
                 "these",
                 "those",
                 "am",
                 "is",
                 "are",
                 "was",
                 "were",
                 "be",
                 "been",
                 "being",
                 "have",
                 "has",
                 "had",
                 "having",
                 "do",
                 "does",
                 "did",
                 "doing",
                 "a",
                 "an",
                 "the",
                 "and",
                 "but",
                 "if",
                 "or",
                 "because",
                 "as",
                 "until",
                 "while",
                 "of",
                 "at",
                 "by",
                 "for",
                 "with",
                 "about",
                 "against",
                 "between",
                 "into",
                 "through",
                 "during",
                 "before",
                 "after",
                 "above",
                 "below",
                 "to",
                 "from",
                 "up",
                 "down",
                 "in",
                 "out",
                 "on",
                 "off",
                 "over",
                 "under",
                 "again",
                 "further",
                 "then",
                 "once",
                 "here",
                 "there",
                 "when",
                 "where",
                 "why",
                 "how",
                 "all",
                 "any",
                 "both",
                 "each",
                 "few",
                 "more",
                 "most",
                 "other",
                 "some",
                 "such",
                 "no",
                 "nor",
                 "not",
                 "only",
                 "own",
                 "same",
                 "so",
                 "than",
                 "too",
                 "very",
                 "can",
                 "will",
                 "just",
                 "don",
                 "should",
                 "now")
  library("RedditExtractoR")

  #--------
  datareddit<-get_reddit(search_terms = "title:predatory journal", cn_threshold = 0, page_threshold = 100, sort_by = "new", wait_time = 1)
  write.csv(datareddit, "data2.csv")
  #----------
  library(dplyr)
  #readcsv <- read.csv("data1.csv")
  library(readr)
  locale <- readr::locale(encoding = "latin1")
  SAA <- suppressMessages(
    readr::read_csv(file="data2.csv",
                    locale=locale))
  readcsv <-as.data.frame(SAA)
  
  data2<-select(readcsv, title, post_text, URL)
  datacomment<-select(readcsv,comment)
  datacomment<-unique(datacomment)
  data3<-unique(data2)
  data3$ID<-seq.int(nrow(data3))
  data4<-reactive({data3})
  
  #--------------------
  
  datafortbl<-reactive({data4()})
  fulldata<-data3
  write.csv(fulldata, "data3.csv")
  reactive({write.csv(data4(), "data4.csv")})
  #https://bookdown.org/bastiaan_bruinsma/quantitative_text_analysis/Book.pdf
  # install.packages("quanteda")
  # install.packages("topicmodels")
  library("quanteda")
  library("topicmodels")
  posttext<-fulldata$post_text
  fulldata <- paste(fulldata$title,fulldata$post_text)
  fulldata <- as.character(fulldata)
  #-----
  #https://rstudio-pubs-static.s3.amazonaws.com/132792_864e3813b0ec47cb95c7e1e2e2ad83e7.html
  library(tm)
  corpus = Corpus(VectorSource(fulldata))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("the", "and", mystopwords, stopwords("english")))
  corpus =  tm_map(corpus, stripWhitespace)
  dtm <- DocumentTermMatrix(corpus)
  #-----
  
  seed <-123
  k<-reactive({input$ktopics})
  kws<-reactive({input$kwords})
  lda <-reactive({LDA(dtm,k =k(),method ="Gibbs",control =list(seed =seed))})
  #install.packages(c("tidytext","dplyr","DT"))
  library(tidytext)
  library(dplyr)
  library(ggplot2)
  lda_topics <-  reactive({tidy(lda(),matrix ="beta")})
  
  lda_topterms <-reactive({lda_topics()%>%group_by(topic)%>%top_n(kws(), beta)%>%ungroup()%>%arrange(topic,-beta)})
  output$distPlot1 <- renderPlot({lda_topterms()%>%mutate(term =reorder(term, beta))%>%ggplot(aes(term, beta,fill =factor(topic)))+geom_col(show.legend =FALSE)+facet_wrap(~topic,scales ="free")+coord_flip()})
  
  
  lda_documents <-reactive({tidy(lda(),matrix ="gamma")})
  lda_topdoc <-reactive({lda_documents()%>%group_by(topic)%>%top_n(5, gamma)%>%ungroup()%>%arrange(topic,-gamma)})
  #write.csv(lda_topdoc,'lda_topdoc.csv')
  # write.csv(fulldata,'fulldata.csv')
  
  #https://shiny.rstudio.com/articles/datatables.html
  library(DT)
  output$mytable1 = DT::renderDataTable({
    lda_topdoc()
  })
  
  library(DT)
  output$mytable2 = DT::renderDataTable({
    datafortbl()
  })
  
  
  #-----------
  
  #install.packages("LSAfun")
  #install.packages("unpivotr")
  library(stringi)
  library(LSAfun)
  library(unpivotr)
  library(utf8)
  as_utf8(posttext, normalize = TRUE)
  kposts<-reactive({input$postsummary})
  
  #-------------------------
  #https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/
  #install.packages("lexRankr")
  library(lexRankr)
  
  #perform lexrank for top 3 sentences
  top_3 = reactive({lexRankr::lexRank(posttext,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(posttext)),
                            #return 3 sentences to mimick /u/autotldr's output
                            n = kposts(),
                            continuous = TRUE)})
  

  output$summary1 <- DT::renderDataTable({
       as.data.frame(top_3())
     })
  #-----------
  
}


# Run the application 
shinyApp(ui = ui, server = server)

