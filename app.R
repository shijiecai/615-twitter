rsconnect::setAccountInfo(name='615shiny', token='FE7C1BFC8A1AD8AE85C6B604A72BCAF4', secret='M2MHQqsMVWm8VwO13DvNDWOVPnkaJyBhBlR3AIhl')

library(shiny)
library(ggplot2)
library(wordcloud)
library(plotly)
library(dplyr)
library(rsconnect)
library(XML)


crword<-readRDS('CristianoRonaldo.rds')
ronaldomap<-readRDS('ronaldomap.rds')
cris<-readRDS("cristiano.sentiment.rds")
state<-map_data('state')
lmword<-readRDS('LeoMessi.rds')
messimap<-readRDS('messimap.rds')
messi<-readRDS("messi.sentiment.rds")
cr<-readRDS("crtimeline.rds")
lm<-readRDS("lmtimeline.rds")


ui<-shinyUI(fluidPage(titlePanel(fluidRow(
  column(8, 
         h3("MA615 Twitter Project"),
         h6("Presented by Shijie Cai. 
            All codes can be found at GITHUB", a(href="https://github.com/shijiecai/615-twitter", "shijiecai Github"))
         ))),
  navbarPage(title= "",
             tabPanel("Introduction",
                      hr("Cristiano VS Messi"),
                      p("Cristiano Ronaldo and Leo Messi are the world most famous active soccer players. 
                        I think both of them are the best player ever, however, comparison between them never stop and this is why I choose these two to compare in my project.")),
             tabPanel("Maps",
                      h4("Where are these tweets from in the United States"),
                      p("Soccer may not be the dominant sport in U.S. 
                        However, these two great players must have great influence, the maps below show you where do people talk about these two player on twitter."),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "mapinput",
                                      label="Select players",
                                      choices = c("Cristiano Ronaldo","Leo Messi"))
                        ),
                        mainPanel(plotOutput("maps"))
                      )
             ),
             
             tabPanel("Word Cloud",
                      h4("Word Cloud for two players"),
                      p("As you can see, because Ronaldo just won his 5th ballon, so the word ballon shows much often in the word cloud. 
On the other side, the team Barcelona, where Messi belongs, shows very often but people seem making a lot of comparisons between Messi and other players. 
Players like Ronaldo and Lewandows appear a lot.
                        "),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "wordinput",
                                      label="Select players",
                                      choices = c("Cristiano Ronaldo","Leo Messi")),
                          sliderInput("freq", "Minimum Frequency:",
                                      min = 1,  max = 50, value = 15),
                          sliderInput("max", "Maximum Number of Words:", 
                                      min = 1,  max = 300,  value = 100)),
                        mainPanel(
                          plotOutput("wordcloud")))
                      ),
             
             
             tabPanel("Sentiment",
                      h4("Sentiment Analysis"),
                      p(" It seems that Cristiano is a more controversial player than Leo Messi. 
                        There are a lot of negative words for Ronaldo than Messi, however, Ronaldo is a more trust worth than Leo Messi."),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "sentiinput",
                                                 label="Select players",
                                                 choices = c("Cristiano Ronaldo","Leo Messi"))
                        ),
                        mainPanel(plotOutput("sentiment")))
             )
             
             
             
             ))
)










# Define server logic required to draw a histogram
server<-function(input, output){
  ### maps
  output$maps<-renderPlot({
    if(input$mapinput=="Cristiano Ronaldo"){
      ggplot(state) + 
        geom_map(aes(map_id = region),  
                 map = state,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = state$long, y = state$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = ronaldomap,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "blue")+labs(title="Map for Cristiano Ronaldo")
    }
    else{
      ggplot(state) + 
        geom_map(aes(map_id = region),  
                 map = state,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = state$long, y = state$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = messimap,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "red")+labs(title="Map for Messi")
    }
  }
  ) 
  ##word cloud
  output$wordcloud<-renderPlot({
    if(input$wordinput=="Cristiano Ronaldo"){
      wordcloud(crword, max.words = 100, random.order = FALSE)
    }
    else{
      wordcloud(lmword, max.words = 100, random.order = FALSE)
    }
  }
  ) 
  
  ##sentiment
  output$sentiment<-renderPlot({
    if(input$sentiinput=="Cristiano Ronaldo"){
      
      cm<-ggplot(cris, aes(variable,value))
      cm<-cm +geom_bar(stat = "identity")+coord_flip()
      cm<- cm + ggtitle("Sentiment Analysis for Cristiano")
      cm
    }
    else{
      bm<-ggplot(messi, aes(variable,value))
      bm<-bm +geom_bar(stat = "identity")+coord_flip()
      bm<-bm + ggtitle("Sentiment Analysis for Messi")
      bm
    }
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

