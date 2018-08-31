#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(filesstrings)
library(tidyr)
library(readr)
library(lubridate)
library(hms)
library(chron)
library(stringr)

####################################################-Functions-##########################################################################

videoData <- data.frame('Title' = "What You Don't Know - Buses", 'Duration' = 13, stringsAsFactors = F)

saveVideoData <- function(data){
  fileName <- 'videoData.csv'
  write.csv(data, 'videoData.csv')
}

saveVideoData(videoData)

loadVideoData <- function(){
  read.csv('videoData.csv') -> data
  data
}

convertTimeForChron <- function(time){
  add <- '00:00:00'
  len <- str_length(time)
  add2 <- str_sub(add, 1, 8 - len)
  paste0(add2, time) %>% return()
}
########################################################-UI-#############################################################################

ui <- fluidPage(
  
  titlePanel("Old Faithful Geyser Data"),
   
  sidebarLayout(
    sidebarPanel(
      dataTableOutput('videoData')
    ),
      

    mainPanel(
      actionButton(inputId = 'Submit', 'Submit'),
      textInput(inputId = 'URL', 'URL', value = 'https://www.youtube.com/watch?v=i_cTTgkNdVY'),
      uiOutput('Video')
    )
  )
  
)

########################################################-Server-#############################################################################

server <- function(input, output) {
  rv <- reactiveValues(video = 0) 
  file.remove('MainVideo.mp4')
  file.remove('www/MainVideo.mp4')
  system('www/youtube-dl.exe https://www.youtube.com/watch?v=i_cTTgkNdVY -f mp4 -o MainVideo.mp4')
  file.move('MainVideo.mp4', 'www', overwrite = TRUE)

  ydlcall <- reactive({
    sprintf('www/youtube-dl.exe %s -f mp4 -o MainVideo.mp4', input$URL)
    #input$URL
  })
   
  ydlcall2 <- reactive({
    sprintf('www/youtube-dl.exe %s --get-title --get-duration', input$URL)
    #input$URL
  })
   
  reactiveVideoData <- reactive({
    rv$video
    {loadVideoData()}
  })
   
  observeEvent(input$Submit, {
    file.remove('www/MainVideo.mp4')
    file.remove('www/MainVideo.mp4')
    #print(ydlcall())
    system(ydlcall())
    print('Test')
    print(ydlcall2())
    system(ydlcall2(), intern = TRUE) -> tmp
    print(tmp)
    oldDat <- loadVideoData()
    newDat <- data.frame('Title' = tmp[1], 'Duration' = (chron(times = convertTimeForChron(tmp[2])) %>% as.numeric)*24*60*60, stringsAsFactors = F)
    oldDat[,2:3] %>% rbind(newDat) -> fullDat
    saveVideoData(fullDat)
    print(fullDat)
    file.move('MainVideo.mp4', 'www', overwrite = TRUE)
    rv$video <- rv$video + 1
  })
    
    
   
  output$Video <- renderUI({
    rv$video
    tags$video(id="video2", type = "video/mp4",src = 'MainVideo.mp4', controls = NA, autoplay = TRUE)
  })
   
  output$videoData <- renderDataTable({rv$video
    print(reactiveVideoData())
    reactiveVideoData()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

