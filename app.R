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
library(shinyjs)

#install.packages('shiny')
#install.packages('filesstrings')
#install.packages('tidyr')
#install.packages('readr')
#install.packages('lubridate')
#install.packages('hms')
#install.packages('chron')
#install.packages('stringr')
#install.packages('shinyjs')
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

emptyVideoCache <- function(){
  do.call(file.remove, list(list.files('www', pattern = 'o\\d', full.names = T)))
}
########################################################-UI-#############################################################################

ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("Old Faithful Geyser Data"),
   
  sidebarLayout(
    sidebarPanel(
      dataTableOutput('videoData')
    ),
      

    mainPanel(
      actionButton(inputId = 'Submit', 'Submit'),
      actionButton(inputId = 'EmergencyStart', 'Emergency Start'),
      textInput(inputId = 'URL', 'URL', value = 'https://www.youtube.com/watch?v=i_cTTgkNdVY'),
      uiOutput('Video')
    )
  )
  
)

########################################################-Server-#############################################################################

server <- function(input, output) {
  rv <- reactiveValues(video = 0, videoFinished = 0, videoDownloaded = 0, numberVideos = 1) 
  file.remove('MainVideo.mp4')
  file.remove('www/MainVideo.mp4')
  system('youtube-dl https://www.youtube.com/watch?v=i_cTTgkNdVY -f mp4 -o MainVideo.mp4')
  file.move('MainVideo.mp4', 'www', overwrite = TRUE)

  ydlcall <- reactive({
    sprintf('youtube-dl %s -f mp4 -o MainVideo%s.mp4', input$URL, rv$numberVideos)
  })
   
  ydlcall2 <- reactive({
    sprintf('youtube-dl %s --get-title --get-duration', input$URL)
  })
   
  reactiveVideoData <- reactive({
    rv$video
    {loadVideoData()}
  })
   
  observeEvent(input$Submit, {
    file.remove('www/MainVideo.mp4')
    file.remove('www/MainVideo.mp4')
    system(ydlcall())
    system(ydlcall2(), intern = TRUE) -> tmp
    oldDat <- loadVideoData()
    newDat <- data.frame('Title' = tmp[1], 'Duration' = ((chron(times = convertTimeForChron(tmp[2])) %>% as.numeric)*24*60*60), stringsAsFactors = F)
    oldDat[,2:3] %>% rbind(newDat) -> fullDat
    saveVideoData(fullDat)
    file.move(sprintf('MainVideo%s.mp4', rv$numberVideos), 'www', overwrite = TRUE)
    file.copy(sprintf('www/MainVideo%s.mp4', rv$numberVideos), 'www/MainVideo.mp4', overwrite = TRUE)
    rv$numberVideos <- rv$numberVideos + 1
    #delay((chron(times = loadVideoData()[dim(loadVideoData())[1],3] %>% as.hms %>% as.character()) %>% as.numeric)*24*60*60*1000, {rv$videoFinished <- rv$videoFinished + 1
    #})
  })
  
  observeEvent(input$EmergencyStart, {
    rv$video <- rv$video + 1
  })
  
  observeEvent(rv$videoFinished, {print('Finished')
    rv$video <- rv$video + 1
    delay((chron(times = loadVideoData()[rv$numberVideos,3] %>% as.hms %>% as.character()) %>% as.numeric)*24*60*60*1000, {rv$videoFinished <- rv$videoFinished + 1
    })
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

