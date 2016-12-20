library(imager)
library(shiny)
library(shinyjs)
library(spatstat)
library(sp)
library(plotKML)
library(maptools)
library(raster)
library(DT)
function(input, output, session) {
  
  ### Function to read in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }
  
  ### Function to select points
  ### Returns a mask that's the same dimension as the image
  ### With a 1 if that point is to be included
  select.points <- function(im, x, y){
    if(is.null(x) | is.null(y)){
      mask <- matrix(1L, nrow=nrow(im), ncol=ncol(im))
    }else{
      xy<-cbind(x,y)
      xy<-as.data.frame(xy)
      coordinates(xy)=c("x","y")
      pnts<-vect2rast(xy)
      poly <- owin(poly=list(x=x, y=y), check=F)
      SpP<- as(poly,  "SpatialPolygons")
      attr  =  data.frame(a=1,  b=1)
      SrDf  =  SpatialPolygonsDataFrame(SpP,  attr)
      rast <- vect2rast(SrDf,cell.size=1)
      r <- raster(rast)
      tum <- coordinates(r)[!is.na(values(r)),]
      tum <- as.data.frame(tum)
      mask <- matrix(0L, nrow=nrow(im), ncol=ncol(im))
      for(x.coord in unique(tum$x)){
        t <- tum[tum$x==x.coord,]
        mask[t$x, t$y] <- 1
      }
    }
    mask
  }
  
  
  ### Makes all points in image that have a 0 in the mask white
  removePoints <- function(im, mask){
    im[mask==0] <- 1
    im
  }
  
  ### Calculates the percentage of the paw taken up by lesions
  calculatePercentage <- function(pawmask, tumormask) {
    if (is.null(pawmask) | is.null(tumormask)) {
      0
    } else{
      tumor.points <- sum(tumormask == 1 & pawmask == 1)
      all.points <- sum(pawmask == 1)
      tumor.points / all.points
    }
  }
  
  ### Generic function for plotting the paw
  app.plot <- function(im, clicks.x = NULL, clicks.y = NULL, lineslist = NULL){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      #plot(paw, xaxt='n', yaxt='n', ann=FALSE)
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{
      plot(im, xaxt='n', yaxt='n', ann=FALSE, xlim=ranges$x,  ylim=c(ranges$y[2], ranges$y[1]))
      #plot(im, xlim=ranges$x, ylim=c(ranges$y[2], ranges$y[1]))
    }
    if(length(clicks.x) > 1){
      lines(c(clicks.x, clicks.x[1]), c(clicks.y, clicks.y[1]), col='red')
    }
    if(!is.null(lineslist)){
      for(i in 1:length(lineslist)){
        x <- lineslist[[i]][[1]]
        y <- lineslist[[i]][[2]]
        lines(c(x, x[1]), c(y, y[1]), col='red')
      }
    }
  }
  
  ### Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)

  ### Code to zoom in on brushed area when double clicking for plot 1
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ### Set position of clicks
  v <- reactiveValues(
    originalImage = NULL,
    paw = NULL,
    pawWithTum = NULL,
    pawMask = NULL,
    tumorMask = NULL,
    pawclick.x = NULL,
    pawclick.y = NULL,
    tumclick.x = NULL,
    tumclick.y = NULL,
    trace.paw = FALSE,
    trace.tum = FALSE,
    tumor.lines = NULL,
    percent.tum = 0,
    data = data.frame(image = character(), pct.lesion=double(), stringsAsFactors=FALSE),
    imageName = NULL
  )
  
  ### Read in image
  ### Automatically set image name to file name
  observeEvent(input$file1, {
    v$originalImage <- read.image(input$file1$datapath)
    v$paw = NULL
    v$pawWithTum = NULL
    v$pawMask = NULL
    v$tumorMask = NULL
    v$pawclick.x = NULL
    v$pawclick.y = NULL
    v$tumclick.x = NULL
    v$tumclick.y = NULL
    v$trace.paw = FALSE
    v$trace.tum = FALSE
    v$tumor.lines = NULL
    v$percent.tum = 0
    v$imageName <- gsub("(.jpg|.png)","", input$file1$name)
    updateTextInput(session, inputId = "imgName", label = NULL, value = v$imageName)
    output$plot1 <- renderPlot({
      app.plot(v$originalImage,v$pawclick.x, v$pawclick.y)
    })
    output$plot2 <- renderPlot({
      app.plot(v$paw, v$tumclick.x, v$tumclick.y)
    })
  })
  
  output$viewData <- renderDataTable({
    datatable(v$data,colnames=c("Image Names", "Lesion Percentage")) %>%
      formatRound(columns=2, digits=4)
  })
  observeEvent(input$imgName, {
    v$imageName <- input$imgName
  })
  
  # Handle clicks on the plot for tracing paw & tumors
  observeEvent(input$selectPaw, {
    v$trace.paw <- TRUE
    disable("selectPaw")
    enable("pauseTracePaw")
    enable("resetTracePaw")
    enable("cropBackground")
  })
  
  observeEvent(input$selectTumor, {
    v$trace.tum <- TRUE
    disable("selectTumor")
    enable("pauseTraceTum")
    enable("pauseTraceTumor")
    enable("resetTraceTumorLast")
    enable("resetTraceTumorAll")
    enable("findTumor")
  })
  
  ### Pause tracing
  observeEvent(input$pauseTracePaw, {
    v$trace.paw <- FALSE
    disable("pauseTracePaw")
    enable("selectPaw")
    enable("resetTracePaw")
    enable("cropBackground")
  })
  
  observeEvent(input$pauseTraceTumor, {
    v$trace.tum <- FALSE
    enable("selectTumor")
    disable("pauseTraceTum")
    enable("resetTraceTumorLast")
    enable("resetTraceTumorAll")
    enable("findTumor")
  })
  
  
  ## Reset background correction
  observeEvent(input$resetTracePaw, {
    v$paw <- NULL
    v$pawMask <- NULL
    v$trace.paw <- FALSE
    v$pawclick.x  <- NULL
    v$pawclick.y <- NULL
    enable("pauseTracePaw")
    enable("selectPaw")
    disable("resetTracePaw")
    enable("cropBackground")
    output$plot1 <- renderPlot({
      app.plot(v$originalImage,v$pawclick.x, v$pawclick.y)
    })
  })
  
  observeEvent(input$resetTraceTumorLast, {
    v$trace.tum <- FALSE
    v$tumclick.x  <- NULL
    v$tumclick.y <- NULL
    enable("selectTumor")
    enable("pauseTraceTum")
    disable("resetTraceTumorLast")
    enable("resetTraceTumorAll")
    enable("findTumor")
  })
  
  observeEvent(input$resetTraceTumorAll, {
    v$pawWithTum <- NULL
    v$tumorMask <- NULL
    v$trace.tum <- FALSE
    v$tumclick.x  <- NULL
    v$tumclick.y <- NULL
    v$tumor.lines <- NULL
    enable("selectTumor")
    enable("pauseTraceTum")
    enable("resetTraceTumorLast")
    disable("resetTraceTumorAll")
    enable("findTumor")
    output$plot2 <- renderPlot({
      app.plot(v$paw, v$tumclick.x, v$tumclick.y)
    })
  })
  
  observeEvent(input$cropBackground,{
    if(is.null(v$pawclick.x) | is.null(v$pawclick.y)){
      v$paw <- v$originalImage
      v$pawMask <- select.points(v$originalImage, v$pawclick.x, v$pawclick.y)
    }else{
      v$pawMask <- select.points(v$originalImage, v$pawclick.x, v$pawclick.y)
      v$paw <- removePoints(v$originalImage, v$pawMask)
      v$trace.paw <- FALSE
      v$pawclick.x  <- NULL
      v$pawclick.y <- NULL
      enable("pauseTracePaw")
      enable("selectPaw")
      enable("resetTracePaw")
      disable("cropBackground")
    }
    output$plot1 <- renderPlot({
      app.plot(v$paw)
    })
  })
  
  observeEvent(input$findTumor,{
    newTumMask <-  select.points(v$paw, v$tumclick.x, v$tumclick.y)
    if(is.null(v$tumorMask)){
      v$tumorMask <- newTumMask
    }else{
      v$tumorMask <- v$tumorMask | newTumMask
    }
    v$pawWithTum <- removePoints(v$paw, 1-v$tumorMask)
    v$trace.tum <- FALSE
    v$tumor.lines[[length(v$tumor.lines) + 1]] <- list(v$tumclick.x, v$tumclick.y)
    v$tumclick.x  <- NULL
    v$tumclick.y <- NULL
    enable("selectTumor")
    enable("pauseTraceTum")
    enable("resetTraceTumorLast")
    enable("resetTraceTumorAll")
    disable("findTumor")
    output$plot2 <- renderPlot({
      #app.plot(v$pawWithTum,v$tumclick.x, v$tumclick.y)
      app.plot(v$paw,v$tumclick.x, v$tumclick.y, v$tumor.lines)
    })
  })
  
  observeEvent(input$calculatePercentage, {
    v$percent.tum <- calculatePercentage(v$pawMask, v$tumorMask)
  })
  
  observeEvent(input$addToData, {
    if(!(v$imageName %in% v$data[1,])){
      v$data[nrow(v$data)+1,] <- c(v$imageName, v$percent.tum)
    }else{
      index <- which(v$data[1,]==v$imageName)
      v$data[index,2] <- v$percent.tum
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "tumorPercentages.csv",
    content = function(file){
      write.csv(v$data, file, row.names=FALSE)
    }
  )
  
  ### Keep track of click locations if tracing paw or tumor 
  observeEvent(input$plot1_click, {
      # Keep track of number of clicks for line drawing
    if(v$trace.paw){
      v$pawclick.x <- c(v$pawclick.x, round(input$plot1_click$x))
      v$pawclick.y <- c(v$pawclick.y, round(input$plot1_click$y))
    }
  })
  
  observeEvent(input$plot2_click, {
    # Keep track of number of clicks for line drawing
    if(v$trace.tum){
      v$tumclick.x <- c(v$tumclick.x, round(input$plot2_click$x))
      v$tumclick.y <- c(v$tumclick.y, round(input$plot2_click$y))
    }
  })
  
 
  
  ### Text for app testing
  output$info <- renderText({
    paste("The current percentage of the paw taken up by lesions is", 
          paste0(format(round(v$percent.tum*100, 2), nsmall=2), "%"))
    #paste(nrow(v$pawMask), ncol(v$pawMask), ncol(v$tumorMask), ncol(v$tumorMask))
  })
  
  ### Original plot
  output$plot1 <- renderPlot({
    app.plot(v$originalImage, v$pawclick.x, v$pawclick.y)
    })
  
  output$plot2 <- renderPlot({
    app.plot(v$paw, v$tumclick.x, v$tumclick.y)
  })

}