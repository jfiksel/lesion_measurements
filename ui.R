library(shinyjs)
navbarPage(
  title="Mouse Lesion Measurements",
  tabPanel("Remove Background",
           useShinyjs(),
           #titlePanel("Mouse Lesion Measurements"),
           sidebarLayout(
             sidebarPanel(
               fileInput("file1", "Choose Paw Image",
                         accept=c(".png",
                                  ".jpg")),
               textInput("imgName", "Image Name", ""),
               actionButton("selectPaw", "Trace paw"),
               p("Click the button to trace the paw"),
               actionButton("pauseTracePaw", "Pause Tracing"),
               p("Click the button to pause background cropping"),
               actionButton("resetTracePaw", "Reset Tracing"),
               p("Click the button to reset background cropping"),
               actionButton("cropBackground", "Crop Background"),
               p("Click after you have traced out the paw"),
               downloadButton("downloadData", "Download Dataset")
             ),
             mainPanel(
               plotOutput("plot1", click="plot1_click",
                          dblclick = "plot1_dblclick",
                          brush = brushOpts(
                            id = "plot1_brush",
                            resetOnNew = TRUE
                          ))
             )
           )
  ),
  tabPanel("Calculate Tumor Size",
           sidebarLayout(
             sidebarPanel(
               actionButton("selectTumor", "Trace Lesion"),
               p("Click the button to trace a lesion"),
               actionButton("pauseTraceTumor", "Pause Tracing"),
               p("Click the button to pause lesion tracing"),
               actionButton("resetTraceTumorLast", "Reset Current Lesion Tracing"),
               p("Click the button to reset tracing of current lesion"),
               actionButton("resetTraceTumorAll", "Reset All Lesion Tracings"),
               p("Click the button to reset all lesion tracings"),
               actionButton("findTumor", "Finish Tracing Lesion"),
               p("Click after you have traced out a lesion"),
               actionButton("calculatePercentage", "Calculate Lesion Percentage"),
               p("Click to calculate the percentage of the paw taken up by lesions"),
               actionButton("addToData", "Add Image to Data"),
               p("Click once you have finished processing the image")
             ),
             mainPanel(
               plotOutput("plot2", click="plot2_click",
                          dblclick = "plot2_dblclick",
                          brush = brushOpts(
                            id = "plot2_brush",
                            resetOnNew = TRUE
                          )),
               verbatimTextOutput("info")
             )
           )
  ),
  tabPanel("View Dataset",
           mainPanel(
             tableOutput("viewData")
           ))
)


