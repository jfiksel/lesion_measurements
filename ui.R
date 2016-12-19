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
               actionButton("selectPaw", "Trace paw"),
               p("Click the button to trace the paw"),
               actionButton("pauseTracePaw", "Pause Tracing"),
               p("Click the button to pause background cropping"),
               actionButton("resetTracePaw", "Reset Tracing"),
               p("Click the button to reset background cropping"),
               actionButton("cropBackground", "Crop Background"),
               p("Click after you have traced out the paw")
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
               actionButton("selectTumor", "Trace Tumor"),
               p("Click the button to trace a tumor"),
               actionButton("pauseTraceTumor", "Pause Tracing"),
               p("Click the button to pause tumor tracing"),
               actionButton("resetTraceTumorLast", "Reset Current Tumor Tracing"),
               p("Click the button to reset tracing of current tumor"),
               actionButton("resetTraceTumorAll", "Reset All Tumor Tracings"),
               p("Click the button to reset all tumor tracings"),
               actionButton("findTumor", "Finish Tracing Tumor"),
               p("Click after you have traced out a tumor"),
               actionButton("calculatePercentage", "Calculate Lesion Percentage"),
               p("Click to calculate the percentage of the paw taken up by lesions")
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
  )
)


