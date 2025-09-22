library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# FUNCTIONS

normalize <- function(x, range = c(min(x), max(x))){
  x_norm <- (x - range[1]) / (range[2] - range[1])
  return(x_norm)
}

smoothen <- function(x, y, bw = 15){
  smoothened <- as_tibble(ksmooth(x,y, kernel = "normal", bandwidth = bw))
  return(smoothened)
}

slope2 <- function(y_smooth){
  dy <- diff(y_smooth$y) / diff(y_smooth$x)
  dy <- c(NA, dy)
  y_smooth$dy <- dy
  return(y_smooth)
}

slopeCal <- function(y_smooth){
  dymax <- max(y_smooth$dy, na.rm = T)
  tnode <- y_smooth$x[which(y_smooth$dy==dymax)]
  ynode <- y_smooth$y[which(y_smooth$dy==dymax)]
  
  yintercept <- ynode - (dymax * tnode)
  xintercept <- -yintercept/dymax
  
  out <- list()
  out$node <- c("tnode" = tnode,"ynode" = ynode,"dymax" = dymax)
  out$intercept <- c("xintercept" = xintercept, "yintercept" = yintercept)
  return(out)
}

avg <- function(df,t_begin,t_end){
  df1 <- df %>% filter(time >= t_begin & time <= t_end)
  return(mean(df1$value))
}

t_x <- function(df){
  t_5 <- max((df %>% filter(normVal <= 0.05))$time)
  t_50 <- min((df %>% filter(normVal >= 0.5))$time)
  t_95 <- min((df %>% filter(normVal >= 0.95))$time)
  return(c(t_5, t_50, t_95))
}

#######


ui <- fluidPage(
  fileInput("upload", "Upload a file", accept = c(".csv")),
  tableOutput("head"),
  selectInput("sampleName","Select sample",choices = NULL),
  fluidRow(
    column(width = 6, class = "well",
           h4("Brush and double click to zoom"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    column(width = 6, class = "well",
           h4("Select and confirm regions"),
           plotOutput("p1", height = 300)
    )
  ),
  fluidRow(
    actionButton("baseline", "Set baseline", class = "btn-primary"),
    actionButton("growth", "Set growth", class = "btn-warning"),
    actionButton("plateau", "Set plateau", class = "btn-success"),
    HTML('&emsp;'),
    actionButton("calculate", "Calculate", icon("person-running"), 
                 style="color: #fff; background-color: #b76e33; border-color: #a84818")
  ),
  tableOutput("selected"),
  downloadButton("downloadData", "Download this table"),
  
  plotOutput("p2"),
  
  verbatimTextOutput("smoothened")
  
  
  )



server <- function(input, output, session) {
  rawdata <- reactive({
    req(input$upload)
    rawdata <- read_csv(input$upload$datapath)
  })
  
  output$head <- renderTable({
    head(rawdata())
  })
  
  userRanges <- reactiveValues(data = tibble(sample = character(0),
                       baselineMin = numeric(0),
                       baselineMax = numeric(0),
                       baseline = numeric(0),
                       plateauMin = numeric(0),
                       plateauMax = numeric(0),
                       plateau = numeric(0),
                       growthMin = numeric(0),
                       growthMax = numeric(0),
                       lagtime = numeric(0),
                       tnode = numeric(0),
                       t5 = numeric(0),
                       t50 = numeric(0),
                       t95 = numeric(0),
                       dymax = numeric(0),
                       dymax_normalized = character(0),
                       dymax_raw = numeric(0)
                       )
  )
  
  
  observeEvent(rawdata(),{
    colOpts <- colnames(rawdata()[-1])
    updateSelectInput(inputId = "sampleName", choices = colOpts)
    #userRanges$data$sample <- colOpts
    temptibble <- tibble(sample = colOpts,
                   baselineMin = rep(0, length(colOpts)),
                   baselineMax = rep(0, length(colOpts)),
                   baseline = rep(0, length(colOpts)),
                   plateauMin = rep(0, length(colOpts)),
                   plateauMax = rep(0, length(colOpts)),
                   plateau = rep(0, length(colOpts)),
                   growthMin = rep(0, length(colOpts)),
                   growthMax = rep(0, length(colOpts)),
                   lagtime = rep(0, length(colOpts)),
                   tnode = rep(0, length(colOpts)),
                   t5 = rep(0, length(colOpts)),
                   t50 = rep(0, length(colOpts)),
                   t95 = rep(0, length(colOpts)),
                   dymax = rep(0, length(colOpts)),
                   dymax_normalized = rep(character(1), length(colOpts)),
                   dymax_raw = rep(0, length(colOpts))
    )
    userRanges$data <- temptibble
  })
  
  ## Updating baseline, growth and plateau values
  
  observeEvent(input$baseline,{
    brush <- input$plot1_brush
    rowsToUpdate <- tibble(sample = input$sampleName,
                           baselineMin = brush$xmin,
                           baselineMax = brush$xmax)
    rowsToUpdate$baseline <- avg(dataSample(),rowsToUpdate$baselineMin,rowsToUpdate$baselineMax)
    userRanges$data <- rows_update(userRanges$data, rowsToUpdate, by = "sample")
  })
  
  observeEvent(input$growth,{
    brush <- input$plot1_brush
    rowsToUpdate <- tibble(sample = input$sampleName,
                           growthMin = brush$xmin,
                           growthMax = brush$xmax)
    userRanges$data <- rows_update(userRanges$data, rowsToUpdate, by = "sample")
  })
  
  observeEvent(input$plateau,{
    brush <- input$plot1_brush
    rowsToUpdate <- tibble(sample = input$sampleName,
                           plateauMin = brush$xmin,
                           plateauMax = brush$xmax)
    rowsToUpdate$plateau <- avg(dataSample(),rowsToUpdate$plateauMin,rowsToUpdate$plateauMax)
    userRanges$data <- rows_update(userRanges$data, rowsToUpdate, by = "sample")
  })
  
  
  selectedRanges <- reactive({
    userRanges$data %>% filter(sample == input$sampleName)
  })
  
  selectedData <- reactive({
    userRanges$data %>% select(c(sample, baseline, plateau, lagtime, tnode, t5, t50, t95, dymax_raw, dymax_normalized))
  })
  
  output$selected <- renderTable({
    selectedData()
    # userRanges$data %>% select(c(sample, baseline, plateau, lagtime, tnode, t5, t50, t95, dymax_raw, dymax_normalized))
  },align = "lrrrrrrrrr")
  
  
  dataSample <- reactive({
    req(input$sampleName)
    rawdata() %>% select(c("time",input$sampleName)) %>% rename(value = input$sampleName)
  })
  
  
  output$p1 <- renderPlot({
    p <- ggplot(dataSample(), aes(x = time, y = value)) +
      geom_line()
    brush <- input$plot1_brush
    if(!is.null(brush)){
      p <- p + annotate("rect", xmin = brush$xmin, xmax = brush$xmax, ymin = -Inf, ymax = Inf,
                    fill = "red", alpha = 0.1)
    }
      p <- p + annotate("rect", xmin = selectedRanges()$baselineMin, xmax = selectedRanges()$baselineMax, ymin = -Inf, ymax = Inf,
                        fill = "blue", alpha = 0.1)
      p <- p + annotate("rect", xmin = selectedRanges()$plateauMin, xmax = selectedRanges()$plateauMax, ymin = -Inf, ymax = Inf,
                        fill = "green", alpha = 0.1)
      p <- p + annotate("rect", xmin = selectedRanges()$growthMin, xmax = selectedRanges()$growthMax, ymin = -Inf, ymax = Inf,
                        fill = "orange", alpha = 0.1)
    p
  })
  
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(dataSample(), aes(x = time, y = value)) +
      geom_line() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
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
  
  # Calculate when button pressed
  calc <- observeEvent(input$calculate,{
    normalization_range <- c(selectedRanges()$baseline,selectedRanges()$plateau)
    normData <- dataSample() %>% mutate(normVal = normalize(value, range = normalization_range))
    normSmoothData <- smoothen(normData$time, normData$normVal) # new
    normSmoothDataSlope <- slope2(normSmoothData)
    growthPhase <- normSmoothDataSlope %>% filter(x >= selectedRanges()$growthMin & x <= selectedRanges()$growthMax)
    slopeResults <- slopeCal(growthPhase)
    # normDataGrowth <- normData %>% filter(time >= selectedRanges()$growthMin & time <= selectedRanges()$growthMax)
    # slopeResults <- slope(normDataGrowth$time, normDataGrowth$normVal)
    # time at 5%, 50% and 95%
    tx <- t_x(normData)
    rowsToUpdate <- tibble(sample = input$sampleName,
                           lagtime = slopeResults$intercept[1],
                           tnode = slopeResults$node[1],
                           t5 = tx[1],
                           t50 = tx[2],
                           t95 = tx[3],
                           dymax = slopeResults$node[3],
                           dymax_normalized = format(slopeResults$node[3], scientific = T, digits = 3)
                           )
    userRanges$data <- rows_update(userRanges$data, rowsToUpdate, by = "sample")
    userRanges$data <- userRanges$data %>% mutate(dymax_raw = dymax * (plateau - baseline))
    
    output$smoothened <- renderPrint(slopeResults)
    
    plotSlope <- slopeResults$node[3]
    plotYintercept <- slopeResults$intercept[2]
    
    output$p2 <- renderPlot({
      p <- ggplot() +
        geom_line(data = normData, aes(x = time, y = normVal, color = "Raw data")) +
        geom_line(data = normSmoothData, aes(x = x, y = y, color = "Smoothened data")) +
        xlab("Time (min)") +
        ylab("Normalized value") +
        labs(color = "") +
        theme_bw() +
        theme(legend.position = "bottom",
              text = element_text(size = 18, face = "bold"))
        
      p <- p + annotate("rect", xmin = selectedRanges()$baselineMin, xmax = selectedRanges()$baselineMax, ymin = -Inf, ymax = Inf,
                        fill = "blue", alpha = 0.1)
      p <- p + annotate("rect", xmin = selectedRanges()$plateauMin, xmax = selectedRanges()$plateauMax, ymin = -Inf, ymax = Inf,
                        fill = "green", alpha = 0.1)
      p <- p + annotate("rect", xmin = selectedRanges()$growthMin, xmax = selectedRanges()$growthMax, ymin = -Inf, ymax = Inf,
                        fill = "orange", alpha = 0.1)
      p <- p + geom_hline(yintercept = 0, colour = "blue", lty = 2)
      p <- p + geom_abline(slope = plotSlope, intercept = plotYintercept, colour = "red", lty = 2)
      
      p
    })
  })
  
  # Downloadable csv of table -----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(fs::path_ext_remove(input$upload$name), "_results.csv", sep = "")
    },
    content = function(file) {
      write.csv(selectedData(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)