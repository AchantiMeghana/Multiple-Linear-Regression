
library(shiny)
library(vroom)
library(rmarkdown)
library(shinyWidgets)

# Define UI for application
ui <- fluidPage(
  
  #Title
  titlePanel(strong("Linear Regression")),
  
  fluidRow(
    column(3,
           
           #Upload dataset
           fileInput("upload","Upload your dataset",multiple= FALSE,
                     accept = c(".csv",".tsv")),
           
           # Select independent variable x           
          uiOutput("x"),
           br(),
           # Select independent variable y          
           uiOutput("y"),
           br(),
           #Choose no. of rows of table to be displayed
           sliderInput("n","Choose the number of rows of data to be displayed",
                       min= 0, max =0 ,step = 1,value =0)
    ),
    
    column(9,
           #Plot output
           plotOutput("multiple_regression_plot")
    )),
  
  fluidRow(
    column(2,
           # Download reports        
           downloadButton("download","Get your report!", icon = shiny::icon("download")) 
    )),
  
  #display columns of dataset
  fluidRow(
    column(9,
           tableOutput("table")   
    )))  

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #storing dataset in reactive variable
  data <- reactive({
    req(input$upload,cancelOutput = TRUE) #program doesn't run if condition is false
    ext <- tools::file_ext(input$upload$name)
    switch (ext,
            tsv = vroom::vroom(input$upload$datapath,delim ="\t"),
            csv  = vroom::vroom(input$upload$datapath,delim =","),
            validate("Upload a .csv or .tsv file"))}) 
  
  #Update slider range
  observeEvent(data(),{
    updateSliderInput(inputId = "n",max = nrow(data()))
  }) 
  
  #Displaying dataset    
  observeEvent(data(),{
    output$table <- renderTable(head(data(),input$n))
  })
  
  #Update choices in widgets for x,y
  output$x <- renderUI({
    req(data())
    xcol <- colnames(data())
    pickerInput(inputId = "x_sel",label = "Choose x",
                choices = c(xcol[1:length(xcol)]),selected =xcol[1],multiple = TRUE)
    
  })
  
  output$y <- renderUI({
    req(data())
    ycol <- colnames(data())
    pickerInput(inputId = "y_sel",label = "Choose y",
                choices = c(ycol[1:length(ycol)]),selected =ycol[1],multiple = FALSE)
    
  })
 
  #Columns of x,y widgets
  ycol <- reactive({
    req(data())
    colnames(data())
  })
  
  xcol <- reactive({
    req(data())
    colnames(data())
  })
  
  #Multiple linear regression model
  lmmodel <- reactive({
    req(data(),input$x_sel,input$y_sel)
    x <- as.numeric(data()[[as.name(input$x_sel)]])
    y <- as.numeric(data()[[as.name(input$y_sel)]])
    current_formula <- paste0(input$y_sel, " ~ ", paste0(input$x_sel, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = data(), na.action=na.exclude)
    return(model)
  })
  
  
  #Plotting multiple regression
    output$multiple_regression_plot <- renderPlot({
      req(lmmodel())
      plot(lmmodel())
      })
    
  # Store summary of model
    result <- reactive({
      
      req(lmmodel())
      summary(lmmodel())
    })
  
   #Downloading the report
  output$download <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      
      #Using a temporary directory
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Setting up parameters to pass to Rmd document
      params <- result()
      
      #Knitting the document
      rmarkdown::render(tempReport, output_file = file,
                        params = params)}
  )
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
