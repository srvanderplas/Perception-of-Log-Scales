library(shiny)
library(shinythemes)
library(RSQLite)

server <- function(input, output, session) {
    
    values <- reactiveValues(result = "")
    
    picture_details <- reactive({
        if (is.null(input$picture_details) | is.null(input$try_picture_details)) return(NULL)
        
        picture_details <- read.csv(input$picture_details$datapath, stringsAsFactors = FALSE)
        try_picture_details <- read.csv(input$try_picture_details$datapath, stringsAsFactors = FALSE)
        
        ## Cleanup
        picture_details$pic_name <- gsub(".*/(.*\\.svg)", "\\1", as.character(picture_details$pic_name))
        picture_details$trial <- 0
        try_picture_details$pic_name <- gsub(".*/(.*\\.svg)", "\\1", as.character(try_picture_details$pic_name))
        try_picture_details$trial <- 1
        
        deets <- rbind(picture_details, try_picture_details)
        
        return(deets)
    })
    
    experiment_details <- reactive({
        if (input$experiment == "") return(NULL)
        
        my.df <- data.frame(experiment = input$experiment, 
                            question = input$experiment_question, 
                            reasons = input$experiment_reasons,
                            lpp = input$lpp, 
                            trials_req = input$trials_req)
        
        return(my.df)
    })
    
    observeEvent(input$submit, {
        con <- dbConnect(SQLite(), dbname = "data/turk.db")
        
        try({dbGetQuery(con, paste0("DELETE FROM picture_details WHERE experiment = '", experiment_details()$experiment[1], "'"))})
        try({dbGetQuery(con, paste0("DELETE FROM experiment_details WHERE experiment = '", experiment_details()$experiment[1], "'"))})
        dbWriteTable(con, "picture_details", picture_details(), append = TRUE, row.names = FALSE)
        dbWriteTable(con, "experiment_details", experiment_details(), append = TRUE, row.names = FALSE)
        
        values$result <- "Submitted Successfully!"
        
        dbDisconnect(con)
    })
    
    output$result <- renderText({
        return(values$result)
    })
    
}

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                titlePanel("Lineups - Admin"),
                
                sidebarLayout(
                    sidebarPanel(
                        textInput("experiment", "Experiment Name"),
                        textInput("experiment_question", "Experiment Question", value = "Which plot is the most different from the other plots?"),
                        textInput("experiment_reasons", "Experiment Reasons (Comma-Separated)", value = "Strongest Trend,Groups are Separated,Large Variance,Other"),
                        
                        hr(),
                        
                        numericInput("lpp", "Lineups Per Person", 10),
                        numericInput("trials_req", "Correct Trials Needed", 2),
                        
                        hr(),
                        
                        fileInput("picture_details", "Upload Picture Details"),
                        fileInput("try_picture_details", "Upload Try Picture Details"),
                        
                        hr(),
                        
                        actionButton("submit", "Submit Experiment", icon = icon("caret-right"))
                    ),
                    
                    mainPanel(
                        textOutput("result")
                    )
                )
)

shinyApp(ui = ui, server = server)
