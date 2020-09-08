library(shiny)
library(shinyjs)
library(shinythemes)

fluidPage(theme = shinytheme("cerulean"),
          
    useShinyjs(),
          
    sidebarLayout(
        sidebarPanel(width = 3,
            conditionalPanel(condition = "!input.expchosen",
                             selectizeInput("expname", "Experiment", choices = NULL),
                             actionButton("confirmexp", "Confirm Choice")
            ),
            conditionalPanel(condition = "input.expchosen && !input.welcome",
                             h4("Welcome"),
                             
                             helpText("In this survey a series of similar looking charts will be presented.  We would like you to respond to the following questions."),
                             helpText("1. Pick the plot based on the survey question"),
                             helpText("2. Provide reasons for choice"),
                             helpText("3. How certain are you?"),
                             helpText("Finally we would like to collect some information about you. (age category, education and gender)"),
                             helpText("Your response is voluntary and any information we collect from you will be kept confidential. By  clicking on the button below you agree that the data we collect may be used in research study."),
                             
                             checkboxInput("consent", HTML(paste0("I have read the ", a("informed consent", href = "http://104.236.245.153:8080/mahbub/turk16/consent.html", target = "_blank"), " and agree."))),

                             actionButton("beginexp", "Begin Experiment", class = "btn btn-info")                 
            ),
            conditionalPanel(condition = "input.welcome && !input.ready",
                             h4("Demographic Information"),
                             textInput("turk", "Turk ID"),
                             selectizeInput("age", "Age Range", choices = c("", "Under 18", "18-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "Over 60", "I choose not to provide this information")),
                             radioButtons("gender", "Gender", choices = c("Female", "Male", "I choose not to provide this information"), selected = NA),
                             selectizeInput("education", "Highest Education Level", choices = c("", "High School or Less", "Some Undergraduate Courses",
                                                                                                "Undergraduate Degree", "Some Graduate Courses",
                                                                                                "Graduate Degree", "I choose not to provide this information")),
                             
                             actionButton("submitdemo", "Submit Demographics", class = "btn btn-info")                 
            ),
            
            conditionalPanel(condition = "input.response_no == null",
                             checkboxInput("expchosen", "Experiment Chosen", value = FALSE)                
            ),
            
            conditionalPanel(condition = "input.response_no == null",
                             checkboxInput("welcome", "Welcome", value = FALSE)                
            ),
            
            conditionalPanel(condition = "input.response_no == null",
                             checkboxInput("otheronly", "", value = FALSE)   
            ),
            
            conditionalPanel(condition = "input.response_no == null",
                checkboxInput("ready", "Ready", value = FALSE)                
            ),
            
            conditionalPanel(condition = "input.response_no == null",
                             checkboxInput("done", "Done", value = FALSE)                
            ),
            
            conditionalPanel(condition = "input.ready && !input.done", 
                 h4("Selection"),
                 textInput("response_no", "Choice (Click on plot to select)", value = ""),
                 conditionalPanel(condition = "!input.otheronly",
                    checkboxGroupInput("reasoning", "Reasoning", choices = "")
                 ),
                 conditionalPanel(condition = "input.reasoning.indexOf('Other') > -1 || input.otheronly",
                                  textInput("other", "Other Reason")
                 ),
                 selectizeInput("certain", "How certain are you?", choices = c("", "Very Uncertain", "Uncertain", "Neutral", "Certain", "Very Certain")),
                 actionButton("submit", "Submit", icon = icon("caret-right"), class = "btn btn-info"),
                 hr(),
                 h4("Status"),
                 h5(textOutput("status"))
            )
        ),
        
        mainPanel(width = 9,
            #h4(textOutput("debug")),
            conditionalPanel(condition = "!input.expchosen",
                    h4("Description of Experiments"),
                    helpText("Experiment #16: Examining the use of color and its effect on the perception of plots."),
                    helpText("Experiment #18: Investigating the perception of association between variables in plots."),
                    helpText("Experiment #19: Follow up to Experiment #16, examining the use of color and its effect on the perception of plots."),
                    helpText("Experiment #20: Pilot study assessing the similarity between curves derived from images of bullets."),
                    helpText("Experiment #21: Pilot study on the simple and complex structures in graphs."),
                    helpText("Experiment #22: Study on graphs and structure that is seen in spite of randomness.")
            ),
            conditionalPanel(condition = "input.expchosen && !input.welcome",
                h4(textOutput("welcome_header")),
                uiOutput("welcome_text"),
                
                h4(textOutput("example1_q")),
                imageOutput("example1_plot"),
                
                br(),
                br(),
                
                uiOutput("example1_a"),
                
                h4(textOutput("example2_q")),
                imageOutput("example2_plot"),
                
                br(),
                br(),
                
                uiOutput("example2_a"),
                
                hr(),
                
                textInput("password", ""),
                conditionalPanel(condition = paste0("input.password == '", readLines("password.txt"), "'"),
                                 downloadButton("downloadDB", "Download DB")
                )
            ),
            conditionalPanel(condition = "input.welcome && !input.ready",
                h4(textOutput("demo_text"))
            ),
            
            conditionalPanel(condition = "input.ready",
                h3(textOutput("question"))
                #conditionalPanel(condition = "input.done",
                #    HTML("<a href='https://prolificacademic.co.uk/submissions/56293369c8ffc200055132fd/complete?cc=XYA822O3'>https://prolificacademic.co.uk/submissions/56293369c8ffc200055132fd/complete?cc=XYA822O3</a>")
                #)
            ),
            hr(),
            uiOutput("lineup")
        )
    ),
    
    includeScript("www/js/action.js")
)
