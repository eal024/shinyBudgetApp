
#
library(shiny)
library(navR)
library(tidyverse)
library(lubridate)
source("~/R-prosjekter/shinyBudgetApp/app_functions.R")    


## input
periode_navn <- str_c( rep(c(2019:2021), each = 12), map_chr( rep(1:12, times = length(2019:2021) ), function(x) ifelse( x < 10, str_c("0", x), x) ) )
periode <- setNames(periode_navn, periode_navn)
kapittelpost <- setNames( unique(regnskap$kapittelpost), unique(mottakere$kapittelpost))
responses_anslag <- ""

# make_ui <- function(x, var){
#     
#     if(is.numeric(x)){ rng <- }
# }


## 
ui <- fluidPage(
    
        # App title        
        titlePanel("Budsjettanalyse"),     
    
        # Sidebar panel for inputs
        # sidebarPanel()
        
        navlistPanel(
            id = "Info",
            
            "Instillinger",
            tabPanel( "Periode og kapittelpost",
                     fluidRow(
                         column(6,  selectInput( "periode", "Velg periode",           choices =  periode     , width = "80%") ),
                         column(4,  selectInput( "kapittelpost", "Velg kapittelpost", choices =  kapittelpost, width = "80%") )
                            )
                    ),

        "Regnskap",
        tabPanel("Regnskap", 
                 fluidRow( column(12, tableOutput("regnskapTabell") )
                           ),
                 fluidRow(
                     column(10, plotOutput("regnskapGraf"))
                        )
                 ),
        tabPanel("Maned",
                 fluidRow( column(12, tableOutput("ManedesTabell") )
                 )
        ),
        
        "Mottakere",
        tabPanel("Mottakere", 
                 fluidRow( column(12, tableOutput("antallMottakere") )
                    )
                 ),
        tabPanel("Maned",
                 fluidRow( column(12, tableOutput("mottakereMnd") )
                 )
        ),
        "Anslag",
        tabPanel("Anslag", 
                 sidebarPanel(
                    textInput(   "o", "Anslagsnavn" , value = "Navn"),
                    numericInput("a", "Regnskap", value = 0*10^6, min = 0),
                    numericInput("b", "Volum"   , value = 1, min = -1),
                    numericInput("c", "underliggende\nytelse", value = 1, min = -1),
                    numericInput("d", "tiltak"   , value = 1, min = 0),
                    numericInput("e", "pris", value = 1) #,
                    #uiOutput("se")
                    ),
            fluidRow(
                column(6, tableOutput("anslag") ),
                # actionButton("submit", "Legg til"),
                # hr(),
                column(6, textOutput("info")),
                #column(6, tableOutput("responses")),
                column(6, actionButton("submit","Lagre anslag") )
                # tags$hr(),
                # textInput("name", "Name", ""),
                # checkboxInput("user_shiny", "I have built a shiny app i R before", FALSE),
                # sliderInput("r_num_years", "Number of years using R", min =  0,  max = 25, value =  2, ticks = F),
                    )
                ),
        "Dekomponering",
        tabPanel("Dekomponering",
                 fluidRow(
                     column(6,  selectInput( "anslag1", "Velg anslag 1",           choices =  responses_anslag     , width = "80%") ),
                     column(6,  selectInput( "anslag2", "Velg anslag 2",           choices =  responses_anslag     , width = "80%") ),
                     column(6,  tableOutput( "anslagValg1")),
                     column(6,  tableOutput( "anslagValg2"))
                 )
            )
    )
)
        


df <- tibble( name = "", tekst = "", prosent = "", tall = vector(mode = "numeric", length = 0) )
fields <- c("name", "user_shiny", "r_num_years")

server <- function(input, output, session) {
    
    data        <- reactive(  bud(    p = input$periode , r = regnskap , m = mottakere, input$kapittelpost) ) 
    anslag      <- reactive( anslagFun( navn = input$o , r = ifelse( input$a == 0, data()$giRegnskapstallIfjor(), input$a*10^6 ), v = input$b , yt = input$c, pris = input$e , tiltak = input$d ) )
    responses   <- reactive( loadData() )
    anslagValg1 <- reactive( responses() %>% filter( name == input$anslag1))
    anslagValg2 <- reactive( responses() %>% filter( name == input$anslag2))    


    observe({
        updateSelectInput(session, "anslag1",
                          label = "Velg anslag 1",
                          choices = loadData()$name,
                          selected = loadData()$name)
    })
    
    observe({
        updateSelectInput(session, "anslag2",
                          label = "Velg anslag 2",
                          choices = loadData()$name,
                          selected = loadData()$name)
    })
    
    
    # Info om data
    n_row_info  <- reactive( ( paste0("Antall lagret: ", as.character(input$submit) )) )
    
    # store data
    formatData <- reactive({
        # data <- sapply(fields, function(x) input[[x]] )
        data <- anslag()$giDfAnslag() %>% mutate(name = input$o )
    })
    

    output$regnskapTabell <- renderTable( {
            data()$lagRegnskapTabell() %>% regnskapsTabellDisplay( )
        }, res = 96)
    
    output$regnskapGraf <- renderPlot({
         regnskapGraf( data(), tekst = input$kapittelpost) 
        }, res = 96)

    # Mnd tabell
    output$ManedesTabell  <-  renderTable( {
        data()$lagTabellMndUtviklingRegnskap() %>% mndTabellDisplay( )
    }, res = 96)
    

    output$antallMottakere  <-  renderTable( {
        antallMottakereDisplay( data() )
    }, res = 96)
    
    output$mottakereMnd  <-  renderTable( {
        mndMottakereDisplay( data() )
    }, res = 96)
    

    output$anslag <- renderTable( {anslag()$giDfAnslag() }, res = 96 ) 
    
    # Save the form data
    observeEvent(input$submit, {saveData(formatData())})
    
    
    # # Show the prev response
    output$responses <- DT::renderDataTable({
        input$submit
        loadData() 
    })

    
    output$info <- renderText({ n_row_info() })

    
    output$anslagValg1 <- renderTable({ anslagValg1() })
    output$anslagValg2 <- renderTable({ anslagValg2() })
}

shinyApp(ui, server)


