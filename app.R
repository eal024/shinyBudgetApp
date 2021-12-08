
#
library(shiny)
library(navR)
library(tidyverse)
library(lubridate)

#
bud <- function(periode = "202106"){ navR::Budsjett$new(name      = "Budsjettet",
                                                        dfRegnskap  = navR::regnskap %>% rename( pris = g),
                                                        dfMottakere = navR::mottakere %>% mutate(kategori = "post 70"),
                                                        periode =  periode,
                                                        pris_gjeldende = 100000,
                                                        PRIS_VEKST = 1.02,
                                                        kapittelpost = "2026.70")
                                }
    
## Period
periode_navn <- str_c( rep(c(2019:2021), each = 12), map_chr( rep(1:12, times = length(2019:2021) ), function(x) ifelse( x < 10, str_c("0", x), x) ) )
periode <- setNames(periode_navn, periode_navn)

    
ui <- fluidPage(
    fluidRow(
        column(6,  selectInput("periode", "Velg periode", choices =  periode, width = "80%") ),
             ),
        fluidRow( column(6, tableOutput("regnskapTabell") )
             )
    )

server <- function(input, output, session) {
    
    data <- reactive( bud(periode = input$periode))
    
    output$regnskapTabell <- renderTable(
        data( )$lagRegnskapTabell() %>%
            select(ar, regnskap, endring = endring_regnskap, pst = regnskap_vekst) %>% 
            mutate(across(
                .cols = c(regnskap, endring),
                .fns = function(x) {format( x =  x/10^6, digits = 0) }
            )
            ) %>% 
            rename_with( .cols = everything(), .fn = function(x) str_to_title(x)  )
        ,
        width = "100%" )
}



shinyApp(ui, server)





