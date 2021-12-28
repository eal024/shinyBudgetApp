

## Importer data
regnskap <- readxl::read_excel("data/2021-12-09 regnskap.xlsx") %>% mutate( dato = ymd(dato)) 
mottakere <- readxl::read_excel("data/2021-12-09 mottakere.xlsx") %>% mutate( dato = ymd(dato)) 

bud <- function(p = "202106",r, m, kp ){ 
    
    # Input er regnskapdata og mottakere.
    dfR  <-   r  %>% filter( kapittelpost == kp) %>% filter( dato <= lubridate::ymd(p,  truncated = 1) )
    dfM <-    m  %>% filter( kapittelpost == kp) %>% filter( dato <= lubridate::ymd(p,  truncated = 1) )

    navR::Budsjett$new(name = "Kap. 2020 Enslig mor eller far",
                 kapittelpost =  unique(dfR$kapittelpost),
                 name_nytt_anslag = "Oktober 2021",
                 periode = as.numeric(p),
                 dfRegnskap = dfR,
                 dfMottakere =  dfM %>% mutate(kategori = "post70"),
                 pris_gjeldende = 100000,
                 PRIS_VEKST = 1.02
    )
    
}




regnskapsTabellDisplay <- function(df) {
    df %>%
        select(ar, regnskap, endring = endring_regnskap, pst = regnskap_vekst) %>%
        mutate(across(
            .cols = c(regnskap, endring),
            .fns = function(x) {format( x/10^6, digits = 0) }
        )
        ) %>%
        rename_with( .cols = everything(), .fn = function(x) str_to_title(x)  )
}

mndTabellDisplay <- function( df , mill_kr = T){
    df %>%
        select( periode = 1,2, 5,6) %>% 
        mutate( across(.cols = matches("anslag"), .fns = function(x) format( ifelse( mill_kr == F, x, x/10^6) , big.mark = " ", digits = 0) ) ) %>% 
        rename_with( .cols = everything(), .fn = function(x) str_to_title(str_replace(x, pattern = "_", " "))  )
}





antallMottakereDisplay <- function( bud ){
    bud$lagMottakerTabell() %>% 
        mutate( across( .cols = c(gjennomsnitt,per_des ), .fns = function(x) format(x, digits = 0, big.mark = " ") )) %>% 
        select( Ar = ar, `Gj. antall` = gjennomsnitt, Vekst = vekst_pst_gj, `Per siste mnd` = per_des) 
        
}


mndMottakereDisplay <- function( budsjett){
    
    budsjett$lagTabellMndUtviklingMottakere() %>% mndTabellDisplay( mill_kr = F)
}



# Regnskapsgraf -----------------------------------------------------------

regnskapGraf <- function( df, tekst) {
        df$lagRegnskapTabell() %>% 
        filter( str_length(ar) == 4 ) %>% 
        ggplot( aes( x = ar, y = regnskap/10^6) ) +
        geom_col() +
        labs( x = " ", y = "mill. kroner", title = paste("Kapittelpost ", tekst) )   
}


# Anslag ------------------------------------------------------------------

anslagFun <- function( navn , r  , v , yt , pris  , tiltak  ){

        a <- Anslag$new( 
        name            = navn,
        regnskap_ifjor  = r,
        volumvekst      = v,
        vekst_ytelse    = yt, 
        prisvekst       = pris,
        tiltak          = tiltak,
        underregulering = NULL,
        kortnavn        = NULL
    )
    #a
}



# save and load -----------------------------------------------------------

saveData <- function(data) {
    #data <- as.data.frame(t(data))
    data <- data
    # Write the file to the local system
    if( exists("responses")){ 
#        responses <<- rbind(responses, data) 
         responses        <<- bind_rows(responses, data) %>% distinct() 
         responses_anslag <<- setNames( unique(responses$name), unique(responses$name))
    } else{ 
        responses <<- data
        responses_anslag <<- setNames( unique(responses$name), unique(responses$name))
    }
}

loadData <- function(){
    if(exists("responses") ) {
        responses
    }
}








# Funksjoner til budsjett og anslag ---------------------------------------



a <- bud( r = regnskap, m = mottakere, kp = "2620.70")
anslag <- anslagFun( "Test", r = 1*10^6, v = 1, yt = 1, pris = 1, tiltak = 1)

a$leggTilHistoriskAnslag( anslag = anslag, rekkefolge = 1)

a$giHistoriskeAnslag()



