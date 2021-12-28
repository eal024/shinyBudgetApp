

test <- bud( periode = "202110")


test$lagRegnskapTabell()

# Celing date
test$lagMottakerTabell()


#
test$lagTabellMndUtviklingRegnskap() %>%
    select( periode = lengde,2, 5,6) %>% 
    mutate( across(.cols = matches("anslag"), .fns = function(x) format(x/10^6, big.mark = " ", digits = 0) ) ) %>% 
    rename_with( .cols = everything(), .fn = function(x) str_to_title(str_replace(x, pattern = "_", " "))  )
    



test$lagRegnskapTabell() %>% 
    filter(  str_length(ar) == 4) %>% 
    ggplot( aes(x = factor(ar) , y  = regnskap/10^6,  ar) ) +
    geom_col( ) +
    labs( x = NULL, y = "Regnskap (mill. kroner)")

