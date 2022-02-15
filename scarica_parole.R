library(data.table)
library(rvest)

# scarico parole
yr <- data.table()
for(ln in 3:15){
    message('Elaboro lunghezza ', ln)
    url <- paste0('https://www.listediparole.it/', ln, 'lettereparole')
    y <- read_html(paste0(url, '.htm')) %>% 
            html_elements('.mot') %>% 
            html_text()
    yr <- rbindlist(list( yr, yr[, .( lunga = ln, parola = tstrsplit(y, ' ', fixed = TRUE) ) ] ))
    pg <- 2
    repeat({
        y <- tryCatch({ read_html(paste0(url, 'pagina', pg, '.htm')) } ,
                 error=function(cond) {   return(NULL) }
        )
        if(is.null(y)) break
        y <- y %>%  html_elements('.mot') %>% html_text()
        yr <- rbindlist(list( yr, yr[, .( lunga = ln, parola = tstrsplit(y, ' ', fixed = TRUE) ) ] ))
        pg <- pg + 1
    })
}
yr[, `:=`( parola = tolower(unlist(parola)), tipo = NA_character_, comune = NA_character_ ) ]
saveRDS(yr, './parole')

# scarico verbi
y <- data.table(parola = character(0))
for(lt in setdiff(letters, c('h', 'j', 'k', 'w', 'x', 'y'))){
    message('Elaboro lettera ', lt)
    url <- paste0('https://www.dizy.com/it/verbi/', lt)
    y <- rbindlist(list( y, 
                         read_html(url) %>%  
                            html_elements('table:nth-child(4) td') %>% 
                            html_text() %>% 
                            as.data.table()
    ), use.names = FALSE)
    pg <- 2
    repeat({
        yx <- read_html(paste0(url, '?p=', pg)) %>%
                    html_elements('table:nth-child(6) td') %>% 
                    html_text()
        if(length(yx) == 0) break
        y <- rbindlist(list( y, yx %>% as.data.table() ), use.names = FALSE)
        pg <- pg + 1
    })
}
yr[parola %in% y$parola, tipo := 'v']

# scarico aggettivi
y <- data.table(parola = character(0))
for(lt in setdiff(letters, c('h', 'j', 'k', 'w', 'x', 'y'))){
    message('Elaboro lettera ', lt)
    url <- paste0('https://www.dizy.com/it/aggettivi/', lt)
    y <- rbindlist(list( y, 
                         read_html(url) %>%  
                            html_elements('table:nth-child(7) td') %>% 
                            html_text() %>% 
                            as.data.table()
    ), use.names = FALSE)
    pg <- 2
    repeat({
        yx <- read_html(paste0(url, '?p=', pg)) %>%
                    html_elements('table:nth-child(9) td') %>% 
                    html_text()
        if(length(yx) == 0) break
        y <- rbindlist(list( y, yx %>% as.data.table() ), use.names = FALSE)
        pg <- pg + 1
    })
}
yr[parola %in% y$parola & is.na(tipo), tipo := 'a']

# scarico sinonimi/contrari
y <- character(0)
for(lt in setdiff(letters, c('h', 'j', 'k', 'w', 'x', 'y'))){
    message('Elaboro lettera ', lt)
    url <- paste0('https://www.dizy.com/it/sinonimi/', lt)
    y <- c( y, read_html(url) %>% html_elements('table:nth-child(4) td') %>% html_text() )
    pg <- 2
    repeat({
        yx <- read_html(paste0(url, '?p=', pg)) %>%
                    html_elements('table:nth-child(6) td') %>% 
                    html_text()
        if(length(yx) == 0) break
        y <- c(y, yx)
        pg <- pg + 1
    })
}
yx <- data.table()
for(wd in y){
    message('Elaboro parola ', wd)
    url <- paste0('https://www.dizy.com/it/voce/', wd, '/sinonimi')
    yx <- read_html(url)
    yx <- rbindlist(list( yx, 
                         yx %>%  
                            html_elements('table:nth-child(6) td') %>% 
                            html_text() %>% 
                            as.data.table()
    ), use.names = FALSE)


# funzioni
