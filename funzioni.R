library(data.table)

parole <- readRDS('./parole')

estrai_parole <- function(schema, lunga = NULL, inizio = NULL, fine = NULL, staccate = FALSE, ordine = TRUE){
    if(staccate){
        if(ordine){
            schema <- paste0(strsplit(schema, '')[[1]], collapse = '.*')
        } else {
            schema <- paste0(strsplit(schema, '')[[1]], collapse = '.*')
        }
    }
    if(is.null(inizio) & is.null(fine)){
        schema <- paste0('.*', schema, '.*')
    } else {
        if(!is.null(inizio)){
            if(tolower(substr(inizio, 1, 1)) == 's'){ 
                schema <- paste0('^', schema, '.*')
            } else if(tolower(substr(inizio, 1, 1)) == 'n'){
                schema <- paste0('.{1}.*', schema, '.*')
            }
        }
        if(!is.null(fine)){
            if(tolower(substr(fine, 1, 1)) == 's'){ 
                schema <- paste0('.*', schema, '$')
            } else if(tolower(substr(fine, 1, 1)) == 'n'){
                schema <- paste0('.*', schema, '.*.{1}')
            }
        }
    }
    y <- parole[grepl(schema, word)][order(word)]
    if(is.null(lunga)){ y[, word] } else { y[len %in% lunga, word] }
}

confronta_parole <- function(x, y){ # x > target, y > guess
    xc <- strsplit(x, '')[[1]]
    yc <- strsplit(y, '')[[1]]
    yr <- as.numeric(xc == yc) * 2
    if(sum(yr) > 1) xc <- xc[-which(yr > 0)]
    for(idx in which(yr == 0)){
        yx <- grep(yc[idx], xc)[1]
        if(!is.na(yx)){
            yr[idx] <- 1
            xc <- xc[-yx]
        }
    }
    yr
}
