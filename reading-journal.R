## Analysis of reading journal
## Author: Stanislav Vlasov
## Date: 2018-02-17

## Load additional packages
## --------------------------------------------------------------------------------

packages <- c("magrittr"
            , "jsonlite"
            , "stringr"
            , "igraph"
            , "htmlwidgets"
            , "networkD3"
            , "pbapply"
            , "dplyr"
            , "xlsx"  # You need to install Java to work with xlsx
            , "lubridate"
              )

packages.check <- lapply(packages, function(p) {
    if (!require(p, character.only = TRUE)) {
        install.packages(p
                       , repos = 'http://cloud.r-project.org'
                       , dependencies = TRUE)
        library(p, character.only = TRUE)}})



## Set URL to list of papers and reading journals
## --------------------------------------------------------------------------------
rj.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vScOHwKBns9I5NA-jgvTukwqTV4rGRc-lxhIH3pXyP0wxdAp6pIp3tK-RwydEF-9QCTCpPqsqRXdZsA/pub?output=xlsx"

## Where to save file when downloading
rj.filename <- "reading-journal-2018-02-16.xlsx"

## Download the file
download.file(rj.url, rj.filename, mode="wb")



## Get list of papers
## --------------------------------------------------------------------------------
papers <- read.xlsx(rj.filename
                  , sheetIndex = 1
                  , colIndex = 1:5
                  , stringsAsFactors = FALSE) %>%
    extract(!is.na(.[[1]]), )



## Get reading journals
## --------------------------------------------------------------------------------
rj.workbook <- loadWorkbook(rj.filename)

rj.names <- getSheets(rj.workbook) %>%
    names %>%
    extract(-1)

rj <- rj.names %>%
    lapply(function(sheetname) {
        read.xlsx(rj.filename
                , sheetName = sheetname
                , colIndex = c(2,3,4)
                , stringsAsFactors = FALSE) %>%
            extract(!is.na(.[[1]]), )
    }) %>% setNames(rj.names)


## Cleaning the data
## --------------------------------------------------------------------------------
rj.raw <- rj  # save

## rj <- rj.raw

## Delete missing dates
rj <- lapply(rj, function(j) filter(j, !is.na(date)))

## Parse the dates
rj %>% lapply(function(name) {
        name %>%
        extract2(2) %>%
        extract(!is.na(.)) %>% 
        parse_date_time(c("ymd", "dmy", "ydm"))
})

rj %<>% lapply(function(name) {
    name$date <-
        name %>%
        extract2(2) %>%
        extract(!is.na(.)) %>% 
        parse_date_time(c("ymd", "dmy", "ydm"))
    return(name)
})






## How much time it took for all (in hours)
## --------------------------------------------------------------------------------
sum(unlist(lapply(rj, function(x) sum(x$minutes, na.rm = TRUE)/60/16/10)))

lapply(rj, function(x) sum(x$minutes, na.rm = TRUE)/60)

## Make a time grid
rj.pooled <- do.call(rbind, rj)


rj.days <- seq(min(as.Date(rj.pooled$date))
             , max(as.Date(rj.pooled$date))
             , by = "day")




rj.times <- sapply(rj.days
                 , function(d) {
                     rj.pooled$minutes[as.Date(rj.pooled$date) == d] %>%
                         sum(., na.rm = TRUE) %>%
                         "/"(.,60)
                 })

barplot(rj.times
      , names.arg = rj.days
      , las = 2
      , cex.names = 0.7)



## Reading network (bipartile graph)
## --------------------------------------------------------------------------------
reading.mat <- sapply(rj, function(j) papers$paper %in% j$paper)

rownames(reading.mat) <- papers$paper

reading <- graph_from_incidence_matrix(reading.mat) %>%
    igraph_to_networkD3

reading$nodes$groups <- c(rep(1, nrow(reading.mat))
                        , rep(2, ncol(reading.mat)))

forceNetwork(Links = reading$links
           , Nodes = reading$nodes
           , Source = "source"
           , Target = "target"
           , NodeID = "name"
           , Group = "groups"
           , zoom = TRUE)


## Coreading network
## --------------------------------------------------------------------------------
coread.mat <- t(reading.mat) %*% reading.mat

coread <- graph_from_adjacency_matrix(coread.mat
                                    , weighted = TRUE
                                    , diag = TRUE) %>%
    igraph_to_networkD3

forceNetwork(Links = coread$links, Nodes = coread$nodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name", Group = 1
           , zoom = TRUE)




## Most read papers
## --------------------------------------------------------------------------------
papers.chart <- rowSums(reading.mat)

papers.chart[order(papers.chart, decreasing = TRUE)]



