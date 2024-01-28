library(rvest)
library(tidyverse)

# Create dataset of homeless students in CA across years and race
get_homeless <- function(cds = "00",
                         level = "state",
                         year = "2022-23") {
    request_url <- paste0(
        "https://dq.cde.ca.gov/dataquest/DQCensus/HmlsEnrByDT.aspx?cds=",
        cds,
        "&agglevel=",
        level,
        "&year=",
        year
    )

    extract <- read_html(request_url)

    name_table <- extract %>%
        html_element("#ContentPlaceHolder1_grdHmlsEnrByDTTotals") %>%
        html_table()

    extract %>%
        html_element("#ContentPlaceHolder1_grdHmlsEnrByDT") %>%
        html_table() %>%
        select(1, 3) %>%
        rename(a = 1, b = 2) %>%
        mutate(b = as.character(b)) %>%
        pivot_wider(names_from = a, values_from = b) %>%
        mutate_all(~ ifelse(. == "*", NA, as.numeric(gsub(",", "", .)))) %>%
        mutate(
            county = name_table[[1]][1],
            cds = cds,
            year = year
        ) %>%
        return()
}

get_homeless_by_county <- function(year = "2022-23", range = 1:58) {
    lapply(range, function(i) {
        get_homeless(sprintf("%02d", i), "county", year)
    }) %>%
        bind_rows() %>%
        return()
}

totals <- get_homeless(year = "2022-23") %>%
    select(-year) %>%
    mutate(county = "California")

get_homeless_by_county("2022-23") %>%
    select(-year) %>%
    bind_rows(totals) %>%
    write.csv("data/intermediate/homeless_counts_race_2022.csv",
        row.names = FALSE
    )

# test <- get_homeless(year = "2022-23", cds = "01", level = "county")
#
# homeless_counts_race_year <-
#   lapply(c("2016-17", "2017-18", "2018-19", "2020-21"), function(year) {
#     get_homeless_by_county(year)
#   }) %>%
#   bind_rows()
#
# write.csv(homeless_counts_race_year,
#           'data/homeless_counts_race_year.csv',
#           row.names = FALSE)
