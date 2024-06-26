library(tidyverse)
library(googlesheets4)
## Estimate total homeless counts across county, year and race in California

# Prerequisite 1: Create student homelessness dataset
# Prerequisite 2: Create total homeless multipliers by race
# Prerequisite 3: Create doubled up estimates
# Prerequisite 4: Total population estimates by race and county for 2019:
#                 https://data.census.gov/cedsci/table?t=Race%20and%20Ethnicity&g=0400000US06,06%240500000&d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2019.DP05
#                 Filter data to only contain race rows for ease of processing


homeless_counts_race <-
    read.csv("data/intermediate/homeless_counts_race_2022.csv") %>%
    rename(
        Black = African.American,
        AIAN = American.Indian.or.Alaska.Native,
        Latino = Hispanic.or.Latino,
        Other = Not.Reported,
        `Two or More` = Two.or.More.Races
    ) %>%
    rowwise() %>%
    mutate(
        AAPI = sum(Asian, Filipino, Pacific.Islander, na.rm = TRUE),
        .keep = "unused",
        .after = "AIAN"
    ) %>%
    mutate(AAPI = ifelse(AAPI == 0, NA, AAPI)) %>%
    pivot_longer(1:7, names_to = "race", values_to = "students") %>%
    filter(race != "Total", !is.na(students)) %>%
    select(-cds)

homeless_multipliers_race <-
    read.csv("data/intermediate/homeless_multipliers_race_2021.csv") %>%
    select(-YEAR)

doubledup_estimates_race <-
    read.csv("data/intermediate/doubled_up_race_2021.csv")

total_counts_race <-
    read.csv("data/raw/total_counts_race_2021.csv") %>%
    select(
        county = 1,
        White = 3,
        Black = 4,
        AIAN = 5,
        Asian = 6,
        PI = 7,
        Other = 8,
        `Two or More` = 9,
        Latino = 10,
    ) %>%
    mutate(county = gsub(" County, California", "", county)) %>%
    mutate(across(2:9, ~ as.numeric(gsub(",", "", .)))) %>%
    mutate(AAPI = Asian + PI, .keep = "unused") %>%
    relocate(county) %>%
    pivot_longer(2:8, names_to = "race", values_to = "total")


estimated_homeless <-
    # Merge student homeless counts with total population
    homeless_counts_race %>%
    merge(total_counts_race,
        by = c("county", "race"),
        all = TRUE
    ) %>%
    # Calculate the proportion of each race for each county
    group_by(county) %>%
    mutate(total_prop = total / sum(total)) %>%
    # Remove county-race combos that have less than 1000 individuals
    filter(total > 1000) %>%
    # Merge multipliers by race and calculate estimated homeless populations
    merge(homeless_multipliers_race,
        by.x = "race",
        by.y = "RACE_COMB"
    ) %>%
    mutate(
        estimated_total = ifelse(is.na(students), 0, students * MULT),
        estimated_total_se = ifelse(is.na(students), 0, students * MULT_se)
    ) %>%
    select(-MULT, -MULT_se) %>%
    arrange(county, race) %>%
    # Calculate the proportion of estimated homeless by race for each county
    group_by(county) %>%
    mutate(estimated_prop = estimated_total / sum(estimated_total)) %>%
    # Merge in doubled up estimates
    merge(
        doubledup_estimates_race,
        by.x = c("county", "race"),
        by.y = c("county", "RACE_COMB"),
        all = TRUE
    ) %>%
    rename(doubledup = COUNT, doubledup_se = COUNT_se) %>%
    group_by(county) %>%
    # Calculate the proportion of doubled up by race for each county
    mutate(doubledup_prop = doubledup / sum(doubledup, na.rm = TRUE))

estimated_homeless %>%
    select(-ends_with("_se")) %>%
    write.csv(
        "data/processed/homeless_estimates.csv",
        row.names = FALSE
    )

sheet_write(
    estimated_homeless,
    "1Hbn66qNCI4ejuDiHv55n0EN2NQffbIEK8NPe1VhjJ-w",
    sheet = "Estimated Homeless 2022"
)

# estimated_homeless %>%
#     pivot_wider(names_from = race, values_from = 3:11) %>%
#     sheet_write(
#         "1Hbn66qNCI4ejuDiHv55n0EN2NQffbIEK8NPe1VhjJ-w",
#         sheet = "HOUSING INSECURITY"
#     )
