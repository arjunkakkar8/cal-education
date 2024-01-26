library(srvyr)
library(tidyverse)
library(robsurvey)
library(acs)

# Read in the data downloaded from IPUMS and check that the variables and number of observations are as expected.
# Next, we unite the STATEFIP with PUMA to create unique IDs (this is more important when we have more than one state in the sample or we are planning to join with other data sets).
raw <-
  read.csv("data/raw/acs_2021_5_year.csv",  blank.lines.skip = TRUE) %>%
  mutate(STATEFIP = as.character(STATEFIP),
         PUMA = as.character(PUMA)) %>%
  unite("GEOID", c(STATEFIP, PUMA), remove = FALSE, sep = "")

names(raw) #names of variables
nrow(raw) #number of observations


# Next, we use the robsurvey pacakge to apply the household weights to the variables. This is to create the median rent index to adjust the poverty variables. You will see the message "summarise()` ungrouping output (override with `.groups` argument)" which can be ignored.
area_data <- raw %>%
  filter(OWNERSHP == 2, BEDROOMS == 3, KITCHEN == 4, PLUMBING == 20, PERNUM == 1) %>%
  group_by(GEOID) %>%
  summarise(AMGR = weighted_median(RENTGRS, HHWT))

# Next, join the original dataset with the area_data frame that only has GEOID and the area median gross rent.
# Next, we construct new variables needed for the doubling up measure. In this sample code, 1086 represents the national median gross rent for 2019 for a 2 bedroom, but this should be updated for future or past years (Use Census TableID: B25031)
# Average 2 bedroom rate is 1150 for 2021 5-year ACS
# Average 2 bedroom rate is 1294 for 2022 1-year ACS
national_amgr <- 1150
computed <-
  left_join(raw, area_data,  by = "GEOID") %>%
  mutate(
    adjustment = ifelse(
      OWNERSHP == 1 & MORTGAGE == 1,
      .402 * (AMGR / national_amgr) + .598,
      ifelse(
        OWNERSHP == 1 &
          MORTGAGE == 2 |
          MORTGAGE == 3  | MORTGAGE == 4,
        .504 * (AMGR / national_amgr) + .496,
        ifelse(OWNERSHP == 2 &
                 MORTGAGE == 0, .514 * (AMGR / national_amgr) + .486, NA)
      )
    ),
    adjustedpoverty_head = POVERTY_HEAD * (1 / adjustment),
    adjustedpoverty = POVERTY * (1 / adjustment),
    roundedadjustedpoverty = round(adjustedpoverty, 0),
    roundedadjustedpoverty_head = round(adjustedpoverty_head, 0),
    overcrowded = ifelse((NUMPREC / 2) > (BEDROOMS - 1), 1, 0),
    DUrelative = ifelse(RELATE %in% c(5, 6, 7, 8, 10)  &
                          AGE < 65, 1, 0),
    special1 = ifelse(RELATE == 7 &
                        AGE < 18 & MOMLOC == 0 & POPLOC == 0, 1, 0),
    special2 = ifelse(
      RELATE == 7 &
        AGE > 17 & MOMLOC == 0 & POPLOC == 0 & MOMLOC_HEAD == 0  &
        POPLOC_HEAD == 0 &
        SPLOC == 0 & SPLOC_HEAD == 0 &
        NCHILD == 0 &
        NCHILD_HEAD == 0,
      1,
      0
    ),
    DUrelative = ifelse(special1 == 1 |
                          special2 == 1, 0, DUrelative),
    DUolderrelative = ifelse(RELATE  %in% c(5, 6, 7, 8, 10) &
                               AGE >= 65 & overcrowded == 1, 1, 0),
    DUmiddlegen = ifelse(RELATE %in% c(3, 4) &
                           SFTYPE %in% c(3, 4) & AGE >= 18, 1, 0),
    DUmarriedchild = ifelse(RELATE %in% c(3, 4) &
                              SFTYPE %in% c(1, 2), 1, 0),
    DUgrandchild = ifelse(
      RELATE !=  9,
      0,
      ifelse(
        RELATE == 9 & GCRESPON_HEAD == 2 & AGE < 18,
        0,
        ifelse(
          RELATE == 9 & SFTYPE == 4  & AGE_MOM < 18,
          0,
          ifelse(RELATE == 9 &
                   SFTYPE == 3 &  AGE_POP < 18, 0, 1)
        )
      )
    ),
    DUsingadcrowd = ifelse(RELATE %in% c(3, 4) &
                             SFTYPE == 0 &
                             AGE > 17 & overcrowded == 1, 1, 0),
    DUnonrelative = ifelse(RELATED == 1260, 1, 0),
    special6 = ifelse(
      RELATED == 1260 & (RELATED_MOM %in% 1114 | RELATED_POP %in% 1114) &
        (AGE < 18 |
           (AGE >= 18 & overcrowded == 0)),
      1,
      0
    ),
    DUnonrelative = ifelse(special6 == 1, 0, DUnonrelative),
    povertylev = ifelse(
      roundedadjustedpoverty_head <= 125 &
        roundedadjustedpoverty <= 125,
      1,
      0
    ),
    DU1 = ifelse(povertylev == 1 &
                   DUmiddlegen == 1, 1, 0),
    DU2 = ifelse(povertylev == 1 &
                   DUrelative == 1, 1, 0),
    DU3 = ifelse(povertylev == 1 &
                   DUgrandchild == 1, 1, 0),
    DU4 = ifelse(povertylev == 1 &
                   DUmarriedchild == 1, 1, 0),
    DU5 = ifelse(povertylev == 1 &
                   DUnonrelative == 1, 1, 0),
    DU6 = ifelse(povertylev == 1 &
                   DUsingadcrowd == 1, 1, 0),
    DU7 = ifelse(povertylev == 1 &
                   DUolderrelative == 1, 1, 0),
    doubledup = ifelse(DU1 == 1 |
                         DU2 == 1 |
                         DU3 == 1 | DU4 == 1 | DU5 == 1 | DU6 == 1 | DU7 == 1, 1, 0)
  )


person_weighted <-
  computed %>%
  # Add computed race variable that creates expected categories
  mutate(
    RACE_COMB = case_when(
      HISPAN != 0 ~ "Latino",
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 3 ~ "AIAN",
      RACE == 4 | RACE == 5 | RACE == 6 ~ "AAPI",
      RACE == 8 | RACE == 9 ~ "Two or More",
      RACE == 7 ~ "Other",
      TRUE ~ "Other"
    ),
    STUDENT_AGED = ifelse(AGE <= 17 & AGE >= 5, 1, 0)
  ) %>%
  # Finally, we enter the survey design information to compute weighted frequencies and
  # standard errors using ACS person weights (PERWT) and Replicate Weights (REPWTP).
  as_survey_design(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "JK1",
    scale = 4 / 80,
    rscales = rep(1, 80),
    mse = TRUE
  )


totals <- person_weighted %>%
  group_by(YEAR, RACE_COMB) %>%
  summarise(COUNT = survey_total(doubledup)) %>%
  ungroup() %>%
  select(-YEAR) %>%
  mutate(county = "California")

person_weighted %>%
  group_by(YEAR, COUNTYFIP, RACE_COMB) %>%
  summarise(COUNT = survey_total(doubledup)) %>%
  ungroup() %>%
  select(-YEAR) %>%
  filter(COUNTYFIP != 0) %>%
  merge(geo.lookup("CA", .$COUNTYFIP),
        by.x = "COUNTYFIP",
        by.y = "county") %>%
  mutate(county = gsub(" County", "", county.name)) %>%
  select(-state, -state.name, -county.name, -COUNTYFIP) %>%
  filter(COUNT != 0) %>%
  bind_rows(totals) %>%
  write.csv("data/intermediate/doubled_up_race_2021.csv", row.names = FALSE)

person_weighted %>%
  group_by(YEAR, RACE_COMB) %>%
  summarise(MULT = survey_ratio(doubledup, (STUDENT_AGED == 1) * doubledup)) %>%
  write.csv("data/intermediate/homeless_multipliers_race_2021.csv",
            row.names = FALSE)
