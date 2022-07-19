library(tidyverse)
library(data.table)
library(sf)
library(parallel)
library(rvest)


census <-
  fread(
    'https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2021.txt'
  )
graduation <-
  fread('https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2021.txt')
career <-
  fread('https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2021.txt')

#########################################
########## Homelessness #################

homeless_counts_race_year <- read.csv('data/homeless_counts_race_year.csv')

homeless_counts_race_year %>%
  mutate(total = rowSums(select(., 1:9), na.rm = TRUE),
         latino_percentage =  Hispanic.or.Latino / total) %>%
  View


homeless_counts_race_year %>%
  mutate(total = rowSums(select(., 1:9), na.rm = TRUE)) %>%
  group_by(year) %>%
  summarise(
    latino = sum(Hispanic.or.Latino, na.rm = TRUE),
    total = sum(total, na.rm = TRUE)
  ) %>%
  mutate(percentage = latino * 100 / total)

homeless_counts_race_year %>%
  mutate(total = rowSums(select(., 1:9), na.rm = TRUE)) %>%
  filter(year == "2020-21") %>%
  mutate(percentage = Hispanic.or.Latino * 100 / total) %>% View


#########################################
########## College Readiness ############
enrollment <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2021-22&cCat=Enrollment&cPage=filesenr.asp'
  )

race_codes <- c(
  "Unknown",
  "AIAN",
  "Asian",
  "PI",
  "Filipino",
  "Latino",
  "Black",
  "White",
  "None",
  "Bi/Multiracial"
)


schools_labelled <- enrollment %>%
  group_by(CDS_CODE, SCHOOL, ETHNIC) %>%
  summarize(COUNT = sum(ENR_TOTAL)) %>%
  mutate(PROP = COUNT / sum(COUNT),
         MAJOR = ifelse(PROP > 0.5, ETHNIC, NA)) %>%
  summarize(MAJOR = paste0(MAJOR[!is.na(MAJOR)], collapse = "")) %>%
  mutate(MAJOR = ifelse(MAJOR == "", "Mixed", race_codes[as.numeric(MAJOR) + 1]))

barplot(table(schools_labelled$MAJOR) / nrow(schools_labelled))



enrollment %>%
  group_by(CDS_CODE, SCHOOL, ETHNIC) %>%
  summarize(COUNT = sum(ENR_TOTAL)) %>%
  mutate(PROP = COUNT / sum(COUNT)) %>%
  summarize(MAX_PROP = max(PROP)) %>%
  ggplot(aes(MAX_PROP)) +
  geom_histogram() +
  theme_bw()


enrollment %>%
  group_by(CDS_CODE, SCHOOL, ETHNIC) %>%
  summarize(COUNT = sum(ENR_TOTAL)) %>%
  mutate(PROP = COUNT / sum(COUNT),
         ETHNIC = race_codes[as.numeric(ETHNIC) + 1]) %>%
  ggplot(aes(PROP)) +
  geom_histogram() +
  facet_wrap(vars(ETHNIC)) +
  theme_bw()

#########################################
############# BROADBAND #################

st_par <- function(namespace, sf_func, sf_df, ...) {
  # n_cores <- detectCores(logical = TRUE)
  n_cores <- 12
  cl <- makeCluster(n_cores)
  clusterExport(cl, namespace)
  
  # Create a vector to split the data set up by.
  split_vector <-
    rep(1:n_cores,
        each = nrow(sf_df) / n_cores,
        length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <-
    parLapply(cl, split(sf_df, split_vector), function(x)
      sf_func(x, ...))
  
  stopCluster(cl)
  
  return(do.call("rbind", split_results))
}

school_districts <-
  st_read('data/California_School_District_Areas_2020-21')
broadband <-
  st_read('data/CA_Broadband_Dec2020_Public.gdb', layer = "Fixed_Consumer_Deployment") %>%
  st_transform(st_crs(school_districts))

intersections <- st_intersects(school_districts, broadband)

intersection_pct <-
  st_par(
    list("st_intersection", "school_districts"),
    st_intersection,
    broadband[c("MaxAdDn", "MaxAdUp")],
    school_districts["CDCode"]
  ) %>%
  mutate(area = st_area(.)) %>%
  group_by(CDCode) %>%
  summarize(
    AvgDn = sum(area * MaxAdDn) / sum(area),
    AvgUp = sum(area * MaxAdUp) / sum(area)
  )

broadband_average <- intersection_pct %>% st_drop_geometry()

school_districts <-
  merge(school_districts, broadband_average, by = "CDCode")

intersection_pct <-
  st_intersection(broadband[c("MaxAdDn", "MaxAdUp")], school_districts["CDCode"]) %>%
  mutate(area = st_area(.)) %>%
  group_by(CDCode) %>%
  summarize(
    AvgDn = sum(area * MaxAdDn) / sum(area),
    AvgUp = sum(area * MaxAdUp) / sum(area)
  )

#############

absenteeism <-
  fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs2021.txt') %>%
  mutate(CDS_CODE = paste0(`County Code`, `District Code`, `School Code`))


#########################################
############# ENROLLMENT ################

# Download and merge 5 years of enrollment data
enrollment_2017 <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2017-18&cCat=Enrollment&cPage=filesenr.asp'
  )
enrollment_2018 <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2018-19&cCat=Enrollment&cPage=filesenr.asp'
  )
enrollment_2019 <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2019-20&cCat=Enrollment&cPage=filesenr.asp'
  )
enrollment_2020 <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2020-21&cCat=Enrollment&cPage=filesenr.asp'
  )
enrollment_2021 <-
  fread(
    'https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2021-22&cCat=Enrollment&cPage=filesenr.asp'
  )

enrollment_17_21 <- bind_rows(
  "2017" = enrollment_2017,
  "2018" = enrollment_2018,
  "2019" = enrollment_2019,
  "2020" = enrollment_2020,
  "2021" = enrollment_2021,
  .id = "YEAR"
) %>%
  mutate(
    YEAR = factor(YEAR),
    COUNTY = factor(COUNTY),
    ETHNIC = factor(
      ETHNIC,
      levels = c(0:7, 9),
      labels = c(
        "Unknown",
        "AIAN",
        "Asian",
        "PI",
        "Filipino",
        "Latino",
        "Black",
        "White",
        "Bi/Multiracial"
      )
    )
  )

# Enrollment over time by grade
enrollment_17_21 %>%
  pivot_longer(cols = KDGN:UNGR_SEC,
               names_to = "GRADE",
               values_to = "COUNT") %>%
  group_by(YEAR, GRADE) %>%
  summarize(COUNT = sum(COUNT), .groups = "drop") %>%
  filter(!GRADE %in% c('UNGR_ELM', 'UNGR_SEC')) %>% View
ggplot(aes(
  x = YEAR,
  y = COUNT,
  color = GRADE,
  group = GRADE
)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Enrollment over time by Race
enrollment_17_21 %>%
  group_by(YEAR, ETHNIC) %>%
  summarize(COUNT = sum(ENR_TOTAL), .groups = "drop") %>%
  ggplot(aes(
    x = YEAR,
    y = COUNT,
    color = ETHNIC,
    group = ETHNIC
  )) +
  geom_point() +
  geom_line() +
  theme_bw()


# Enrollment over time by County
enrollment_17_21 %>%
  group_by(YEAR, COUNTY) %>%
  summarize(COUNT = sum(ENR_TOTAL), .groups = "drop") %>%
  group_by(COUNTY) %>%
  mutate(PROP = COUNT / first(COUNT)) %>%
  filter(COUNT > 10000) %>%
  ggplot(aes(
    x = YEAR,
    y = PROP,
    color = COUNTY,
    group = COUNTY
  )) +
  geom_line(aes(size = COUNT), alpha = 0.5) +
  theme_bw()

# Latino percentage enrollment by year and county
latino_percentage <- enrollment_17_21 %>%
  group_by(YEAR, COUNTY, ETHNIC) %>%
  summarize(COUNT = sum(ENR_TOTAL), .groups = "drop_last") %>%
  mutate(PROP = COUNT / sum(COUNT)) %>%
  filter(ETHNIC == "Latino") %>%
  select(YEAR, COUNTY, PROP_LATINO = PROP)

# Latino percentage over time
enrollment_17_21 %>%
  group_by(YEAR, COUNTY) %>%
  summarize(COUNT = sum(ENR_TOTAL), .groups = "drop") %>%
  group_by(COUNTY) %>%
  mutate(PROP = COUNT / first(COUNT)) %>%
  merge(latino_percentage, by = c("YEAR", "COUNTY")) %>%
  ggplot(aes(
    x = YEAR,
    y = PROP_LATINO,
    color = COUNTY,
    group = COUNTY
  )) +
  geom_line(aes(size = COUNT), alpha = 0.5) +
  theme_bw()


#########################################
############ ABSENTEEISM ################

# Download and merge 4 years of absenteeism data
absenteeism_2016 <-
  fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs1617.txt')

absenteeism_2017 <-
  fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs1718.txt')

absenteeism_2018 <-
  fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs1819.txt')

absenteeism_2020 <-
  fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs2021.txt')

names(absenteeism_2020) <- gsub(" ", "", names(absenteeism_2020))

absenteeism_16_20 <- bind_rows(absenteeism_2016,
                               absenteeism_2017,
                               absenteeism_2018,
                               absenteeism_2020) %>%
  mutate(CDS_CODE = paste0(
    sprintf("%02d", CountyCode),
    sprintf("%05d", DistrictCode),
    sprintf("%07d", SchoolCode)
  )) %>%
  filter(AggregateLevel == "S") %>%
  select(-AggregateLevel) %>%
  mutate(ReportingCategory = ifelse(ReportingCategory == "GRK", "GRKN", ReportingCategory))

# Absenteeism by race over time
absenteeism_16_20 %>%
  filter(ReportingCategory %in% c("RB", "RI", "RA", "RF", "RH", "RD", "RP", "RT", "RW")) %>%
  group_by(AcademicYear, ReportingCategory) %>%
  summarize(
    Rate = sum(ChronicAbsenteeismCount, na.rm = TRUE) / sum(ChronicAbsenteeismEligibleCumula, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(
    x = AcademicYear,
    y = Rate,
    color = ReportingCategory,
    group = ReportingCategory
  )) +
  theme_bw()

# Absenteeism by grade level over time
absenteeism_16_20 %>%
  filter(ReportingCategory %in% c("GRKN", "GR13", "GR46", "GR78", "GR912")) %>%
  group_by(AcademicYear, ReportingCategory) %>%
  summarize(
    Rate = sum(ChronicAbsenteeismCount, na.rm = TRUE) / sum(ChronicAbsenteeismEligibleCumula, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(
    x = as.Date(AcademicYear, format = "%Y"),
    y = Rate,
    color = ReportingCategory,
    group = ReportingCategory
  )) +
  theme_bw()

# Absenteeism by county over time
absenteeism_16_20 %>%
  filter(ReportingCategory == "TA") %>%
  group_by(AcademicYear, CountyName) %>%
  summarize(
    Rate = sum(ChronicAbsenteeismCount, na.rm = TRUE) / sum(ChronicAbsenteeismEligibleCumula, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(
    x = AcademicYear,
    y = Rate,
    color = CountyName,
    group = CountyName
  )) +
  theme_bw()
