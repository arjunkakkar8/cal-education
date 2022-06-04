library(tidyverse)
library(data.table)
library(sf)
library(parallel)


census <-
  fread(
    'https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2021.txt'
  )
graduation <-
  fread('https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2021.txt')
career <-
  fread('https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2021.txt')


#########################################
############# BROADBAND #################

st_par <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  # Combine results back together. Method of combining depends on the output from the function.
  if ( class(split_results[[1]]) == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call("rbind", split_results)
  }
  
  # Return result
  return(result)
}

school_districts <-
  st_read('California_School_District_Areas_2020-21')
broadband <-
  st_read('CA_Broadband_Dec2020_Public.gdb/', layer = "Fixed_Consumer_Deployment") %>%
  st_transform(st_crs(school_districts))

intersections <- st_intersects(school_districts, broadband)

intersection_pct <- st_par(school_districts, st_intersection, 8, broadband)
  
  st_intersection(school_districts, broadband) %>%
  mutate(pct = st_area(.))

#st_layers('California_School_District_Areas_2020-21/')

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
