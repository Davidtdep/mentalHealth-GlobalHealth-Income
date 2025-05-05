###############################################################################
# 1. LIBRARIES
###############################################################################
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)
library(ggsci)
library(tidyverse)
library(igraph)
library(wbstats)
library(rvest)
library(tidyr)
library(purrr)
library(pheatmap)
library(writexl)
library(ggbreak)
library(metafor)
library(openxlsx)


###############################################################################
# 2. DATA INPUT
###############################################################################
# This section reads external files that contain the data for analysis.

# Load main data (all as character to avoid type-guessing issues)
papers_data <- read_xlsx(
  "~/mentalHealth/data/data.xlsx",
  col_types = "text",  # Import all as character
  guess_max = 500000
)

# Load dataframe with indicator names
indicators_df <- read_xlsx("~/mentalHealth/data/column_names.xlsx")

# Load Scimago data
scimago_data <- read_delim("~/mentalHealth/data/scimagojr2023.csv", delim = ";")

# Load the filtered dataset
filtered_data <- read.csv("~/mentalHealth/data/data_filtered.csv", check.names = FALSE)
names(filtered_data)[1] <- "id"  # Rename the first column to "id"


###############################################################################
# 3. DATA CLEANING AND NORMALIZATION
###############################################################################
# This section cleans, normalizes, and merges columns in 'papers_data'.

# -- Helper function to clean and standardize text (journal names, etc.) --
clean_text <- function(x) {
  x %>%
    str_trim() %>%                     # Remove spaces at beginning and end
    str_to_lower() %>%                 # Convert to lowercase
    str_replace_all("[^a-z0-9 ]", "")  # Remove special characters
}

# Rename certain columns of interest upfront to keep them in English
# (avoids repeated rename steps later in the code).
papers_data <- papers_data %>%
  rename(
    document_type = DT,         # originally "DT"
    mental_health_care_delivery = `Mental health care delivery` # originally in Spanish context
  )

# Clean/normalize journal names in both datasets
papers_data <- papers_data %>% mutate(journal_clean = clean_text(SO))
scimago_data <- scimago_data %>% mutate(title_clean = clean_text(Title))

# We also rename Scimago columns for clarity
scimago_data <- scimago_data %>%
  rename(
    best_quartile = `SJR Best Quartile`,
    scimago_h_index = `H index`
  )

# Merge quartile and H-index from scimago_data into papers_data
papers_data <- papers_data %>%
  left_join(
    scimago_data %>% select(title_clean, best_quartile, scimago_h_index),
    by = c("journal_clean" = "title_clean")
  ) %>%
  mutate(
    quartile = best_quartile,
    h_index = scimago_h_index
  ) %>%
  select(-best_quartile, -scimago_h_index)

# Convert quartile "-" to NA
papers_data <- papers_data %>%
  mutate(quartile = na_if(quartile, "-"))

# Convert PY (year) and TC (times cited) to numeric, then rename
papers_data$PY <- as.numeric(papers_data$PY)
papers_data$TC <- as.numeric(papers_data$TC)
papers_data <- papers_data %>%
  rename(
    date = PY,              # "date" replaces "PY"
    times_cited = TC        # "times_cited" replaces "TC"
  )

# Standardize "mental_health_care_delivery" Yes/No
papers_data$mental_health_care_delivery <- ifelse(
  papers_data$mental_health_care_delivery == "yes",
  "Yes",
  papers_data$mental_health_care_delivery
)

# Clean incoherent values in 'income' and 'region'
papers_data$income <- ifelse(papers_data$income %in% c("NO TIENE", "0"), NA, papers_data$income)
papers_data$region[papers_data$region == "NO TIENE"] <- NA

# Remove certain document types
papers_data <- papers_data %>%
  filter(!(document_type %in% c("ARTICLE IN PRESS",
                                "BOOK CHAPTER",
                                "CONFERENCE PAPER",
                                "CONFERENCE REVIEW",
                                "ERRATUM",
                                "RETRACTED")))

# Remove columns 49:120 and 122 (original numeric indicators, presumably not needed)
papers_data <- papers_data[, -c(49:120, 122)]


###############################################################################
# 4. MERGE WORLD BANK (WB) INDICATORS INTO filtered_data
###############################################################################
# We'll integrate WB indicators into the filtered_data object
# (which already includes some prior filtering steps).

# Extract the valid WB indicator IDs (ignore "MANUAL" or "ALREADY IMPORTED")
wb_indicators <- indicators_df$WB[!is.na(indicators_df$WB)]
wb_indicators <- setdiff(wb_indicators, c("MANUAL", "ALREADY IMPORTED"))

# Download data for those WB indicators
wb_data_all <- wb_data(indicator = wb_indicators)

# Rename columns using label attributes if present
for (i in 5:ncol(wb_data_all)) {
  colnames(wb_data_all)[i] <- attr(wb_data_all[[i]], "label")
}

# Keep only rows in filtered_data that have a region
filtered_data <- filtered_data %>%
  filter(!is.na(region))

# Normalize country names in wb_data_all for merging
wb_data_all$country <- toupper(wb_data_all$country)

# Fix mismatched country names in filtered_data
country_corrections <- c(
  "TAIWAN" = "CHINA",
  "CZECH REPUBLIC" = "CZECHIA",
  "YUGOSLAVIA" = "SERBIA",
  "EGYPT" = "EGYPT, ARAB REP.",
  "SUIZA" = "SWITZERLAND",
  "ENGLAND" = "UNITED KINGDOM",
  "BRASIL" = "BRAZIL",
  "RUSSIA" = "RUSSIAN FEDERATION",
  "ISLAND" = "ICELAND",
  "DINAMARK" = "DENMARK",
  "IRAN" = "IRAN, ISLAMIC REP.",
  "TURKEY" = "TURKIYE",
  "VENEZUELA" = "VENEZUELA, RB",
  "SLOVAKIA" = "SLOVAK REPUBLIC",
  "SOUTH KOREA" = "KOREA, REP.",
  "SYRIAN" = "SYRIAN ARAB REPUBLIC",
  "YEMEN" = "YEMEN, REP.",
  "PALESTINE" = "WEST BANK AND GAZA"
)
filtered_data$country <- ifelse(
  filtered_data$country %in% names(country_corrections),
  country_corrections[filtered_data$country],
  filtered_data$country
)

# Rename filtered_data$PY to "date" (already done in the code that produced it),
# but we do it again here if needed:
colnames(filtered_data)[colnames(filtered_data) == "PY"] <- "date"

# Merge the WB indicators into filtered_data by "date" and "country"
filtered_data <- filtered_data %>%
  left_join(wb_data_all[, c("date", "country", colnames(wb_data_all)[5:16])],
            by = c("date", "country"))


###############################################################################
# 5. MERGE INCOME GROUPS CLASSIFICATION
###############################################################################
# We merge official WB income groups classification

income_groups <- wb_cachelist$countries %>%
  select(iso3c, country, region, income_level)

# Normalize name
income_groups$country <- toupper(income_groups$country)

# Fix more country names in filtered_data
extra_corrections <- c(
  "CZECHIA" = "CZECH REPUBLIC",
  "TURKIYE" = "TURKEY",
  "VIET NAM" = "VIETNAM"
)
filtered_data$country <- ifelse(
  filtered_data$country %in% names(extra_corrections),
  extra_corrections[filtered_data$country],
  filtered_data$country
)

# We had an 'income' column; rename that to 'income_level' to avoid confusion
colnames(filtered_data)[colnames(filtered_data) == "income"] <- "income_level"

# Merge correct WB income groups
filtered_data <- filtered_data %>%
  left_join(income_groups %>% select(country, income_level),
            by = "country") %>%
  mutate(income_level = coalesce(income_level.y, income_level.x)) %>%
  select(-income_level.x, -income_level.y)

# Reposition the 'income_level' column
filtered_data <- filtered_data %>%
  relocate(income_level, .before = colnames(filtered_data)[21])


###############################################################################
# 6. MERGE WHO GHO INDICATORS INTO filtered_data
###############################################################################
# Query WHO GHO indicators, then pivot them wide and merge.

# Step 1: Get full list of indicators from GHO
gho_url <- "https://ghoapi.azureedge.net/api/Indicator"
gho_response <- GET(gho_url)
gho_indicators_full <- fromJSON(content(gho_response, "text"), flatten = TRUE)$value

# Fix mismatches between 'indicators_df$GHO' and GHO names
who_indicators <- indicators_df$GHO
gho_corrections <- c(
  "Healthy life expectancy (HALE) at birth" =
    "Healthy life expectancy (HALE) at birth (years)",
  "Mental health day treatment facilities, per 100,000" =
    "Mental health day treatment facilities (per 100,000)",
  "Nurses working in mental health sector (per 100 000)" =
    "Nurses working in mental health sector (per 100,000)",
  "Policy and financing" = "Mental health policy",
  "Mental health care delivery" = "Stand-alone mental health legislation"
)
who_indicators <- ifelse(
  who_indicators %in% names(gho_corrections),
  gho_corrections[who_indicators],
  who_indicators
)

# Identify which GHO indicators we can match
matched_gho <- gho_indicators_full %>%
  filter(IndicatorName %in% who_indicators) %>%
  select(IndicatorCode, IndicatorName) %>%
  na.omit()

# Step 2: Fetch WHO country dimension table
country_url <- "https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues"
country_resp <- GET(country_url)
countries_gho_df <- fromJSON(content(country_resp, "text"), flatten = TRUE)$value %>%
  select(Code, Title) %>%
  rename(COUNTRY = Code, COUNTRY_NAME = Title)

# Step 3: Function to fetch each indicator from GHO
fetch_gho_data <- function(indicator_code, indicator_name) {
  url <- paste0("https://ghoapi.azureedge.net/api/", indicator_code)
  resp <- GET(url)
  if (status_code(resp) == 200) {
    json_data <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    if (!is.null(json_data$value)) {
      df <- as.data.frame(json_data$value)
      names(df) <- tolower(names(df))
      required_cols <- c("spatialdim", "timedim", "numericvalue")
      if (all(required_cols %in% names(df))) {
        df <- df %>%
          filter(is.na(dim1) | dim1 == "BTSX") %>%  # Both sexes
          select(COUNTRY = spatialdim, YEAR = timedim, VALUE = numericvalue) %>%
          mutate(INDICATOR = indicator_name)
        return(df)
      }
    }
  }
  return(NULL)
}

# Download each matched indicator
gho_data_list <- map2(
  matched_gho$IndicatorCode,
  matched_gho$IndicatorName,
  ~ fetch_gho_data(.x, .y)
)

# Remove any NULL dataframes and bind
gho_data_df <- bind_rows(compact(gho_data_list))

# Merge with the country names
gho_data_df <- gho_data_df %>%
  left_join(countries_gho_df, by = "COUNTRY") %>%
  select(COUNTRY = COUNTRY_NAME, YEAR, VALUE, INDICATOR)

# Pivot wide
gho_data_df <- pivot_wider(
  data = gho_data_df,
  id_cols = c(COUNTRY, YEAR),
  names_from = INDICATOR,
  values_from = VALUE
)

# Rename for merging
colnames(gho_data_df)[colnames(gho_data_df) == "COUNTRY"] <- "country"
colnames(gho_data_df)[colnames(gho_data_df) == "YEAR"] <- "date"

# Normalize to uppercase
gho_data_df$country <- toupper(gho_data_df$country)

# Reconcile with filtered_data country names
filtered_data <- filtered_data %>%
  mutate(country = recode(country,
                          "UNITED STATES" = "UNITED STATES OF AMERICA",
                          "UNITED KINGDOM" = "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND",
                          "CZECH REPUBLIC" = "CZECHIA",
                          "EGYPT, ARAB REP." = "EGYPT",
                          "NETHERLANDS" = "NETHERLANDS (KINGDOM OF THE)",
                          "IRAN, ISLAMIC REP." = "IRAN (ISLAMIC REPUBLIC OF)",
                          "TANZANIA" = "UNITED REPUBLIC OF TANZANIA",
                          "TURKIYE" = "TÜRKIYE",
                          "VENEZUELA, RB" = "VENEZUELA (BOLIVARIAN REPUBLIC OF)",
                          "SLOVAK REPUBLIC" = "SLOVAKIA",
                          "KOREA, REP." = "REPUBLIC OF KOREA",
                          "VIETNAM" = "VIET NAM",
                          "SYRIAN ARAB REPUBLIC" = "SYRIA",
                          "MOLDOVA" = "REPUBLIC OF MOLDOVA",
                          "BOLIVIA" = "BOLIVIA (PLURINATIONAL STATE OF)",
                          "YEMEN, REP." = "YEMEN"
                          # Some entries like "PUERTO RICO", "BERMUDA", "KOSOVO" may not appear in GHO
  ))

# Merge GHO indicators
filtered_data <- filtered_data %>%
  left_join(gho_data_df, by = c("country", "date"))


###############################################################################
# 7. MERGE MANUAL WHO REGIONAL INDICATORS
###############################################################################
# Example: Health researchers per million & ODA per capita by region

REGIONAL_Health_researchers <- read.csv(
  "~/mentalHealth/data/indicators/GHO/Health researchers (in full-time equivalent) per million inhabitants, by WHO Region.csv"
)
REGIONAL_ODA <- read.csv(
  "~/mentalHealth/data/indicators/GHO/Official development assistance (ODA) for medical research and basic health sectors per capita, by region.csv"
)

# Reshape the first dataset to long format
gho_manual_df <- REGIONAL_Health_researchers %>%
  rename(region = Location) %>%
  pivot_longer(
    cols = -region,
    names_to = "date",
    values_to = "Health_researchers_per_million"
  ) %>%
  mutate(date = as.numeric(gsub("X", "", date)))

# Reshape the second dataset to long format
oda_df <- REGIONAL_ODA %>%
  rename(region = Location) %>%
  pivot_longer(
    cols = -region,
    names_to = "date",
    values_to = "ODA_per_capita"
  ) %>%
  mutate(date = as.numeric(gsub("X", "", date)))

# Merge both
gho_manual_df <- full_join(gho_manual_df, oda_df, by = c("region", "date")) %>%
  arrange(region, date)

# Merge into filtered_data
filtered_data <- filtered_data %>%
  left_join(gho_manual_df, by = c("region", "date"))


###############################################################################
# 8. MERGE OUR WORLD IN DATA (OWID) INDICATORS
###############################################################################
# Pull valid OWID URLs from indicators_df

valid_owid_urls <- indicators_df$OWID[!is.na(indicators_df$OWID) &
                                        indicators_df$OWID != "MANUAL"]

# Function to get and rename each dataset from OWID
get_owid_data <- function(url_no_csv_extension) {
  full_url <- paste0(url_no_csv_extension, ".csv?useColumnShortNames=true")
  df <- read_csv(full_url, show_col_types = FALSE)
  
  # Retain only Entity, Code, Year, and indicator columns
  indicator_cols <- setdiff(names(df), c("Entity", "Code", "Year", "owid_region"))
  
  # Build new column names
  prefix <- gsub("https://ourworldindata.org/grapher/", "", url_no_csv_extension)
  new_cols <- paste0(prefix, "__", indicator_cols)
  colnames(df)[colnames(df) %in% indicator_cols] <- new_cols
  
  df %>% select(Entity, Code, Year, all_of(new_cols))
}

# Download each OWID dataset and combine
owid_df_merged <- reduce(
  map(valid_owid_urls, get_owid_data),
  full_join,
  by = c("Entity", "Code", "Year")
)

# Rename columns for merging
owid_df_merged <- owid_df_merged %>%
  rename(
    country = Entity,
    date = Year
  )

owid_df_merged$country <- toupper(owid_df_merged$country)

# Adjust country names in filtered_data to match
filtered_data <- filtered_data %>%
  mutate(country = case_when(
    country == "UNITED STATES OF AMERICA" ~ "UNITED STATES",
    country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND" ~ "UNITED KINGDOM",
    country == "NETHERLANDS (KINGDOM OF THE)" ~ "NETHERLANDS",
    country == "RUSSIAN FEDERATION" ~ "RUSSIA",
    country == "IRAN (ISLAMIC REPUBLIC OF)" ~ "IRAN",
    country == "UNITED REPUBLIC OF TANZANIA" ~ "TANZANIA",
    country == "TÜRKIYE" ~ "TURKEY",
    country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)" ~ "VENEZUELA",
    country == "REPUBLIC OF KOREA" ~ "SOUTH KOREA",
    country == "VIET NAM" ~ "VIETNAM",
    country == "SYRIAN ARAB REPUBLIC" ~ "SYRIA",
    country == "REPUBLIC OF MOLDOVA" ~ "MOLDOVA",
    country == "BOLIVIA (PLURINATIONAL STATE OF)" ~ "BOLIVIA",
    country == "WEST BANK AND GAZA" ~ "PALESTINE",
    TRUE ~ country
  ))

# Merge
filtered_data <- filtered_data %>%
  left_join(owid_df_merged, by = c("country", "date"))


###############################################################################
# 9. MERGE MANUAL OWID INDICATORS
###############################################################################
# Example: Extreme poverty & Deaths from certain causes

extreme_poverty <- read.csv(
  "~/mentalHealth/data/indicators/OWID/share-of-population-living-in-extreme-poverty.csv"
)
deaths_data <- read.csv(
  "~/mentalHealth/data/indicators/OWID/deaths.csv"
)

# Rename columns
extreme_poverty <- extreme_poverty %>%
  rename(
    date = Year,
    country = Country,
    `Share-of-population-living-in-extreme-poverty` = `Share.below..2.15.a.day`
  )
deaths_data <- deaths_data %>%
  rename(
    date = Year,
    country = Entity,
    Deaths = `Deaths...Sex..all...Age..all...Variant..estimates`
  )

# Normalize to uppercase
extreme_poverty$country <- toupper(extreme_poverty$country)
deaths_data$country <- toupper(deaths_data$country)

# Correct multi-part country names in extreme_poverty if needed
correction_pov <- c(
  "ARGENTINA (URBAN)" = "ARGENTINA",
  "CHINA (RURAL)" = "CHINA",
  "CHINA (URBAN)" = "CHINA",
  "COLOMBIA (URBAN)" = "COLOMBIA",
  "BOLIVIA (URBAN)" = "BOLIVIA",
  "ECUADOR (URBAN)" = "ECUADOR",
  "HONDURAS (URBAN)" = "HONDURAS",
  "INDIA (RURAL)" = "INDIA",
  "INDIA (URBAN)" = "INDIA",
  "MICRONESIA (COUNTRY)" = "MICRONESIA",
  "MICRONESIA (COUNTRY) (URBAN)" = "MICRONESIA",
  "RWANDA (RURAL)" = "RWANDA",
  "SURINAME (URBAN)" = "SURINAME",
  "URUGUAY (URBAN)" = "URUGUAY"
)
extreme_poverty <- extreme_poverty %>%
  mutate(country = recode(country, !!!correction_pov))

# Merge into filtered_data
filtered_data <- filtered_data %>%
  left_join(extreme_poverty, by = c("date", "country")) %>%
  left_join(deaths_data, by = c("date", "country"))


###############################################################################
# 10. MERGE MANUAL WB INDICATOR: CORRUPTION PERCEPTION INDEX
###############################################################################

cpi_data <- read_excel("~/mentalHealth/data/indicators/WB/Corruption Perception Index.xlsx")

# Filter CPI scores only
cpi_data <- cpi_data %>%
  filter(Indicator == "Corruption Perceptions Index Score")

# Reshape to longer format
cpi_data <- cpi_data %>%
  pivot_longer(
    cols = `2012`:`2023`,
    names_to = "date",
    values_to = "Corruption_Perception_Index"
  )

# Drop unneeded columns
cpi_data <- cpi_data[, -c(1,3:8)]

# Rename economy column
colnames(cpi_data)[colnames(cpi_data) == "Economy Name"] <- "country"

# Normalize country names
cpi_data$country <- toupper(cpi_data$country)

# Match names with filtered_data
cpi_data <- cpi_data %>%
  mutate(country = case_when(
    country == "EGYPT, ARAB REP." ~ "EGYPT",
    country == "RUSSIAN FEDERATION" ~ "RUSSIA",
    country == "IRAN, ISLAMIC REP." ~ "IRAN",
    country == "KOREA, REP." ~ "SOUTH KOREA",
    country == "SLOVAK REPUBLIC" ~ "SLOVAKIA",
    country == "SYRIAN ARAB REPUBLIC" ~ "SYRIA",
    country == "YEMEN, REP." ~ "YEMEN",
    country == "VENEZUELA, RB" ~ "VENEZUELA",
    country == "TURKIYE" ~ "TURKEY",
    TRUE ~ country
  ))

# Convert date to numeric
cpi_data$date <- as.numeric(cpi_data$date)

# Merge with filtered_data
filtered_data <- filtered_data %>%
  left_join(cpi_data, by = c("date", "country"))


###############################################################################
# 11. MERGE MANUAL GBD INDICATOR (ALL-CAUSE DALYs)
###############################################################################

gbd_all_causes <- read.csv(
  "~/mentalHealth/data/indicators/GBD/Disability-Adjusted Life Years (DALYs) per 100,000 individuals from all causes.csv"
)

# Filter for measure = "DALYs" and metric = "Rate"
gbd_all_causes <- gbd_all_causes %>%
  filter(measure_name != "Deaths") %>%
  filter(metric_name == "Rate")

colnames(gbd_all_causes)[colnames(gbd_all_causes) == "location_name"] <- "country"
colnames(gbd_all_causes)[colnames(gbd_all_causes) == "year"] <- "date"
colnames(gbd_all_causes)[colnames(gbd_all_causes) == "val"] <-
  "DALYs_all_causes_per100k"

# Normalize country names
gbd_all_causes$country <- toupper(gbd_all_causes$country)

# Fix mismatches
gbd_corrections <- c(
  "UNITED STATES OF AMERICA" = "UNITED STATES",
  "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND" = "UNITED KINGDOM",
  "CZECH REPUBLIC" = "CZECHIA",
  "RUSSIAN FEDERATION" = "RUSSIA",
  "ISLAMIC REPUBLIC OF IRAN" = "IRAN",
  "UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
  "BOLIVARIAN REPUBLIC OF VENEZUELA" = "VENEZUELA",
  "CÔTE D'IVOIRE" = "COTE D'IVOIRE",
  "REPUBLIC OF KOREA" = "SOUTH KOREA",
  "SOCIALIST REPUBLIC OF VIET NAM" = "VIETNAM",
  "SYRIAN ARAB REPUBLIC" = "SYRIA",
  "REPUBLIC OF MOLDOVA" = "MOLDOVA",
  "PLURINATIONAL STATE OF BOLIVIA" = "BOLIVIA"
)
gbd_all_causes <- gbd_all_causes %>%
  mutate(country = recode(country, !!!gbd_corrections))

# Merge
filtered_data <- filtered_data %>%
  left_join(gbd_all_causes, by = c("date", "country"))


###############################################################################
# 12. MERGE POPULATION AND LAND AREA (WB)
###############################################################################

# -- Population --
pop_data <- wb_data(indicator = "SP.POP.TOTL")
pop_data$country <- toupper(pop_data$country)

# Fix mismatches
filtered_data <- filtered_data %>%
  mutate(country = recode(country,
                          "EGYPT" = "EGYPT, ARAB REP.",
                          "RUSSIA" = "RUSSIAN FEDERATION",
                          "IRAN" = "IRAN, ISLAMIC REP.",
                          "TURKEY" = "TURKIYE",
                          "VENEZUELA" = "VENEZUELA, RB",
                          "SLOVAKIA" = "SLOVAK REPUBLIC",
                          "SOUTH KOREA" = "KOREA, REP.",
                          "VIETNAM" = "VIET NAM",
                          "SYRIA" = "SYRIAN ARAB REPUBLIC",
                          "YEMEN" = "YEMEN, REP.",
                          "PALESTINE" = "WEST BANK AND GAZA"
  ))

colnames(pop_data)[colnames(pop_data) == "SP.POP.TOTL"] <- "Population_total"

filtered_data <- filtered_data %>%
  left_join(
    pop_data %>% select(country, date, Population_total),
    by = c("country", "date")
  )

# Remove possible leftover "Code" column if it exists
if ("Code" %in% names(filtered_data)) {
  filtered_data$Code <- NULL
}

# -- Land area --
land_area <- wb_data(indicator = "AG.LND.TOTL.K2")
land_area$country <- toupper(land_area$country)
colnames(land_area)[colnames(land_area) == "AG.LND.TOTL.K2"] <-
  "Land_area_sqkm"

filtered_data <- filtered_data %>%
  left_join(
    land_area %>% select(country, date, Land_area_sqkm),
    by = c("country", "date")
  )


###############################################################################
# 13. MERGE BIRTH RATE, DEATH RATE, MPI/HEAD, ETC.
###############################################################################

birth_rate <- wb_data(indicator = "SP.DYN.CBRT.IN")
death_rate <- wb_data(indicator = "SP.DYN.CDRT.IN")
mpi_head <- wb_data(indicator = "SI.POV.MPWB")
pop_15_64 <- wb_data(indicator = "SP.POP.1564.TO")
pop_65_plus <- wb_data(indicator = "SP.POP.65UP.TO.ZS")

# Normalize countries
birth_rate$country <- toupper(birth_rate$country)
death_rate$country <- toupper(death_rate$country)
mpi_head$country <- toupper(mpi_head$country)
pop_15_64$country <- toupper(pop_15_64$country)
pop_65_plus$country <- toupper(pop_65_plus$country)

# Rename for merging
colnames(birth_rate)[colnames(birth_rate) == "SP.DYN.CBRT.IN"] <- "Birth_rate_crude_per_1000"
colnames(death_rate)[colnames(death_rate) == "SP.DYN.CDRT.IN"] <- "Death_rate_crude_per_1000"
colnames(mpi_head)[colnames(mpi_head) == "SI.POV.MPWB"] <-
  "Multidim_poverty_headcount_ratio_WB_percent"
colnames(pop_15_64)[colnames(pop_15_64) == "SP.POP.1564.TO"] <-
  "Population_15_64_total"
colnames(pop_65_plus)[colnames(pop_65_plus) == "SP.POP.65UP.TO.ZS"] <-
  "Population_65_plus_percent"

# Merge them
filtered_data <- filtered_data %>%
  left_join(
    birth_rate %>% select(country, date, Birth_rate_crude_per_1000),
    by = c("country", "date")
  ) %>%
  left_join(
    death_rate %>% select(country, date, Death_rate_crude_per_1000),
    by = c("country", "date")
  ) %>%
  left_join(
    mpi_head %>%
      select(country, date, Multidim_poverty_headcount_ratio_WB_percent),
    by = c("country", "date")
  ) %>%
  left_join(
    pop_15_64 %>% select(country, date, Population_15_64_total),
    by = c("country", "date")
  ) %>%
  left_join(
    pop_65_plus %>% select(country, date, `Population ages 65 and above (% of total population)`),
    by = c("country", "date")
  )


###############################################################################
# 14. MERGE ADDITIONAL IHME GBD MENTAL DISORDER INDICATORS
###############################################################################
# Example includes DALYs & prevalence for mental disorders (Bipolar, Anxiety, etc.)

ihme_data <- read.csv("~/mentalHealth/data/indicators/IHME/IHME-GBD_2021_DATA-df7c11ea-1.csv")

# Modify measure_name to shorter label: "DALYs" or "Prevalence"
ihme_data_mod <- ihme_data %>%
  mutate(
    measure_name = ifelse(measure_name == "DALYs (Disability-Adjusted Life Years)", 
                          "DALYs", measure_name),
    cause_measure = paste0(cause_name, " (", measure_name, ")")
  ) %>%
  select(year, location_name, cause_measure, val)

ihme_wide <- ihme_data_mod %>%
  pivot_wider(
    names_from = cause_measure,
    values_from = val
  )

colnames(ihme_wide)[colnames(ihme_wide) == "year"] <- "date"
colnames(ihme_wide)[colnames(ihme_wide) == "location_name"] <- "country"

# Toupper
ihme_wide$country <- toupper(ihme_wide$country)

# Fix mismatches
ihme_wide <- ihme_wide %>%
  mutate(country = recode(country,
                          "UNITED STATES OF AMERICA" = "UNITED STATES",
                          "EGYPT" = "EGYPT, ARAB REP.",
                          "IRAN (ISLAMIC REPUBLIC OF)" = "IRAN, ISLAMIC REP.",
                          "UNITED REPUBLIC OF TANZANIA" = "TANZANIA",
                          "TÜRKIYE" = "TURKIYE",
                          "VENEZUELA (BOLIVARIAN REPUBLIC OF)" = "VENEZUELA, RB",
                          "SLOVAKIA" = "SLOVAK REPUBLIC",
                          "CÔTE D'IVOIRE" = "COTE D'IVOIRE",
                          "REPUBLIC OF KOREA" = "KOREA, REP.",
                          "REPUBLIC OF MOLDOVA" = "MOLDOVA",
                          "BOLIVIA (PLURINATIONAL STATE OF)" = "BOLIVIA",
                          "YEMEN" = "YEMEN, REP.",
                          "PALESTINE" = "WEST BANK AND GAZA"
  ))

# Merge
filtered_data <- filtered_data %>%
  left_join(ihme_wide, by = c("date", "country"))


###############################################################################
# 15. MERGE HDI (HUMAN DEVELOPMENT INDEX)
###############################################################################

hdi_data <- read_excel("~/mentalHealth/data/indicators/HDI.xlsx")

# Rename
colnames(hdi_data)[colnames(hdi_data) == "year"] <- "date"
colnames(hdi_data)[colnames(hdi_data) == "value"] <- "Human_Development_Index"

hdi_data$country <- toupper(hdi_data$country)

# Fix mismatches
hdi_data <- hdi_data %>%
  mutate(country = recode(country,
                          "EGYPT" = "EGYPT, ARAB REP.",
                          "IRAN (ISLAMIC REPUBLIC OF)" = "IRAN, ISLAMIC REP.",
                          "TANZANIA (UNITED REPUBLIC OF)" = "TANZANIA",
                          "TURKEY" = "TURKIYE",
                          "VENEZUELA (BOLIVARIAN REPUBLIC OF)" = "VENEZUELA, RB",
                          "SLOVAKIA" = "SLOVAK REPUBLIC",
                          "CÔTE D'IVOIRE" = "COTE D'IVOIRE",
                          "KOREA (REPUBLIC OF)" = "KOREA, REP.",
                          "MOLDOVA (REPUBLIC OF)" = "MOLDOVA",
                          "BOLIVIA (PLURINATIONAL STATE OF)" = "BOLIVIA",
                          "YEMEN" = "YEMEN, REP.",
                          "PALESTINE, STATE OF" = "WEST BANK AND GAZA"
                          # Some like "PUERTO RICO", "BERMUDA", or "KOSOVO" might not appear
  ))
hdi_data$date <- as.numeric(hdi_data$date)

filtered_data <- filtered_data %>%
  left_join(
    hdi_data %>% select(country, date, Human_Development_Index),
    by = c("country", "date")
  )


###############################################################################
# 16. FINAL CLEANUP OF filtered_data
###############################################################################
# Remove any obviously unneeded columns or rows

# Example: Removing an unused annotation column if it exists
if ("share-of-people-who-say-they-are-happy__821407-annotations" %in% names(filtered_data)) {
  filtered_data$`share-of-people-who-say-they-are-happy__821407-annotations` <- NULL
}

# Remove rows without a defined document_type
filtered_data <- filtered_data %>%
  filter(!is.na(document_type))


###############################################################################
# 17. GROUP AND SUMMARIZE BY INCOME LEVEL & YEAR
###############################################################################
# Weighted means, sums, etc.

# Helper functions
weighted_mean_min <- function(x, w, min_obs = 3) {
  if (sum(!is.na(x)) >= min_obs) {
    return(weighted.mean(x, w, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

mean_min <- function(x, min_obs = 3) {
  if (sum(!is.na(x)) >= min_obs) {
    return(mean(x, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

median_min <- function(x, min_obs = 3) {
  if (sum(!is.na(x)) >= min_obs) {
    return(median(x, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

# For ratio: sum(numerators)/sum(denominators), requiring at least min_obs
normalize_indicator <- function(numerator, denominator, min_obs = 3) {
  valid_idx <- !is.na(numerator) & !is.na(denominator)
  if (sum(valid_idx) >= min_obs) {
    sum_num <- sum(numerator[valid_idx], na.rm = TRUE)
    sum_den <- sum(denominator[valid_idx], na.rm = TRUE)
    if (sum_den == 0) {
      return(NA_real_)
    } else {
      return(sum_num / sum_den)
    }
  } else {
    return(NA_real_)
  }
}

# Indicators that get a population-weighted mean
indicators_weighted <- c(
  "Research and development expenditure (% of GDP)",
  "GDP per capita (current US$)",
  "Nurses and midwives (per 1,000 people)",
  "Physicians (per 1,000 people)",
  "Current health expenditure (% of GDP)",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Death rate, crude (per 1,000 people).x",
  "Life expectancy at birth, total (years)",
  "Population ages 65 and above (% of total population)",
  "Government expenditures on mental health as a percentage of total government expenditures on health (%)",
  "Life expectancy of women",
  "Life expectancy of men",
  "Life expectancy both sexes",
  "Mental hospital admissions (per 100,000)",
  "Outpatient visits (per 100,000)",
  "Mental health outpatient facilities (per 100,000)",
  "Mental health day treatment facilities (per 100,000)",
  "Mental health units in general hospitals admissions (per 100,000)",
  "Psychiatrists working in mental health sector (per 100,000)",
  "Nurses working in mental health sector (per 100,000)",
  "Social workers working in mental health sector (per 100,000)",
  "Beds for mental health in general hospitals (per 100,000)",
  "Beds in mental hospitals (per 100,000)",
  "Mental health units in general hospitals (per 100,000)",
  "Psychologists working in mental health sector (per 100,000)",
  "Beds in community residential facilities (per 100,000)",
  "Share of population living in extreme poverty",
  "Share of people who say they are happy",
  "Average years of schooling",
  "Literacy rate, youth total (% of people ages 15-24)",
  "Literacy rate, adult total (% of people ages 15 and above)",
  "The Universal Health Coverage (UHC) Service Coverage Index",
  "Homicide rate",
  "\`Corruption Perception Index\`",
  "Disability-Adjusted Life Years (DALYs) per 100,000 individuals from all causes",
  "Human Development Index",
  "Schizophrenia (DALYs)",
  "Bipolar disorder (DALYs)",
  "Anxiety disorders (DALYs)",
  "Eating disorders (DALYs)",
  "Schizophrenia (Prevalence)",
  "Anxiety disorders (Prevalence)",
  "Eating disorders (Prevalence)",
  "Bipolar disorder (Prevalence)"
)

# Indicators to sum
indicators_summed <- c(
  "Charges for the use of intellectual property, payments (BoP, current US$)",
  "Population, total",
  "Deaths"
)

# Indicators for simple mean
indicators_mean <- c(
  "Gini index",
  "Self-reported life satisfaction",
  "Income inequality: Atkinson index",
  "Human rights index",
  "Private civil liberties index",
  "LGBT+ legal equality index",
  "Rigorous and impartial public administration index",
  "State capacity index",
  "Functioning government index",
  "Political corruption index"
)

# Indicators weighted by land area
indicators_weighted_area <- c(
  "Percentage of territory effectively controlled by government"
)



df_income_year <- data_filtered %>%
  ungroup() %>%
  group_by(income_level, date) %>%
  summarise(
    publications = n(),
    citations = sum(TC, na.rm = TRUE),
    h_index_median = median_min(`Índice H`),
    
    # Weighted by total population
    across(
      all_of(indicators_weighted),
      ~ weighted_mean_min(.x, `Population, total`),
      .names = "{.col}"
    ),
    
    # Weighted by land area
    across(
      all_of(indicators_weighted_area),
      ~ weighted_mean_min(.x, `Land area (sq. km)`),
      .names = "{.col}"
    ),
    
    # Summed
    across(
      all_of(indicators_summed),
      sum, na.rm = TRUE, .names = "{.col}"
    ),
    
    # Simple mean
    across(
      all_of(indicators_mean),
      ~ mean_min(.x), .names = "{.col}"
    ),
    
    # Normalized
    `Child mortality rate` = normalize_indicator(
      `Child mortality rate`,
      `Birth rate, crude (per 1,000 people)`
    ) * 1000,
    
    `Lifespan inequality: Gini coefficient in women` = normalize_indicator(
      `Lifespan inequality: Gini coefficient in women`,
      `Death rate, crude (per 1,000 people).x`
    ),
    
    `Lifespan inequality: Gini coefficient in men` = normalize_indicator(
      `Lifespan inequality: Gini coefficient in men`,
      `Death rate, crude (per 1,000 people).x`
    ),
    
    `Multidimensional Poverty Index (MPI)` = normalize_indicator(
      `Multidimensional Poverty Index (MPI)`,
      `Multidimensional poverty headcount ratio (World Bank) (% of population)`
    ),
    
    `Share of population with no formal education` = normalize_indicator(
      `Share of population with no formal education`,
      `Population ages 15-64, total`
    ),
    
    .groups = "drop"
  )


