################################################################################
# 1. LIBRARIES
################################################################################

# Only called here, as requested
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("readr")) install.packages("readr")
library(readr)
if (!require("stringr")) install.packages("stringr")
library(stringr)
if (!require("httr")) install.packages("httr")
library(httr)
if (!require("jsonlite")) install.packages("jsonlite")
library(jsonlite)
if (!require("ggsci")) install.packages("ggsci")
library(ggsci)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("igraph")) install.packages("igraph")
library(igraph)
if (!require("wbstats")) install.packages("wbstats")
library(wbstats)
if (!require("rvest")) install.packages("r")
library(rvest)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)
if (!require("purrr")) install.packages("purrr")
library(purrr)
if (!require("pheatmap")) install.packages("pheatmap")
library(pheatmap)
if (!require("writexl")) install.packages("writexl")
library(writexl)
if (!require("ggbreak")) install.packages("ggbreak")
library(ggbreak)
if (!require("metafor")) install.packages("metafor")
library(metafor)
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)

################################################################################
# 2. GROUPING INDICATORS
################################################################################

# 2.1. Economic development & education indicators
economy_development_education <- c(
  "Current health expenditure (% of GDP)",
  "GDP per capita (current US$)",
  "Human Development Index",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Research and development expenditure (% of GDP)",
  "Charges for the use of intellectual property, payments (BoP, current US$",
  "Average years of schooling",
  "Literacy rate, adult total (% of people ages 15 and above)",
  "Literacy rate, youth total (% of people ages 15-24)",
  "Share of population with no formal education"
)
IDs <- LETTERS[1:length(economy_development_education)]
economy_development_education <- data.frame(
  ID = IDs,
  Indicator = economy_development_education,
  Role = rep("i", length(economy_development_education))
)

# 2.2. General health indicators
general_health <- c(
  "Disability-Adjusted Life Years (DALYs)",
  "Homicide rate",
  "Life expectancy at birth, total (years)",
  "Life expectancy both sexes",
  "Life expectancy of men",
  "Life expectancy of women",
  "Nurses and midwives (per 1,000 people)",
  "Physicians (per 1,000 people)",
  "The Universal Health Coverage (UHC) Service Coverage Index",
  "Child mortality rate",
  "Deaths"
)
IDs <- LETTERS[1:length(general_health)]
general_health <- data.frame(
  ID = IDs,
  Indicator = general_health,
  Role = c(
    "d", "d", "d", "d", "d", "d", "i", "i", "d", "d", "d"
  )
)

# 2.3. Mental health & well-being indicators
mental_health_well_being <- c(
  "Anxiety disorders (DALYs)",
  "Anxiety disorders (Prevalence)",
  "Beds for mental health in general hospitals (per 100,000)",
  "Beds in community residential facilities (per 100,000)",
  "Beds in mental hospitals (per 100,000)",
  "Bipolar disorder (DALYs)",
  "Bipolar disorder (Prevalence)",
  "Eating disorders (DALYs)",
  "Eating disorders (Prevalence)",
  "Government expenditures on mental health (% of health expenditures)",
  "Mental health day treatment facilities (per 100,000)",
  "Mental health outpatient facilities (per 100,000)",
  "Mental health units in general hospitals (per 100,000)",
  "Mental health units in general hospitals admissions (per 100,000)",
  "Mental hospital admissions (per 100,000)",
  "Nurses in mental health sector (per 100,000)",
  "Outpatient visits (per 100,000)",
  "Psychiatrists in mental health sector (per 100,000)",
  "Psychologists in mental health sector (per 100,000)",
  "Schizophrenia (DALYs)",
  "Schizophrenia (Prevalence)",
  "Social workers in mental health sector (per 100,000)",
  "Self-reported life satisfaction",
  "Share of people who say they are happy"
)
IDs <- LETTERS[1:length(mental_health_well_being)]
mental_health_well_being <- data.frame(
  ID = IDs,
  Indicator = mental_health_well_being,
  Role = c(
    "d", "d", "d", "d", "d", "d", "d", "d", "d",
    "i", "d", "d", "d", "d", "d", "i", "d", "i",
    "i", "d", "d", "d", "d", "d"
  )
)

# 2.4. Inequality & poverty indicators
inequality_poverty <- c(
  "Share of population living in extreme poverty",
  "Gini index",
  "Income inequality: Atkinson index",
  "Lifespan inequality: Gini coefficient in men",
  "Lifes inequality: Gini coefficient in women",
  "Multidimensional Poverty Index (MPI)"
)
IDs <- LETTERS[1:length(inequality_poverty)]
inequality_poverty <- data.frame(
  ID = IDs,
  Indicator = inequality_poverty,
  Role = rep("i", length(inequality_poverty))
)

# 2.5. Governance indicators
governance <- c(
  "Functioning government index",
  "Human rights index",
  "LGBT+ legal equality index",
  "Political corruption index",
  "Private civil liberties index",
  "Rigorous and impartial public administration index",
  "State capacity index",
  "Corruption Perception Index",
  "Percentage of territory effectively controlled by government"
)
IDs <- LETTERS[1:length(governance)]
governance <- data.frame(
  ID = IDs,
  Indicator = governance,
  Role = rep("i", length(governance))
)

################################################################################
# 3. REGRESSION MODELS
################################################################################

# 3.1. Auxiliary function (no changes)
extract_model_info <- function(model) {
  sm <- summary(model)
  cf <- sm$coefficients
  f  <- sm$fstatistic
  pF <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  data.frame(
    beta_1              = cf[2, 1],
    intercept           = cf[1, 1],
    p_value             = cf[2, 4],
    std_error_beta1     = cf[2, 2],
    residual_std_error  = sm$sigma,
    t_value             = cf[2, 3],
    r_squared           = sm$r.squared,
    adjusted_r_squared  = sm$adj.r.squared,
    f_statistic         = f[1],
    p_value_f           = pF,
    DF_residual         = sm$df[2]
  )
}

# 3.2. List of data frames with indicators
df_list <- list(
  economy_development_education = economy_development_education,
  general_health                = general_health,
  mental_health_well_being      = mental_health_well_being,
  inequality_poverty            = inequality_poverty,
  governance                    = governance
)

# 3.3. Scientometric variables
scientometric_vars <- c("publications", "citations", "h_index_median")

# 3.4. Main loop – using income_level
for (sciento_var in scientometric_vars) {
  for (df_name in names(df_list)) {
    indicators_df <- df_list[[df_name]]
    results       <- data.frame()
    # Groups of income
    all_levels <- unique(df_income_year$income_level)
    for (lvl in all_levels) {
      df_subset <- subset(df_income_year, income_level == lvl)
      for (row_i in seq_len(nrow(indicators_df))) {
        indicator_name <- indicators_df$Indicator[row_i]
        role           <- indicators_df$Role[row_i]
        indicator_id   <- indicators_df$ID[row_i]
        if (!(indicator_name %in% names(df_subset))) next
        # Build formula according to role
        formula <- if (role == "d") {
          as.formula(sprintf("`%s` ~ `%s`", indicator_name, sciento_var))
        } else {
          as.formula(sprintf("`%s` ~ `%s`", sciento_var, indicator_name))
        }
        # Prepare a blank template with NA
        model_info <- data.frame(
          beta_1 = NA, beta_1_standardized = NA, intercept = NA,
          p_value = NA, std_error_beta1 = NA, residual_std_error = NA,
          t_value = NA, r_squared = NA, adjusted_r_squared = NA,
          f_statistic = NA, p_value_f = NA, DF_residual = NA
        )
        # Fit model only if ≥ 3 complete cases
        if (sum(complete.cases(df_subset[, c(indicator_name, sciento_var)])) >= 3) {
          model_raw   <- lm(formula, data = df_subset)
          model_info  <- extract_model_info(model_raw)
          # Manually standardize
          tmp <- df_subset[, c(indicator_name, sciento_var)]
          tmp <- scale(tmp[complete.cases(tmp), ])
          colnames(tmp) <- c("y", "x")
          beta_std <- coef(lm(y ~ x, data = as.data.frame(tmp)))[2]
          model_info$beta_1_standardized <- beta_std
        }
        # Add metadata
        model_info$income_level    <- lvl
        model_info$ID              <- indicator_id
        model_info$indicator       <- indicator_name
        model_info$indicator_role  <- role
        # Reorder columns
        model_info <- model_info[, c(
          "income_level", "ID", "indicator", "indicator_role",
          "beta_1", "beta_1_standardized", "intercept", "p_value",
          "std_error_beta1", "residual_std_error", "t_value",
          "r_squared", "adjusted_r_squared", "f_statistic",
          "p_value_f", "DF_residual"
        )]
        results <- rbind(results, model_info)
      }
    }
    # Add significance stars
    results$p_stars <- cut(
      results$p_value,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", "")
    )
    # Place p_stars column right after p_value
    idx <- match("p_value", names(results))
    results <- cbind(
      results[, 1:idx, drop = FALSE],
      p_stars = results$p_stars,
      results[, (idx + 1):(ncol(results) - 1), drop = FALSE]
    )
    # Save data frame under a unique name
    assign(paste(sciento_var, df_name, "income", sep = "."), results)
  }
}

################################################################################
# 4. HEATMAP OF STANDARDIZED COEFFICIENTS
################################################################################

plot_heatmap_beta <- function(df_result,
                              main_title = NA,
                              number_fontsize = 9) {
  
  df <- df_result %>%
    mutate(ID_label = ifelse(indicator_role == "i",
                             paste0(ID, "*"), ID))
  
  heat_data <- df %>%
    select(ID_label, income_level, beta_1_standardized) %>%
    pivot_wider(names_from = income_level,
                values_from = beta_1_standardized) %>%
    column_to_rownames("ID_label")
  
  stars_matrix <- df %>%
    select(ID_label, income_level, p_stars) %>%
    pivot_wider(names_from = income_level,
                values_from = p_stars) %>%
    column_to_rownames("ID_label")
  
  keep <- rowSums(!is.na(heat_data)) > 0
  heat_data   <- heat_data[keep, ]
  stars_matrix <- stars_matrix[keep, ]
  
  heat_data[is.na(heat_data)] <- 0
  heat_data <- heat_data[order(-rowMeans(heat_data)), ]
  stars_matrix <- stars_matrix[rownames(heat_data), ]
  stars_matrix[is.na(stars_matrix)] <- ""
  
  # Abbreviated labels for income groups
  col_labels <- colnames(heat_data)
  label_map <- c(
    "High income" = "HIC",
    "Upper middle income" = "UMIC",
    "Lower middle income" = "LMIC",
    "Low income" = "LIC"
  )
  new_labels <- label_map[col_labels]
  
  # Final plot without dendrograms
  pheatmap(
    heat_data,
    cluster_rows     = FALSE,
    cluster_cols     = FALSE,
    scale            = "none",
    display_numbers  = stars_matrix,
    number_color     = "black",
    fontsize_number  = number_fontsize,
    color            = colorRampPalette(c("blue", "white", "red"))(100),
    border_color     = "0",
    breaks           = seq(-1, 1, length.out = 100),
    main             = main_title,
    labels_col       = new_labels
  )
}

# Example usage
plot_heatmap_beta(publications.mental_health_well_being.income,
                  main_title = NA,
                  number_fontsize = 10)

################################################################################
# 5. META-ANALYSIS
################################################################################

# Vector of the original dataframe names
df_names <- c("publications.economy_development_education.income",
              "publications.general_health.income",
              "publications.governance.income",
              "publications.inequality_poverty.income",
              "publications.mental_health_well_being.income")

# Function to run meta-analysis for each indicator
run_meta_analysis <- function(df) {
  indicators <- unique(df$indicator)
  result_list <- lapply(indicators, function(ind) {
    sub_df <- df[df$indicator == ind, ]
    # Remove rows with NA in beta or std error
    sub_df <- sub_df[!is.na(sub_df$beta_1) & !is.na(sub_df$std_error_beta1), ]
    if (nrow(sub_df) < 2) {
      return(data.frame(
        indicator = ind, k = nrow(sub_df),
        beta_1_meta = NA, ci_lower = NA, ci_upper = NA, p_value = NA,
        p_stars = NA, tau2 = NA, I2 = NA, Q = NA, Q_p_value = NA
      ))
    }
    tryCatch({
      meta <- rma(yi = beta_1, sei = std_error_beta1, data = sub_df, method = "REML")
      p_val <- meta$pval
      p_stars <- ifelse(is.na(p_val), NA,
                        ifelse(p_val < 0.001, "***",
                               ifelse(p_val < 0.01, "**",
                                      ifelse(p_val < 0.05, "*", ""))))
      data.frame(
        indicator = ind,
        k = meta$k,
        beta_1_meta = meta$b[1],
        ci_lower = meta$ci.lb,
        ci_upper = meta$ci.ub,
        p_value = p_val,
        p_stars = p_stars,
        tau2 = meta$tau2,
        I2 = meta$I2,
        Q = meta$QE,
        Q_p_value = meta$QEp
      )
    }, error = function(e) {
      data.frame(
        indicator = ind, k = nrow(sub_df),
        beta_1_meta = NA, ci_lower = NA, ci_upper = NA, p_value = NA,
        p_stars = NA, tau2 = NA, I2 = NA, Q = NA, Q_p_value = NA
      )
    })
  })
  do.call(rbind, result_list)
}

# Loop over all dataframes and create .meta versions
for (df_name in df_names) {
  df <- get(df_name)
  meta_df <- run_meta_analysis(df)
  assign(paste0(df_name, ".meta"), meta_df)
}

################################################################################
# 6. META REGRESSION ANALYSIS
################################################################################

# 6.1. Structural moderators

## 6.1A. Moderators from 'data_filtered'
open_access_terms <- c("GOLD OPEN ACCESS", "BRONZE OPEN ACCESS",
                       "GREEN OPEN ACCESS", "HYBRID GOLD OPEN ACCESS")

is_open_access <- function(x) {
  if (is.na(x)) FALSE else any(grepl(paste(open_access_terms, collapse = "|"), x))
}

mods_1 <- data_filtered %>%
  mutate(open_access = sapply(OA, is_open_access)) %>%
  group_by(income_level) %>%
  summarise(
    citations               = mean(TC, na.rm = TRUE),
    h_index_median          = median(`Índice H`, na.rm = TRUE),
    iqr_h_index             = IQR(`Índice H`, na.rm = TRUE),
    avg_citations_per_paper = mean(TC, na.rm = TRUE),
    proportion_Q1           = mean(Cuartil == "Q1", na.rm = TRUE),
    proportion_Q2           = mean(Cuartil == "Q2", na.rm = TRUE),
    proportion_Q3           = mean(Cuartil == "Q3", na.rm = TRUE),
    proportion_Q4           = mean(Cuartil == "Q4", na.rm = TRUE),
    proportion_article      = mean(DT == "ARTICLE",   na.rm = TRUE),
    proportion_review       = mean(DT == "REVIEW",    na.rm = TRUE),
    proportion_editorial    = mean(DT == "EDITORIAL", na.rm = TRUE),
    proportion_letter       = mean(DT == "LETTER",    na.rm = TRUE),
    proportion_note         = mean(DT == "NOTE",      na.rm = TRUE),
    proportion_short_survey = mean(DT == "SHORT SURVEY", na.rm = TRUE),
    proportion_book         = mean(DT == "BOOK",      na.rm = TRUE),
    proportion_data_paper   = mean(DT == "DATA PAPER",na.rm = TRUE),
    proportion_open_access  = mean(open_access,       na.rm = TRUE)
  )

## 6.1B. Macro-social moderators in 'df_income_year'
extra_cols <- c(
  "Current health expenditure (% of GDP)",
  "GDP per capita (current US$)",
  "Human Development Index",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Research and development expenditure (% of GDP)",
  "Charges for the use of intellectual property, payments (BoP, current US$)",
  "Average years of schooling",
  "Literacy rate, adult total (% of people ages 15 and above)",
  "Literacy rate, youth total (% of people ages 15-24)",
  "Share of population with no formal education",
  "Share of population living in extreme poverty",
  "Gini index",
  "Income inequality: Atkinson index",
  "Lifespan inequality: Gini coefficient in men",
  "Lifespan inequality: Gini coefficient in women",
  "Multidimensional Poverty Index (MPI)",
  "Functioning government index",
  "Human rights index",
  "LGBT+ legal equality index",
  "Political corruption index",
  "Private civil liberties index",
  "Rigorous and impartial public administration index",
  "State capacity index",
  "`Corruption Perception Index`",
  "Percentage of territory effectively controlled by government"
)
mods_2 <- df_income_year %>%
  group_by(income_level) %>%
  summarise(across(all_of(extra_cols), \(x) mean(x, na.rm = TRUE)))

## 6.1C. Combine both sets of moderators
moderators_income <- mods_1 %>%
  left_join(mods_2, by = "income_level")
moderator_vars <- setdiff(names(moderators_income), "income_level")

# 6.2. Simple REML wrapper
safe_reml <- function(formula, sei, data) {
  tryCatch(
    rma(formula, sei = sei, data = data, method = "REML",
        control = list(stepadj = 0.5, maxiter = 1000)),
    error = function(e) NA
  )
}

# 6.3. Univariate meta-regressions (REML)
run_meta_regressions <- function(df_name, mods_df, moderators) {
  raw_df <- get(df_name, envir = .GlobalEnv)
  df_mod <- raw_df %>% left_join(mods_df, by = "income_level")
  
  bind_rows(lapply(unique(df_mod$indicator), function(ind) {
    ind_df <- df_mod %>% filter(indicator == ind,
                                !is.na(beta_1),
                                !is.na(std_error_beta1))
    if (nrow(ind_df) < 3)
      return(tibble(indicator = ind, moderator = NA_character_, k = nrow(ind_df)))
    
    bind_rows(lapply(moderators, function(mod) {
      df_m <- ind_df %>% filter(!is.na(.data[[mod]]))
      if (nrow(df_m) < 3)
        return(tibble(indicator = ind, moderator = mod, k = nrow(df_m)))
      
      df_m <- df_m %>% mutate(yi = beta_1)
      clean_mod  <- gsub("`", "", mod)  # Remove backticks
      fmla <- as.formula(paste0("yi ~ `", clean_mod, "`"))
      
      model <- safe_reml(fmla, sei = df_m$std_error_beta1, data = df_m)
      if (is.na(model)[1])
        return(tibble(indicator = ind, moderator = mod, k = nrow(df_m)))
      
      tibble(
        indicator  = ind,
        moderator  = mod,
        k          = model$k,
        beta_meta  = model$b[1,1],
        se         = model$se[1],
        ci_lower   = model$ci.lb[1],
        ci_upper   = model$ci.ub[1],
        p_value    = model$pval[1],
        I2         = model$I2,
        beta_mod   = model$b[2,1],
        p_mod      = model$pval[2]
      )
    }))
  }))
}

# 6.4. Execute and overwrite the five *.meta_reg dataframes
df_names <- c(
  "publications.economy_development_education.income",
  "publications.general_health.income",
  "publications.governance.income",
  "publications.inequality_poverty.income",
  "publications.mental_health_well_being.income"
)

for (nm in df_names) {
  assign(paste0(nm, ".meta_reg"),
         run_meta_regressions(nm, moderators_income, moderator_vars),
         envir = .GlobalEnv)
}

# 6.5. Quick check (optional)
head(publications.general_health.income.meta_reg)





################################################################################
# 7. META REGRESSION PLOTS
################################################################################




