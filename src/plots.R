###############################################################################
# 0. LOAD REQUIRED LIBRARIES
###############################################################################
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(tidyr)       # For data manipulation
library(scales)      # For scaling axis labels
library(ggplot2)     # For data visualization
library(ggrepel)     # For enhanced text labels in ggplot2
library(ggbreak)     # For breaking scales in ggplot2

###############################################################################
# 1. LOAD DATA
###############################################################################
# Load data from Excel sheets
step_1 <- read_excel("~/Supplementary Material 1.xlsx", sheet = "Global Health")
step_2 <- read_excel("~/Supplementary Material 2.xlsx", sheet = "Global Health")
step_3 <- read_excel("~/Supplementary Material 3.xlsx", sheet = "Global Health")
moderators_income <- read_excel("~/moderators_income.xlsx")

###############################################################################
# 2. PLOT LIFE EXPECTANCY
###############################################################################
# Prepare data for life expectancy
indicators_of_interest <- c(
  "Life expectancy at birth, total (years)",
  "Life expectancy both sexes",
  "Life expectancy of men",
  "Life expectancy of women"
)

# Abbreviations for indicators
indicator_labels <- c(
  "Life expectancy at birth, total (years)" = "LE at birth, total (years)",
  "Life expectancy both sexes" = "LE both sexes",
  "Life expectancy of men" = "LE of men",
  "Life expectancy of women" = "LE of women"
)

data_plot <- step_1 %>%
  filter(indicator %in% indicators_of_interest) %>%
  mutate(
    income_level = factor(income_level,
                          levels = c("High income", "Upper middle income", "Lower middle income", "Low income"),
                          labels = c("HIC", "UMIC", "LMIC", "LIC")),
    indicator = recode(indicator, !!!indicator_labels),
    indicator = factor(indicator, levels = rev(indicator_labels))
  )

# Calculate averages
means_group <- data_plot %>%
  group_by(income_level) %>%
  summarise(mean_beta = mean(beta_1, na.rm = TRUE)) %>%
  ungroup()

# Create the plot
p <- ggplot(data_plot, aes(x = beta_1, y = income_level, color = income_level)) +
  geom_linerange(aes(xmin = 0, xmax = beta_1), size = 0.8, alpha = 0.6) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  scale_x_log10() +
  facet_wrap(~ indicator, scales = "free_x", ncol = 1) +
  scale_color_manual(values = c("HIC" = "#E41A1C", "UMIC" = "#984EA3", "LMIC" = "#4DAF4A", "LIC" = "#377EB8")) +
  labs(x = "β coefficient", y = "Income group", color = "Income group") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines")
  )
print(p)

###############################################################################
# 3. PLOT CAUSES OF DEATH
###############################################################################
# Reorder income_level
step_1$income_level <- recode(step_1$income_level,
                              "High income" = "HIC",
                              "Upper middle income" = "UMIC",
                              "Lower middle income" = "LMIC",
                              "Low income" = "LIC")
step_1$income_level <- factor(step_1$income_level, levels = c("HIC", "UMIC", "LMIC", "LIC"))

# Create bar plot for causes of death
ggplot(step_1 %>% filter(indicator == "Deaths"), aes(x = income_level, y = beta_1, fill = p_value < 0.05)) +
  geom_bar(stat = "identity", width = 0.8, color = "white", show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(aes(label = paste0(round(beta_1 / 1e6, 1), "M"), y = beta_1 + 5e5), size = 2, color = "black") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("TRUE" = "#d62728", "FALSE" = "gray60")) +
  labs(y = expression("Regression Coefficient (" * beta[1] * ")"), x = NULL) +
  theme_minimal(base_size = 7.5) +
  theme(
    text = element_text(family = "Roboto"),
    axis.text = element_text(size = 6.5),
    axis.title = element_text(size = 7.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    plot.margin = margin(5, 10, 5, 5) # margin to the right
  )

###############################################################################
# 4. META-REGRESSION PLOTS
###############################################################################
##### 4.1 Life Expectancy (Both Sexes)
# Prepare the data
data_plot_life <- step_1 %>%
  filter(indicator == "Life expectancy both sexes") %>%
  mutate(
    income_label = recode(income_level,
                          "HIC" = "High income",
                          "UMIC" = "Upper middle income",
                          "LMIC" = "Lower middle income",
                          "LIC" = "Low income"),
    income_abbr = recode(income_level,
                         "HIC" = "HIC",
                         "UMIC" = "UMIC",
                         "LMIC" = "LMIC",
                         "LIC" = "LIC")
  ) %>%
  left_join(
    moderators_income %>%
      select(
        income_label = income_level,
        literacy_rate = `Literacy rate, adult total (% of people ages 15 and above)`
      ),
    by = "income_label"
  )

# Normalize (Z-score) Literacy Rate
data_plot_life <- data_plot_life %>%
  mutate(
    literacy_rate_z = (literacy_rate - mean(literacy_rate, na.rm = TRUE)) / sd(literacy_rate, na.rm = TRUE)
  )

# Create the plot
ggplot(data_plot_life, aes(x = literacy_rate_z, y = beta_1)) +
  geom_smooth(method = "lm", se = TRUE, color = "#377EB8", fill = "#377EB8", alpha = 0.1, linewidth = 0.5) +
  geom_point(aes(fill = income_abbr), size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = income_abbr), size = 3, min.segment.length = 0, box.padding = 0.4, show.legend = FALSE) +
  scale_fill_manual(
    name = "Income Group",
    values = c("HIC" = "#E41A1C", "UMIC" = "#377EB8", "LMIC" = "#4DAF4A", "LIC" = "#984EA3"),
    labels = c("HIC", "UMIC", "LMIC", "LIC")
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(x = "Adult Literacy Rate (Z-score)", y = expression("Regression Coefficient (" * beta[1] * ")")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4),
    plot.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

##### 4.2 Nurses and Midwives
# Prepare data
data_plot_nurses <- step_1 %>%
  filter(indicator == "Nurses and midwives (per 1,000 people)") %>%
  mutate(
    income_label = recode(income_level,
                          "HIC" = "High income",
                          "UMIC" = "Upper middle income",
                          "LMIC" = "Lower middle income",
                          "LIC" = "Low income"),
    income_abbr = recode(income_level,
                         "HIC" = "HIC",
                         "UMIC" = "UMIC",
                         "LMIC" = "LMIC",
                         "LIC" = "LIC")
  ) %>%
  left_join(
    moderators_income %>%
      select(
        income_label = income_level,
        political_corruption = `Political corruption index`
      ),
    by = "income_label"
  )

# Normalize (Z-score) Political Corruption
data_plot_nurses <- data_plot_nurses %>%
  mutate(
    political_corruption_z = (political_corruption - mean(political_corruption, na.rm = TRUE)) / sd(political_corruption, na.rm = TRUE)
  )

# Create the plot
ggplot(data_plot_nurses, aes(x = political_corruption_z, y = beta_1)) +
  geom_smooth(method = "lm", se = TRUE, color = "#E41A1C", fill = "#E41A1C", alpha = 0.1, linewidth = 0.5) +
  geom_point(aes(fill = income_abbr), size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = income_abbr), size = 3, min.segment.length = 0, box.padding = 0.4, show.legend = FALSE) +
  scale_fill_manual(
    name = "Income Group",
    values = c("HIC" = "#E41A1C", "UMIC" = "#377EB8", "LMIC" = "#4DAF4A", "LIC" = "#984EA3"),
    labels = c("HIC", "UMIC", "LMIC", "LIC")
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x = "Political Corruption Index (Z-score)", y = expression("Regression Coefficient (" * beta[1] * ")")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4),
    plot.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

##### 4.3 Physicians Density
# Prepare the data
data_plot_physicians <- step_1 %>%
  filter(indicator == "Physicians (per 1,000 people)") %>%
  mutate(
    income_label = recode(income_level,
                          "HIC" = "High income",
                          "UMIC" = "Upper middle income",
                          "LMIC" = "Lower middle income",
                          "LIC" = "Low income"),
    income_abbr = recode(income_level,
                         "HIC" = "HIC",
                         "UMIC" = "UMIC",
                         "LMIC" = "LMIC",
                         "LIC" = "LIC")
  ) %>%
  left_join(
    moderators_income %>%
      select(
        income_label = income_level,
        health_expenditure = `Current health expenditure (% of GDP)`
      ),
    by = "income_label"
  )

# Normalize (Z-score) Health Expenditure
data_plot_physicians <- data_plot_physicians %>%
  mutate(
    health_expenditure_z = (health_expenditure - mean(health_expenditure, na.rm = TRUE)) / sd(health_expenditure, na.rm = TRUE)
  )

# Create the plot
ggplot(data_plot_physicians, aes(x = health_expenditure_z, y = beta_1)) +
  geom_smooth(method = "lm", se = TRUE, color = "#377EB8", fill = "#377EB8", alpha = 0.1, linewidth = 0.5) +
  geom_point(aes(fill = income_abbr), size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = income_abbr), size = 3, min.segment.length = 0, box.padding = 0.4, show.legend = FALSE) +
  scale_fill_manual(
    name = "Income Group",
    values = c("HIC" = "#E41A1C", "UMIC" = "#377EB8", "LMIC" = "#4DAF4A", "LIC" = "#984EA3"),
    labels = c("HIC", "UMIC", "LMIC", "LIC")
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x = "CHE (% of GDP) (Z-score)", y = expression("Regression Coefficient (" * beta[1] * ")")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4),
    plot.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

##### 4.4 Universal Health Coverage (UHC) Service Coverage Index
# Prepare the data
data_plot_uhc <- step_1 %>%
  filter(indicator == "The Universal Health Coverage (UHC) Service Coverage Index") %>%
  mutate(
    income_label = recode(income_level,
                          "HIC" = "High income",
                          "UMIC" = "Upper middle income",
                          "LMIC" = "Lower middle income",
                          "LIC" = "Low income"),
    income_abbr = recode(income_level,
                         "HIC" = "HIC",
                         "UMIC" = "UMIC",
                         "LMIC" = "LMIC",
                         "LIC" = "LIC")
  ) %>%
  left_join(
    moderators_income %>%
      select(
        income_label = income_level,
        territory_control = `Percentage of territory effectively controlled by government`
      ),
    by = "income_label"
  )

# Normalize (Z-score) Territory Control
data_plot_uhc <- data_plot_uhc %>%
  mutate(
    territory_control_z = (territory_control - mean(territory_control, na.rm = TRUE)) / sd(territory_control, na.rm = TRUE)
  )

# Create the plot
ggplot(data_plot_uhc, aes(x = territory_control_z, y = beta_1)) +
  geom_smooth(method = "lm", se = TRUE, color = "#4DAF4A", fill = "#4DAF4A", alpha = 0.1, linewidth = 0.5) +
  geom_point(aes(fill = income_abbr), size = 3, shape = 21, color = "black", stroke = 0.5) +
  geom_text_repel(aes(label = income_abbr), size = 3, min.segment.length = 0, box.padding = 0.4, show.legend = FALSE) +
  scale_fill_manual(
    name = "Income Group",
    values = c("HIC" = "#E41A1C", "UMIC" = "#377EB8", "LMIC" = "#4DAF4A", "LIC" = "#984EA3"),
    labels = c("HIC", "UMIC", "LMIC", "LIC")
  ) +
  labs(x = "TEC (%) (Z-score)", y = expression("Regression Coefficient (" * beta[1] * ")")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4),
    plot.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
