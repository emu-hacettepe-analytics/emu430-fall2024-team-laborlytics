# EMU430 Data Analytics Project
# Labor Market Analysis - Setup

library(tidyverse)
library(readr)
library(lubridate)
library(scales)
library(knitr)


labor_data <- read_csv(
  "labor_final_clean.csv",
  locale = locale(encoding = "UTF-8")
)

# Check object names loaded from the RData file
ls()

# 3. Fix data types
labor_data <- labor_data |>
  mutate(
    date = as.Date(date),
    year = as.integer(year),
    month_no = as.integer(month_no),
    gender = factor(gender, levels = c("Total", "Male", "Female")),
    month_en = factor(
      month_en,
      levels = c(
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"
      )
    )
  )

# 4. Create useful subsets
total_data <- labor_data |>
  filter(gender == "Total")

gender_data <- labor_data |>
  filter(gender %in% c("Male", "Female"))

# 5. Yearly summary for total population
yearly_total <- total_data |>
  group_by(year) |>
  summarize(
    avg_unemp_rate = mean(unemp_rate, na.rm = TRUE),
    avg_emp_rate = mean(emp_rate, na.rm = TRUE),
    avg_lfp_rate = mean(lfp_rate, na.rm = TRUE),
    avg_employment = mean(employment, na.rm = TRUE),
    avg_unemployment = mean(unemployment, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Yearly summary by gender
yearly_gender <- gender_data |>
  group_by(year, gender) |>
  summarize(
    avg_unemp_rate = mean(unemp_rate, na.rm = TRUE),
    avg_emp_rate = mean(emp_rate, na.rm = TRUE),
    avg_lfp_rate = mean(lfp_rate, na.rm = TRUE),
    avg_employment = mean(employment, na.rm = TRUE),
    avg_unemployment = mean(unemployment, na.rm = TRUE),
    .groups = "drop"
  )

# 7. Basic data checks
data_check <- labor_data |>
  summarize(
    number_of_rows = n(),
    number_of_columns = ncol(labor_data),
    first_date = min(date),
    last_date = max(date),
    number_of_genders = n_distinct(gender),
    missing_values = sum(is.na(labor_data))
  )

print(data_check)

# 8. Variable dictionary
variable_dictionary <- tibble(
  variable = c(
    "year", "month_no", "month_en", "date", "gender",
    "pop_15_plus", "labor_force", "employment", "unemployment",
    "not_in_labor_force", "lfp_rate", "emp_rate", "unemp_rate"
  ),
  meaning = c(
    "Year of observation",
    "Month number",
    "Month name in English",
    "Monthly date",
    "Gender group: Total, Male, Female",
    "Population aged 15 and over, in thousand persons",
    "Labor force, in thousand persons",
    "Employed population, in thousand persons",
    "Unemployed population, in thousand persons",
    "Population not in labor force, in thousand persons",
    "Labor force participation rate (%)",
    "Employment rate (%)",
    "Unemployment rate (%)"
  )
)

print(variable_dictionary)

# ============================================================
# Analysis 1: General Trend Analysis
# Cleaner Faceted Version
# ============================================================

# Create Total data
total_data <- labor_data |>
  filter(gender == "Total")

# Create yearly averages
yearly_total <- total_data |>
  group_by(year) |>
  summarize(
    `Unemployment Rate` = mean(unemp_rate, na.rm = TRUE),
    `Employment Rate` = mean(emp_rate, na.rm = TRUE),
    `Labor Force Participation Rate` = mean(lfp_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Convert wide data to long format for ggplot
yearly_total_long <- yearly_total |>
  pivot_longer(
    cols = c(`Unemployment Rate`, `Employment Rate`, `Labor Force Participation Rate`),
    names_to = "indicator",
    values_to = "rate"
  )

# Plot
ggplot(yearly_total_long, aes(x = year, y = rate)) +
  geom_line(linewidth = 1.1, color = "#2C5F8A") +
  geom_point(size = 2.4, color = "#2C5F8A") +
  facet_wrap(~ indicator, scales = "free_y", ncol = 1) +
  labs(
    title = "Yearly Average Labor Market Rates in Turkey",
    subtitle = "Annual averages calculated from monthly observations, 2005–2026",
    x = "Year",
    y = "Average Rate (%)"
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2026, by = 3)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# ============================================================
# Analysis 2: Gender Comparison
# ============================================================

# Create gender-level data
gender_data <- labor_data |>
  filter(gender %in% c("Male", "Female"))

# Yearly averages by gender
yearly_gender <- gender_data |>
  group_by(year, gender) |>
  summarize(
    `Unemployment Rate` = mean(unemp_rate, na.rm = TRUE),
    `Employment Rate` = mean(emp_rate, na.rm = TRUE),
    `Labor Force Participation Rate` = mean(lfp_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to long format
yearly_gender_long <- yearly_gender |>
  pivot_longer(
    cols = c(`Unemployment Rate`, `Employment Rate`, `Labor Force Participation Rate`),
    names_to = "indicator",
    values_to = "rate"
  )

# Plot gender comparison
ggplot(yearly_gender_long, aes(x = year, y = rate, color = gender)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 1) +
  labs(
    title = "Gender-Based Labor Market Comparison in Turkey",
    subtitle = "Yearly averages by gender, 2005–2026",
    x = "Year",
    y = "Average Rate (%)",
    color = "Gender"
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2026, by = 3)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Analysis 3: Economic & Political Period Analysis

total_data <- labor_data |>
  filter(gender == "Total")

ggplot(total_data, aes(x = date, y = unemp_rate)) +
  
  # 2008-2009 Global Crisis
  annotate(
    "rect",
    xmin = as.Date("2008-01-01"),
    xmax = as.Date("2009-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12,
    fill = "#D55E00"
  ) +
  
  # 2015 Political Uncertainty
  annotate(
    "rect",
    xmin = as.Date("2015-01-01"),
    xmax = as.Date("2015-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12,
    fill = "#0072B2"
  ) +
  
  # 2016 Political Shock
  annotate(
    "rect",
    xmin = as.Date("2016-01-01"),
    xmax = as.Date("2016-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12,
    fill = "#CC79A7"
  ) +
  
  # 2017 Currency Pressure
  annotate(
    "rect",
    xmin = as.Date("2017-01-01"),
    xmax = as.Date("2017-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12,
    fill = "#009E73"
  ) +
  
  # COVID Period
  annotate(
    "rect",
    xmin = as.Date("2020-03-01"),
    xmax = as.Date("2021-06-30"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12,
    fill = "gray40"
  ) +
  
  geom_line(
    linewidth = 1.1,
    color = "#1F3B5B"
  ) +
  
  labs(
    title = "Unemployment Rate Across Major Economic and Political Periods",
    subtitle = "Turkey labor market trends between 2005 and 2026",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Analysis 4: Monthly Seasonality Pattern

total_data <- labor_data |>
  filter(gender == "Total")

monthly_pattern <- total_data |>
  group_by(month_no, month_en) |>
  summarize(
    avg_unemp_rate = mean(unemp_rate, na.rm = TRUE),
    avg_emp_rate = mean(emp_rate, na.rm = TRUE),
    avg_lfp_rate = mean(lfp_rate, na.rm = TRUE),
    .groups = "drop"
  )

# ordering the months
monthly_pattern <- monthly_pattern |>
  mutate(
    month_en = factor(
      month_en,
      levels = c(
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"
      )
    )
  )

# plotting the monthly unemployment seasonality
ggplot(monthly_pattern, aes(x = month_en, y = avg_unemp_rate)) +
  geom_col(fill = "#2C5F8A", alpha = 0.85) +
  geom_text(
    aes(label = round(avg_unemp_rate, 1)),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Average Unemployment Rate by Month",
    subtitle = "Monthly averages across 2005–2026",
    x = "Month",
    y = "Average Unemployment Rate (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


# Analysis 5: Gender Gap Analysis

# yearly averages by gender
yearly_gender_gap_data <- labor_data |>
  filter(gender %in% c("Male", "Female")) |>
  group_by(year, gender) |>
  summarize(
    avg_unemp_rate = mean(unemp_rate, na.rm = TRUE),
    avg_emp_rate = mean(emp_rate, na.rm = TRUE),
    avg_lfp_rate = mean(lfp_rate, na.rm = TRUE),
    .groups = "drop"
  )

# converting gender rows into columns
gender_gap <- yearly_gender_gap_data |>
  pivot_wider(
    names_from = gender,
    values_from = c(avg_unemp_rate, avg_emp_rate, avg_lfp_rate)
  ) |>
  mutate(
    unemployment_gap = avg_unemp_rate_Female - avg_unemp_rate_Male,
    employment_gap = avg_emp_rate_Male - avg_emp_rate_Female,
    participation_gap = avg_lfp_rate_Male - avg_lfp_rate_Female
  )

# converting gap variables to long format
gender_gap_long <- gender_gap |>
  select(year, unemployment_gap, employment_gap, participation_gap) |>
  pivot_longer(
    cols = c(unemployment_gap, employment_gap, participation_gap),
    names_to = "gap_type",
    values_to = "gap_value"
  ) |>
  mutate(
    gap_type = recode(
      gap_type,
      unemployment_gap = "Female - Male Unemployment Gap",
      employment_gap = "Male - Female Employment Gap",
      participation_gap = "Male - Female Participation Gap"
    )
  )

# Plot gender gaps
ggplot(gender_gap_long, aes(x = year, y = gap_value)) +
  geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed", color = "gray45") +
  geom_line(linewidth = 1.1, color = "#2C5F8A") +
  geom_point(size = 2.2, color = "#2C5F8A") +
  facet_wrap(~ gap_type, scales = "free_y", ncol = 1) +
  labs(
    title = "Gender Gaps in Labor Market Indicators",
    subtitle = "Yearly average differences between male and female labor market rates",
    x = "Year",
    y = "Gap in percentage points"
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2026, by = 3)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Analysis 6: Highest and Lowest Unemployment Months

total_data <- labor_data |>
  filter(gender == "Total")

# top 10 highest unemployment months
highest_unemployment <- total_data |>
  arrange(desc(unemp_rate)) |>
  select(year, month_en, date, unemp_rate, emp_rate, lfp_rate) |>
  slice_head(n = 10)

# top 10 lowest unemployment months
lowest_unemployment <- total_data |>
  arrange(unemp_rate) |>
  select(year, month_en, date, unemp_rate, emp_rate, lfp_rate) |>
  slice_head(n = 10)

print(highest_unemployment)
print(lowest_unemployment)
# combine of highest and lowest months
unemployment_extremes <- bind_rows(
  highest_unemployment |> mutate(group = "Highest Unemployment"),
  lowest_unemployment |> mutate(group = "Lowest Unemployment")
) |>
  mutate(
    period_label = paste(year, month_en),
    period_label = reorder(period_label, unemp_rate)
  )

# plot extremes
ggplot(unemployment_extremes, aes(x = period_label, y = unemp_rate, fill = group)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Highest and Lowest Unemployment Months",
    subtitle = "Top 10 highest and lowest monthly unemployment rates, 2005–2026",
    x = "Period",
    y = "Unemployment Rate (%)",
    fill = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )