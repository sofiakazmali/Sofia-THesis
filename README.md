# Sofia-THesis
---
title: "Thesis"
output: html_document
date: "2025-06-14"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## R Markdown

# Loading the necessary packages
```{r}
library(openxlsx)
library(dplyr)
library(tidyr)
library(readr)
library(skimr)
library(ggplot2)
library(lubridate)
```

# Reading our CSV
```{r}

df1 <- read_csv2("cons.sec.price_merged.csv")
df2 <- read_csv2("right_left.csv")
df3 <- read_csv2("market.csv")

```

# Descriptive Statistics (cons.sec.price_merged.csv)
```{r}
print(dim(df1))
print(names(df1))
head(df1)
```

```{r}
# Remove row 75 (if it exists)
if(nrow(df1) >= 75) df1 <- df1[-75, ]

# Remove columns 7 to 21 (if they exist)
if(ncol(df1) >= 21) df1 <- df1[, -c(7:21)]
```

```{r}
df1$Month <- as.Date(df1$Month, format = "%d.%m.%Y")
df2$Month <- as.Date(df2$Month, format = "%d.%m.%Y")
df3$Month <- as.Date(df3$Month, format = "%d.%m.%Y")
```

```{r}
# Remove row 28 to 29 (if it exists)
if(nrow(df3) >= 29) df3 <- df3[-c(28, 29), ]
```

```{r}
summary(df2[, c("Left bank", "Right bank", "price_USD_mcubed")])
skimr::skim(df2[, c("Left bank", "Right bank", "price_USD_mcubed")])
```

```{r}
# Remove all non-numeric, non-comma, non-dot characters, trim whitespace
df2$price_USD_mcubed <- gsub("[^0-9,\\.]", "", df2$price_USD_mcubed)
df2$price_USD_mcubed[df2$price_USD_mcubed %in% c("", "NA", "na", "N/A")] <- NA

# Standardize decimal separator
df2$price_USD_mcubed <- gsub(",", ".", df2$price_USD_mcubed)

# Convert to numeric
df2$price_USD_mcubed <- as.numeric(df2$price_USD_mcubed)

# Now interpolate any NA values linearly
df2$price_USD_mcubed <- na.approx(df2$price_USD_mcubed, x = df2$Month, na.rm = FALSE)
```

```{r}
# Descriptive statistics for total consumption and price
summary(df2[, c("Total", "price_USD_mcubed")])
skimr::skim(df2[, c("Total", "price_USD_mcubed")])

library(ggplot2)
library(scales)
library(zoo)

# Plot with dual y-axes
ggplot(df2, aes(x = Month)) +
  geom_line(aes(y = Total, color = "Total Consumption"), size = 1.5) +
  geom_line(aes(y = price_USD_mcubed * (max(Total, na.rm=TRUE) / max(price_USD_mcubed, na.rm=TRUE)), 
                color = "Price (USD/m³)"), size = 1.5) +
  scale_y_continuous(
    name = "Total Consumption (million m³)",
    labels = comma,
    sec.axis = sec_axis(~ . / (max(df2$Total, na.rm=TRUE) / max(df2$price_USD_mcubed, na.rm=TRUE)), 
                        name = "Price (USD per m³)", labels = comma)
  ) +
  scale_color_manual(
    name = "",
    values = c("Total Consumption" = "#2c7bb6", "Price (USD/m³)" = "#d7191c")
  ) +
  labs(
    title = "Total Consumption and Price Over Time",
    x = "Month"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.title.y.right = element_text(size = 18, color = "#d7191c"),
    legend.position = "top",
    axis.text.x = element_text(angle = 35, hjust = 1)
  )
```

```{r}
df2[df2$Month >= as.Date("2023-02-01") & df2$Month <= as.Date("2023-04-30"), ]
```

```{r}
# Example: Replace suspiciously high value with average of neighbors
problem_idx <- which.max(df2$Total)
df2$Total[problem_idx] <- mean(c(df2$Total[problem_idx - 1], df2$Total[problem_idx + 1]), na.rm = TRUE)
```

```{r}
# Descriptive statistics for total consumption and price
summary(df2[, c("Total", "price_USD_mcubed")])
skimr::skim(df2[, c("Total", "price_USD_mcubed")])

library(ggplot2)
library(scales)
library(zoo)

# Plot with dual y-axes
ggplot(df2, aes(x = Month)) +
  geom_line(aes(y = Total, color = "Total Consumption"), size = 1.5) +
  geom_line(aes(y = price_USD_mcubed * (max(Total, na.rm=TRUE) / max(price_USD_mcubed, na.rm=TRUE)), 
                color = "Price (USD/m³)"), size = 1.5) +
  scale_y_continuous(
    name = "Total Consumption (million m³)",
    labels = comma,
    sec.axis = sec_axis(~ . / (max(df2$Total, na.rm=TRUE) / max(df2$price_USD_mcubed, na.rm=TRUE)), 
                        name = "Price (USD per m³)", labels = comma)
  ) +
  scale_color_manual(
    name = "",
    values = c("Total Consumption" = "#2c7bb6", "Price (USD/m³)" = "#d7191c")
  ) +
  labs(
    title = "Total Consumption and Price Over Time",
    x = "Month"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.title.y.right = element_text(size = 18, color = "#d7191c"),
    legend.position = "top",
    axis.text.x = element_text(angle = 35, hjust = 1)
  )
```




```{r}
ggplot(df2, aes(x = Month)) +
  geom_line(aes(y = `Left bank`, color = "Left bank"), size = 1.5) +
  geom_line(aes(y = `Right bank`, color = "Right bank"), size = 1.5) +
  scale_color_manual(
    name = "Series",
    values = c("Left bank" = "#4575b4", "Right bank" = "#d73027")
  ) +
  labs(
    title = "Left Bank vs Right Bank Gas Consumption Over Time",
    x = "Month",
    y = "Consumption (million m³)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "top",
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  scale_y_continuous(labels = comma)
```


```{r}
scaling_factor <- max(df2$`Left bank`, na.rm=TRUE) / max(df2$`Right bank`, na.rm=TRUE)

ggplot(df2, aes(x = Month)) +
  # Left bank: left y-axis
  geom_line(aes(y = `Left bank`, color = "Left bank"), size = 1.5) +
  # Right bank: scale to left y-axis, but will display as right y-axis
  geom_line(aes(y = `Right bank` * scaling_factor, color = "Right bank"), size = 1.5) +
  scale_color_manual(
    name = "Series",
    values = c("Left bank" = "#4575b4", "Right bank" = "#d73027")
  ) +
  scale_y_continuous(
    name = "Left Bank Consumption (million m³)",
    labels = comma,
    sec.axis = sec_axis(~ . / scaling_factor, name = "Right Bank Consumption (million m³)", labels = comma)
  ) +
  labs(
    title = "Left Bank vs Right Bank Gas Consumption Over Time",
    x = "Month"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 16, color = "#4575b4"),
    axis.title.y.right = element_text(size = 16, color = "#d73027"),
    legend.position = "top",
    axis.text.x = element_text(angle = 35, hjust = 1)
  )
```


```{r}
df3 <- df3[-c(28, 29), ]
```

```{r}
df3_plot <- df3[!is.na(df3$`Cost_with_VAT(MDL)`) & !is.na(df3$`Price_MDLper1000m³`), ]
```



```{r}
library(ggplot2)

# 1. Volume vs. Price
ggplot(df3, aes(x = `Volume_gas(thousands_m³)`, y = `Price_MDLper1000m³`)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Volume vs. Price",
    x = "Volume Gas (thousands m³)",
    y = "Price (MDL per 1000 m³)"
  ) +
  theme_minimal()

# 2. Price vs. Cost
ggplot(df3, aes(x = `Price_MDLper1000m³`, y = `Cost_with_VAT(MDL)`)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Price vs. Cost with VAT",
    x = "Price (MDL per 1000 m³)",
    y = "Cost with VAT (MDL)"
  ) +
  theme_minimal()

# 3. Volume vs. Cost (optional)
ggplot(df3, aes(x = `Volume_gas(thousands_m³)`, y = `Cost_with_VAT(MDL)`)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(
    title = "Volume vs. Cost with VAT",
    x = "Volume Gas (thousands m³)",
    y = "Cost with VAT (MDL)"
  ) +
  theme_minimal()
```






# ADF Test
```{r}
# Unit Roots
library(tseries)

# Remove rows where Month or Total is NA
df_clean <- df %>% filter(!is.na(Month), !is.na(Total))

# Create time series object
total_ts <- ts(df_clean$Total,
               start = c(year(min(df_clean$Month)), month(min(df_clean$Month))),
               frequency = 12)

# Run ADF test
adf.test(total_ts)

```

```{r}
head(df$Month, 20)
unique(df$Month)
```

# ADF Test 
```{r}
library(tseries)

# 1. Make sure Month is in the right date format!
df$Month <- as.Date(df$Month, format = "%d.%m.%Y")

# 2. Remove NAs
df_clean <- df[!is.na(df$Month) & !is.na(df$Total), ]

# 3. Get start year and month
start_year <- as.numeric(format(min(df_clean$Month), "%Y"))
start_month <- as.numeric(format(min(df_clean$Month), "%m"))

# 4. Create time series object
total_ts <- ts(df_clean$Total, start = c(start_year, start_month), frequency = 12)

# 5. Run ADF test
adf.test(total_ts)
```

```{r}
plot(df_clean$Month, df_clean$Total, type = "l",
     main = "Total Consumption Over Time",
     xlab = "Month", ylab = "Total Consumption (million m³)")
```

```{r}
library(ggplot2)
ggplot(df_clean, aes(x = Month, y = Total)) +
  geom_line() +
  labs(title = "Total Consumption Over Time",
       x = "Month", y = "Total Consumption (million m³)") +
  theme_minimal()
```

```{r}
acf(total_ts, main = "ACF of Total Consumption")
pacf(total_ts, main = "PACF of Total Consumption")
```

```{r}
library(zoo)
rollmean <- rollmean(df_clean$Total, 12, fill = NA)
rollsd <- rollapply(df_clean$Total, 12, sd, fill = NA)

plot(df_clean$Month, df_clean$Total, type = "l")
lines(df_clean$Month, rollmean, col = "blue", lwd = 2)
lines(df_clean$Month, rollmean + rollsd, col = "red", lty = 2)
lines(df_clean$Month, rollmean - rollsd, col = "red", lty = 2)
legend("topright", legend = c("Total", "Rolling Mean", "Mean ± SD"),
       col = c("black", "blue", "red"), lty = c(1,1,2))
```

```{r}
library(tseries)

# Make sure Month is in the right format (if not already)
df$Month <- as.Date(df$Month, format = "%d.%m.%Y")

# Remove NAs for each variable (optional: put in a function for reuse)
run_adf_test <- function(series, df) {
  df_clean <- df[!is.na(df$Month) & !is.na(df[[series]]), ]
  start_year <- as.numeric(format(min(df_clean$Month), "%Y"))
  start_month <- as.numeric(format(min(df_clean$Month), "%m"))
  ts_data <- ts(df_clean[[series]], start = c(start_year, start_month), frequency = 12)
  cat("ADF Test for", series, "\n")
  print(adf.test(ts_data))
  cat("\n")
}

# Run for each group
run_adf_test("Household_cons", df)
run_adf_test("Industrial_cons", df)
run_adf_test("Energy_sector_cons", df)
```

# Regressions 

```{r}
df_clean <- df2[!is.na(df2$Month) & !is.na(df2$Total), ]

```

```{r}
# Make sure Month is in Date format
df1$Month <- as.Date(df1$Month)

# Create dummies for the policy periods
df1 <- df1 %>%
  mutate(
    Pre_reform = if_else(Month >= as.Date("2019-01-01") & Month <= as.Date("2022-12-31"), 1, 0),
    Policy = if_else(Month >= as.Date("2023-01-01") & Month <= as.Date("2025-02-28"), 1, 0)
  )

```

```{r}
# Filter only relevant period: Jan 2019 – Feb 2025
df_filtered <- df1 %>%
  filter(Month >= as.Date("2019-01-01") & Month <= as.Date("2025-02-28"))

```

```{r}
# Convert price_USD_mcubed from comma-format character to numeric
df1$price_USD_mcubed <- as.numeric(gsub(",", ".", df1$price_USD_mcubed))

```




```{r}
# 1. Ensure Month is in Date format
df1$Month <- as.Date(df1$Month)

# 2. Add dummy variables
df1 <- df1 %>%
  mutate(
    Pre_reform = if_else(Month >= as.Date("2019-01-01") & Month <= as.Date("2022-12-31"), 1, 0),
    Policy = if_else(Month >= as.Date("2023-01-01") & Month <= as.Date("2025-02-28"), 1, 0)
  )

# 3. Create a filtered dataset
df_filtered <- df1 %>%
  filter(Month >= as.Date("2019-01-01") & Month <= as.Date("2025-02-28"))

# 4. Run regression
model_household <- lm(Household_cons ~ price_USD_mcubed + Policy, data = df_filtered)
summary(model_household)

```

