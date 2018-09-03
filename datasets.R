# Sat Sep 01 07:53:36 2018 ------------------------------

# Generate the Facebook Like data
library(truncnorm)
n = 30; avg = 50; stdev = 20;
set.seed(20)
likes = round(rtruncnorm(n, mean = avg, sd = stdev), digits = 0)
fblikes = data.frame(likes)
df <- rbind(head(fblikes), c("..."), tail(fblikes))


# Generate the daily internet usage data

library(lubridate)
start_date = '2018-01-01'
end_date = '2018-08-31'
dates = seq(as.Date(start_date), as.Date(end_date), by='days')
days = weekdays(dates)
days <- factor(days, levels= c("Sunday", "Monday",
                               "Tuesday", "Wednesday",
                               "Thursday", "Friday",
                               "Saturday"))
n = length(dates); avg = 5; stdev = .3;
set.seed(20)
usage = round(rlnorm(n, mean = avg, sd = stdev), digits = 2)
internet_usage = data.frame(dates, days, usage)
knitr::kable(head(internet_usage))

library(ggplot2)
# Basic box plot
p <- ggplot(internet_usage, aes(x = as.factor(days), y = usage)) +
  geom_boxplot() +
  labs(x="Day of the week", y = "Internet usage")
p

# Percapita spenditure on students in public universities
# Source: http://data.gov.bd/dataset/annual-expenditure-student-financial-year-basis/resource/602d73d4-8e21-4544-bc37

library(tidyverse)
fname = "annual_expenditure_per_student_2018.xlsx"

df <- readxl::read_xlsx(fname, sheet = 1, range = "B9:T24", col_names = F)
for (i in 2:9) {
  df0 <- readxl::read_xlsx(fname, sheet = i, range = "B9:T24", col_names = F)
  df <- rbind(df, df0)
}

head(df)

# Remove all columns with NA
df <- df %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

# Drop col X__10

df <- df %>%
  select(- X__10)

# Remove all rows with all NAs
df <- df[!apply(is.na(df), 1, all),]

# Drop the row with NA
df <- df %>%
  filter(!is.na(X__1 ))

dim(df)
colnames(df) <- vnames
head(df)
