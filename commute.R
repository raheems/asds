
# Thu Sep  6 21:10:57 2018 ------------------------------

# Commuting time from Mirpur to Motijheel data set simulation

# This data set represents time spent in traffic jam in Dhaka city
# for a regular commute from Mirpur section 12 to Motijheel, the
# downtown of Dhaka city

library(truncnorm)
library(lubridate)

start_date = '2017-01-01'
end_date = '2018-08-31'
dates = seq(as.Date(start_date), as.Date(end_date), by='days')
day_name = weekdays(dates)
day_name <- factor(day_name, levels= c("Sunday", "Monday",
                               "Tuesday", "Wednesday",
                               "Thursday", "Friday",
                               "Saturday"))
n = length(dates);

# Parameters for morning commute
busavg = 2.5; bussd = 1;
uberavg = 1.5; ubersd = 0.75;
pathaoavg = 1; pathaosd = .5;

set.seed(19)
bus = rtruncnorm(n, a = 1, b = 5, mean = busavg, sd = bussd)
uber = rtruncnorm(n, a = 1, b = 3, mean = uberavg, sd = ubersd)
pathao = rtruncnorm(n, a = 1, b = 3, mean = pathaoavg, sd = pathaosd)
commute_time = data.frame(dates, day_name, bus, uber, pathao)

commute_time_morning <- commute_time %>%
  mutate(
    # time_of_day
    time_of_day = "Morning",

    adj = rtruncnorm(length(dates), a = .10, b = 1, mean = .75, sd = .25),
    # Reduce the time on Fridays and Saturdays
    bus = ifelse(day_name %in% c("Friday", "Saturday"),
                   (bus - adj), bus),
    uber = ifelse(day_name %in% c("Friday", "Saturday"),
                 (uber - adj), uber),
    pathao = ifelse(day_name %in% c("Friday", "Saturday"),
                 (pathao - adj), pathao)
  ) %>%
  mutate(
    bus = round(bus, 1),
    uber = round(uber, 1),
    pathao = round(pathao, 1)
  ) %>%
  select(-adj)

head(commute_time_morning)
write_excel_csv(commute_time_morning, "commute_morning.csv")


# Parameters for Afternoon commute
busavg = 2; bussd = 1;
uberavg = 1.25; ubersd = 0.75;
pathaoavg = .8; pathaosd = .5;

set.seed(19)
bus = rtruncnorm(n, a = 1, b = 5, mean = busavg, sd = bussd)
uber = rtruncnorm(n, a = 1, b = 3, mean = uberavg, sd = ubersd)
pathao = rtruncnorm(n, a = 1, b = 3, mean = pathaoavg, sd = pathaosd)
commute_time = data.frame(dates, day_name, bus, uber, pathao)

commute_time_afternoon <- commute_time %>%
  mutate(
    # time_of_day
    time_of_day = "Afternoon",

    adj = rtruncnorm(length(dates), a = .10, b = 1, mean = .75, sd = .25),
    # Reduce the time on Fridays and Saturdays
    bus = ifelse(day_name %in% c("Friday", "Saturday"),
                 (bus - adj), bus),
    uber = ifelse(day_name %in% c("Friday", "Saturday"),
                  (uber - adj), uber),
    pathao = ifelse(day_name %in% c("Friday", "Saturday"),
                    (pathao - adj), pathao)
  ) %>%
  mutate(
    bus = round(bus, 1),
    uber = round(uber, 1),
    pathao = round(pathao, 1)
  ) %>%
  select(-adj)

head(commute_time_afternoon)
write_excel_csv(commute_time_afternoon, "commute_afternoon.csv")



# Parameters for evening commute
busavg = 1.75; bussd = 1;
uberavg = 1; ubersd = 0.75;
pathaoavg = .7; pathaosd = .5;

set.seed(19)
bus = rtruncnorm(n, a = 1, b = 5, mean = busavg, sd = bussd)
uber = rtruncnorm(n, a = 1, b = 3, mean = uberavg, sd = ubersd)
pathao = rtruncnorm(n, a = 1, b = 3, mean = pathaoavg, sd = pathaosd)
commute_time = data.frame(dates, day_name, bus, uber, pathao)

commute_time_evening <- commute_time %>%
  mutate(
    # time_of_day
    time_of_day = "Evening",

    adj = rtruncnorm(length(dates), a = .10, b = 1, mean = .75, sd = .25),
    # Reduce the time on Fridays and Saturdays
    bus = ifelse(day_name %in% c("Friday", "Saturday"),
                 (bus - adj), bus),
    uber = ifelse(day_name %in% c("Friday", "Saturday"),
                  (uber - adj), uber),
    pathao = ifelse(day_name %in% c("Friday", "Saturday"),
                    (pathao - adj), pathao)
  ) %>%
  mutate(
    bus = round(bus, 1),
    uber = round(uber, 1),
    pathao = round(pathao, 1)
  ) %>%
  select(-adj)

head(commute_time_evening)
write_excel_csv(commute_time_evening, "commute_evening.csv")


commute_time <- rbind(commute_time_morning, commute_time_afternoon, commute_time_evening)

# Set the order of the time_of_day variable

commute_time <- commute_time %>%
  mutate(
    time_of_day = ordered(time_of_day,
                           levels = c("Morning", "Afternoon", "Evening"))
  ) %>%
  # Sort the date and time_of_day columns in ascending order
  arrange(
    dates, time_of_day
  )

head(commute_time)


# Save the commute_time data (combined)

write_excel_csv(commute_time, "commute.csv")

df <- commute_time %>%
  gather(transport, hours, bus:pathao)

head(df, 20)

#


ggplot(df, aes(x=day_name, y=hours, fill=time_of_day)) +
  geom_boxplot()

