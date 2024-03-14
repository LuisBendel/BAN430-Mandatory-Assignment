# Libraries
library(fpp3)
library(fable)
library(tidyverse)
library(lubridate)
library(urca)
library(scales)

# setwd("~/Master/NHH/2_Courses/BAN430/BAN430 Mandatory Assignment")


# (1) ----
# Load and transform data to tsibble
walmart <- readr::read_csv("https://raw.githubusercontent.com/holleland/BAN430/master/data/Walmart.csv") %>% 
  mutate(Date = yearweek(as.Date(Date, format = "%d-%m-%Y"))) %>% 
  rename(Yearweek = Date) %>% 
  as_tsibble(index = Yearweek,
             key = Store)

# (2) ----

# Is Trend or seasonality present
# Preliminary analysis of the time series
walmart %>% autoplot(Weekly_Sales) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Weekly sales of 45 Walmart stores") +
  xlab("Week") + ylab("weekly sales in USD")
  

walmart.month <- walmart %>% 
  group_by(Store) %>% 
  index_by(Month =~yearmonth(.)) %>% 
  summarize(Mean_Monthly_Sales = mean(Weekly_Sales))

# Mean Sales per month for all stores
walmart.month %>% autoplot() +
  theme(legend.position = "none")

# Mean Sales per calendar month for all stores
walmart.month %>%
  as_tibble() %>% 
  mutate(Month = month(Month)) %>% 
  group_by(Month, Store) %>% 
  summarise(Mean = mean(Mean_Monthly_Sales)) %>% 
  ggplot(aes(x = as.factor(Month), y = Mean, group = Store, color = Store)) +
  geom_line()

# season plots for stores 1-5
walmart %>% 
  filter(Store %in% c(1:5)) %>% 
  gg_season(Weekly_Sales) +
  theme_minimal() +
  labs(title = "Weekly sales per year for stores 1-5") +
  scale_y_continuous(labels = label_number()) +
  ylab("Weekly Sales")

# Are any visible effects of holidays, temperature, fuel price or unemployment present
walmart %>% 
  filter(Store %in% c(1,2,3,4,5)) %>% 
  ggplot(aes(x = Yearweek, y = Weekly_Sales, color = as.factor(Holiday_Flag))) +
  geom_point()

# Boxplot to show if there is a difference in means for holidays vs non-holidays
walmart %>% 
  ggplot(aes(x = as.factor(Holiday_Flag), y = Weekly_Sales)) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Boxplot comparison of holiday sales and non-holiday sales") +
  xlab("Holiday") + ylab("Weekly Sales")

# scatterplot of weekly sales on temperature, fuel price, and unemployment
walmart %>% 
  pivot_longer(c("Temperature", "Fuel_Price", "Unemployment"),
               values_to = "Value",
               names_to = "Predictor") %>% 
  ggplot(aes(x = Value, y = Weekly_Sales)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Predictor, scales = "free_x") +
  theme_minimal() +
  labs(title = "Weekly sales on temperature, fuel price, and unemployment") +
  ylab("Weekly Sales")

# should we inflation adjust the data?
walmart %>% 
  mutate(Weekly_Sales_Adj = (Weekly_Sales / CPI * 100)) %>% 
  pivot_longer(c("Weekly_Sales", "Weekly_Sales_Adj"),
               values_to = "Weekly_Sales",
               names_to = "Measure") %>% 
  mutate(Measure = as.factor(Measure)) %>% 
  as_tibble() %>% 
  group_by(Yearweek, Measure) %>% 
  summarise(Weekly_Sales = mean(Weekly_Sales)) %>% 
  ggplot(aes(x = Yearweek, y = Weekly_Sales)) +
  geom_line() +
  facet_grid(Measure ~ .)

# should we use a mathematical transformation?
walmart %>% 
  mutate(Log_Weekly_Sales = log(Weekly_Sales)) %>% 
  pivot_longer(c("Weekly_Sales", "Log_Weekly_Sales"),
               values_to = "Weekly_Sales",
               names_to = "Measure") %>% 
  mutate(Measure = as.factor(Measure)) %>% 
  as_tibble() %>% 
  group_by(Yearweek, Measure) %>% 
  summarise(Weekly_Sales = mean(Weekly_Sales)) %>% 
  ggplot(aes(x = Yearweek, y = Weekly_Sales)) +
  geom_line() +
  facet_grid(Measure ~ ., scales = "free_y")



# (3) ----
# Split data into train and test data
train_split <- max(walmart$Yearweek) - 20

walmart.train <- walmart %>% 
  filter(Yearweek < train_split)

walmart.test <- walmart %>% 
  filter(Yearweek >= train_split)


# (4) ----
# Filter data first to Walmart store number 3
walmart.3 <- walmart %>% filter(Store == 3)
walmart.train.3 <- walmart.train %>% filter(Store == 3)
walmart.test.3 <- walmart.test %>% filter(Store == 3)

# do a unit root kpss test
walmart.train.3 %>% 
  features(log(Weekly_Sales), unitroot_kpss)

# how many seasonal differences
walmart.train.3 %>% 
  features(log(Weekly_Sales), unitroot_nsdiffs)

# how many differences
walmart.train.3 %>% 
  features(log(Weekly_Sales), unitroot_ndiffs)

# as suggested by the tests above, first difference and 1 seasonal difference
walmart.train.3.tf <- walmart.train.3 %>% 
  mutate(Weekly_Sales = log(Weekly_Sales) %>% difference(lag = 52) %>% difference()) %>% 
  filter(!is.na(Weekly_Sales))

walmart.train.3.tf %>% autoplot()


# (5) ----
# ACF and PACF plots
walmart.train.3.tf %>% 
  gg_tsdisplay(Weekly_Sales, plot_type = "partial")



# (6) ----
# Fit all models on the training data
fit <-
  walmart.train.3 %>% 
  model(
    MEAN = MEAN(Weekly_Sales),
    DRIFT = RW(Weekly_Sales ~ drift()),
    NAIVE = NAIVE(Weekly_Sales),
    SNAIVE = SNAIVE(Weekly_Sales ~ lag(52)),
    ETS_ANN = ETS(Weekly_Sales ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(Weekly_Sales ~ error("A") + trend("A") + season("N")),
    ARIMA113010 = ARIMA(Weekly_Sales ~ pdq(3,1,0) + PDQ(0,1,0)),
    ARIMA011010 = ARIMA(Weekly_Sales ~ pdq(0,1,1) + PDQ(0,1,0)),
    ARIMAauto = ARIMA(Weekly_Sales),
    DYNAMIC = ARIMA(Weekly_Sales ~ 1 + as.factor(Holiday_Flag))
  )

# forecast based on test data
fc <- fit %>% 
  forecast(new_data = walmart.test.3)

# display forecast vs. test data
fc %>% 
  autoplot() + 
  autolayer(walmart.test.3, .vars = Weekly_Sales) +
  facet_wrap(~ .model, scales = "free_y") +
  theme_minimal() +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Test data (black) vs forecasts for each model") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, size = 8, hjust = 1))

# compute the RMSE for all models
accuracy(fc, data = walmart.test.3) %>% 
  select(.model, Store, RMSE) %>% 
  arrange(RMSE)

# plot innovation residuals for all models
augment(fit) %>% 
  autoplot(.innov) +
  facet_wrap(~ .model) +
  theme_minimal() +
  labs(title = )

# plot autocorrelation functions of residuals for models that assume uncorrelated errors
fit %>%
  select(ARIMA113010, ARIMA011010, ARIMAauto, ETS_ANN, ETS_AAN, DYNAMIC) %>% 
  augment() %>%
  ACF(.innov) %>%
  autoplot() +
  facet_wrap(~.model) +
  theme_minimal() +
  labs(title = "Autocorrelation plots for selected models.")
