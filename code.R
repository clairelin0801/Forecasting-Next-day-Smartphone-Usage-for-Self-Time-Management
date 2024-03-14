#install.packages("urca")
library(fabletools) 
library(fable)
library(tsibble)
library(feasts)
library(slider)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(rlang)
library(readr)
library(scales)
library(GGally)

### Read data ##############################

qsdata = read.csv("data.csv")
qsdata <- qsdata %>% mutate(Date = date(Date)) %>% as_tsibble(index = Date)

### Convert hms into minutes ###################

for (i in 1:8){
  res <- hms(qsdata[,i+1][[1]])
  qsdata[,i+1] <- hour(res)*60 + minute(res)
}

### Create Lag-1 variables for SocialMedia and Pickups ###########

qsdata <- qsdata %>% dplyr::mutate(
  PersonA.Lag1 = c(NA, PersonA[1:(nrow(qsdata)-1)]),
  PersonB.Lag1 = c(NA, PersonB[1:(nrow(qsdata)-1)]),
  PersonC.Lag1 = c(NA, PersonC[1:(nrow(qsdata)-1)]),
  PersonD.Lag1 = c(NA, PersonD[1:(nrow(qsdata)-1)]),
  PickupsA.Lag1 = c(NA, PickupsA[1:(nrow(qsdata)-1)]),
  PickupsB.Lag1 = c(NA, PickupsB[1:(nrow(qsdata)-1)]),
  PickupsC.Lag1 = c(NA, PickupsC[1:(nrow(qsdata)-1)]),
  PickupsD.Lag1 = c(NA, PickupsD[1:(nrow(qsdata)-1)])
)

### The first six rows of the data looks like ##################
head(qsdata)

plot(qsdata$AllPersonA, qsdata$PersonA.Lag1)
plot(qsdata$AllPersonA, qsdata$PickupsA.Lag1)

### Plot ######################

#### Key series (smartphone usage) ######
autoplot(qsdata|>
           pivot_longer(cols = 6:9, names_to = "Person", values_to = "Value"), .vars = Value) +
  geom_line(linewidth = 0.8) + # makes sure the lines are plotted
  facet_wrap(vars(Person), scales = 'free_y', nrow = 1) +
  xlab("Date") + ylab("Times (Minutes)") + theme(legend.position = "none")

#### social and entertainment usage ######
autoplot(qsdata|>
           pivot_longer(cols = 2:5, names_to = "Person", values_to = "Value"), .vars = Value) +
  geom_line(linewidth = 0.8) + # makes sure the lines are plotted
  facet_wrap(vars(Person), scales = 'free_y', nrow = 1) +
  xlab("Date") + ylab("Times (Minutes)") + theme(legend.position = "none")

#### class hour ######
autoplot(qsdata|>
           pivot_longer(cols = 11:14, names_to = "Person", values_to = "Value"), .vars = Value) +
  geom_line(linewidth = 0.8) + # makes sure the lines are plotted
  facet_wrap(vars(Person), scales = 'free_y', nrow = 1) +
  xlab("Date") + ylab("Times (Hours)") + theme(legend.position = "none")

#### Pickups ######
autoplot(qsdata|>
           pivot_longer(cols = 15:18, names_to = "Person", values_to = "Value"), .vars = Value) +
  geom_line(linewidth = 0.8) + # makes sure the lines are plotted
  facet_wrap(vars(Person), scales = 'free_y', nrow = 1) +
  xlab("Date") + ylab("Pickup times") + theme(legend.position = "none")

#### Working hour ##########
autoplot(qsdata|>
           pivot_longer(cols = 19:21, names_to = "Person", values_to = "Value"), .vars = Value) +
  geom_line(linewidth = 0.8) + # makes sure the lines are plotted
  facet_wrap(vars(Person), scales = 'free_y', nrow = 1) +
  xlab("Date") + ylab("Working Hour") + theme(legend.position = "none")

#### Data splitting ##########

qsdata_long <- qsdata |>
  pivot_longer(cols = 6:9, names_to = "Person", values_to = "Value")

qsdata.train <- qsdata_long |> filter_index("2023-09-25"~"2023-11-30")
qsdata.valid <- qsdata_long |> filter_index("2023-12-01"~"2023-12-16")

### Model fitting ##########

#### Person A #####################
pp = "AllPersonA"
data.person <- qsdata_long %>% filter(Person == pp) %>% ungroup()
data.person.train <- qsdata.train %>% filter(Person == pp) %>% ungroup()
data.person.valid <- qsdata.valid %>% filter(Person == pp) %>% ungroup()

set.seed(123)

##### fit model #################
fit.personA <- data.person.train |>
  model(naive = NAIVE(Value),
        snaive = SNAIVE(Value),
        sample.mean = ARIMA(Value ~ pdq(0,0,0) + PDQ(0,0,0)),
        ets = ETS(Value),
        arima = ARIMA(Value, stepwise = F),
        tslm = TSLM(Value ~ trend() + season()),
        twolayer = ARIMA(Value ~ trend() + season(), stepwise = F),
        tslm.ext = TSLM(Value ~ trend() + season() + IsHoliday + ClassHourA + IsEventA + PickupsA.Lag1 + PersonA.Lag1),
        arima.ext = ARIMA(Value ~ IsHoliday + ClassHourA + IsEventA + PickupsA.Lag1 + PersonA.Lag1, stepwise = F),
        twolayer.ext = ARIMA(Value ~ trend() + season() + IsHoliday + ClassHourA + IsEventA + PickupsA.Lag1 + PersonA.Lag1 + PDQ(0,0,0), stepwise = F),
        nnet = NNETAR(Value),
        nnet.ext = NNETAR(Value ~ IsHoliday + ClassHourA + IsEventA + PickupsA.Lag1 + PersonA.Lag1)
  )

augmented.train = augment(fit.personA)

fitted_values <- fitted(fit.personA) |> dplyr::select(Date, .model, .fitted) |>
  rename(fitted = .fitted, model = .model)

##### residuals pairs plot #################
residual_values <- augmented.train |> dplyr::select(Date, .model, .resid) |>
  rename(residual = .resid, model = .model)
residual_plot <- residual_values %>%  
  pivot_wider(names_from = model, values_from =  residual)

ggpairs(residual_plot[,-1]) ### check the correlation of residuals

##### 1-step-ahead roll forward #################
fits.roll <- refit(fit.personA, data.person %>% filter_index("2023-09-25"~"2023-12-16"), reestimate = FALSE) %>% 
  augment() |>
  filter(Date > as.Date("2023-11-30"))

valid.roll.accuracy <- fits.roll |>
  as_tibble() |> # tsibble objects maintain each date separately
  group_by(.model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / Value), na.rm = TRUE) * 100,
    UFP = mean((.resid) < 0, na.rm = TRUE) * 100
  )

fabletools::accuracy(fit.personA) ### training performance
valid.roll.accuracy ### one-step-ahead performance

forecast_values <- fits.roll %>% as_tibble() |>
  dplyr::select(Date, .model, .fitted) |> rename(fc.value = .fitted, model = .model)

data_long <- data.person %>% dplyr::select(Date, Value) %>% mutate(
  naive = Value, snaive = Value, sample.mean = Value, ets = Value, arima = Value, arima.ext = Value,
  tslm = Value, twolayer = Value, tslm.ext = Value, arima.dummy = Value,
  twolayer.ext = Value, nnet = Value, nnet.ext = Value
)

data_long <- data_long %>%
  pivot_longer(cols = 3:15, names_to = "model", values_to = "actual")

combined_data1 <- data_long |>
  left_join(fitted_values, by = c("Date", "model")) |>
  left_join(residual_values, by = c("Date", "model")) |>
  left_join(forecast_values, by = c("Date", "model")) |>
  mutate(fc.error = Value - fc.value) 

combined_data <- combined_data1 %>% filter(model %in% c("tslm", 'tslm.ext', "sample.mean"))
combined_data$model <- factor(combined_data$model, levels = c("sample.mean", "tslm", "tslm.ext"))

##### plot #################
p1 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = Value), color = "black", linewidth = 0.8) +  # actual data
  geom_line(aes(y = fitted, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.value, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Actual Time Usage vs. Fitted and Forecasted Values",
       x = "Date", y = "Time Usage (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

p2 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = residual, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.error, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Forecast Error plot",
       x = "Date", y = "Forecast Error (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

grid.arrange(p1, p2)

lvl = c("naive", "snaive", "sample.mean", "tslm", "tslm.ext", "ets", "arima", "arima.ext", "twolayer", "twolayer.ext", "nnet", "nnet.ext")
combined_data1$model <- factor(combined_data1$model, levels = lvl)
lvlwant = c("naive", "snaive", "sample.mean", "tslm", "tslm.ext", "nnet", "arima", "nnet.ext")
# pA1 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = residual, x = model, fill = model)) + ylim(c(-450, 300)) + theme_bw()
# pA2 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = fc.error, x = model, fill = model)) + ylim(c(-450, 300)) + theme_bw()
# 
# grid.arrange(pA1, pA2)

df1 <- combined_data1 %>% filter(model %in% lvlwant) %>% select(model, residual, fc.error) %>% 
  rename(Training = residual, Validation = fc.error) %>% pivot_longer(cols = 2:3) %>% drop_na() %>%
  rename(Period = name, Error = value)
ggplot(df1, aes(model, Error, fill = Period)) + geom_boxplot() + theme_bw()

##### summary of selected model ##########

fit.personA %>% dplyr::select(tslm) %>% report() 
fit.personA %>% dplyr::select(tslm.ext) %>% report()

##### forecast #################
fit_finalA = data.person %>% filter_index(.~"2023-12-16") %>% model(tslm = TSLM(Value ~ trend() + season()))
fit_finalA %>% forecast(new_data = data.person %>% filter_index("2023-12-17"))

#### Person B ####################

pp = "AllPersonB"
data.person <- qsdata_long %>% filter(Person == pp) %>% ungroup()
data.person.train <- qsdata.train %>% filter(Person == pp) %>% ungroup()
data.person.valid <- qsdata.valid %>% filter(Person == pp) %>% ungroup()

set.seed(123)

##### fit model #################
fit.personB <- data.person.train |>
  model(naive = NAIVE(Value),
        snaive = SNAIVE(Value),
        sample.mean = ARIMA(Value ~ pdq(0,0,0) + PDQ(0,0,0)),
        ets = ETS(Value),
        arima = ARIMA(Value, stepwise = F),
        tslm = TSLM(Value ~ trend() + season()),
        twolayer = ARIMA(Value ~ trend() + season(), stepwise = F),
        tslm.ext = TSLM(Value ~ trend() + season() + IsHoliday + ClassHourB + WorkingHourB + IsEventB + UseInstagramB + PickupsB.Lag1 + PersonB.Lag1),
        arima.ext = ARIMA(Value ~ IsHoliday + ClassHourB + WorkingHourB + IsEventB + UseInstagramB + PickupsB.Lag1 + PersonB.Lag1, stepwise = F),
        twolayer.ext = ARIMA(Value ~ trend() + season() + IsHoliday + ClassHourB + WorkingHourB + IsEventB + UseInstagramB + PickupsB.Lag1 + PersonB.Lag1 + PDQ(0,0,0), stepwise = F),
        nnet = NNETAR(Value),
        nnet.ext = NNETAR(Value ~ IsHoliday + ClassHourB + WorkingHourB + IsEventB + UseInstagramB + PickupsB.Lag1 + PersonB.Lag1)
        )

augmented.train = augment(fit.personB)

fitted_values <- fitted(fit.personB) |> dplyr::select(Date, .model, .fitted) |>
  rename(fitted = .fitted, model = .model)

##### residuals pairs plot #################
residual_values <- augmented.train |> dplyr::select(Date, .model, .resid) |>
  rename(residual = .resid, model = .model)
residual_plot <- residual_values %>%  
  pivot_wider(names_from = model, values_from =  residual)
ggpairs(residual_plot[,-1])

##### 1-step-ahead roll forward #################
fits.roll <- refit(fit.personB, data.person %>% filter_index("2023-09-25"~"2023-12-16"), reestimate = FALSE) %>% 
  augment() |>
  filter(Date > as.Date("2023-11-30"))

valid.roll.accuracy <- fits.roll |>
  as_tibble() |> # tsibble objects maintain each date separately
  group_by(.model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / Value), na.rm = TRUE) * 100,
    UFP = mean((.resid) < 0, na.rm = TRUE) * 100
  )

fabletools::accuracy(fit.personB)
valid.roll.accuracy

forecast_values <- fits.roll %>% as_tibble() |>
  dplyr::select(Date, .model, .fitted) |> rename(fc.value = .fitted, model = .model)

data_long <- data.person %>% dplyr::select(Date, Value) %>% mutate(
  naive = Value, snaive = Value, sample.mean = Value, ets = Value, arima = Value, arima.ext = Value,
  tslm = Value, twolayer = Value, tslm.ext = Value, arima.dummy = Value,
  twolayer.ext = Value, nnet = Value, nnet.ext = Value
)

data_long <- data_long %>%
  pivot_longer(cols = 3:15, names_to = "model", values_to = "actual")

combined_data1 <- data_long |>
  left_join(fitted_values, by = c("Date", "model")) |>
  left_join(residual_values, by = c("Date", "model")) |>
  left_join(forecast_values, by = c("Date", "model")) |>
  mutate(fc.error = Value - fc.value) %>% filter(model %in% c("arima", 'nnet', "naive"))

combined_data1 <- data_long |>
  left_join(fitted_values, by = c("Date", "model")) |>
  left_join(residual_values, by = c("Date", "model")) |>
  left_join(forecast_values, by = c("Date", "model")) |>
  mutate(fc.error = Value - fc.value) 

combined_data <- combined_data1 %>% filter(model %in% c("naive", 'arima', "nnet"))
combined_data$model <- factor(combined_data$model, levels = c("naive", "arima", "nnet"))

##### plot #################
p1 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = Value), color = "black", linewidth = 0.8) +  # actual data
  geom_line(aes(y = fitted, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.value, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Actual Time Usage vs. Fitted and Forecasted Values",
       x = "Date", y = "Time Usage (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

p2 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = residual, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.error, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Forecast Error plot",
       x = "Date", y = "Forecast Error (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

grid.arrange(p1, p2)

lvl = c("naive", "snaive", "sample.mean", "arima", "nnet", "arima.ext", "nnet.ext", "ets", "tslm")
combined_data1$model <- factor(combined_data1$model, levels = lvl)
lvlwant = c("naive", "snaive", "sample.mean", "arima", "nnet", "arima.ext", "nnet.ext", "ets")
# pB1 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = residual, x = model)) + theme_bw()
# pB2 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = fc.error, x = model)) + theme_bw()
# 
# grid.arrange(pB1, pB2)
df1 <- combined_data1 %>% filter(model %in% lvlwant) %>% select(model, residual, fc.error) %>% 
  rename(Training = residual, Validation = fc.error) %>% pivot_longer(cols = 2:3) %>% drop_na() %>%
  rename(Period = name, Error = value)
ggplot(df1, aes(model, Error, fill = Period)) + geom_boxplot() + theme_bw()

##### summary of selected model ##########

fit.personB %>% dplyr::select(arima) %>% report() 
fit.personB %>% dplyr::select(nnet) %>% report()

##### forecast #################
fit_finalB = data.person %>% filter_index(.~"2023-12-16") %>% model(ARIMA(Value~pdq(0,1,1)+PDQ(1,0,2)))
fit_finalB %>% forecast(new_data = data.person %>% filter_index("2023-12-17"))

#### Person C ####################

pp = "AllPersonC"
data.person <- qsdata_long %>% filter(Person == pp) %>% ungroup()
data.person.train <- qsdata.train %>% filter(Person == pp) %>% ungroup()
data.person.valid <- qsdata.valid %>% filter(Person == pp) %>% ungroup()

set.seed(123)

##### fit model #################
fit.personC <- data.person.train |>
  model(naive = NAIVE(Value),
        snaive = SNAIVE(Value),
        sample.mean = ARIMA(Value ~ pdq(0,0,0) + PDQ(0,0,0)),
        ets = ETS(Value),
        arima = ARIMA(Value, stepwise = F),
        tslm = TSLM(Value ~ trend() + season()),
        twolayer = ARIMA(Value ~ trend() + season(), stepwise = F),
        tslm.ext = TSLM(Value ~ trend() + season() + IsHoliday + WorkingHourC + ClassHourC + IsEventC + PickupsC.Lag1 + PersonC.Lag1),
        arima.ext = ARIMA(Value ~ IsHoliday + WorkingHourC + ClassHourC + IsEventC, stepwise = F),
        twolayer.ext = ARIMA(Value ~ trend() + season() + IsHoliday + WorkingHourC + ClassHourC + IsEventC + PickupsC.Lag1 + PersonC.Lag1 + PDQ(0,0,0), stepwise = F),
        nnet = NNETAR(Value),
        nnet.ext = NNETAR(Value ~ IsHoliday + WorkingHourC + ClassHourC + IsEventC + PickupsC.Lag1 + PersonC.Lag1)
        )

augmented.train = augment(fit.personC)

fitted_values <- fitted(fit.personC) |> dplyr::select(Date, .model, .fitted) |>
  rename(fitted = .fitted, model = .model)

##### residuals pairs plot #################
residual_values <- augmented.train |> dplyr::select(Date, .model, .resid) |>
  rename(residual = .resid, model = .model)
residual_plot <- residual_values %>%  
  pivot_wider(names_from = model, values_from =  residual)
ggpairs(residual_plot[,-1])

##### 1-step-ahead roll forward #################
fits.roll <- refit(fit.personC, data.person %>% filter_index("2023-09-25"~"2023-12-16"), reestimate = FALSE) %>% 
  augment() |>
  filter(Date > as.Date("2023-11-30"))

valid.roll.accuracy <- fits.roll |>
  as_tibble() |> # tsibble objects maintain each date separately
  group_by(.model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / Value), na.rm = TRUE) * 100,
    UFP = mean((.resid) < 0, na.rm = TRUE) * 100
  )

fabletools::accuracy(fit.personC)
valid.roll.accuracy

forecast_values <- fits.roll %>% as_tibble() |>
  dplyr::select(Date, .model, .fitted) |> rename(fc.value = .fitted, model = .model)

data_long <- data.person %>% dplyr::select(Date, Value) %>% mutate(
  naive = Value, snaive = Value, sample.mean = Value, ets = Value, arima = Value, arima.ext = Value,
  tslm = Value, twolayer = Value, tslm.ext = Value, arima.dummy = Value,
  twolayer.ext = Value, nnet = Value, nnet.ext = Value
)

data_long <- data_long %>%
  pivot_longer(cols = 3:15, names_to = "model", values_to = "actual")

combined_data1 <- data_long |>
  left_join(fitted_values, by = c("Date", "model")) |>
  left_join(residual_values, by = c("Date", "model")) |>
  left_join(forecast_values, by = c("Date", "model")) |>
  mutate(fc.error = Value - fc.value) 

combined_data <- combined_data1 %>% filter(model %in% c("arima", 'ets', "naive"))
combined_data$model <- factor(combined_data$model, levels = c("naive", "ets", "arima"))

##### plot #################
p1 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = Value), color = "black", linewidth = 0.8) +  # actual data
  geom_line(aes(y = fitted, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.value, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Actual Time Usage vs. Fitted and Forecasted Values",
       x = "Date", y = "Time Usage (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

p2 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = residual, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.error, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Forecast Error plot",
       x = "Date", y = "Forecast Error (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

grid.arrange(p1, p2)

lvl = c("naive", "snaive", "sample.mean", "ets", "arima", "tslm", "arima.ext", "twolayer", "twolayer.ext", "nnet", "nnet.ext")
combined_data1$model <- factor(combined_data1$model, levels = lvl)
lvlwant = c("naive", "snaive", "sample.mean", "tslm", "tslm.ext", "ets", "nnet.ext", "arima", "nnet")
# pC1 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = residual, x = model, fill = model)) + theme_bw()
# pC2 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = fc.error, x = model, fill = model)) + theme_bw()
# 
# grid.arrange(pC1, pC2)
df1 <- combined_data1 %>% filter(model %in% lvlwant) %>% select(model, residual, fc.error) %>% 
  rename(Training = residual, Validation = fc.error) %>% pivot_longer(cols = 2:3) %>% drop_na() %>%
  rename(Period = name, Error = value)
ggplot(df1, aes(model, Error, fill = Period)) + geom_boxplot() + theme_bw()

#### summmary of selected model

fit.personC %>% dplyr::select(ets) %>% report()
fit.personC %>% dplyr::select(arima) %>% report()

##### forecast #################
set.seed(123)
fit_finalC = data.person %>% filter_index(.~"2023-12-16") %>% model(ets = ETS(Value ~ error("M") + trend("N") + season("N")))
fit_finalC %>% forecast(new_data = data.person %>% filter_index("2023-12-17"))

#### Person D ####################
pp = "AllPersonD"
data.person <- qsdata_long %>% filter(Person == pp) %>% ungroup()
data.person.train <- qsdata.train %>% filter(Person == pp) %>% ungroup()
data.person.valid <- qsdata.valid %>% filter(Person == pp) %>% ungroup()

set.seed(123)

##### fit model #################
fit.personD <- data.person.train |>
  model(naive = NAIVE(Value),
        snaive = SNAIVE(Value),
        sample.mean = ARIMA(Value ~ pdq(0,0,0) + PDQ(0,0,0)),
        ets = ETS(Value),
        arima = ARIMA(Value, stepwise = F),
        tslm = TSLM(Value ~ trend() + season()),
        twolayer = ARIMA(Value ~ trend() + season(), stepwise = F),
        tslm.ext = TSLM(Value ~ trend() + season() + IsHoliday + WorkingHourD + ClassHourD + IsEventD + PickupsD.Lag1 + PersonD.Lag1),
        arima.ext = ARIMA(Value ~ IsHoliday + WorkingHourD + ClassHourD + IsEventD + PickupsD.Lag1 + PersonD.Lag1, stepwise = F),
        twolayer.ext = ARIMA(Value ~ trend() + season() + IsHoliday + WorkingHourD + ClassHourD + IsEventD + PickupsD.Lag1 + PersonD.Lag1 + PDQ(0,0,0), stepwise = F),
        nnet = NNETAR(Value),
        nnet.ext = NNETAR(Value ~ IsHoliday + WorkingHourD + ClassHourD + IsEventD + PickupsD.Lag1 + PersonD.Lag1)
  )

augmented.train = augment(fit.personD)

fitted_values <- fitted(fit.personD) |> dplyr::select(Date, .model, .fitted) |>
  rename(fitted = .fitted, model = .model)

##### residuals pairs plot #################
residual_values <- augmented.train |> dplyr::select(Date, .model, .resid) |>
  rename(residual = .resid, model = .model)
residual_plot <- residual_values %>%  
  pivot_wider(names_from = model, values_from =  residual)
ggpairs(residual_plot[,-1])

##### 1-step-ahead roll forward #################
fits.roll <- refit(fit.personD, data.person %>% filter_index("2023-09-25"~"2023-12-16"), reestimate = FALSE) %>% 
  augment() |>
  filter(Date > as.Date("2023-11-30"))

valid.roll.accuracy <- fits.roll |>
  as_tibble() |> # tsibble objects maintain each date separately
  group_by(.model) |>
  summarise(
    MAE = mean(abs(.resid), na.rm = TRUE),
    RMSE = sqrt(mean((.resid)^2, na.rm = TRUE)),
    MAPE = mean(abs((.resid) / Value), na.rm = TRUE) * 100,
    UFP = mean((.resid) < 0, na.rm = TRUE) * 100
  )

fabletools::accuracy(fit.personD)
valid.roll.accuracy

forecast_values <- fits.roll %>% as_tibble() |>
  dplyr::select(Date, .model, .fitted) |> rename(fc.value = .fitted, model = .model)

combined_data <- combined_data1 |>
  mutate(fc.error = Value - fc.value) %>% filter(model %in% c("ets", 'arima', "naive"))

data_long <- data.person %>% dplyr::select(Date, Value) %>% mutate(
  naive = Value, snaive = Value, sample.mean = Value, ets = Value, arima = Value, arima.ext = Value,
  tslm = Value, twolayer = Value, tslm.ext = Value, arima.dummy = Value,
  twolayer.ext = Value, nnet = Value, nnet.ext = Value
)

data_long <- data_long %>%
  pivot_longer(cols = 3:15, names_to = "model", values_to = "actual")

combined_data1 <- data_long |>
  left_join(fitted_values, by = c("Date", "model")) |>
  left_join(residual_values, by = c("Date", "model")) |>
  left_join(forecast_values, by = c("Date", "model")) |>
  mutate(fc.error = Value - fc.value) 

combined_data <- combined_data1 %>% filter(model %in% c("tslm", 'ets', "snaive"))
combined_data$model <- factor(combined_data$model, levels = c("snaive", "tslm", "ets"))

##### plot #################
p1 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = Value), color = "black", linewidth = 0.8) +  # actual data
  geom_line(aes(y = fitted, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.value, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Actual Time Usage vs. Fitted and Forecasted Values",
       x = "Date", y = "Time Usage (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

p2 <- ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = residual, color = model), alpha = 0.7, linewidth = 0.8) +  # fitted data
  geom_line(aes(y = fc.error, color = model), linetype = "dashed", alpha = 0.7, linewidth = 0.8) +  # forecasted data
  labs(title = "Forecast Error plot",
       x = "Date", y = "Forecast Error (Minutes)") + 
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), color = "grey40", linewidth=0.7)

grid.arrange(p1, p2)

lvl = c("naive", "snaive", "sample.mean", "tslm", "ets", "tslm.ext", "arima", "arima.ext", "twolayer", "twolayer.ext", "nnet", "nnet.ext")
combined_data1$model <- factor(combined_data1$model, levels = lvl)
lvlwant = c("naive", "snaive", "sample.mean", "tslm", "tslm.ext", "ets", "nnet.ext", "arima")
# pD1 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = residual, x = model)) + theme_bw()
# pD2 <- ggplot(combined_data1 %>% filter(model %in% lvlwant)) + geom_boxplot(aes(y = fc.error, x = model)) + theme_bw()
# 
# grid.arrange(pD1, pD2)

df1 <- combined_data1 %>% filter(model %in% lvlwant) %>% select(model, residual, fc.error) %>% 
  rename(Training = residual, Validation = fc.error) %>% pivot_longer(cols = 2:3) %>% drop_na() %>%
  rename(Period = name, Error = value)
ggplot(df1, aes(model, Error, fill = Period)) + geom_boxplot() + theme_bw()


fit.personD %>% dplyr::select(tslm) %>% report()
fit.personD %>% dplyr::select(ets) %>% report()

##### forecast #################
fit_finalD = data.person %>% filter_index(.~"2023-12-16") %>% model(tslm = TSLM(Value ~ trend() + season()))
fit_finalD %>% forecast(new_data = data.person %>% filter_index("2023-12-17"))

