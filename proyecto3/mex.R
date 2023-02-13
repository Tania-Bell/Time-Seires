knitr::opts_chunk$set(echo = FALSE)
library(tsibble)
library(GGally)
library(plotly)
library(ggplot2)
library(quantmod)
library("easypackages")
packages("tidyverse", "tidyquant", "lubridate", "patchwork", "fpp2","fpp3","scales")

desempleo <- tq_get("LRUN64TTMXQ156S", get = "economic.data", from = "2005-01-01",
                    to = "2020-01-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date) %>% 
  rename(Porcentaje = price) %>% 
  rename(Date = date)

has_gaps(desempleo, .full = TRUE)

desempleo <- desempleo %>% 
  mutate(Date = yearquarter(Date))

has_gaps(desempleo)

grafica <- autoplot(desempleo) + xlab("Año") + ylab("Porcentaje") + 
  ggtitle("Tasa de desempleo en México (2005-2019)")

grafica

boxplot <- desempleo %>% 
  ggplot(aes(x = factor(year(Date)), y = Porcentaje)) +
  xlab("Año") + ggtitle("Outliers de tasa de desempleo en México") +
  geom_boxplot()
boxplot
ggplotly(boxplot)

desempleo %>% 
  ACF(Porcentaje) %>%  autoplot() | desempleo %>% 
  PACF(Porcentaje) %>%  autoplot()

desempleo %>% 
  features(Porcentaje, unitroot_kpss)

desempleo %>%
  features(Porcentaje, ljung_box, lag = 10)

desempleo <- desempleo %>% 
  mutate(diff_Por = difference(Porcentaje))

desempleo %>% 
  features(diff_Por, unitroot_nsdiffs)

desempleo %>% 
  features(diff_Por, unitroot_kpss)

desempleo %>% 
  ACF(diff_Por) %>%  autoplot() | desempleo %>% 
  PACF(diff_Por) %>%  autoplot()

fit3 <- desempleo %>%
  model(ARIMA(Porcentaje ~ PDQ(0,0,0),
              stepwise = FALSE, approximation = FALSE))
report(fit3)

fit <- desempleo %>%
  model(
    arima110 = ARIMA(Porcentaje ~ pdq(1,1,0) + PDQ(0,0,0)),
    arima112 = ARIMA(Porcentaje ~ pdq(1,1,2) + PDQ(0,0,0)),
    arima011 = ARIMA(Porcentaje ~ pdq(0,1,1) + PDQ(0,0,0)),
    arima012 = ARIMA(Porcentaje ~ pdq(0,1,2) + PDQ(0,0,0))
  )

glance(fit)

fit %>% select(arima012) %>% gg_tsresiduals()

fit %>% 
  select(arima012) %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 24, dof = 4)

pronostico <- fit %>% select(arima012) %>% forecast(h = 6) %>% autoplot(desempleo) +
  xlab("Año") + ylab("Porcentaje") + 
  ggtitle("Tasa de desempleo en México (2005-2019)")

zoom <-  pronostico + tidyquant::coord_x_date(xlim = c("2017-01-01","2022-01-01")) + ggtitle("Comparaciones pronósticos temperatura Melbourne")

zoom

fit <- desempleo %>%
  model(
    `Seasonal naive` = SNAIVE(Porcentaje),
    hwamort = ETS(Porcentaje ~ error("M") + trend("Ad") + season("M")),
    additive = ETS(Porcentaje ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Porcentaje ~ error("M") + trend("A") + season("M")),
    `Holt` = ETS(Porcentaje ~ error("A") + trend("A") + season("N")),
    `Damped Holt` = ETS(Porcentaje ~ error("A") + trend("Ad") + season("N")),
    SES = ETS(Porcentaje ~ error("A") + trend("N") + season("N"))
  )

fc <- fit %>% forecast(h = 6)

grafica <- fc %>%
  autoplot(desempleo, level = NULL) +
  xlab("Año") + ylab("Porcentaje") +
  ggtitle("Desempleo") +
  guides(colour = guide_legend(title = "Forecast"))

zoom2 <-  grafica + tidyquant::coord_x_date(xlim = c("2017-01-01","2022-01-01")) + ggtitle("Comparaciones pronósticos temperatura Melbourne")

zoom2

acc <- accuracy(fit)

acc

# fit <- temp2 %>%
#   model(
#     hwamort = ETS(Porcentaje ~ error("M") + trend("Ad") + season("M"))
#   )

# fc <- fit %>% forecast(h = "2 years")
# 
# grafica_4 <- fc %>%
#   autoplot(temp, level = NULL) +
#   xlab("Fecha") + ylab("Grados (Celsius)") +
#   ggtitle("Mejor pronóstico temperatura Melbourne") +
#   guides(colour = guide_legend(title = "Forecast"))
# 
# zoom3 <-  grafica + tidyquant::coord_x_date(xlim = c("1989-01-01","1991-01-01")) + ggtitle("Mejor pronóstico temperatura Melbourne 1989-1991")

