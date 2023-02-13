knitr::opts_chunk$set(echo = FALSE)
library(tsibble)
library(GGally)
library(plotly)
library(patchwork)
library(ggplot2)
#library(fable.prophet)
library("easypackages")
packages("tidyverse", "tidyquant", "lubridate", "patchwork", "fpp2","fpp3",
         "fabletools","scales")

# import unemp ---------------------------------------------------------

unemp <- tq_get("UNRATE", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol)

unemp <- unemp %>% 
  as_tsibble(index = date)

unemp <- unemp %>% index_by(Date = yearquarter(date)) %>% summarise(sum(price))

names(unemp)[2]<- 'Unemployment'
names(unemp)[1]<- 'Date'

has_gaps(unemp, .full = TRUE)

unemp<- unemp %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearquarter(Date))

has_gaps(unemp)
unemp

# import inf --------------------------------------------------------------

inf <- tq_get("T10YIEM", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date)
inf

inf <- inf %>% index_by(Date = yearquarter(date)) %>% summarise(mean(price))

names(inf)[2]<- 'Inflation'

inf<- inf %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearquarter(Date))
inf


# import produccion -------------------------------------------------------

prod <- tq_get("ULQELP01USQ661S", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date) %>% 
  rename(Date = date)
prod

names(prod)[2]<- 'Productivity'

prod <- prod %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearquarter(Date))
prod


# import pib --------------------------------------------------------------

gdp <- tq_get("A191RL1Q225SBEA", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date) %>% 
  rename(Date = date)
gdp

names(gdp)[2]<- 'GDP'

gdp <- gdp %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearquarter(Date))

gdp


# import curr -----------------------------------------------------------

curr <- tq_get("CURRCIR", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date)
curr

curr <- curr %>% index_by(Date = yearquarter(date)) %>% summarise(sum(price))

names(curr)[2]<- 'Currency'

curr <- curr %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearquarter(Date))

curr

# datos -------------------------------------------------------------------
usa <- bind_cols(unemp, inf$Inflation, prod$Productivity, gdp$GDP, curr$Currency) %>% 
  as_tsibble(index = Date)
usa
names(usa)[3]<- 'Inflation'
names(usa)[4]<- 'Productivity'
names(usa)[5]<- 'GDP'
names(usa)[6]<- 'Currency'

usa

# graficas ----------------------------------------------------------------

linea <- ggplot(data = usa) +
  geom_line(aes(x = Date, y = Unemployment, color = "Desempleo (%)")) +
  geom_line(aes(x = Date, y = Inflation, color = "Inflacion (%)")) +
  geom_line(aes(x = Date, y = Productivity, color = "Productivity (puntos)")) +
  geom_line(aes(x = Date, y = GDP, color = "GDP (%)")) +
  geom_line(aes(x = Date, y = Currency, color = "Currency")) +
  ggtitle("Indicadores economicos en EEUU") +
  guides(colour = guide_legend(title="Indicadores")) + ylab("Indicadores") +
  xlab("Fecha")
linea 
autoplot(usa$)
fun_grafica <- function(variable) {
  usa %>% 
    autoplot(variable)
}
fun_grafica("Unemployment")
desc_inf <- usa %>%
  model(STL(Inflation)) %>%
  components() %>%
  autoplot() + xlab("Fecha") + ylab("Tasa de inflacion (%)") +
  ggtitle("Descomposicion de la inflacion") +
  theme(legend.position = "bottom")
desc_inf

# modelos previos ---------------------------------------------------------

modelos <-  usa %>%  
  model(
    hwamort = ETS(Inflation ~ error("M") + trend("Ad") + season("M")),
    additive = ETS(Inflation ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Inflation ~ error("M") + trend("A") + season("M")),
    `Holt` = ETS(Inflation ~ error("A") + trend("A") + season("N")),
    `Damped Holt` = ETS(Inflation ~ error("A") + trend("Ad") + season("N")),
    SES = ETS(Inflation ~ error("A") + trend("N") + season("N"))
  ) %>%  
  forecast(h = 8) %>%  
  autoplot(inf, level = NULL) + 
  xlab("Fecha") + ylab("Porcentaje") +
  ggtitle("Comparaciones pronosticos de Tasa de Inflacion") +
  guides(colour = guide_legend(title = "Forecast")) 

zm_modelos <-  modelos + tidyquant::coord_x_date(xlim = c("2018-01-01","2022-01-01"))
zm_modelos

# residuales --------------------------------------------------------------

resid <- inf %>%
  gg_tsdisplay((Inflation), plot_type = "partial") +
  ggtitle("Residuales de tasa de inflacion EEUU")
resid

# arima -------------------------------------------------------------------

fit_arima <- usa %>% 
  model(arima = ARIMA(Inflation,
                      stepwise = FALSE, approximation = FALSE))
fit_arima

resid_fit_arima <- fit_arima %>% select(arima) %>% gg_tsresiduals() +
  ggtitle("Residuales de ARIMA seleccionado para EEUU")

resid_fit_arima

fc_arima <- fit_arima %>% select(arima) %>% 
  forecast(h = 8) %>% autoplot(inf)+ 
  ggtitle("Pronostico ARIMA para tasa de inflacion de EEUU")
fc_arima
zm_arima <-  fc_arima + tidyquant::coord_x_date(xlim = c("2017-01-01","2022-01-01"))
zm_arima

# regresion ---------------------------------------------------------------

corr <- usa %>% 
  as_tibble() %>% 
  select(-Date) %>% 
  GGally::ggpairs()
corr

fit_reg <- usa %>% 
  model(reg_lin_simple = TSLM(Inflation ~ Unemployment + Productivity + GDP + Currency)
  )

report(fit_reg)

grafica_fit_reg <- augment(fit_reg) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Inflation, color = "Datos")) +
  geom_line(aes(y = .fitted, color = "Fitted"))+
  xlab("Fecha") + ylab(NULL) +
  ggtitle("Cambios porcentuales en la inflacion de EEUU") +
  guides(color = guide_legend(title = NULL))
grafica_fit_reg

linea_puntitos_reg <- augment(fit_reg) %>% 
  ggplot(aes(x = Inflation, y = .fitted)) +
  geom_point() +
  ylab("Fitted (valores ajustados)") +
  xlab("Datos (reales hist?ricos)") +
  ggtitle("Cambios porcentuales en la inflacion en EEUU") +
  geom_abline(intercept = 0, slope = 1)

linea_puntitos_reg

resid_fit_reg <- fit_reg %>% 
  gg_tsresiduals() +
  ggtitle("Residuales de tasa de inflacion EEUU")
resid_fit_reg

ljung_fit_reg <- augment(fit_reg) %>% 
  features(.resid, ljung_box, lag= 10, dof = 2)
ljung_fit_reg


# accuracy ----------------------------------------------------------------

acc <- usa %>% 
  model(r1 = TSLM(Inflation ~ Currency),
        r2 = TSLM(Inflation ~ Currency + GDP),
        r3 = TSLM(Inflation ~ Currency + GDP + Productivity),
        r4 = TSLM(Inflation ~ Currency + GDP + Productivity + Unemployment),
        r5 = TSLM(Inflation ~ Currency + GDP + Unemployment)
  )
acc2 <- acc %>% 
  glance() %>% 
  select(.model, adj_r_squared, AIC, AICc, BIC)

selecc <- acc %>% 
  select(r4) %>% 
  report()
selecc

# pronostico reg lineal ----------------------------------------------------------
curr
optimista <- new_data(usa,8) %>% 
  mutate(Unemployment = 10, 
         GDP = 15, 
         Productivity = 70,
         Currency = 140)

pesimista <- new_data(usa,8) %>% 
  mutate(Unemployment = 8, 
         GDP = 12, 
         Productivity = 40,
         Currency = 100)
usa
fit_escenarios <- usa %>% 
  model(lineal = TSLM(Inflation ~ Currency + GDP + Productivity + Unemployment))

fc_optimista <- forecast(fit_escenarios, new_data = optimista) %>% 
  mutate(Escenario = "Optimista") %>% 
  as_fable(response = "Inflation", key = c("Escenario",".model"))


fc_pesimista <- forecast(fit_escenarios, new_data = pesimista) %>% 
  mutate(Escenario = "Pesimista") %>% 
  as_fable(response = "Inflation", key = c("Escenario",".model"))
fc1 <- usa %>%
  autoplot(Inflation) +
  autolayer(fc_optimista) +
  xlab("Fecha") + ggtitle("Optimista") +
  ylab("%")
fc2 <- usa %>%
  autoplot(Inflation) +
  autolayer(fc_pesimista) +
  xlab("Fecha") + ggtitle("Pesimista") +
  ylab("%")
fc1/fc2

gdp_pronostico <- usa %>% 
  model(ETS = ETS(GDP),
        ARIMA = ARIMA(GDP)
  ) %>% fabletools::forecast(h = 8)
gdp_plot <- gdp_pronostico %>% 
  autoplot(gdp, level = NULL) + xlab("Fecha") +
  theme(legend.position = "none")

curr_pronostico <- usa %>% 
  model(ETS = ETS(Currency),
        ARIMA = ARIMA(Currency)
  ) %>% fabletools::forecast(h = 8)
curr_plot <- curr_pronostico %>% 
  autoplot(curr, level = NULL)+ xlab("Fecha") +
  theme(legend.position = "none")

prod_pronostico <- usa %>% 
  model(ETS = ETS(Productivity),
        ARIMA = ARIMA(Productivity)
  ) %>% fabletools::forecast(h = 8)
prod_plot <- prod_pronostico %>% 
  autoplot(prod, level = NULL)+ xlab("Fecha") +
  theme(legend.position = "none")

unemp_pronostico <- usa %>% 
  model(ETS = ETS(Unemployment),
        ARIMA = ARIMA(Unemployment)
  ) %>% fabletools::forecast(h = 8)

unemp_plot <- unemp_pronostico %>% 
  autoplot(unemp, level = NULL)+ xlab("Fecha") +
  theme(legend.position = "none")

plots <- (gdp_plot + unemp_plot) / (prod_plot + curr_plot)

fit_reglin <- usa %>% 
  model(
    `Regresion lineal multiple` = TSLM(Inflation ~ Unemployment + Productivity + GDP + Currency)
  )

datos_fut <- new_data(usa,8) %>% 
  mutate(Unemployment = unemp_pronostico %>% filter(.model == "ARIMA") %>% pull(.mean), 
         Currency = curr_pronostico %>% filter(.model == "ARIMA") %>% pull(.mean),
         Productivity = prod_pronostico %>% filter(.model == "ARIMA") %>% pull(.mean),
         GDP = gdp_pronostico %>% filter(.model == "ARIMA") %>% pull(.mean))

datos_fut

fc <- forecast(fit_reglin, datos_fut)

fc %>% 
  autoplot(usa)

fc %>% 
  autoplot()