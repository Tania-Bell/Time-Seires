knitr::opts_chunk$set(echo = FALSE)
library(tsibble)
library(GGally)
library(plotly)
library(ggplot2)
# library(quantmod)
library("easypackages")
packages("tidyverse", "tidyquant", "lubridate", "patchwork", "fpp2","fpp3",
         "fabletools","scales")
 


# import data mex -------------------------------------------------------------

mex <- tq_get("LRUN64TTMXQ156S", get = "economic.data", from = "2005-01-01",
                    to = "2020-01-01") %>% 
  select(-symbol) %>% 
  as_tsibble(index = date) %>% 
  rename(Porcentaje = price) %>% 
  rename(Date = date) %>% 
  mutate(Periodo = case_when(
    year(Date) <= 2008 ~ "Antes",
    year(Date) <= 2012 ~ "Durante",
    year(Date) <= 2016 ~ "Recuperacion",
    TRUE ~ "Despues"
  )
  )
    
has_gaps(mex, .full = TRUE)

mex <- mex %>% 
  mutate(Date = yearquarter(Date))

has_gaps(mex)


# import data usa ---------------------------------------------------------

usa <- tq_get("UNRATE", get="economic.data", from = "2005-01-01",to = "2019-12-01") %>% 
  select(-symbol)


usa <- usa %>% 
  as_tsibble(index = date)


usa <- usa %>% index_by(Date = yearquarter(date)) %>% summarise(sum(price))
names(usa)[2]<- 'Porcentaje'
names(usa)[1]<- 'Date'

usa <- usa %>% 
  mutate(Periodo = case_when(
    year(Date) <= 2008 ~ "Antes",
    year(Date) <= 2012 ~ "Durante",
    year(Date) <= 2016 ~ "Recuperacion",
    TRUE ~ "Despues"
  )
  )


# graficas ----------------------------------------------------------------

linea <- function(info, nombre){
  autoplot(info) + xlab("Fecha") + ylab("Porcentaje") + 
    ggtitle(paste0("Tasa de desempleo en ", nombre))
}

box <- function(info, nombre){
  info %>% 
    ggplot(aes(x = factor(year(Date)), y = Porcentaje)) +
    geom_boxplot()+
    facet_wrap(~Periodo, scales = "free") +
    xlab("Fecha") + ggtitle(paste0("Outliers de tasa de desempleo en ", nombre)) +
    geom_boxplot()
}

# descomposicion ----------------------------------------------------------
desc <- function(info){
  info %>%
    model(STL(Porcentaje)) %>%
    components() %>%
    autoplot() + xlab("Fecha") + ylab("Tasa de Desempleo (%)") +
    ggtitle(paste("Descomposicion")) +
    theme(legend.position = "bottom")
}

# modelos previos ---------------------------------------------------------
modelos <-  usa %>%  
    model(
      hwamort = ETS(Unemployment ~ error("M") + trend("Ad") + season("M")),
      additive = ETS(Unemployment ~ error("A") + trend("A") + season("A")),
      multiplicative = ETS(Unemployment ~ error("M") + trend("A") + season("M")),
      `Holt` = ETS(Unemployment ~ error("A") + trend("A") + season("N")),
      `Damped Holt` = ETS(Unemployment ~ error("A") + trend("Ad") + season("N")),
      SES = ETS(Unemployment ~ error("A") + trend("N") + season("N"))
    ) %>%  
    forecast(h = "1 year") %>%  
    autoplot(unemp, level = NULL) + 
    xlab("Fecha") + ylab("Porcentaje") +
    ggtitle("Comparaciones pronosticos de Tasa de Desempleo") +
    guides(colour = guide_legend(title = "Forecast")) 
modelos

# mejor pronostico --------------------------------------------------------
fit <- function(info){
  info %>% 
    model(
      hwamort = ETS(Porcentaje ~ error("M") + trend("Ad") + season("M")),
      additive = ETS(Porcentaje ~ error("A") + trend("A") + season("A")),
      multiplicative = ETS(Porcentaje ~ error("M") + trend("A") + season("M")),
      `Holt` = ETS(Porcentaje ~ error("A") + trend("A") + season("N")),
      `Damped Holt` = ETS(Porcentaje ~ error("A") + trend("Ad") + season("N")),
      SES = ETS(Porcentaje ~ error("A") + trend("N") + season("N"))
    )
}

mejor_fc <- function(info){
  info %>% 
  model(
    hwamort = ETS(Porcentaje ~ error("M") + trend("Ad") + season("M")),
  ) %>% 
  forecast(h = 8) %>% 
  autoplot(info, level = NULL) +
  xlab("Fecha") + ylab("Porcentaje") +
  ggtitle("Mejor pronostico tasa de desempleo") +
  guides(colour = guide_legend(title = "Forecast"))
}

# diferenciaciones -------------------------------------------------------------------
ndiff_mex <- mex %>% 
  features(Porcentaje, unitroot_ndiffs)

ndiff_usa <- usa %>% 
  features(Porcentaje, unitroot_ndiffs)

kpss_mex <- mex %>% 
  features(Porcentaje, unitroot_kpss)

kpss_usa <- usa %>% 
  features(Porcentaje, unitroot_kpss)

nsdiff_mex <- mex %>% 
  features(Porcentaje, unitroot_nsdiffs)

nsdiff_usa <- usa %>% 
  features(Porcentaje, unitroot_nsdiffs)

# residuales --------------------------------------------------------------
residual_usa <- function(info){
  info %>%
    gg_tsdisplay(difference(Porcentaje), plot_type = "partial") +
    ggtitle("Residuales de tasa de desempleo EEUU")
}

residual_mex <- function(info){
  info %>%
    gg_tsdisplay(Porcentaje, plot_type = "partial") +
    ggtitle("Residuales de tasa de desempleo Mexico")
}

# seleccion ARIMA ---------------------------------------------------------
fit_mex <- mex %>%
  model(ARIMA(Porcentaje,
              stepwise = FALSE, approximation = FALSE))

fit_usa <- usa %>%
  model(ARIMA(Porcentaje,
                          stepwise = FALSE, approximation = FALSE))

fit_prueba_mex <- mex %>%
  model(
    arima100 = ARIMA(Porcentaje ~ pdq(1,0,0) + PDQ(0,0,0)),
    arima101 = ARIMA(Porcentaje ~ pdq(1,0,1) + PDQ(0,0,0)),
    arima102 = ARIMA(Porcentaje ~ pdq(1,0,2) + PDQ(0,0,0)),
    arima200 = ARIMA(Porcentaje ~ pdq(2,0,0) + PDQ(0,0,0)),
    arima201 = ARIMA(Porcentaje ~ pdq(2,0,1) + PDQ(0,0,0)),
    arima202 = ARIMA(Porcentaje ~ pdq(2,0,2) + PDQ(0,0,0)),
    arima001 = ARIMA(Porcentaje ~ pdq(0,0,1) + PDQ(0,0,0))
  )

fit_prueba_usa <- usa %>%
  model(
    arima010_101 = ARIMA(Porcentaje ~ pdq(0,1,0) + PDQ(1,0,1)),
    arima111_100 = ARIMA(Porcentaje ~ pdq(1,1,1) + PDQ(1,0,0)),
    arima012_001 = ARIMA(Porcentaje ~ pdq(0,1,2) + PDQ(0,0,1)),
    arima011_001 = ARIMA(Porcentaje ~ pdq(0,1,1) + PDQ(0,0,1)),
    arima112_100 = ARIMA(Porcentaje ~ pdq(1,1,2) + PDQ(1,0,0)),
    arima110_002 = ARIMA(Porcentaje ~ pdq(1,1,0) + PDQ(0,0,2)),
    arima011_101 = ARIMA(Porcentaje ~ pdq(0,1,1) + PDQ(1,0,1))
  )

# resid ARIMA seleccionado -----------------------------------------------------

residual_arima_mex <- function(fitvariable){
  fitvariable %>% select(arima102) %>% gg_tsresiduals() +
    ggtitle("Residuales de ARIMA seleccionado para Mexico")
}

residual_arima_mex(fit_prueba_mex)

residual_arima_usa <- function(fitvariable){
  fitvariable %>% select(arima110_002) %>% gg_tsresiduals() +
    ggtitle("Residuales de ARIMA seleccionado para EEUU")
}

residual_arima_usa(fit_prueba_usa)

ruido_mex <- fit_prueba_mex %>% 
  select(arima102) %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 12, dof = 4)

ruido_usa <- fit_prueba_usa %>% 
  select(arima110_002) %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 12, dof = 4)

# pronostico --------------------------------------------------------------

ARIMA_mex <- fit_prueba_mex %>% select(arima102) %>% 
  forecast(h = 8) %>% autoplot(mex) + 
  ggtitle("Pronostico ARIMA para tasa de desempleo de Mexico")

ARIMA_usa <- fit_prueba_usa %>% select(arima110_002) %>% 
  forecast(h = 8) %>% autoplot(usa)+ 
  ggtitle("Pronostico ARIMA para tasa de desempleo de EEUU")


