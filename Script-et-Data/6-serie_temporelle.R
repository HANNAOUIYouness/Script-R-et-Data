library(dplyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(urca)
library(tseries)
library(forecast)
library(prophet)


serie_temporelle <- Data_durees %>%
  mutate(DATE_REFERENCE_FLUX = as.Date(DATE_REFERENCE_FLUX)) %>%
  mutate(T = floor_date(DATE_REFERENCE_FLUX, "month")) %>%
  group_by(T) %>%
  summarize(budget = sum(MTT_TOTAL_RSA))

# Exclure la valeur correspondant à "2023-05-01"
serie_temporelle <- serie_temporelle[serie_temporelle$T != as.Date("2023-05-01"), ]

# Exclure également les années 2013 et 2014
serie_temporelle <- serie_temporelle[serie_temporelle$T >= as.Date("2015-01-01"), ]

serie_temporelle$budget <- as.numeric(serie_temporelle$budget)

ggplot(serie_temporelle) +
  geom_line(aes(x = T, y = budget, color = "budget"), size = 1) +
  #geom_line(aes(x = T, y = budget_reelle, color = "budget_reelle"), size = 1) +
  labs(x = "Date", y = "Budget", title = "Variation mensuel de janvier 2013 à avril 2023") +
  scale_color_manual(values = c("budget" = "blue"))



# Convertir en série temporelle (TS)
ST <- ts(serie_temporelle$budget, start = c(2015, 1), frequency = 12)

# Appliquer la décomposition
decompsition <- decompose(ST)
trend <- decompsition$trend
seasonal <- decompsition$seasonal
random <- decompsition$random

# Visualiser les composants décomposés
plot(decompsition)
# trend et la serie 
# Créer un dataframe pour les données de tendance et de série originale
data <- data.frame(T = index(trend), Trend = trend, ST = ST)

# Tracer le graphique
ggplot(data) +
  geom_line(aes(x = T, y = Trend, color = "Tendance"), size = 1) +
  geom_line(aes(x = T, y = ST, color = "ST"), size = 1) +
  labs(x = "Date", y = "Valeur", title = "Tendance et Série Originale") +
  scale_color_manual(values = c("Tendance" = "blue", "ST" = "red")) +
  theme_minimal()

#Serie desaisonnalisees
#éliminé les variations saisonnières de la série temporelle:
#en estimant la composante saisonnière et en soustrayant la composante saisonnière estimée de la série chronologique originale
ST_adjusted <- ST - seasonal
ST_adjusted

plot(ST_adjusted)

#la stationnarité
#fonction d'autocorrélation : AFC 
acf <- acf(ST_adjusted)
plot(acf)
acf

#fonction d'autocorrélation partielle :PAFC
pacf <- pacf(ST_adjusted)
plot(pacf)
pacf

#Test de stationnarité sur la série avant differenciation 
#Le test de Dickey-Fuller
adf.test(ST_adjusted, alternative = 'stationary')

#p-value = 0.4452
#on ne peut pas rejette l'hypothèse nulle de non-stationnarité de la série temporell

#différenciation d'ordre 1 pour obtenir une série sationnaire qui indique les différences.
serie_diff1 <- diff(ST_adjusted, differences = 1)
plot(serie_diff1)

#Prévisions à l’aide du lissage exponentiel

#on peut ajuster une exponentielle simple lissage du modèle prédictif à l’aide du Fonction « HoltWinters() » dans R
#HoltWinters() pour un lissage exponentiel simple,

serie_diff1_forecast <- HoltWinters(serie_diff1, beta=FALSE, gamma=FALSE)
serie_diff1_forecast 
#alpha: 0.04218112

serie_diff1_forecast$fitted

#tracer la série chronologique originale
plot(serie_diff1_forecast)

#calculer la somme du carré les erreurs pour les erreurs de prévision dans l’échantillon

serie_diff1_forecast$SSE


#une prévision sur l'horison "h = 12"
serie_diff1_forecast2 <- forecast(serie_diff1_forecast, h=12)
serie_diff1_forecast2

#Les prévisions. une prédiction de 80% intervalle pour la prévision et un intervalle de prédiction de 95 % pour la prévision.
plot(serie_diff1_forecast2)

#Analyse des AFC residu 
acf(na.omit(serie_diff1_forecast2$residuals))

# vérifier si les erreurs de prévision sont normalement distribuées avec une moyenne nulle et une variance constante.
plot.ts(serie_diff1_forecast2$residuals)

#distrubution normale 
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(na.omit(serie_diff1_forecast2$residuals))


#Le graphique montre que la distribution des erreurs de prévision est fortement centrée sur zéro, et est plus ou moins normalement distribué

####le lissage exponontiel en prenant compte de la saisonalité 
logST <- log(ST)
HW_ST <- HoltWinters(logST)
HW_ST

# La valeur d’alpha (0.719) est relativement fort, ce qui indique que l’estimation du niveau à l’heure actuelle est basé principalement sur des observations dans un passé plus lointain.

#SSE
HW_ST$SSE
#0.0562

plot(HW_ST)
#les valeurs prévues sous forme de ligne rouge
#Nous voyons dans le graphique que la méthode exponentielle de Holt-Winters est très efficace pour prédire la tendance.

#prévisions pour les temps futurs
HW_ST2 <- forecast(HW_ST, h=12)
plot(HW_ST2)

#étudier si le modèle prédictif peut être amélioré en vérifiant la correlation des erreurs de prévision
acf(na.omit(HW_ST2$residuals, lag.max=20))

#Le test de Box-Ljung, également connu sous le nom de test de Ljung-Box

Box.test(HW_ST2$residuals, lag=20, type="Ljung-Box")
#p-value = 0.0001469

###Prophet

# Créez un dataframe avec les colonnes "ds" et "y"
data <- data.frame(ds = serie_temporelle$T, y = serie_temporelle$budget)

# Créez le modèle Prophet
model <- prophet(data)

# Obtenez les prédictions
future <- make_future_dataframe(model, periods = 12,freq = "month")  # Prévisions pour les 12 prochains mois
tail(future)
forecast <- predict(model, future)
forecast

#Plot estimation

dyplot.prophet(model,forecast)
A <- prophet_plot_components(model,forecast)
A
