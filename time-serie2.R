install.packages("readxl")
install.packages("forecast")
install.packages("tseries")
# Za³adowanie bibliotek 
library(quantmod) 
library(lattice)
library(forecast)
library(tseries)
library("readxl")


# Wczytanie danych
szereg1 <- read_xls("D:/studia/2rok 2 semestr in¿ynieria i analiza danych/szeregi czasowe/time series/szereg1.xls", na=" ")
?read_xls

# Szereg1 dotyczy dotycz¹cy produkcji energii elektrycznej w Chinach


# sprawdzenie jak nasze szeregi wygl¹daj¹ 
View(szereg1)
head(szereg1)


# Zmiana typu danych z DataFrame na TimeSeries
szereg1_ts <- ts(szereg1[ ,2], start=c(1999,1), frequency = 12)


#Wykresy szereg1
par(mar=c(5,5,3,2)) 

plot(szereg1_ts, xlab = ("Rok"), ylab = ("Gigawatogodziny"), year.labels = TRUE, main="Produkcja enegrii elektycznej: Chiny")

monthplot(szereg1_ts, xlab = ("Miesi¹c"), ylab = ("Gigawatogodziny"), main="Produkcja enegrii elektycznej: Chiny")

seasonplot(szereg1_ts, xlab = ("Miesi¹c"), ylab = ("Gigawatogodziny"), year.labels = TRUE, col = rainbow(25), pch =19, type = "o", main="Produkcja enegrii elektycznej: Chiny")

boxplot(szereg1_ts ~ cycle(szereg1_ts), col = "firebrick1", border = "black", main="Produkcja enegrii elektycznej: Chiny")

lag.plot(szereg1_ts, diag.col = "red", type = "p", lags = 12, do.lines=FALSE, main="Produkcja enegrii elektycznej: Chiny")

acf(szereg1_ts, main="Produkcja enegrii elektycznej: Chiny", lag.max = 48)

pacf(szereg1_ts, main="Produkcja enegrii elektycznej: Chiny", lag.max = 48)



# Zastosowanie œrednich ruchomych (lewostronnych) do wyg³adzania wykresu
# srednia 2-miesieczna
szereg1_œr_ruchoma <- filter(szereg1_ts, sides = 1, filter = rep(1/2,2)) 
# srednia 6-miesieczna
szereg1_œr_ruchoma_1 <- filter(szereg1_ts, sides = 1, filter = rep(1/6,6)) 
# srednia 12-miesieczna
szereg1_œr_ruchoma_2 <- filter(szereg1_ts, sides = 1, filter = rep(1,12)/12)

# Wizualizacja rednich ruchomych 
plot(szereg1_ts,xlab = ("Rok"), ylab = ("Gigawatogodziny"), year.labels = TRUE, main="Produkcja enegrii elektycznej: Chiny")
lines(szereg1_œr_ruchoma,col="deeppink", lwd=2)
lines(szereg1_œr_ruchoma_1,col="chartreuse", lwd=2)
lines(szereg1_œr_ruchoma_2,col="blue", lwd=2)


# Dekompozycje na podstawie modelu regresji: trend liniowy/wielomianowy, sezonowoœæ
szereg1_T <- tslm(szereg1_ts ~ trend) 
szereg1_S <- tslm(szereg1_ts ~ season) 
szereg1_TS <- tslm(szereg1_ts ~ trend + season)
szereg1_TBS <- tslm((szereg1_ts) ~ trend + season, lambda = 0) 
# Trend wielomianowy
szereg1_WS <- tslm(szereg1_ts ~ poly(trend,raw=TRUE,degree=2)+season)
szereg1_WSB <- tslm(szereg1_ts ~ poly(trend,raw=TRUE,degree=2)+season, lambda = 0) #model kwadratowej funkcji trendu po transformacji Box-Coxa.
summary(szereg1_TBS) # Informacje o przyk³adowym z wybranych modeli

# Wizualizacja dekompozycji na podstawie modelu regresji  
plot(szereg1_ts,xlab = ("Rok"), ylab = ("Gigawatogodziny"), year.labels = TRUE, main="Produkcja enegrii elektycznej: Chiny", lwd=2)
lines(fitted(szereg1_T), col = "red", lwd=3) # Wizualizacja wykrytego trendu
lines(fitted(szereg1_S), col = "darkblue", lwd=2, lty = 2)  # Wizualizacja wykrytej sezonowoœci
lines(fitted(szereg1_TS), col = "green", lwd=2) # Wizualizacja wykrytych: trendu, sezonowoœæi
lines(fitted(szereg1_TBS), col = "gold", lwd=2) # Wizualizacja wykrytych: jw., wariancji
plot(szereg1_ts,xlab = ("Rok"), ylab = ("Gigawatogodziny"), year.labels = TRUE, main="Produkcja enegrii elektycznej: Chiny", lwd=2)
lines(fitted(szereg1_WS), col = "gold", lwd=2, lty=2) # Wizualizacja wykrytych: jw, trend kwadratowy
lines(fitted(szereg1_WSB), col = "red", lwd=2, lty=2) # Wizualizacja wykrytych: jw, trend kwadratowy

# Wykres reszty 
tsdisplay(residuals(szereg1_WS), main = "Reszty: kwadratowa funkcja trendu po trnasformacji B-C")

# Dekompozycja 
szereg1_D <- decompose((szereg1_ts), type = "additive") 
tsdisplay(szereg1_D$random) # Wykryte reszty z funkcji decompose()
tsdisplay(residuals(szereg1_T)) # Wyeliminowany trend
tsdisplay(residuals(szereg1_S)) # Wyeliminowana sezonowoœæ
tsdisplay(residuals(szereg1_TS)) # Wyeliminowany trend i sezonowoœæ
tsdisplay(residuals(szereg1_TBS)) # Wyeliminowany trend i sezonowoœæ, stabilizacja wariancji
tsdisplay(residuals(szereg1_WSB)) # Wyeliminowany trend wielomianowy, sezonowoœæ, stabilizacja wariancji


# Wykres dekompozycji addytywnej
szereg1_PD <- decompose(szereg1_ts, type="additive")
plot(szereg1_PD)

# Wykres dekompozycji multiplikatywnej
szereg1_PD1 <- decompose(szereg1_ts, type="multiplicative")
plot(szereg1_PD1)


# Eliminacja sezonowoœci przed identyfikacj¹ trendu 
szereg1_odsezonowane <- seasadj(szereg1_PD1)
plot(szereg1_ts,xlab = ("Rok"), ylab = ("Gigawatogodziny"), year.labels = TRUE, main="Produkcja enegrii elektycznej: Chiny")
lines(szereg1_odsezonowane, col="red", lty=2, lwd=2)
tsdisplay(szereg1_odsezonowane)
acf(szereg1_odsezonowane)

# Uczynienie szeregu stacjonarnym - usuniêcie trendu, sezonowoœci: ró¿nicowanie, 
# Sprawdzenie, czy s¹ realizacj¹ szumu bia³ego, którego rzêdu modele AR(p), MA(q) warto braæ pod uwagê
ndiffs(szereg1_odsezonowane) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem wartoœci jeden
nsdiffs(szereg1_odsezonowane) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem sezonowym 
hist(szereg1_odsezonowane,main="Histogram reszt") 

# Zastosowanie trensformacji Boxa-Coxa
szereg1_odsezonowane <- BoxCox(szereg1_odsezonowane, lambda = "auto")
szereg1_tsMS <- diff(x=szereg1_odsezonowane, lag=1, differences=1) 
hist(szereg1_tsMS,main="Histogram reszt") 
# Rozk³ad wartoœci jest zbli¿ony do normalnego.
tsdisplay(szereg1_tsMS,lag.max=200)


# Sprawdzenie stacjonarnoœci szeregu 
library(tseries)
(pwynik <- adf.test(szereg1_tsMS)$p.value)
ifelse(pwynik < 0.01,
       "Odrzucamy hipotezê zerow¹ na poziomie istotnoœci 0.01", "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 0.01") 
# Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 0.01. Hipoteza zerowa: nie stacjonarny. 

# Wyznaczamy rzêdy p oraz q modeli AR(p) oraz MA(q)
Acf(szereg1_tsMS,lag.max=70) # Warto sprawdziæ MA(61), MA(37), MA(35)
Pacf(szereg1_tsMS,lag.max = 50) #warto sprawdziæ AR(35), AR(2), AR(11)

# Wyznaczamy wspó³czynniki modelu autoregresji z wykorzystaniem funkcji ar() 
# oraz okreœlonego wczeœniej rzêdu p u¿ywaj¹c dwóch ró¿nych metod estymacji. 

# Zajmê sie rzêdem 11

szereg1_sr_yule <- ar(szereg1_tsMS, aic = FALSE, order.max =11, method = c("yule-walker")) 
# Mo¿na ewentualnie zobaczyæ strukturê tego modelu: str(TimeSerialMS.ar12)
print(szereg1_sr_yule) # Wyznaczenie wspó³czynników modelu

szereg1_sr_ols <- ar(szereg1_tsMS, aic = FALSE, order.max =11, method = c("ols")) 
print(szereg1_sr_ols) # Wyznaczenie wspó³czynników modelu

szereg1_sr_mle <- ar(szereg1_tsMS, aic = FALSE, order.max =11, method = c("mle")) 
print(szereg1_sr_mle) # Wyznaczenie wspó³czynników modelu

szereg1_sr_yw <- ar(szereg1_tsMS, aic = FALSE, order.max =11, method = c("yw")) 
print(szereg1_sr_yw) # Wyznaczenie wspó³czynników modelu

szereg1_sr_burg <- ar(szereg1_tsMS, aic = FALSE, order.max =11, method = c("burg")) 
print(szereg1_sr_burg) # Wyznaczenie wspó³czynników modelu

# Automatycznie dobrana wartoœæ rzêdu dla metody yule-walker
szereg1_sr_yw <- ar(szereg1_tsMS, aic = TRUE, order.max = 100, method = c("yule-walker"))
print(szereg1_sr_yw)
# Okaza³o siê ¿e rzêdem najlepiej dopasowanym jest rz¹d 4

# Automatycznie dobrana wartoœæ rzêdu dla metody yw
szereg1_sr_yww <- ar(szereg1_tsMS, aic = TRUE, order.max = 100, method = c("yw"))
print(szereg1_sr_yww)
# Równie¿ otrzymujemy rz¹d 4 

# Automatycznie dobrana wartoœæ rzêdu dla metody ols
szereg1_sr_ols <- ar(szereg1_tsMS, aic = TRUE, order.max = 100, method = c("ols"))
print(szereg1_sr_ols)
# Rz¹d 98 

# Automatycznie dobrana wartoœæ rzêdu dla metody burg
szereg1_sr_burg <- ar(szereg1_tsMS, aic = TRUE, order.max = 100, method = c("burg"))
print(szereg1_sr_burg)
# Równie¿ otrzymujemy rz¹d 4

# Arima
Arimaa <- Arima(szereg1_tsMS, order = c(11,0,0))
summary(Arimaa)

# Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima() oraz wyznaczenie ich wspó³czynników
(szereg1_auto_aicc <- auto.arima(szereg1_tsMS, ic = "aicc"))
summary(szereg1_auto_aicc) 
# Wyznaczono: ARIMA(1,0,2)(0,0,1)[12] z zerow¹ œredni¹ 
(szereg1_auto_aic <- auto.arima(szereg1_tsMS, ic= "aic"))
summary(szereg1_auto_aic) 
# Wyznaczono: ARIMA(1,0,2)(0,0,1)[12] z zerow¹ œredni¹ 
(szereg1_auto_bic <- auto.arima(szereg1_tsMS, ic= "bic"))
summary(szereg1_auto_bic) 
# Wyznaczono: ARIMA(1,0,1)(0,0,1)[12] z zerow¹ œredni¹ 
# Wspó³czynniki ka¿dego z trzech powy¿szych modeli s¹ bardzo zbli¿one, a pierwszych dwóch nawet identyczne. 


# Sprawdzenie, czy otrzymane modele mo¿na uznaæ za szum bia³y
tsdisplay(szereg1_auto_bic$residuals, lag.max = 100)
str(szereg1_auto_bic)
Acf(szereg1_auto_bic$residuals, lag.max = 100)
hist(szereg1_auto_bic$residuals) # Histogram 
#Szereg jest realizacj¹ szumu bia³ego 

# Sprawdzenie za³o¿enia o normalnoœci rozk³adu reszt
(pwynik <- shapiro.test(szereg1_auto_bic$residuals)$p.value)
ifelse(pwynik < 0.05,
       "Odrzucamy hipotezê zerow¹ na poziomie istotnoœci 0.05", "Nie ma podstaw do odrzucenia hipotezy zerowej na poziomie istotnoœci 0.05") 
# Odrzucamy hipotezê o rozk³adzie normalnym 
tsdiag(szereg1_auto_bic, gof.lag=50)


# Prognozowanie z wykorzystaniem metod naiwnych
# Prognoza oparta na œredniej
szereg1_meanf_auto <- meanf(szereg1_ts, h = 20) 
plot(szereg1_meanf_auto) 

# Naiwne prognozowanie
szereg1_naive <- naive(szereg1_ts, h=20)
plot(szereg1_naive)

# Sezonowe naiwne prognozowanie
szereg1_snaive <- snaive(szereg1_ts, h=20)
plot(szereg1_snaive)

# Prognozowanie z dryfem
szereg1_rwf <- rwf(szereg1_ts, h = 20, drift = TRUE)
plot(szereg1_rwf)

# Spr poprawnoœci dopasowania 
accuracy(szereg1_meanf_auto)
accuracy(szereg1_naive)
accuracy(szereg1_snaive)
accuracy(szereg1_rwf)

