### Zainstalowanie, oraz wczytanie bibliotek odpowiadaj�cych za:
#   import plik�w xls/csv, operacje na �a�cuchac tekstowych i szeregach czasowych
#   (W tym usuni�cia brakuj�cych warto�ci), wykresy

if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl")
library("readxl")  # Do wczytania plik�w typu csv
if (!('dplyr' %in% rownames(installed.packages()))) install.packages('dplyr')
library('dplyr') # Dla fitrowania/wybierania danych z ramki
if (!('timeSeries' %in% rownames(installed.packages()))) install.packages('timeSeries')
library('timeSeries')  # Do usuni�cia brakuj�cych warto�ci w szeregu
if (!('forecast' %in% rownames(installed.packages()))) install.packages('forecast')
library('forecast')  # Do wykres�w (Acf, Pacf)
if (!('stats' %in% rownames(installed.packages()))) install.packages('stats')
library('stats')  # Do wykres�w (np. monthplot)
if (!('lattice' %in% rownames(installed.packages()))) install.packages('lattice')
library('lattice')  # Do wykres�w (np. xyplot)
if (!('stringi' %in% rownames(installed.packages()))) install.packages('stringi')
library('stringi')  # Do funkcji tekstowej (np. stri_sub)



### Wczytanie pliku z danymi

my_data <- read.csv("D:/studia/2rok 2 semestr in�ynieria i analiza danych/szeregi czasowe/time series/country_vaccinations.csv")


# Wy�wietlenie informacji o tabeli z danymi
# str(my_data)
# summary(my_data)
# head(my_data)



### Wyb�r kraju dla analizy jej danych (np. "Poland", "Palestine")
sort(unique(my_data$country))
ChosenCountry <- "Poland"
ChosenRows <- which(my_data[,1] == ChosenCountry,TRUE)

### "Wyci�gni�cie" odpowiednich danych dla wybranego kraju
ExaminedData <- select(my_data, country,date,people_fully_vaccinated)
dim(ExaminedData) # Wymiary ca�ego zestawu danych
# Przypisanie danych wy��cznie danego kraju
NData <- ExaminedData[
  sapply(ExaminedData$country, 
         function (x) (
           if (x ==  ChosenCountry
               | is.null(x) | is.na(x)) TRUE else FALSE)), 1:dim(ExaminedData)[2]]

# Wy�wietlenie informacji o liczbie os�b ca�kowicie zaszczepionych z danego kraju
summary(NData$people_fully_vaccinated) # Dla danych dziennych



### Utworzenie szereg�w czasowych
TimeSerialD<-as.ts(NData$people_fully_vaccinated)
# Ewentualnie: TimeSerialD <- ts(NData$people_fully_vaccinated,start=c(2020,12,8),frequency=365.25)
# Interpolacja obserwacji brakuj�cych
TimeSerialD<-na.interp(TimeSerialD)
ts.plot(TimeSerialD,xlab = "Dzie�", ylab = "Liczba ca�kowitych szczepie�",col = c("red"),main="Liczba ca�kowitych szczepie� na COVID19 w Polsce") 


### Om�wienie g��wnych cech analizowanego szeregu na podstawie r�nych typ�w wykres�w, wnioski
#   Wykres z u�yciem xyplot()
xyplot(TimeSerialD, main="Liczba ca�kowitych szczepie� w Polsce", 
       xlab = "Dzie�",ylab="Liczba zaszczepionych", aspect = 1/3) 

# Wykres panelowy
xyplot(TimeSerialD, main="Liczba ca�kowitych szczepie� w Polsce", 
       xlab = "Dzie�",ylab="Liczba zaszczepionych", aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka cz�� danych powt�rzona w kolejnym panelu) 

# Wykres pude�kowy (Na wykresach: kwartyle 1,3 rz�du, mediany, warto�� minimalna/maksymalna, odstaj�ce)
boxplot(TimeSerialD ~ cycle(TimeSerialD), main="Liczba ca�kowitych szczepie� w Polsce", 
        xlab = "Dzie�",ylab="Liczba zaszczepionych", aspect = 1/3)

# Wykres rozrzutu dla warto�ci op�nionych
lag.plot(TimeSerialD, lags=12, do.lines = FALSE) 
lag.plot(TimeSerialD) # Dobrze widoczny trend wzrostowy
# Najsilniejsza korelacja (zale�no��) dla op�nienia pierwszego pokazuje widoczny trend wzrostowy
plot(TimeSerialD)

# Wykresy: funkcji autokorelacji - Acf, cz�stkowej funkcji autokorelacji - Pacf

# acf(TimeSerialD, lag.max = 30, xlab = "Odst�p dzienny", ylab="Korelacja danych",
#    main = "Korelogram dla liczby ca�kowitych szczepie� w Polsce")
par(mfrow = c(2,1))
Acf(TimeSerialD, lag.max = 30, xlab = "Odst�p dzienny", ylab="Korelacja danych",
    main = "Korelogram dla �rednich temperatur miesi�cznych w Polsce")
# Wida� wyra�ny trend poprzez �agodne zmiany na wykresie

# pacf(TimeSerialD, lag.max = 30, xlab = "Odst�p dzienny", ylab="Bezpo�rednia korelacja danych",
#      main = "Korelogram dla liczby ca�kowitych szczepie� w Polsce")
Pacf(TimeSerialD, lag.max = 30, xlab = "Odst�p dzienny", ylab="Bezpo�rednia korelacja danych",
     main = "Korelogram dla liczby ca�kowitych szczepie� w Polsce")
# Wida� do�� mocny trend tak�e dzi�ki bliskiej warto�ci - jeden warto�ci dla "laga" pierwszego 



### Zastosowanie �rednich ruchomych (lewostronnych) do wyg�adzania wykresu
ma3 <- filter(TimeSerialD, sides = 1, filter = rep(1/3,3)) # Dla trzech dni
ma14 <- filter(TimeSerialD, sides = 1, filter = rep(1/14,14)) # Dla 14-stu dni

# Por�wnanie wykres�w
plot(TimeSerialD, main = "Liczba ca�kowitych szczepie� w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(ma3,col="red", lty = 8)
lines(ma14,col="green", lty = 8)



### Dekompozycje na podstawie modelu regresji: trend liniowy/wielomianowy, sezonowo��, transformacja Boxa-Coxa 
TimeSerialD_T <- tslm(TimeSerialD ~ trend) 
# TimeSerialD_S <- tslm(TimeSerialD ~ season) # Brak sezonowo�ci 
TimeSerialD_TSB5 <- tslm((TimeSerialD) ~ trend, lambda = 0.5) # Pierwiastkowa
TimeSerialD_TSB1 <- tslm((TimeSerialD) ~ trend, lambda = 0) # Logarytmiczna transformacja
TimeSerialD_TSBA <- tslm((TimeSerialD) ~ trend, lambda = "auto") # Automatyczny dob�r rz�du

# Trend kwadratowy
TimeSerialD_WS<- tslm(TimeSerialD ~ poly(trend,raw=TRUE,degree=2))
# Trend stopnia trzeciego
TimeSerialD_WS3<- tslm(TimeSerialD ~ poly(trend,raw=TRUE,degree=3))
summary(TimeSerialD_TSBA) # Informacje o przyk�adowym z wybranych modeli

# Por�wnamy metody
par(mfrow = c(2,1))
plot(TimeSerialD, main="Liczba ca�kowitych szczepie� w Polsce",
     xlab = "Dzie�", ylab="Liczba zaszczepionych")
lines(fitted(TimeSerialD_T), col = "green", lty = 2) # Wizualizacja wykrytego trendu liniowego
lines(fitted(TimeSerialD_WS), col = "blue", lty = 2) # Wizualizacja wykrytego trendu kwadratowego
lines(fitted(TimeSerialD_WS3), col = "red", lty = 2) # Wizualizacja wykrytego trendu stopnia trzeciego

plot(TimeSerialD, main="Liczba ca�kowitych szczepie� w Polsce",
     xlab = "Dzie�", ylab="Liczba zaszczepionych")
lines(fitted(TimeSerialD_TSB5), col = "green", lty = 2) # Wizualizacja transformacji pierwiastkowej
lines(fitted(TimeSerialD_TSB1), col = "blue", lty = 2) # Wizualizacja transformacji logarytmicznej
lines(fitted(TimeSerialD_TSBA), col = "red", lty = 2) # Wizualizacja transformacji automatycznego rz�du

# Widzimy, �e sama transformacja Boxa_Coxa w tym przypadku nie poprawia wystarczaj�co rezultatu.
# Koniecznym za�o�eniem jest tutaj trend wielomianowy, najprawdopodobniej stopnia trzeciego.


# Por�wnanie reszt r�nymi metodami dekompozycji na podstawie modelu regresji
tsdisplay(residuals(TimeSerialD_T),main="Reszty dla trendu liniowego") # Wyeliminowany trend liniowy
tsdisplay(residuals(TimeSerialD_WS),main="Reszty dla trendu kwadratowego") # Wyeliminowany trend kwadratowy
tsdisplay(residuals(TimeSerialD_WS3),main="Reszty dla trendu stopnia trzeciego") # Wyeliminowany trend stopnia trzeciego



### Uczynienie szeregu stacjonarnym - usuni�cie trendu, sezonowo�ci: r�nicowanie, *transformacja Boxa-Coxa
### Sprawdzenie, czy s� realizacj� szumu bia�ego, kt�rego rz�du modele AR(p), MA(q) warto bra� pod uwag�
tsdisplay(TimeSerialD,main="Rozk�ad liczby ca�kowitych szczepie� w Polsce",
          xlab = "Dzie�", ylab="Liczba zaszczepionych", lag.max=60)
hist(TimeSerialD,main="Histogram reszt") 

TimeSerialDBox<-BoxCox(TimeSerialD,lambda="auto")
tsdisplay(TimeSerialDBox,main="Rozk�ad liczby ca�kowitych szczepie� w Polsce",
          xlab = "Dzie�", ylab="Liczba zaszczepionych", lag.max=60)
# Poni�ej warto�� okre�laj�ca maksymaln� odchy�k� od przedzia�u ufno�ci dla szeregu stacjonarnego
(Val <- 1.96/sqrt(length(TimeSerialDBox))) 
ndiffs(TimeSerialDBox) # Ilo�� potrzebnych r�nicowa� rz�du r�nicowania z op�nieniem warto�ci: jeden

TimeSerialDT <- diff(x=TimeSerialDBox, lag=1) # Usuni�cie trendu z op�nieniem: 1
hist(TimeSerialDT, main="Histogram reszt") 
# Rozk�ad warto�ci jest cz�ciowo zbli�ony do normalnego, jednak nadal nim nie jest.
tsdisplay(TimeSerialDT, main="Rozk�ad liczby ca�kowitych szczepie� w Polsce bez trendu",
          xlab = "Dzie�", ylab="Liczba zaszczepionych",lag.max=60)
(Val <- 1.96/sqrt(length(TimeSerialDT))) 
ndiffs(TimeSerialDT) # Ilo�� potrzebnych r�nicowa� rz�du r�nicowania z op�nieniem warto�ci: jeden

TimeSerialDTS <- diff(x=TimeSerialDT, lag=1) # Usuni�cie trendu z op�nieniem: 1 z otrzymanych reszt
hist(TimeSerialDTS, main="Histogram reszt") 
# Rozk�ad warto�ci jest bardzo zbli�ony do normalnego.
ndiffs(TimeSerialDTS) # Ilo�� potrzebnych r�nicowa� rz�du r�nicowania z op�nieniem warto�ci: jeden
tsdisplay(TimeSerialDTS, main="Rozk�ad zaszczepien w Polsce bez trendu",
          xlab = "Dzie�", ylab="Liczba zaszczepionych",lag.max=length(TimeSerialDTS))

# Wida� jednak dalej pewn� sezonowo�� reszt. W celu doboru rz�du r�nicowania sprawdzimy wykresy rozrzutu.
lag.plot(TimeSerialDTS,lags=12, do.lines = FALSE) # Dobrze widoczny trend wzrostowy
TimeSerialDTS7 <- diff(x=TimeSerialDTS, lag=7) # Usuni�cie trendu z op�nieniem sezonowym (7) z otrzymanych reszt
tsdisplay(TimeSerialDTS7, 
          xlab = "Dzie�", ylab="Liczba zaszczepionych",lag.max=length(TimeSerialDTS7))
# Warto�ci: ACF(1,7,8,21,23,30,31), PACF(1,3,7,*14,21,22) wykraczaj� poza przedzia� ufno�ci
# Warto wzi�� pod uwag� modele  MA(q) oraz AR(p) o wsp�czynnikach poni�ej:
q <- c(1,7,8,21,23,30,31)
p <- c(1,3,7,14,21,22)

# Sprawdzenie stacjonarno�ci szeregu - pomocnicza linia pokazuj�ca, czy warto�� skrajnych odchy�ek nie jest zbyt du�a
MaxDifference <- 1.96/sqrt(length(TimeSerialDTS7))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialDTS7)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialDTS7,lag.max=60)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wi�c szereg stacjonarny.

# Usuni�cie �redniej
TimeSerialDNM<-TimeSerialDTS7-mean(TimeSerialDTS7)



### Wyznaczenie wsp�czynnik�w modelu autoregresji i por�wnanie dopasowania r�nymi metodami estymacji
TimeSerialMS.ar1.yule <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("yule-walker")) # Utworzenie modelu 
# Mo�na ewentualnie zobaczy� struktur� tego modelu: str(TimeSerialMS.ar1)
print(TimeSerialMS.ar1.yule) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar1.burg <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("burg")) # Utworzenie modelu 
print(TimeSerialMS.ar1.burg) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar1.ols <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("ols")) # Utworzenie modelu 
print(TimeSerialMS.ar1.ols) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar1.mle <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("mle")) # Utworzenie modelu 
print(TimeSerialMS.ar1.mle) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar1.yw <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("yw")) # Utworzenie modelu 
print(TimeSerialMS.ar1.yw) # Wyznaczenie wsp�czynnik�w modelu

# Automatycznie dobrana warto�� rz�du
TimeSerialMS.arAIC.yw <- ar(TimeSerialDNM, aic = TRUE, order.max = 100, method = c("yule-walker"))
print(TimeSerialMS.arAIC.yw)
# Warto�� 1 (AR) to pierwsza z pi�ciu i najwi�ksza zaobserwowana na wykresie Pacf korelacja z propozycji.



### Wyznaczenie wsp�czynnik�w modelu ruchomej �redniej z u�yciem funkcji Arima()
TimeSerialDNM.m.ar0i0ma1 <- Arima(TimeSerialDNM, order =c(0,0,1))
summary(TimeSerialDNM.m.ar0i0ma1)                          
TimeSerialDNM.m.ar0i0ma7 <- Arima(TimeSerialDNM, order =c(0,0,7))
summary(TimeSerialDNM.m.ar0i0ma7)
# TimeSerialDNM.m.ar0i0ma30 <- Arima(TimeSerialDNM, order =c(0,0,30))
# summary(TimeSerialDNM.m.ar0i0ma30)

# Por�wnanie modelu wyznaczonego powy�ej z odpowiadaj�cym mu modelem AR
# Analiza dobroci dopasowania, oraz warto�ci b��d�w prognoz
TimeSerialDNM.m.ar1i0ma0 <- Arima(TimeSerialDNM, order = c(1,0,0))
summary(TimeSerialDNM.m.ar1i0ma0)
TimeSerialDNM.ar7i0ma0S <- Arima(TimeSerialDNM, order = c(7,0,0))
summary(TimeSerialDNM.ar7i0ma0S)
# TimeSerialDNM.m.ar21i0ma0 <- Arima(TimeSerialDNM, order = c(21,0,0))
# summary(TimeSerialDNM.m.ar21i0ma0)
# TimeSerialDNM.m.ar22i0ma0 <- Arima(TimeSerialDNM, order = c(22,0,0))
# summary(TimeSerialDNM.m.ar22i0ma0)



### Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima() oraz wyznaczenie ich wsp�czynnik�w
### Por�wnanie modeli, wyb�r najlepszego na podstawie kryteri�w dopasowania: aic, aicc, bi (g��wnie ostatnich).
# Modele automatyczne
(TimeSerialDTSaicc.auto <- auto.arima(TimeSerialDNM, ic = "aicc"))
summary(TimeSerialDTSaicc.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow� �redni� ; AIC=1436.71   AICc=1436.79   BIC=1442.79
(TimeSerialDTSaic.auto <- auto.arima(TimeSerialDNM, ic= "aic"))
summary(TimeSerialDTSaic.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow� �redni� ; AIC=1436.71   AICc=1436.79   BIC=1442.79
(TimeSerialDTSbic.auto <- auto.arima(TimeSerialDNM, ic= "bic"))
summary(TimeSerialDTSbic.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow� �redni� ; AIC=1436.71   AICc=1436.79   BIC=1442.79
# W ka�dym przypadku wyznaczono model ARIMA(1,0,0) z zerow� �redni� o tych wsp�czynnikach dopasowania.

# Modele w�asne
summary(TimeSerialDNM.m.ar0i0ma1) 
# Wyznaczono: ARIMA(0,0,1) z niezerow� �redni� ; AIC=1446.66   AICc=1446.82   BIC=1455.79
summary(TimeSerialDNM.m.ar1i0ma0) 
# Wyznaczono: ARIMA(1,0,0) z niezerow� �redni� ; AIC=1438.71   AICc=1438.86   BIC=1447.84

# Najlepszym modelem na podstawie warto�ci kryteri�w najbli�szych zeru jest model: ARIMA(1,0,0).



### Sprawdzenie, czy otrzymane modele mo�na uzna� za szum bia�y
tsdisplay(TimeSerialDTSbic.auto$residuals)
# Sprawdzenie stacjonarno�ci szeregu z pomocnicz� lini� pokazuj�ca, czy warto�� skrajnych odchy�ek nie jest zbyt du�a
MaxDifference <- 1.96/sqrt(length(TimeSerialDTSbic.auto))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialDTSbic.auto)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialDTSbic.auto$residuals,lag.max=1000)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wi�c szereg stacjonarny, poniewa� �adna z warto�ci nie wystaje znacz�co poza przedzia� ufno�ci.
hist(TimeSerialDTSbic.auto$residuals) # Histogram jest bardzo zbli�ony do rozk�adu normalnego.
# Sprawdzenie za�o�enia o normalno�ci rozk�adu reszt
shapiro.test(TimeSerialDTSbic.auto$residuals) 
# tsdiag(TimeSerialDTSbic.auto, gof.lag=50)
# Po warto�ciach p mo�na stwierdzi�, �e pr�by pochodz� z populacji o rozk�adzie normalnym (poniewa� p.value < 0,05).



### Prognozowanie z wykorzystaniem metod naiwnych
# Prognoza oparta na �redniej
TimeSerialD.auto.meanf <- meanf(TimeSerialD, h = 60) 
plot(TimeSerialD.auto.meanf) 

# Prognozowanie naiwne 
TimeSerialD.naive <-naive(TimeSerialD, h=24)
plot(TimeSerialD.naive)
TimeSerialD.snaive <-snaive(TimeSerialD, h=24)
plot(TimeSerialD.snaive)

# Prognozowanie naiwne z dryfem
TimeSerialD.rwf <- rwf(TimeSerialD, h=24, drift = TRUE)
plot(TimeSerialD.rwf)

# Najlepsz� metod� jest zastosowanie tutaj prognozowania naiwnego z dryfem.





