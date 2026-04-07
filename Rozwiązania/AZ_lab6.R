# Zadanie 5.1 ------------------------------------------------------------------

air <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/airpollution.txt", header = T)

#a)
# Dopasowujemy model regresji liniowej Mortality ~ NOx
model <-  lm(Mortality ~ NOx, data = air)

# Wyciągamy estymator współczynnika przy NOx oraz jego błąd standardowy.
summary(model)$coef[2,c(1,2)]

# Rysujemy wykres rozrzutu i dopasowaną prostą regresji.
plot(air$NOx, air$Mortality)
abline(model, col = "red")
# Prosta ta niezbyt dobrze dopasowała się do danych

# Wykresy diagnostyczne
plot(model,6) # różne cyfry przedstawiają różne wykresy

# W R dostępnych jest 6 podstawowych wykresów diagnostycznych:
#
# 1) plot(model, 1) - Residuals vs Fitted
#    Pokazuje reszty względem wartości predykowanych.
#    Dobrze, gdy punkty są rozrzucone losowo wokół zera a czerwona linia jest jak najbliżej y = 0.
#    Odstępstwa sugerują nieliniowość lub heteroskedastyczność.
#
# 2) plot(model, 2) - Normal Q-Q
#    Porównuje rozkład reszt z rozkładem normalnym.
#    Jeśli punkty leżą blisko prostej, możemy założyć normalność reszt.
#    Duże odchylenia od prostej, szczególnie na końcach, sugerują brak normalności
#    lub obecność obserwacji odstających.
#
# 3) plot(model, 3) - Scale-Location
#    Pokazuje sqrt(|reszt studentyzowanych|) względem wartości predykowanych.
#    Służy głównie do oceny homoskedastyczności.
#    Dobrze, gdy punkty tworzą poziomy pas o podobnej szerokości, a czerwona linia jest w miarę pozioma
#
# 4) plot(model, 4) - Cook's distance
#    Pokazuje odległości Cooka dla poszczególnych obserwacji.
#    Umożliwia wykrycie obserwacji silnie wpływających na dopasowanie modelu.
#    Duże wartości wskazują, że usunięcie danej obserwacji mogłoby istotnie zmienić model.
#
# 5) plot(model, 5) - Residuals vs Leverage
#    Pokazuje reszty standaryzowane względem dźwigni (leverage).
#    Pomaga znaleźć obserwacje jednocześnie odstające i wpływowe.
#    Szczególnie podejrzane są punkty o dużej dźwigni i dużych resztach.
#
# 6) plot(model, 6) - Cook's dist vs Leverage h_ii
#    To kolejny wykres do identyfikacji obserwacji wpływowych.
#    Łączy informację o dźwigni i odległości Cooka.
#    Punkty daleko od większości obserwacji mogą silnie wpływać na wyniki regresji.

# Przykład dla typowego modelu regresji
x <- rnorm(1000)
eps <- rnorm(1000)
y <- 2*x + 3 + eps
model_liniowy <- lm(y ~ x)

# Wykresy nie mają znaczących odchyłek - czerowne linie są zazwyczaj poziome
plot(model_liniowy, 6)

# b)
# Rozważamy teraz model, w którym zależność od NOx jest logarytmiczna:
model_log <-  lm(Mortality ~ log(NOx), data = air)

# Wyciągamy estymator współczynnika przy log(NOx) oraz jego błąd standardowy.
summary(model_log)$coef[2,c(1,2)]

# Rysujemy dane po transformacji i dopasowaną prostą regresji.
plot(log(air$NOx), air$Mortality)
abline(model_log, col = "red")
# Tutaj model już lepiej dopasowuje się do chmury punktów

# Wykresy diagnostyczne
plot(model_log, 5)
# Wykresy wyglądają zdecydowanie lepiej, jednak dalej widać pewne odstępstwa w postaci obserwacji odstającej/wpływowej

# c)
# Szukamy obserwacji odstających za pomocą reszt studentyzowanych
# Obserwacje, dla których |r_i| > 2, traktujemy jako potencjalnie odstające.

# rezydua studentyzowane
rstud <- rstandard(model_log)
which(abs(rstud) > 2)

# Dopasowujemy model po usunięciu wskazanych obserwacji.
model_log_rstud <- lm(Mortality ~ log(NOx), data = air, subset = -c(29,37,47,49) )

# Porównujemy prostą regresji dla pełnych danych (niebieska)
# oraz dla danych po usunięciu obserwacji odstających (czerwona).
plot(log(air$NOx), air$Mortality)
abline(model_log_rstud, col = "red")
abline(model_log, col = "blue")
# Widzimy, że model delikatnie się zmiennił

# Wykresy diagnostyczne 
plot(model_log_rstud)
# Widzimy znów lekką poprawę

summary(model_log)
summary(model_log_rstud)
# Współczynnik R^2 jest większy dla modelu z usuniętą obserwacją odstającą, tzn. 
# że drugi model bez tej jednej obserwacji wyjaśnia więcej zmienności w danych

# Zadanie 5.2 ------------------------------------------------------------------
phila <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/phila.txt", header = T)
# Sprawdzamy, czy w zmiennej HousePrice lub CrimeRate występują braki danych.
any(is.na(phila$HousePrice))
any(is.na(phila$CrimeRate))
# Usuwamy obserwacje z brakami w zmiennej CrimeRate.
phila2 <- phila[-which(is.na(phila$CrimeRate)),]
rownames(phila2) <- NULL # by uniknąć niepotrzebnych błędów

# Dopasowujemy prosty model regresji
model <- lm(HousePrice~CrimeRate, data = phila2)

# Rysujemy dane i dopasowaną prostą.
plot(phila2$HousePrice~phila2$CrimeRate)
abline(model)

# Szukamy obserwacji odstających za pomocą reszt studentyzowanych modyfikowanych.
# Często za podejrzane uznaje się obserwacje z |rstudent| > 2.

# rezydua studentyzowane modyfikowane
which(abs(rstudent(model))>2)

# Obliczamy wartości na diagonali macierzy H
h <- hatvalues(model)
p <- 2
n <- nrow(phila2)

# Za potencjalnie wpływowe
# uznajemy obserwacje o h_ii większym niż 2p/n.
which(h>2*p/n)

# Usuwamy obserwację 63, która jest zarówno potencjalnie odstająca jak i wpływowa i dopasowujemy model ponownie.
model2 <- lm(HousePrice~CrimeRate, data = phila2, subset = -c(63))
abline(model2, col = "red")

summary(model)
summary(model2)
# R^2 jest większy dla drugiego modelu

# Wykresy diagnostyczne wyglądają lepiej dla modelu z usuniętą obserwacją 63
plot(model,1)
plot(model2,1)

#Odległość Cook'a
#cooks_dist <- rstandard(model)^2 * hatvalues(model)/(p*(1-hatvalues(model)))
#which(cooks_dist > 4/(n-p-1))
#cooks.distance(model)

# Zadanie 5.3 ------------------------------------------------------------------
cel <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/cellular.txt", header = T)

# a)
# Dopasowujemy zwykły model liniowy.
model <- lm(Subscribers~ Period, data = cel)

# Rysujemy zależność liczby subskrybentów od czasu.
plot(cel$Period, cel$Subscribers)
abline(model)

# Oglądamy współczynniki i podstawową diagnostykę.
summary(model)
plot(model,1) # Widzimy, że zależność jest zdecydowanie nieliniowa

# b)
# Sprawdzamy kilka transformacji zmiennej Subscribers:
# logarytmiczną, pierwiastkową i pierwiastek czwartego stopnia.
model1 <- lm(log(Subscribers)~ Period, data = cel)
model2 <- lm((Subscribers)^(1/2)~ Period, data = cel)
model3 <- lm((Subscribers)^(1/4)~ Period, data = cel)

plot(cel$Period, log(cel$Subscribers))
abline(model1)

plot(cel$Period, (cel$Subscribers)^(1/2))
abline(model2)

plot(cel$Period, (cel$Subscribers)^(1/4))
abline(model3)

plot(model1,1)
plot(model2,1)
plot(model3,1)

summary(model1)
summary(model2)
summary(model3)

# Najlepiej dopasowuję się model3, jednak dalej możemy zauważyć niedociągnięcia

# c)
# Metoda Boxa-Coxa służy do szukania transformacji z konkretnej rodziny funkcji,
# tak aby model jak najlepiej się dopasował pod względem
library(MASS) # tutaj mamy metodę Boxa-Coxa
# Korzystamy z transformacji Boxa-Coxa, aby dobrać parametr lambda
bc <- boxcox(Subscribers~Period, data = cel, lambda = seq(0, 1, 0.01))
# bc$x - wartości lambd
# bc$y - wartości log-wiarogonodności odpowiadających lambd
# Wybieramy lambda maksymalizujące log-wiarogodność.
lambda <- bc$x[which.max(bc$y)]

# Tworzymy przetransformowaną zmienną odpowiedzi.
new_Sub <- (cel$Subscribers^lambda - 1)/lambda

# Dopasowujemy model do przetransformowanych danych.
model_bc <- lm(new_Sub ~ cel$Period)

# Rysujemy dane i dopasowaną prostą
plot(cel$Period,new_Sub)
abline(model_bc)

plot(model_bc)
summary(model_bc)

# Wykresy diagnostyczne dla tego modelu wyglądają lepiej niż dla wcześniejszych modeli,
# więc transformacja Boxa-Coxa prowadzi tutaj do bardziej adekwatnego modelu liniowego.