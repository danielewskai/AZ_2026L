# Zadanie 2.2 ------------------------------------------------------------------
library(MASS)
# Korzystamy ze zbioru hills i badamy zależność czasu biegu od dwóch zmiennych:
# dystansu (dist) oraz przewyższenia (climb).
hills

# a)
# Najpierw rysujemy wykresy rozrzutu:
# - time względem dist,
# - time względem climb.
# Pozwala to wizualnie ocenić, czy zależności mają charakter rosnący i czy model liniowy ma sens.
par(mfrow=c(1,2))
plot(hills$dist, hills$time)
plot(hills$time ~ hills$climb)
par(mfrow=c(1,1))

# Następnie liczymy współczynniki korelacji, aby ocenić siłę związku.
cor(hills$dist, hills$time)
cor(hills$climb, hills$time)

# b)
# Dopasowujemy dwa osobne modele regresji liniowej:
# time ~ dist oraz time ~ climb.
model_dist <- lm(time ~ dist, data = hills)
model_climb <- lm(time ~ climb, data = hills)

# Następnie nanosimy proste regresji na wykresy.
par(mfrow=c(1,2))
plot(hills$dist, hills$time)
abline(model_dist, col = "red")
plot(hills$time ~ hills$climb)
abline(model_climb, col = "red")
par(mfrow=c(1,1))
# Dzięki temu możemy porównać, która zmienna lepiej wyjaśnia zmienność czasu.

# Sposoby na policzenie predykcji y:
# z definicji
coef(model1)[1] + coef(model1)[2]*hills$dist
# z pomocą własności modelu
model1$fitted.values
# za pomocą funkcji predict
predict(model1, data = hills[,c(-2,-3)])

# Obliczamy także:
# SST - całkowitą zmienność,
# SSR - zmienność wyjaśnioną przez model,
# SSE - zmienność niewyjaśnioną przez model.
# Otrzymany wynik porównujemy z wartością zwracaną przez summary(model)$r.squared.
# Im większe R^2, tym lepiej model wyjaśnia zmienność zmiennej time.

SST <- sum((hills$time-mean(hills$time))^2)
SSR <- sum((model_dist$fitted.values-mean(hills$time))^2)
SSE <- sum((hills$time-model_dist$fitted.values)^2)

# Sprawdzamy zależność SST = SSR + SSE oraz wyznaczamy współczynnik determinacji:
# R^2 = SSR / SST.
SST
SSR + SSE

R2 <- SSR/SST
R2
# Im większe R^2, tym więcej zmienności zmiennej time wyjaśnia model

# To samo wykonujemy dla time ~ climb
SST <- sum((hills$time-mean(hills$time))^2)
SSR <- sum((model_climb$fitted.values-mean(hills$time))^2)
SSE <- sum((hills$time-model_climb$fitted.values)^2)
# sum(model2$residuals^2) - SSE możemy liczyć również jako sumę kwadratów reziduów, które możemy wyciągać za pomocą $residuals z modelu

SST
SSR+ SSE

R2 <- SSR/SST
R2

# Funkcja summary pozwala uzyskać jeszcze więcej informacji o modelu
# jedną z nich jest wartość R^2 
summary(model_dist)$r.sq
summary(model_climb)$r.sq

#c)
# Korzystając z dopasowanego modelu przewidujemy czas biegu dla nowych wartości dist.
# Możemy to zrobić:
# - ręcznie, podstawiając do równania regresji,
# - przez iloczyn skalarny wektora współczynników i wektora cech,
# - za pomocą funkcji predict() - tutaj pamiętajmy, że dla nowych danych używamy parametru newdata i wstawaimy dane jako ramkę danych.
model_dist$coefficients[1] + model_dist$coefficients[2] * 15
sum(model_dist$coef * c(1,15))
predict(model_dist, newdata = data.frame(dist = c(1,15,30)))
# Wszystkie trzy sposoby prowadzą do tych samych wartości.

# Zadanie 2.3 ------------------------------------------------------------------
# W tym zadaniu analizujemy klasyczny przykład 4 zbiorów danych pokazujący, że wskaźniki to nie wszystko - ważne są założenia!
ans <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/anscombe_quartet.txt", header = T)

plot(ans$X1, ans$Y1)
plot(ans$X2, ans$Y2)
plot(ans$X3, ans$Y3)
plot(ans$X4, ans$Y4)

# a)
# Dla każdej z czterech par zmiennych dopasowujemy model regresji liniowej.
m1 <- lm(Y1 ~ X1, data = ans)
m2 <- lm(Y2 ~ X2, data = ans)
m3 <- lm(Y3 ~ X3, data = ans)
m4 <- lm(Y4 ~ X4, data = ans)

# b)
# Porównujemy współczynniki regresji dla wszystkich czterech modeli.
coef(m1)
coef(m2)
coef(m3)
coef(m4)
# Okazuje się, że są one bardzo podobne, mimo że zbiory danych mają zupełnie inny kształt.

# Porównujemy wartości R^2
summary(m1)$r.sq
summary(m2)$r.sq
summary(m3)$r.sq
summary(m4)$r.sq
# Są bardzo zbliżone między sobą.

# Porównujemy wartości współczynników korelacji.
cor(ans$X1, ans$Y1)
cor(ans$X2, ans$Y2)
cor(ans$X3, ans$Y3)
cor(ans$X4, ans$Y4)
# Również one są bardzo zbliżone między zestawami.


# c)
# Dopiero po narysowaniu wykresów z naniesionymi prostymi regresji widzimy,
# że dane wcale nie są do siebie podobne:
# - pierwszy zbiór dobrze pasuje do modelu liniowego,
# - drugi ma zależność nieliniową,
# - trzeci zawiera obserwację odstającą w kierunku pionowym,
# - czwart ma tylko dwie różne wartości na x
par(mfrow=c(2,2))
plot(ans$X1, ans$Y1)
abline(m1)
plot(ans$X2, ans$Y2)
abline(m2)
plot(ans$X3, ans$Y3)
abline(m3)
plot(ans$X4, ans$Y4)
abline(m4)
par(mfrow=c(1,1))
# Wniosek: zawsze należy oglądać wykres danych i sprawdzać założenia, a nie opierać się wyłącznie
# na korelacji, R^2 i współczynnikach regresji (ogólnie na wartościach estymatorów).

# Zadanie 3.1 ------------------------------------------------------------------
# W tym zadaniu analizujemy regresję wielokrtoną liniową dla danych o domach.
# Zmienną objaśnianą jest Price, a pozostałe zmienne traktujemy jako predyktory.
rel <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/realest.txt", header = T)
model <- lm(Price~., data = rel)

# a)
# Tworzymy macierz eksperymentu X, dodając kolumnę jedynek odpowiadającą wyrazowi wolnemu.
# Następnie porównujemy ją z wynikiem funkcji model.matrix(model) tworzącej macierz eksperymentu dla obiektu model.
# Obie metody powinny prowadzić do tej samej macierzy eksperymentu używanej w modelu regresji.
X <- as.matrix(cbind(1, rel[,-1]))
model.matrix(model)

# b)
# Wyznaczamy estymator najmniejszych kwadratów ze wzoru macierzowego:
# beta_hat = (X^T X)^(-1) X^T y
# i porównujemy go z wynikiem funkcji coef(model).
y <- rel$Price
solve(t(X)%*%X)%*%t(X)%*%y
coef(model)
# Otrzymujemy te same współczynniki, co potwierdza zgodność rachunków macierzowych
# z wynikiem zwracanym przez funkcję lm() i coef.

# Następnie obliczamy SST, SSR SSE
# Sprawdzamy także, że SSE jest równe sumie kwadratów resztówek modelu.

SST <- sum((y-mean(y))**2)
SSR <- sum((model$fitted.values-mean(y))^2)
SSE <- sum((y-model$fitted.values)^2)
sum(model$residuals^2)
residuals(model) # wartości reziduów modelu

# Na tej podstawie liczymy współczynnik determinacji:
# R^2 = SSR / SST,
# a wynik porównujemy z wartością zwracaną przez summary(model)$r.squared.
R2 <- SSR/SST
summary(model)$r.sq


# c)
# Interpretujemy współczynniki modelu.
coef(model)
# Każdy współczynnik mówi, jak zmienia się przewidywana cena domu przy jednostkowej zmianie
# danej zmiennej, przy założeniu, że wszystkie pozostałe zmienne pozostają stałe.
# W szczególności współczynnik przy Bedroom może być ujemny:
# oznacza to, że przy stałej powierzchni i innych  ustalonych, stałych cechach domu z większą liczbą sypialni
# może być wyceniany niżej, ponieważ większa liczba sypialni oznacza wtedy mniejsze
# lub mniej przestronne pomieszczenia.
# Istotne jest tu zapamiętanie że w modelu interpretacja współczynnika odbywa się przy założeniu
# że wszystkie pomozostałe zmienne się nie zmieniają!

# Dla porównania dopasowujemy także prostszy model Price ~ Bedroom.
lm(Price~Bedroom, data = rel)
# W tym modelu współczynnik przy Bedroom może być dodatni, ponieważ liczba sypialni
# przejmuje również informację o ogólnej wielkości i standardzie domu.
# To pokazuje, że interpretacja współczynników zależy od tego,
# jakie inne zmienne są uwzględnione w modelu.

# d)
# Korzystamy z modelu do przewidywania ceny domu o zadanych parametrach.
# Możemy to zrobić za pomocą funkcji predict() albo ręcznie,
# podstawiając odpowiednie wartości do równania regresji.
predict(model, newdata = data.frame(Condition = 0, Bedroom = 3, Space = 1500, Room = 8, Lot = 40, Bathroom = 2, Garage = 1, Tax = 1000))
sum(model$coef * c(1, 3, 1500, 8, 40, 1000, 2, 1, 0))
# Obie metody powinny dać ten sam wynik, o ile zachowamy właściwą kolejność zmiennych.

# Przy metodzie predict nie jest istotna kolejność zmiennych, lecz ich nazwa
# Przy liczeniu ręcznym należy zwrócić uwagę na kolejność współczynników tak aby odpowiednio dopasować wartość zmiennej i współczynnika

# e)
# Na końcu wyznaczamy estymator wariancji błędu:
# sigma^2_hat = SSE / (n - p),
# gdzie n oznacza liczbę obserwacji, a p liczbę parametrów modelu
# (łącznie z wyrazem wolnym).
n <- nrow(X)
p <- ncol(X)
SSE/(n-p)

# Funkcja summary umożliwia nam wyciągnięcie tej wartości z obiektu model
# Wynik można porównać z wartością summary(model)$sigma^2.
summary(model)$sigma^2

