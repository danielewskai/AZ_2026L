# Zadanie 3.2 ------------------------------------------------------------------
# a)
# Zauważmy że \sum_{i=1}^n e_i = 0 można zapisać jako 1'e=0. Wektor jedynek jest pierwszą 
# kolumną macierzy eksperymentu, zatem sprawdźmy co się dzieje dla X'e
# liczymy X'e = X'(Y-\hat{Y}) = X'(Y-X(X'X)^{-1}X'Y) = X'Y-X'X(X'X)^{-1}X'Y = X'Y-X'y=0
# W modelu regresji liniowej wektor reszt e jest ortogonalny
# do kolumn macierzy eksperymentu X, czyli zachodzi równość X'e = 0.
# pierwszy wiersz macierzy X', to wiersz jedynek ozn. (1') zatem w szczególności 1'e = \sum_{i=1}^n e_i = 0 z wczesniejszej linijki

# b)
# Generujemy dane z modelu liniowego i sprawdzamy numerycznie własność,
# że suma reszt jest równa zero.
n <- 100
# W pierwszym podejściu jawnie tworzymy macierz X z kolumną jedynek
X <- cbind(1, matrix(rnorm(n*3), ncol = 3))
beta <- c(2, 0.5, 1, 0.7)
eps <- rnorm(n, mean = 0, sd = 10)
y <- X%*%beta + eps

# Gdy uwzględniamy kolumnę jedynek w macierzy eksperymntu, musimy pamiętać, że funkcja
# lm już nie potrzebuję go dodawać, zatem stosujemy formułę y ~ X - 1, pomijając
# automatycznie dodawany wyraz wolny (już go mamy)
model <- lm(y ~ X - 1)

# W drugim podejściu generujemy macierz zmiennych objaśniających bez kolumny jedynek
X <- matrix(rnorm(n*3), ncol = 3)
beta <- c(2, 0.5, 1, 0.7)
eps <- rnorm(n, mean = 0, sd = 10)
y <- 2 + X%*%beta[-1] + eps

# Gdy w danych nie mamy bezpośrednio uwzględnionego wyrazu wolnego (jako kolumny jedynek)
# model lm dodaje go automatycznie
model <- lm(y ~ X)

sum(residuals(model))
# W obu przypadkach model zawiera intercept, więc suma reszt powinna być równa zero
# z dokładnością do błędów numerycznych.

# c)
# Powtarzamy symulację wiele razy i w każdej próbie liczymy estymator wariancji
# błędu:
# sigma^2_hat = SSE / (n - p),
# n - liczba obserwacji (liczba wierszy macierzy eksperymentu)
# p - liczba predyktorów (liczba kolumn macierzy eksperymentu - tutaj uwzględniamy wyraz wolny)
k <- 1000
vars <- numeric(k)
for (i in 1:k){
  X <- matrix(rnorm(n*3), ncol = 3)
  beta <- c(2, 0.5, 1, 0.7)
  eps <- rnorm(n, mean = 0, sd = 10)
  y <- 2 + X%*%beta[-1] + eps
  
  model <- lm(y ~ X)
  
  vars[i] <- sum(residuals(model) * residuals(model))/(n-4) # p = 4, bo model zawiera wyraz wolny oraz trzy zmienne objaśniające
  # vars[i] <- summary(model)$sigma^2 # możemy korzystać z definicji lub wartości które oferuje nam funkcja summary; w tym przyapdku $sigma^2 to wariancja błędów
}
# Otrzymywane wartości różnią się między próbami, ponieważ zależą od losowo wygenerowanych danych,
# ale ich średnia powinna być bliska prawdziwej wariancji błędu.
vars
mean(vars)
# Ponieważ w symulacji odchylenie standardowe błędu wynosi 10, to wariancja teoretyczna wynosi 100.
# Fakt, że średnia z uzyskanych estymatorów jest bliska 100, ilustruje nieobciążoność tego estymatora.

# Zadanie 4.1 ------------------------------------------------------------------
# Korzystamy ze zbioru airpollution i badamy istotność zmiennych
# w modelu regresji liniowej dla zmiennej Mortality.
air <-  read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/airpollution.txt", header = T)
dane <- air[,c(1,2,5,6,8,12,16)]
# zmienne te są nam potrzebne w naszym zadaniu

# a)
# Dopasowujemy pełny model regresji liniowej. Znak '.' oznacza, że uwzględniamy wszystkie
# pozostałe zmienne z ramki danych air (oprócz Mortality, które już wykorzystaliśmy).
model <- lm(Mortality ~ ., data = dane)

# b)
# Ręcznie obliczamy wartość statystyki dla testu t dla współczynnika modelu odpowiadającego zmiennej NOx.
# W tym celu:
# - wyznaczamy macierz eksperymentu X,
# - obliczamy macierz (X^T X)^(-1),
# - wyznaczamy estymator wariancji reszt s^2 = SSE / (n - p),
# - obliczamy błąd standardowy wybranego współczynnika,
# - liczymy statystykę t i odpowiadające jej p-value.
# t_i = beta_hat_i/SE_{beta_hat_i} = beta_hat_i/(sqrt(s^2 (X'X)_{ii}^{-1}))

X <- model.matrix(model)
p <- ncol(X) # dim(X)[2]
n <- nrow(X) # dim(X)[1]
#QR <- qr(X)
#R <- qr.R(QR)
#XX_inv <- solve(R) %*% t(solve(R))
#XX_inv_ii <- diag(XX_inv)[length(diag(XX_inv))]
XX_inv_ii <- solve(t(X)%*%X)['NOx','NOx']
s2 <- residuals(model) %*% residuals(model) / (n-p) # sum(model$residuals^2)/(n-p) lum summary(model)$sigma^2
SE <- sqrt(s2 * XX_inv_ii)
beta_hat <- model$coefficients["NOx"]
t_stat <- beta_hat/SE

# p-val = 2*min{P(T<t),P(T>t)}
2 * min(pt(t_stat, df = n-p), 1-pt(t_stat, df = n-p))
# p-value > 0.05, to nie ma podstaw do odrzucenia hipotezy zerowej,
# że dany współczynnik jest równy zero.
# W tym przypadku oznacza to, że zmienna NOx nie jest istotna statystycznie
# po uwzględnieniu pozostałych zmiennych w modelu.

# Otrzymany wynik porównujemy z tabelą współczynników z summary(model).
summary(model)
# Pierwsza kolumna tej tabeli to wartości wyestymowanych współczynników
# Druga kolumna to wartość błędów standardowych dla poszczególnych współczynników
# Trzecia kolumna to wartości statystyk dla testów istotności poszczególnych zmiennych
# Czwarta kolumna to p-value odpowiednich testów
# Tabele te można wyciągnąć bezpośrednio
summary(model)$coef

# c)
# Następnie przeprowadzamy test istotności całego modelu (wszystkich zmiennych).
# Hipoteza zerowa mówi, że wszystkie współczynniki przy zmiennych objaśniających
# są równe zero, czyli model nie wyjaśnia zmienności zmiennej Mortality lepiej
# niż sam wyraz wolny.
SSR <- sum((model$fitted.values-mean(dane$Mortality))**2)
SSE <- as.vector(residuals(model) %*% residuals(model))

# Obliczamy statystykę F ze wzoru:
# F = [SSR / (p - 1)] / [SSE / (n - p)].
F_stat <- SSR*(n-p)/(SSE*(p-1))
1 - pf(F_stat, p-1, n-p)
# Mała wartość p-value < 0.05 oznacza odrzucenie hipotezy zerowej.
# Tzn, że co najmniej jedna zmienna objaśniająca
# jest istotna (istotnie związana ze zmienną Mortality).

# Wynik można porównać z testem F podawanym w summary(model).
summary(model)

# Wartość statystyki F i stopnie swobody można wyciągnąć bezpośrednio z summary
summary(model)$fstatistic

# Zadanie 4.2 ------------------------------------------------------------------
# W tym zadaniu pokazujemy, że istotność statystyczna w modelu liniowym
# nie musi oznaczać, że model został poprawnie dopasowany.

# Generujemy dane z zależności:
# y = 0.5 + x1^2 + eps,
# czyli prawdziwa relacja między y i x1 jest kwadratowa, a nie liniowa.
# Następnie dopasowujemy model liniowy:
# y ~ x1.
n <-  1000
x1 <- runif(n, 0, 1)
eps <-rnorm(n)
y <- 0.5 + 1 * x1^2 + eps
plot(x1, y)
m <- lm(y ~ x1)
summary(m)$coef[2,3]
summary(m)$coef[2,4]
# Mimo że współczynnik przy x1 może okazać się istotny statystycznie
# (p-value < 0.05), nie oznacza to jeszcze, że model liniowy jest właściwy.
# Istotność mówi tylko, że istnieje pewna zależność między zmiennymi,
# ale nie gwarantuje, że została dobrze opisana prostą.
# Model liniowy pomija składnik kwadratowy, który tutaj jest kluczowy.

# W modelu z jedną zmienną objaśniającą zachodzi zależność:
# F = t^2.
summary(m)$fstatistic
summary(m)$coef[2,3]^2

