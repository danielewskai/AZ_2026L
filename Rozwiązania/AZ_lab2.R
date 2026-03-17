# =========================
# Laboratorium 2
# Metoda MNK
# =========================


# Zadanie 2.1 ------------------------------------------------------------------
# W tym zadaniu generujemy dane z prostego modelu liniowego:
# y = x + eps, gdzie eps jest losowym szumem o zadanym odchyleniu standardowym.
# Chcemy sprawdzić, jak siła szumu wpływa na korelację oraz na dopasowanie prostej regresji.

x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 3)
y <- x + eps

plot(x,y)

# a)
# Współczynnik korelacji Pearsona obliczamy najpierw "ręcznie" ze wzoru,
# a następnie sprawdzamy wynik funkcją cor().
# Obie metody dają ten sam rezultat, co potwierdza poprawność obliczeń.
sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

# b)
# Następnie wyznaczamy współczynniki regresji liniowej:
# b1 - nachylenie prostej,
# b0 - wyraz wolny.
# Najpierw liczymy je bezpośrednio ze wzorów, a potem porównujemy z wynikiem funkcji lm() i coef().
# Otrzymujemy te same estymatory.
b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)
coef(model)
model$coef

# c)
# Rysujemy prostą regresji trzema sposobami:
# 1) podając ręcznie b0 i b1,
plot(x,y)
abline(b0, b1, col = "red")

# 2) korzystając z wektora współczynników modelu,
plot(x,y)
abline(model$coefficients, col = "red")

# 3) przekazując bezpośrednio obiekt modelu do funkcji abline().
plot(x,y)
abline(model, col = "red")
# We wszystkich trzech przypadkach dostajemy tę samą prostą.

# d)
# Powtarzamy całe doświadczenie dla różnych poziomów szumu.
x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 0.5)
y <- x + eps

plot(x,y)

sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)

plot(x,y)
abline(model, col = "red")


x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 5)
y <- x + eps

plot(x,y)

sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)

plot(x,y)
abline(model, col = "red")

# Dla małego szumu (sd = 0.5) punkty leżą blisko prostej y = x,
# korelacja jest bardzo silna, a dopasowanie regresji bardzo dobre.
# Dla dużego szumu (sd = 5) rozrzut punktów jest znacznie większy,
# korelacja maleje, a zależność liniowa staje się mniej wyraźna.
# Wniosek: im większy szum losowy, tym słabsza korelacja i gorsze dopasowanie modelu liniowego.


# Zadanie 2.4 ------------------------------------------------------------------
# a)
# Generujemy dane: ustalamy liczebność próby, wektor parametrów beta,
# budujemy macierz eksperymentu X oraz tworzymy odpowiedź y.
n <- 100
beta <- c(0.5, 1, 0.5, 0.75)
X <- cbind(1,matrix(rnorm(3*n), ncol = 3))
eps <- rnorm(n)
y <- X %*% beta + eps

# b)
# Wyświetlamy macierz X.
# Jej pierwsza kolumna składa się z jedynek i odpowiada wyrazowi wolnemu,
# a pozostałe kolumny są losowo wygenerowanymi zmiennymi objaśniającymi.
X

# c)
# Wyznaczamy rozkład QR macierzy X:
# X = QR,
# gdzie Q ma ortogonalne kolumny, a R jest macierzą górnotrójkątną.
QR <- qr(X)
R <- qr.R(QR)
Q <- qr.Q(QR)
# Następnie numerycznie sprawdzamy:
# - czy rzeczywiście X = QR,
all(Q %*% R -X < 1e-6)
# - czy Q^T Q = I.
all(t(Q) %*% Q - diag(c(1,1,1,1)) < 1e-6)  

# d)

#\hat{\beta}=(X'X)^{-1}X'y
#\hat{\beta}=(R'Q'QR)^{-1}R'Q'y
#\hat{\beta}=(R'R)^{-1}R'Q'y
#\hat{\beta}=R^{-1}R'^{-1}R'Q'y
#\hat{\beta}=R^{-1}Q'y
#R\hat{\beta}=Q'y

# Korzystamy z rozkładu QR, aby przekształcić wzór na estymator MNK:
# beta_hat = (X^T X)^(-1) X^T y.
# Po podstawieniu X = QR dostajemy:
# beta_hat = R^(-1) Q^T y,
# czyli równoważnie:
# R beta_hat = Q^T y.
# Dzięki temu zamiast odwracać dużą macierz, możemy rozwiązać układ równań
# z macierzą górnotrójkątną, co jest bardziej optymalnym rozwiązaniem.


# e)
# Obliczamy estymator beta_hat metodą QR za pomocą solve(R, t(Q) %*% y).
solve(R, t(Q)%*%y)

# Następnie porównujemy czasy działania trzech podejść:
# 1) jawne liczenie (X^T X)^(-1) X^T y,
# 2) rozwiązanie układu solve(X^T X, X^T y),
# 3) wykorzystanie rozkładu QR.
method1 <- numeric(1000)
method2 <- numeric(1000)
method3 <- numeric(1000)
for (i in 1:1000){
  start_time <- Sys.time()
  solve(t(X) %*% X) %*% t(X) %*% y
  end_time <- Sys.time()
  method1[i] <- end_time - start_time
  
  start_time <- Sys.time()
  solve(t(X) %*% X, t(X) %*% y)
  end_time <- Sys.time()
  method2[i] <- end_time - start_time
  
  start_time <- Sys.time()
  solve(R, t(Q)%*%y)
  end_time <- Sys.time()
  method3[i] <- end_time - start_time
}
mean(method1)
mean(method2)
mean(method3)
# W praktyce metoda z jawnym odwracaniem macierzy jest najmniej zalecana ze względu na czas obliczeń,
# natomiast podejście oparte na QR jest stabilne numerycznie i bardzo naturalne
# w obliczeniach regresji liniowej.

# Nie zmienia to faktu że w naszym przypadku odwracanie macierzy będzie wystarczające.



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
# Wniosek: zawsze należy oglądać wykres danych, a nie opierać się wyłącznie
# na korelacji i współczynnikach regresji.
