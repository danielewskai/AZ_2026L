# Zadanie 4.1 ------------------------------------------------------------------
air <-  read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/airpollution.txt", header = T)
dane <- air[,c(1,2,5,6,8,12,16)]
model <- lm(Mortality ~ ., data = dane)

# c)
# Następnie przeprowadzamy test istotności całego modelu (wszystkich zmiennych).
# Hipoteza zerowa mówi, że wszystkie współczynniki przy zmiennych objaśniających
# są równe zero, czyli model nie wyjaśnia zmienności zmiennej Mortality lepiej
# niż sam wyraz wolny.
SSR <- sum((model$fitted.values-mean(dane$Mortality))**2)
SSE <- as.vector(residuals(model) %*% residuals(model))
X <- model.matrix(model)
p <- ncol(X) # dim(X)[2]
n <- nrow(X) # dim(X)[1]

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

# Zadanie 4.3 ------------------------------------------------------------------
# W tym zadaniu badamy moc testów istotności współczynników w modelu regresji liniowej.
# Moc testu rozumiemy jako prawdopodobieństwo poprawnego odrzucenia hipotezy zerowej,
# gdy w rzeczywistości dany współczynnik jest różny od zera.
# Szacujemy ją metodą symulacyjną, powtarzając eksperyment wiele razy.

# a)
# Generujemy B prób z modelu liniowego o zadanych parametrach beta.
# Następnie w każdej próbie dopasowujemy model i sprawdzamy,
# czy test t dla kolejnych współczynników odrzuca hipotezę zerową na poziomie istotności 0.05.
n <- 100
B <- 100
beta <-  c(0.5, 1,  0.5, 0.05)
moc1 <- numeric(B)
moc2 <- numeric(B)
moc3 <- numeric(B)
for (i in 1:B){
  X <- cbind(1,matrix(rnorm(3*n), ncol = 3))
  eps <-  rnorm(n, 0, 1)
  y <- X %*% beta + eps
  M <- lm(y ~ X-1)
  pvals <- summary(M)$coef[,4]
  moc1[i] <- ifelse(pvals[2]<0.05, 1, 0)
  moc2[i] <- ifelse(pvals[3]<0.05, 1, 0)
  moc3[i] <- ifelse(pvals[4]<0.05, 1, 0)
}
# Średnie wartości wektorów moc1, moc2, moc3 są oszacowaniami mocy testu
# dla odpowiednich współczynników.
mean(moc1)
mean(moc2)
mean(moc3)
# Ponieważ współczynniki mają różne wartości rzeczywiste,
# ich moce również są różne.

# Im mniejszy współczynnik rzeczywisty, tym mniejsza moc - trudniej wykryć istotność zmiennej

# b)
# Sprawdzamy teraz, jak moc testu zależy od liczebności próby.
# Dla każdej wartości n powtarzamy symulację B razy
# i zapisujemy średnią częstość odrzuceń hipotezy zerowej
# dla trzech testowanych współczynników.
n <-  c(20,50,100,200,300,400,500)
B <-  100

MOC1 <- numeric(length(n))
MOC2 <- numeric(length(n))
MOC3 <- numeric(length(n))

for (j in 1:length(n)) {
  moc1 <- numeric(B)
  moc2 <- numeric(B)
  moc3 <- numeric(B)
  for (i in 1:B){
    X <- cbind(1,matrix(rnorm(3*n[j]), ncol = 3))
    eps <-  rnorm(n[j], 0, 1)
    y <- X %*% beta + eps
    M <- lm(y ~ X-1)
    pvals <- summary(M)$coef[,4]
    moc1[i] <- ifelse(pvals[2]<0.05, 1, 0)
    moc2[i] <- ifelse(pvals[3]<0.05, 1, 0)
    moc3[i] <- ifelse(pvals[4]<0.05, 1, 0)
  }
  MOC1[j] <- mean(moc1)
  MOC2[j] <- mean(moc2)
  MOC3[j] <- mean(moc3)
}

plot(n, MOC1, type = "b", ylim = c(0,1))
lines(n, MOC2, type = "b", ylim = c(0,1), col = "red")
lines(n, MOC3, type = "b", ylim = c(0,1), col = "blue")

# Wraz ze wzrostem n moc testu będzie rosła,
# ponieważ większa próba pozwala dokładniej oszacować współczynniki modelu.
# Współczynnik o najmniejszej wartości bezwzględnej ma najmniejszą moc,
# ponieważ trudniej wykryć jego istotność.

# c)
# Tym razem interesuje nas moc testu istotności całego modelu.
# Wszystkie współczynniki są małe, ale różne od zera.
# Dla każdej liczebności próby generujemy dane, dopasowujemy model
# i wykonujemy test F dla hipotezy zerowej mówiącej,
# że wszystkie współczynniki przy zmiennych objaśniających są równe zero.
n <-  c(20,50,100,200,300,400,500)
B <-  100
beta <-  c(0.05, 0.05,  0.05, 0.05)

MOC <- numeric(length(n))

for (j in 1:length(n)){
  moc <- numeric(B)
  for (i in 1:B){
    X <- matrix(rnorm(3 * n[j]), ncol = 3)
    X1 <- cbind(1, X)
    eps <- rnorm(n[j])
    y <- X1 %*% beta + eps
    model <- lm(y ~ X)
    F_test <- summary(model)$fstat
    p_value <- 1 - pf(F_test[1], F_test[2], F_test[3])
    moc[i] <- ifelse(p_value < 0.05, 1, 0)
  }
  MOC[j] <- mean(moc)
}

plot(n, MOC, type = "b", ylim = c(0,1))

# Wraz ze wzrostem liczebności próby rośnie moc testu istotności całego modelu.
# Oznacza to, że przy większej liczbie obserwacji łatwiej wykryć,
# że co najmniej jedna zmienna objaśniająca jest istotna.
