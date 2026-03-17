# =========================
# Laboratorium 1
# Test permutacyjny, rozkład spektralny, SVD
# =========================

# Zadanie 1.1 ------------------------------------------------------------------
# pobieramy dane bezpośrednio z repozytorium, pamiętamy o header = T aby poprawnie wczytać nazwy kolumn/zmiennych
# header = TRUE oznacza, że pierwszy wiersz zawiera nazwy zmiennych
# dzięki temu możemy później odwoływać się do kolumn po nazwach, np. air$Mortality
air <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/main/Dane/airpollution.txt", header = TRUE)
#air<-read.table("airpollution.txt",header = T)

# a) Korelacja próbkowa Pearsona: Mortality vs Education
# Wybieramy dwie zmienne, których zależność chcemy zbadać.
# Korelacja Pearsona mierzy siłę !liniowej! zależności między zmiennymi.
mor<-air$Mortality
edu<-air$Education
cor1<-cor(mor,edu)
cor1
# Obliczamy korelację próbkową.
# Wynik bliski -1 oznacza silną zależność malejącą,
# wynik bliski 1 oznacza silną zależność rosnącą,
# a wynik bliski 0 oznacza brak liniowej zależności.


# b) Test permutacyjny: permutujemy Mortality, Education zostaje bez zmian

# Idea testu permutacyjnego:
# H0: brak związku między Mortality i Education (korelacja = 0)
# H1: istnieje związek zgodny z kierunkiem rozważanym w teście (korelacja < 0)
# Jeśli H0 jest prawdziwa, to parowanie obserwacji między tymi dwiema zmiennymi jest przypadkowe.
# Dlatego możemy permutować jedną z nich i badać,
# jak zachowuje się statystyka testowa (tu: korelacja) dla H0.

# Im większe k, tym dokładniejsze przybliżenie rozkładu statystyki testowej dla H0,
# ale tym dłuższy czas obliczeń.
k<-100000
# Tu zapiszemy korelacje uzyskane po kolejnych permutacjach.
corrs<-numeric(k)
for(i in 1:k)
{
  # Permutujemy jedną zmienną, aby zniszczyć ewentualny związek między zmiennymi,
  # ale zachować jej własny rozkład empiryczny.
  morper<-sample(mor)        # permutacja etykiet
  # Przy założeniu hipotezy zerowej o braku związku liniowego (zerowa korelacja) taka permutacja nie powinna zmieniać rozkładu statystyki.
  corrs[i]<-cor(morper,edu)  # korelacja pod H0
}

# c) Histogram rozkładu permutacyjnego i zaznaczenie cor1
# Histogram pokazuje empiryczny rozkład otrzymanych korelacji 
# Czerwona linia oznacza wartość korelacji zaobserwowaną dla oryginalnych danych.
# Jeśli znajduje się ona daleko w ogonie rozkładu permutacyjnego,
# to jest to argument przeciwko H0. Oryginalne dane są w jakiś sposób zależne
hist(corrs)
abline(v = cor1,col='red')  

# d) p-value
# Test jednostronny p = P(cor_perm < cor_obs)
# Dodanie 1 do licznika i mianownika daje stabilniejszy estymator p-value
# i zapobiega sytuacji, w której otrzymamy dokładnie 0.
(1+sum(corrs<cor1))/(1+length(corrs))
#odrzucamy H0 gdyż p_val < 0.05 = poziom istotności

# Uwaga:
# kierunek nierówności w p-value zależy od hipotezy alternatywnej.
# Jeśli sprawdzamy korelację ujemną, używamy lewego ogona: corrs < cor1.
# Dla korelacji dodatniej byłoby corrs > cor1.
# Dla testu dwustronnego można użyć wartości bezwzględnych.

# e) Powtarzamy cały schemat dla pary zmiennych JulyTemp i S02Pot.
jul<-air$JulyTemp
S<-air$S02Pot
cor1<-cor(jul,S)
cor1

k<-100000
corrs<-numeric(k)
for(i in 1:k)
{
  julper<-sample(jul)
  corrs[i]<-cor(julper,S)
}

hist(corrs)
abline(v = cor1,col='red')

sum(corrs<cor1)/length(corrs)
# w tym przypadku brak podstaw do odrzucenia hipotezy H0 gdyż P_val > 0.05 = poziom istotności
# czyli możemy zakładać że zmienne są ze sobą nieskorelowane




# Zadanie 1.2 ------------------------------------------------------------------
# Definiujemy macierz 2x2.
# byrow = TRUE oznacza, że liczby wpisujemy wierszami.
A <- matrix(c(-1, 4, -2, 5), ncol = 2, byrow = TRUE)

# a) Rozkład spektralny: A = P D P^{-1}
# Funkcja eigen() zwraca wartości własne i odpowiadające im wektory własne.
# Jeśli macierz jest diagonalizowalna, możemy zapisać ją w postaci:
# A = P D P^{-1},
# gdzie kolumny P to wektory własne, a D to diagonalna macierz wartości własnych.
eigA <- eigen(A)
P <- eigA$vectors       # wektory własne jako kolumny
D <- diag(eigA$values)  # wartości własne na przekątnej
solve(P)

# b) Sprawdzenie: A ≈ P D P^{-1}
# Sprawdzamy numerycznie, czy z rozkładu spektralnego odzyskujemy macierz A.
# Z powodu błędów numerycznych lepiej porównywać z tolerancją.
all(abs(P %*% D %*% solve(P) - A) < 1e-6)

# c) Wektory bazy diagonalizującej (kolumny P)
# To jest baza, w której macierz A działa diagonalnie.
plot(NULL,xlim=c(-2,2),ylim=c(-2,2))
arrows(0, 0, P[1,1], P[2,1])
text(P[1,1], P[2,1], "x", pos=1)
arrows(0, 0, P[1,2], P[2,2])
text(P[1,2], P[2,2], "y", pos=1)

# Wektory własne zwracane przez eigen() są zwykle znormalizowane do długości 1.
# Sprawdzamy to dla drugiego wektora.
sqrt((P[1,2])^2+(P[2,2])^2)

# d)
# Macierz B jest symetryczna.
# Dla macierzy symetrycznych rzeczywistych:
# - wartości własne są rzeczywiste,
# - wektory własne można wybrać jako ortonormalne,
# - macierz P jest ortogonalna, więc P^{-1} = P^T.
B <- matrix(c(2, 1, 1, 2), ncol = 2, byrow = T)
eigB <- eigen(B)
PB <- eigB$vectors
DB <- diag(eigB$values)

PB
solve(PB)  # dla macierzy ortogonalnej: P^{-1} = P^T

# macierze są identyczne
# Dla macierzy ortogonalnej odwrotność jest równa transpozycji.
all(t(PB) - solve(PB) < 1e-6)

# e) SVD i związek z A^T A
# Rozkład SVD zapisuje macierz w postaci:
# A = U D V^T,
# gdzie:
# - kolumny U i V są ortogonalne,
# - elementy diagonalne D to wartości singularne (nieujemne).
svdA <- svd(A)
t(A) %*% A  # A^T A jest symetryczna i dodatnio półokreślona
svdA$v %*% diag(svdA$d)^2 %*% t(svdA$v) # Porównanie: V diag(d^2) V^T = A^T A

# Ważny związek:
# kolumny macierzy V są wektorami własnymi macierzy A^T A,
# a kwadraty wartości singularnych są wartościami własnymi A^T A.

eigAA <- eigen(t(A) %*% A)
PAA <- eigAA$vectors # powinny odpowiadać V (do znaku)
# Wektory własne i singularne mogą różnić się znakiem.
# To nie jest błąd: jeśli v jest wektorem własnym, to -v też nim jest.
DAA <- eigAA$values # powinny odpowiadać d^2
DAA
svd(A)$d^2
PAA
svd(A)$v

# Zadanie 1.3 ------------------------------------------------------------------
# Wczytujemy obraz zapisany jako macierz 0-1.
# Każdy element macierzy odpowiada jednemu pikselowi.
M <- as.matrix(read.csv("https://raw.githubusercontent.com/danielewskai/AZ_2026L/main/Dane/zebra.csv"))
#M <- as.matrix(read.csv("./dane/zebra.csv"))
# Funkcja image() interpretuje macierz jako obraz.
image(M, asp = TRUE, col = c("white", "black"), xaxt = "n", yaxt = "n")

# a)
# Rozkład SVD pozwala zapisać obraz jako sumę prostych macierzy rzędu 1.
# Największe wartości singularne odpowiadają najważniejszym wzorcom obecnym w obrazie.
svd.M <- svd(M)
u <- svd.M$u
v <- svd.M$v
d <- svd.M$d
n <- nrow(M) 

# b) Rekonstrukcje obrazu przy użyciu: 50%, 10%, 4%, 2% największych wartości singularnych
# Jeśli zostawimy tylko kilka największych wartości singularnych,
# otrzymamy przybliżenie obrazu o mniejszym rzędzie.
# To jest klasyczna idea kompresji:
# zachowujemy najważniejszą strukturę, odrzucamy drobniejsze szczegóły.
# Im mniej wartości  singularnych zostawimy, tym silniejsza kompresja,
# ale też większa utrata informacji.
M50 <- u %*% diag(c(d[c(1:(n/2))], rep(0, n - n/2))) %*% t(v)
M10 <- u %*% diag(c(d[c(1:(n/10))], rep(0, n - n/10))) %*% t(v)
M4 <- u %*% diag(c(d[c(1:(n/25))], rep(0, n - n/25))) %*% t(v)
M2 <- u %*% diag(c(d[c(1:(n/50))], rep(0, n - n/50))) %*% t(v)

# c) Rysowanie 4 obrazów naraz
par(mfrow = c(2, 2))
image(M50, asp = TRUE, col = c("white", "black"))
image(M10, asp = TRUE, col = c("white", "black"))
image(M4, asp = TRUE, col = c("white", "black"))
image(M2, asp = TRUE, col = c("white", "black"))
par(mfrow = c(1, 1))

# d) Wykres wartości singularnych i progi
# Wykres wartości singularnych pokazuje,
# jak szybko maleje ilość informacji niesionej przez kolejne składowe.
# Jeśli wartości singularne szybko spadają,
# to obraz dobrze nadaje się do kompresji.
plot(d)
abline(v = c(n/2, n/10, n/25, n/50), col = "red")
