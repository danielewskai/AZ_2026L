# Zadanie 5.4 ------------------------------------------------------------------
saving <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/refs/heads/main/Dane/savings.txt", header = T)

# a)
# Dopasowujemy model regresji liniowej dla zmiennej Savings.
# Pomijamy zmienną Country, ponieważ jest ona nieuzględniona w naszym zadaniu.
model <- lm(Savings~. - Country, data = saving)

# Szukamy obserwacji odstających za pomocą reszt studentyzowanych modyfikowanych.
# Obserwacje z |rstudent| > 2 traktujemy jako potencjalnie odstające.
which(abs(rstudent(model))>2)

# Sprawdzamy również wartości h_ii
# Za potencjalnie wpływowe uznajemy obserwacje z h_ii > 2p/n,
# gdzie p oznacza liczbę parametrów modelu, a n liczbę obserwacji.
X <- model.matrix(model)
which(hatvalues(model)>2*ncol(X)/nrow(X))

# Rysujemy wykres odległości Cooka.
# Umożliwia on wykrycie obserwacji silnie wpływających na dopasowanie modelu.
# Możemy zrobić to za pomocą jednego z wykresów diagnostycznych
plot(model, 4)

# albo obliczyć wartości ręcznie i narysować wykres słupkowy
#cooks_dist <- rstandard(model)^2 * hatvalues(model)/(p*(1-hatvalues(model))) # wzór uproszczony 
#which(cooks_dist > 4/(n-p-1)) # heurystyka do cooks distance
cd <- cooks.distance(model) # Gotowa funkcja do obliczania odległości Cook'a
barplot(cd)

# Wskazujemy obserwację o największej odległości Cooka, zgodnie z treścią zadania
which.max(cd)

# Usuwamy potencjalnie wpływową obserwację i dopasowujemy model ponownie.
model2 <- lm(Savings~. - Country, data = saving, subset = -c(49))

# Porównujemy oba modele.
# Jeśli po usunięciu obserwacji współczynniki i dopasowanie modelu wyraźnie się zmieniają,
# oznacza to, że ta obserwacja miała istotny wpływ na wyniki regresji.
summary(model)
summary(model2)


# b)
# Badamy cząstkowe zależności między Savings a wybranymi predyktorami.
# Do tego używamy wykresów częściowej regresji
# Taki wykres pokazuje cząstkową zależność między Savings i dpi
# po uwzględnieniu pozostałych predyktorów w modelu.
# Zgodnie z teorią - współczynnik prostej w tak dopasowanych danych jest odzwierciedleniem współczynnika w modelu Y~X
model_dpi <- lm(dpi ~ . - Savings - Country, data = saving, subset = -c(49))
model_bez_dpi <- lm(Savings ~ . - dpi - Country, data = saving, subset = -c(49))
plot(residuals(model_dpi), residuals(model_bez_dpi))
abline(lm(residuals(model_bez_dpi)~residuals(model_dpi)))
# Widzimy, że prosta ta jest prawie pozioma, tzn. dpi nie wpływa znacząco na nasz model - być może informacje niesione przez dpi
# są przekazane już przez inną zmienną. Zależność cząstkowa jest bardzo słaba, więc po uwzględnieniu
# pozostałych zmiennych wpływ dpi na Savings wydaje się niewielki.

# Analogicznie postępujemy dla zmiennej ddpi.
# Dzięki temu możemy wizualnie ocenić, czy po usunięciu wpływu innych zmiennych
# zależność ma charakter w przybliżeniu liniowy.
model_ddpi <- lm(ddpi ~ . - Savings - Country, data = saving, subset = -c(49))
model_bez_ddpi <- lm(Savings ~ . - ddpi - Country, data = saving, subset = -c(49))
plot(residuals(model_ddpi), residuals(model_bez_ddpi))
abline(lm(residuals(model_bez_ddpi)~residuals(model_ddpi)))
# W tym przypadku widzimy, że zmienna ddpi ma spory wpływ na wyniki regresji, prosta jest nachylona (nie pozioma)

# c)
# Sprawdzamy korelację pomiędzy zmiennymi Pop15 i Pop75 po usunięciu obserwacji 49.
# Silna korelacja między predyktorami może świadczyć o współliniowości.
cor(saving$Pop15[-49],saving$Pop75[-49])

# W summary(..., cor = TRUE) otrzymujemy dodatkowo macierz korelacji estymatorów współczynników.
summary(model2, cor = T)

# Zmienne Pop15 i Pop75 są silnie ujemnie skorelowane,
# natomiast estymatory odpowiadających im współczynników regresji
# są dodatnio skorelowane.
# Taka sytuacja jest dopuszczalna i może wskazywać na współliniowość zmiennych objaśniających.

# d)
# Analizujemy wpływ zmiennej Pop15 na Savings.
# Przy takich analizach przydatne są wykresy częściowych rezyduów.
# Wykres Pop15 * beta_hat + reszty vs Pop15 pozwala spojrzeć na zależność od strony składnika tej zmiennej
# oraz sprawdzić, czy zależność wygląda liniowo.
coef(model2)
plot(saving$Pop15[-49],saving$Pop15[-49]*coef(model2)[2]+residuals(model2))
abline(lm(saving$Pop15[-49]*coef(model2)[2]+residuals(model2)~saving$Pop15[-49]))
# W tym przypadku wspołczynnik prostej dopasowanej na takich danych odzwierciedla bezpośrednio współczynnik z bazowego modelu
# Dla tych danych możemy zauważyć dodatkowe zjawisko, wartości zmiennej Pop15 tworzą 
# dwie separowalne od siebie grupy. Rozważając uwzględnienie takiego podziału
# możemy dopasować model lepiej dopasowany do rozważanych danych.

# Dopasowujemy modele do dwóch grup osobno
m3 <- lm(Savings~.-Country, data = saving[-49,], subset = Pop15<35)
m4 <- lm(Savings~.-Country, data = saving[-49,], subset = Pop15>35)

# Na poniższych wykresach możemy zauważyć, że model w zależności od grupy wygląda inaczej
# W ten sposób lepiej odzwierciedlamy zachowanie w danych
saving <- saving[-49,]
plot(saving$Pop15[saving$Pop15<35],saving$Pop15[saving$Pop15<35]*coef(m3)[2]+residuals(m3))
abline(lm(saving$Pop15[saving$Pop15<35]*coef(m3)[2]+residuals(m3)~saving$Pop15[saving$Pop15<35]))

plot(saving$Pop15[saving$Pop15>35],saving$Pop15[saving$Pop15>35]*coef(m4)[2]+residuals(m4))
abline(lm(saving$Pop15[saving$Pop15>35]*coef(m4)[2]+residuals(m4)~saving$Pop15[saving$Pop15>35]))

# Funkcja prplot() z pakietu faraway rysuje wykres częsciowych reziduów
library(faraway)
prplot(model2, 1)

# Zadanie 5.5 ------------------------------------------------------------------

# a)
# Generujemy dane z trzema grupami obserwacji.
# W każdej grupie zmienna x pochodzi z innego przedziału,
# a składnik losowy ma inne odchylenie standardowe.
# Oznacza to, że wariancja błędu nie jest stała - mamy heteroskedastyczność.
n <-  30
x1 <- runif(n,0,10)
x2 <- runif(n,10,20)
x3 <- runif(n,20,30)

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,3)
eps3 <- rnorm(n,0,5)

x <- c(x1,x2,x3)
eps <- c(eps1,eps2,eps3)
y <- x+eps

# Dopasowujemy zwykły model regresji liniowej metodą najmniejszych kwadratów.
model <- lm(y~x)

plot(x,y)
abline(model) # Nie wygląda to na źle dopasowany model, ALE nie spełnia on założeń
# regresji liniowej, która zakłada homoskedastyczność - przez co błędy standardowe, testy istotności i przedziały ufności
# mogą być niedokładne.

# b)
# Oglądamy reszty studentyzowane względem indeksu obserwacji.
# Dzięki temu można zauważyć, że rozrzut reszt nie jest stały.
plot(1:(3*n), rstudent(model), xlab = "Index")
# Podobny efekt widzimy na wykresie Scale-Location
plot(model,3)

# Wykres Residuals vs Fitted również pozwala ocenić heteroskedastyczność.
# Widać że rozrzut danych nie jest losowy, lecz tworzy 'lejek'. 
plot(model,1)

# c)
# Próbujemy uwzględnić heteroskedastyczność za pomocą ważonych najmniejszych kwadratów.
# Przybliżamy zależność między skalą reszt a wartościami dopasowanymi,
# a następnie na tej podstawie konstruujemy przybliżenie wariancji.
res <- residuals(model)
sigma2 <- lm(abs(res)~model$fitted.values)$fitted.values^2
# Wagi przyjmujemy odwrotnie proporcjonalne do oszacowanej wariancji.
# Obserwacje o większej wariancji dostają mniejszą wagę.
weights <- 1/sigma2
# Dopasowujemy model ważony, używając parametru weights w funkcji lm
# Możemy też dopasować wagi za pomocą metody iteracyjnej z wykładu
model2 <- lm(y~x, weights = weights) 

# Porównujemy reszty studentyzowane dla modelu ważonego.
# Jeśli heteroskedastyczność została częściowo skorygowana,
# rozrzut reszt powinien wyglądać bardziej jednorodnie.
# I tak się tutaj dzieje, nie ma już widocznego 'lejka', widzimy chmurę punktów bez konkretnej struktury 
plot(1:(3*n), rstudent(model2), xlab = "Index")

plot(x,y)
abline(model)
abline(model2, col = 'red') 
# Rysujemy obie dopasowane proste na wykresie rozrzutu danych
# Oba modele wydają się bardzo podobne w dopasowaniu, lecz to co najważniejsze
# nie jest w tym przypadu widoczne dla oczu ;)
# Przekonamy się o tym w kolejnym podpunkcie


# d)
# Dodajemy pojedynczą obserwację odstającą.
# Chcemy sprawdzić, jak model reaguje na jednoczesną obecność
# heteroskedastyczności i obserwacji odstającej.
x <- c(x,5)
y <- c(y,10)

# Ponownie analizujemy reszty studentyzowane i wykres diagnostyczny.
plot(x,y)
model <- lm(y~x)
abline(model)

plot(1:(3*n+1), rstudent(model), xlab = "Index")
plot(model,3)

# Dodana obserwacja nie wybija się znacząco ponad pozostałe obserwacje, ale 
# wg założeń w zadaniu jest ona odstająca dla tego przedziału x-ów

res <- residuals(model)
sigma2 <- lm(abs(res)~model$fitted.values)$fitted.values^2
weights <- 1/sigma2
model2 <- lm(y~x, weights = weights) 
plot(1:(3*n+1), rstudent(model2), xlab = "Index")

#Podobnie w tym przypadku - dodana obserwacja nie rzuca się w oczy

# Jednak okazuje się, że nasz podstawowy model nieuwzględniający heteroskedastyczności nie wykrywa obserwacji odstającej 
abs(rstudent(model))>2
# Natomiast model ważony uznaje już taką obserwację za potencjalnie odstającą
abs(rstudent(model2))>2

# I tutaj właśnie jest przewaga modelu ważonego w przypadku heteroskedastyczności
# Model ważony lepiej uwzględnia zmienną wariancję błędu,
# dlatego trafniej identyfikuje obserwacje potencjalnie odstające
# w sytuacji heteroskedastyczności.