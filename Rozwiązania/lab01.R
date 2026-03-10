air <- read.table("https://raw.githubusercontent.com/danielewskai/AZ_2026L/main/Dane/airpollution.txt", header = TRUE)

x <- air$Mortality
y <- air$Education


sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2*sum((y-mean(y))^2)))
cor1 <- cor(x,y)

x


k <- 100000
corrs <- numeric(k)
for(i in 1:k){
  mort_perm <- sample(x)
  corrs[i] <- cor(mort_perm, y)
}

hist(corrs)
abline(v=cor1, col = 'red')



(sum(corrs < cor1)+1)/(k+1)
# odrzucamy hipoteze zerowa o braku skorelowania
# zmienne są skorelowane ujemnie

colnames(air)


x <- air$JulyTemp
y <- air$S02Pot
cor1 <- cor(x,y)

k <- 100000
corrs <- numeric(k)
for(i in 1:k){
  temp_perm <- sample(x)
  corrs[i] <- cor(temp_perm, y)
}

hist(corrs)
abline(v=cor1, col = 'red')



(sum(corrs < cor1)+1)/(k+1)



# 2
A <-  matrix(c(-1,4,-2,5), ncol = 2, byrow = TRUE)
A

D <- diag(eigen(A)$values)
P <- eigen(A)$vectors

all(P %*% D %*% solve(P) - A < 1e-6)

plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
arrows(0,0, P[1,1], P[2,1])
arrows(0,0, P[1,2], P[2,2])

B <- matrix(c(2,1,1,2), ncol = 2,byrow = T)
solve(eigen(B)$vectors)

eigAA <- eigen(t(A) %*% A)
eigAA$vectors
eigAA$values


svd(A)$d^2
svd(A)$v


# 3
M <- as.matrix(read.csv("https://raw.githubusercontent.com/danielewskai/AZ_2026L/main/Dane/zebra.csv"))

u <- svd(M)$u
v <- svd(M)$v
d <-svd(M)$d
n <- nrow(M)

M50 <- u %*% diag(c(d[1:(n/2)], rep(0,n/2))) %*% t(v)
M10 <- u %*% diag(c(d[1:(n/10)], rep(0,n-n/10))) %*% t(v)
M4 <- u %*% diag(c(d[1:(n/25)], rep(0,n-n/25))) %*% t(v)
M2 <- u %*% diag(c(d[1:(n/50)], rep(0,n-n/50))) %*% t(v)


par(mfrow = c(2, 2))
image(M50, asp = TRUE, col = c("white", "black"))
image(M10, asp = TRUE, col = c("white", "black"))
image(M4, asp = TRUE, col = c("white", "black"))
image(M2, asp = TRUE, col = c("white", "black"))
par(mfrow = c(1, 1))

plot(d)
abline(v = c(n/2,n/10,n/25,n/50), col = 'red')
