setwd("D:\\Dokumenty\\STUDIA\\MATEMATYKA dr\\SM2_lab\\Zbiory danych")
d=read.table("dice.dat",header=T)
d

d=d[d$Group=="D",]
d

x=d$r
y=d$Sixes/d$Rolls
plot(x,y)



# Model logistyczny:

g=glm(cbind(Sixes,Rolls)~r,family=binomial,data=d)
summary(g)

#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.72806    0.04878   14.93   <2e-16 ***
# r           -1.92120    0.05395  -35.61   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#     Null deviance: 1753.46  on 71  degrees of freedom
# Residual deviance:  174.70  on 70  degrees of freedom
# AIC: 488.48

# Test dopasowania:

1-pchisq(g$dev,g$df.res)     #  1.432163e-10   model slabo dopasowany

# Test istotnosci:

1-pchisq(g$null.dev-g$dev,g$df.null-g$df.res)   # 0  zmienna jest istotna


points(x,g$fit,col="red")


##############################################################

g2=glm(cbind(Sixes,Rolls)~r+I(r^2),family=binomial,data=d)
summary(g2)

#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.06862    0.14198  -0.483    0.629    
#r            0.48565    0.36883   1.317    0.188    
#I(r^2)      -1.48970    0.22465  -6.631 3.33e-11 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#    Null deviance: 707.822  on 11  degrees of freedom
#Residual deviance:  11.686  on  9  degrees of freedom
#AIC: 85.413

# Test dopasowania:

1-pchisq(g2$dev,g2$df.res)     #    0.2315895   model dobrze dopasowany

# Test istotnosci:

1-pchisq(g2$null.dev-g2$dev,g2$df.null-g2$df.res)   # 0  zmienna jest istotna

points(x,g2$fit,col="blue")


# Mozna jeszcze uproscic 

g3=glm(cbind(Sixes,Rolls)~I(r^2),family=binomial,data=d)
summary(g3)

1-pchisq(g3$dev-g2$dev,g3$df.res-g2$df.res)   #  0.1826278 mniejszy lepszy

#

predict(g3,newdata=data.frame(r=0.21),type="response")



#########################################################################

Model probitowy:

p=glm(cbind(Sixes,Rolls)~r,family=binomial(link="probit"),data=d)
summary(p)

#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.45994    0.04771   9.641   <2e-16 ***
# r           -1.20289    0.05058 -23.780   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#     Null deviance: 707.822  on 11  degrees of freedom
# Residual deviance:  51.361  on 10  degrees of freedom
# AIC: 123.09


# Test dopasowania:

1-pchisq(p$dev,p$df.res)     #   1.498159e-07   model slabo dopasowany

# Test istotnosci:

1-pchisq(p$null.dev-p$dev,p$df.null-p$df.res)   # 0  zmienna jest istotna

points(x,p$fit,col="green")



p2=glm(cbind(Sixes,Rolls)~r+I(r^2),family=binomial(link="probit"),data=d)
summary(p2)

#            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.05645    0.08356   0.676    0.499    
# r           -0.08213    0.20095  -0.409    0.683    
# I(r^2)      -0.63252    0.11378  -5.559 2.71e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
# (Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 707.822  on 11  degrees of freedom
# Residual deviance:  14.296  on  9  degrees of freedom
# AIC: 88.023


# Test dopasowania:

1-pchisq(p2$dev,p2$df.res)     #  0.1121681   model dobrze dopasowany

# Test istotnosci:

1-pchisq(p2$null.dev-p2$dev,p2$df.null-p2$df.res)   # 0  zmienna jest istotna

points(x,p2$fit,col="yellow")


p3=glm(cbind(Sixes,Rolls)~I(r^2),family=binomial(link="probit"),data=d)
summary(p3)

# Test dopasowania:
1-pchisq(p3$dev,p3$df.res)     #  0.1121681   model dobrze dopasowany

# Test istotnosci:
1-pchisq(p3$null.dev-p3$dev,p3$df.null-p3$df.res)   # 0  zmienna jest istotna

points(x,p3$fit,col="red")

#

predict(p3,newdata=data.frame(r=0.21),type="response")


