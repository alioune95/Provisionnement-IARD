
#Provisionnement ChainLadder et Mack sur R


#0 - Pré requis
## installer et charger le package Chain Ladder (majuscules importantes)
install.packages("ChainLadder")
library("ChainLadder")
library("readxl")

triangle<-data.frame(read_xlsx("C:/SAS/Master2/Actuariat/Non vie/MBFA/Projet/triangle_paiement.xlsx")) # pour transformer le csv en tableau de données R
head(triangle) # pour afficher les premières lignes et donc connaître le nom des colonnes

#1 On va  étudier les données

triangle$Montant <- as.numeric(triangle$Montant)
str(triangle)

triangle_cumulé<-as.triangle(triangle, origin = "Annee", dev="Developpement", value = "Montant")
triangle_cumulé
plot(triangle_cumulé)


#2 Provisionnement avec la méthode de ChainLadder
triangle_chain <- chainladder( Triangle = triangle_cumulé, delta=1)

##2.1 on affiche les résultats de chain ladder
triangle_chain
##2.2 on affiche les coefficients de passage (LDF) calculées par la méthode ChainLadder
coef(triangle_chain)
##2.3 on affiche le triangle complété par la méthode de ChainLadder
triangle_complet<-predict(triangle_chain)
triangle_complet
##2.4 la provision ChainLadder
# la diagonale
diagonale<-diag(triangle_cumulé[,10:1])
provision_chain<-triangle_complet[,10]-diagonale
provision_chain
sum(provision_chain)

#3 On va faire un provisionnement avec la méthode de Mack
triangle_mack <- MackChainLadder( Triangle = triangle_cumulé, est.sigma="Mack")
##3.1 on peut avoir directement les résultats de façon graphique !
plot(triangle_mack)

##3.2 On affiche les résultats de la méthode de Mack
triangle_mack

##3.3 On affiche le triangle complété par la méthode de Mack
triangle_mack$FullTriangle


##3.4 on estime la provision
provision_mack<-triangle_mack$FullTriangle[,10]-diagonale
sum( provision_mack)
##3.5 Mais l'intérêt c'est l'erreur d'estimation !
### Totale
triangle_mack$Total.Mack.S.E


#Simulation provision
m <- 1315614
s <- 180189^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x)
var(x)
sd(x)

plot(density(x))
hist(x, breaks = 100)

a <- data.frame(x)
write.csv(sim, file="C:/SAS/Master2/Actuariat/Non vie/MBFA/Projet/simulquant.csv")


#Simulation provision 2011

m <- 0.001475291
s <- 5464.469^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x1 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x1)
var(x1)
sd(x1)

plot(density(x1))
hist(x1, breaks = 100)

#Simulation provision 2012
m <- 7206.94331
s <- 13554.844^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x2 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x2)
var(x2)
sd(x2)

plot(density(x2))
hist(x2, breaks = 100)

#Simulation provision 2013
m <- 11300.39046
s <- 16059.644^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x3 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x3)
var(x3)
sd(x3)

hist(x3, breaks = 100)

#Simulation provision 2014
m <- 22430.46827
s <- 18869.157^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x4 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x4)
var(x4)
sd(x4)

hist(x4, breaks = 100)

#Simulation provision 2015
m <- 27977.7064

s <- 21365.791^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x5 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x5)
var(x5)
sd(x5)

hist(x5, breaks = 100)

#Simulation provision 2016
m <- 76240.31904
s <- 56175.251^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x6 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x6)
var(x6)
sd(x6)

hist(x6, breaks = 100)

#Simulation provision 2017
m <- 144176.0562
s <- 66921.217^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x7 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x7)
var(x7)
sd(x7)

hist(x7, breaks = 100)

#Simulation provision 2018
m <- 267108.7801
s <- 71172.914^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x8 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x8)
var(x8)
sd(x8)

hist(x8, breaks = 100)

#Simulation provision 2019
m <- 759173.7315
s <- 78443.785^2

sig2 = log(s + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x9 = exp(rnorm(1000, mu, sqrt(sig2)))
mean(x9)
var(x9)
sd(x9)

hist(x9, breaks = 100)

sim <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9)
sim$provision <- sim$x1+sim$x2+sim$x3+sim$x4+sim$x5+sim$x6+sim$x7+sim$x8+sim$x9
hist(prov, breaks = 100)


