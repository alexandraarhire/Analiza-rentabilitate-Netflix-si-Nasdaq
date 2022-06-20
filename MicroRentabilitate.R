#Pas 3 tema
install.packages("moments")
library("moments")
install.packages("corrplot")
library("corrplot")
tema<-read.csv(file="Tema_Micromanag_Netflix.csv",header=TRUE,sep=";")
View(tema)
#Transformati seriile de date ce contin preturi (actiune si indice de piată)
# în rentabilităţi conform formulei:
#Rentabilitate Netflix
?length
length(tema$Pret_act_NFLX)
r_netflix=c()
r_netflix[1]=0
r_netflix
for(i in 2:253)
{r_netflix[length(r_netflix)+1]<-tema$Pret_act_NFLX[i]/tema$Pret_act_NFLX[i-1]-1}
View(r_netflix)

#Rentabilitate Nasdaq

length(tema$Pret_indice.Nasdaq)
r_nasdaq=c()
r_nasdaq[1]=0
r_nasdaq
for(i in 2:253){
  r_nasdaq[length(r_nasdaq)+1]<-tema$Pret_indice.Nasdaq[i]/tema$Pret_indice.Nasdaq[i-1]-1
}
View(r_nasdaq)
?cbind

tema<- cbind(tema, r_netflix, r_nasdaq)
View(tema)

summary(tema)

#Analiza volum Netflix
summary(tema$Volum_act_NFLX)
mean(tema$Volum_act_NFLX)
#in medie volumul Netflix este egal cu 4300895

sd(tema$Volum_act_NFLX)
#in medie valorile se abat de la media generala cu 4807701

cv=sd(tema$Volum_act_NFLX)/mean(tema$Volum_act_NFLX)
cv
#cv= 1.117837>0.30 => media nu este reprezentativa si seria nu este omogena

skewness(tema$Volum_act_NFLX)
# coef de asimetrie= 7.401227>0 =>asimetrie pozitiva la dreapta, deci predomina valorile mici

kurtosis(tema$Volum_act_NFLX)
#73.73811>3 => distributie leptokurtica

hist(tema$Volum_act_NFLX, col="yellow", main="Histograma volum Netflix")
boxplot(tema$Volum_act_NFLX, col="navy",main="Boxplot volum Netflix")
#identificare outlieri
outlier_volum_netflix=boxplot(tema$Volum_act_NFLX,plot=F)$out
View(as.matrix(outlier_volum_netflix))

#Analiza volum Nasdaq
summary(tema$Volum_indice.Nasdaq)
mean(tema$Volum_indice.Nasdaq)
#in medie volumul Nasdaq este egal cu 4909650435
sd(tema$Volum_indice.Nasdaq)
#in medie valorile se abat de la media generala cu 1109428059
cv=sd(tema$Volum_indice.Nasdaq)/mean(tema$Volum_indice.Nasdaq)
cv
#cv= 0.2259688<0.30 => media este reprezentativa si seria este omogena

skewness(tema$Volum_indice.Nasdaq)
# coef de asimetrie= 1.699124>0 =>asimetrie pozitiva la dreapta, deci predomina valorile mici

kurtosis(tema$Volum_indice.Nasdaq)
#6.732654>3 => distributie leptokurtica

hist(tema$Volum_indice.Nasdaq, col="aquamarine", main="Histograma volum Nasdaq")
boxplot(tema$Volum_indice.Nasdaq, col="green",main="Boxplot volum Nasdaq")
outlier_volum_nasdaq=boxplot(tema$Volum_indice.Nasdaq,plot=F)$out
View(as.matrix(outlier_volum_nasdaq))

#Analiza pret Netflix
summary(tema$Pret_act_NFLX)
mean(tema$Pret_act_NFLX)
sd(tema$Pret_act_NFLX)
cv=sd(tema$Pret_act_NFLX)/mean(tema$Pret_act_NFLX)
cv
#cv=0.111<0.30=> media reprezentativa si serie omogena
skewness(tema$Pret_act_NFLX)
#coef de asimetrie=0.1091461>0=>asimetrie pozitiva la dreapta
kurtosis(tema$Pret_act_NFLX)
#3.3281>3=>distributie leptokurtica

hist(tema$Pret_act_NFLX,col="orange",main="Histograma preturi Netflix")
boxplot(tema$Pret_act_NFLX,col="purple",main="Boxplot preturi Netflix")
outlier_pret_netflix=boxplot(tema$Pret_act_NFLX,plot=F)$out
View(as.matrix(outlier_pret_netflix))

#Analiza pret Nasdaq
summary(tema$Pret_indice.Nasdaq)
mean(tema$Pret_indice.Nasdaq)
sd(tema$Pret_indice.Nasdaq)
cv=sd(tema$Pret_indice.Nasdaq)/mean(tema$Pret_indice.Nasdaq)
cv
#cv=0.057<0.3=>medie reprezentativa si serie omogena
skewness(tema$Pret_indice.Nasdaq)
#-0.01<0=>asimetrie negativa la stanga predomina valorile mari
kurtosis(tema$Pret_indice.Nasdaq)
#2.02854<3 rezulta distributie platikurtica
hist(tema$Pret_indice.Nasdaq,col="red",main="Histograma preturi Nasdaq")
boxplot(tema$Pret_indice.Nasdaq,col="blue",main="Boxplot preturi Nasdaq")
#outlieri pret nasdaq
outlier_pret_nasdaq=boxplot(tema$Pret_indice.Nasdaq,plot=F)$out
View(as.matrix(outlier_pret_nasdaq))

#Analiza ratei de rentabilitate Netflix
summary(tema$r_netflix)
mean(tema$r_netflix)
sd(tema$r_netflix)
cv_rent_netflix=sd(tema$r_netflix)/mean(tema$r_netflix)
cv_rent_netflix
#-36.82<0.3 => medie reprezentativa si serie omogena
skewness(tema$r_netflix)
#-2.812<0=>asimetrie la stanga predomina valori mari
kurtosis(tema$r_netflix)
#33.8453>3 =>distributie leptokurtica
hist(tema$r_netflix,col="yellow",main="Histograma rata rentabilitate Netflix")
boxplot(tema$r_netflix,col="pink",main="Boxplot rata rentabilitate Netflix")
#outlieri
outlier_r_netflix=boxplot(tema$r_netflix,plot=F)$out
View(as.matrix(outlier_r_netflix))


#Analiza ratei de rentabilitate Nasdaq
mean(tema$r_nasdaq)
sd(tema$r_nasdaq)
cv_rent_nasdaq=sd(tema$r_nasdaq)/mean(tema$r_nasdaq)
cv_rent_nasdaq
skewness(tema$r_nasdaq)
kurtosis(tema$r_nasdaq)
hist(tema$r_nasdaq,col="navy",main="Histograma rata rentabilitatii Nasdaq")
boxplot(tema$r_nasdaq,col="green",main="Boxplot rata rentabilitatii Nasdaq")

#outlieri
outlier_r_nasdaq=boxplot(tema$r_nasdaq,plot=F)$out
View(as.matrix(outlier_r_nasdaq))
# dev.off() ca sa ne apara doar un grafic

#Realizarea matricei de corelatie
matrice_cor=cor(tema[-1],)
View(matrice_cor)

#Realizare grafic matrice de corelatie
corrplot(matrice_cor,method="circle",main="Grafic matrice de corelatie")
dev.off()

#Pas 4


View(tema)
par(mfrow=c(2,1))
plot(tema$r_netflix,col="coral1", main="Graficul rentabilitatii Netflix", type="l")
plot(tema$r_nasdaq,col="firebrick", main="Graficul rentabilitatii Nasdaq", type="l")


plot(tema$Pret_act_NFLX,col="coral2", main="Graficul pretului Netflix", type="l")
plot(tema$Pret_indice.Nasdaq,col="chocolate4", main="Graficul pretului Nasdaq", type="l")


plot(tema$Volum_act_NFLX,col="coral2", main="Graficul volumului Netflix", type="l")
plot(tema$Volum_indice.Nasdaq,col="green3", main="Graficul volumului Nasdaq", type="l")

#identificare outlieri
tema[which.max(tema$Pret_act_NFLX),]
tema[which.max(tema$Pret_indice.Nasdaq),]
tema[which.max(tema$Volum_act_NFLX),]
tema[which.max(tema$Volum_indice.Nasdaq),]
tema[which.max(tema$r_netflix),]
tema[which.max(tema$r_nasdaq),]
getwd()
