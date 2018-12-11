
#source("http://www.info.univ-angers.fr/~gh/statgh.r")
#her <- lit.dar("http://www.info.univ-angers.fr/~gh/Datasets/her.dar")
#write.csv(her,"TP_projet.csv")

setwd("C:/Users/USER/Desktop/acp/projet")
her1=read.csv("TP_projet1.csv",header=T,sep=";")
str(her1)
her<-her1[,c(-1,-2)]
her<-her[,-1]
summary(her)
summary(her1)
str(her)
getwd()

library(FactoMineR)
library(factoextra)
View(her1)
acp<-PCA(her,scale.unit = TRUE,quali.sup =13,quanti.sup = 7,ncp = 3,graph = FALSE)
fviz_screeplot(acp,choice="eigenvalue",addlabels=TRUE)
#cercle de corrélation
fviz_pca_var(acp, col.var="cos2")+
  scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.6) + theme_minimal()

fviz_pca_var(acp, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.6) + theme_minimal()
#nuage de pint
fviz_pca_ind(acp, col.ind="contrib")+
  scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.4) + theme_minimal()
plot(acp,habillage =13, choix = "ind")

fviz_pca(acp,habillage = 13,label = TRUE,addEllipses = TRUE  )
#matrice de corrélation
y=her1[,4:15]
y=data.frame(y)
g=colMeans(y)
g
y=sweep(y,2,g,"-")
y
Sig=sqrt(colMeans(y^2))

y=sweep(y,2,Sig,"/")
n=dim(y)[1]
y<- data.matrix(y)
c=cor(y)
c
A<-acp$eig # les valeurs propres > 1 on les prend comme composante principale.cette methode remplace screeplot
A
plot(acp)
