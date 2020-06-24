my_ari <- function(P0,P1){
  
  #Fonction pour calculer (a b)
  a_2 <- function(a){
    return(a*(a-1)/2)
  }
  
  #Tableau de contingence
  contingence <- table(P0,P1)
  #Calculer differents elements de la formule ARI
  nij <- sum(a_2(contingence))
  ni <- sum(a_2(apply(contingence,1,sum)))
  nj <- sum(a_2(apply(contingence,2,sum)))
  #Calculer ART
  ari <- (nij-(ni*nj)/a_2(length(D$classes)))/(0.5*(ni+nj)-(ni*nj)/a_2(length(D$classes)))
  #Output: valeur reel =<1 donnant la mesure de l'indice de Rand corrige.
  return(ari)
}

#------Brouillon
clusters <- my_spclust(D[["x"]],k=4,similarity = 'gaussian',neighbor = 'seuil',normalized = FALSE,sigma=1,theta=0.35)
plot(D[["x"]],col=clusters)

clusters_baseline <- my_kmeans(D$x,k=4)
plot(D[["x"]],col=clusters_baseline)

clusters_baseline <- my_kmeans(iris[,-5],k=3)
plot(iris[,-5],col=clusters_baseline)

#Tester avec la fonction existente
model <- kmeans(D$x,centers=4,nstart=30)
plot(D$x,col=model$cluster)

sc <- specc(D$x, centers=4,kernel='rbfdot',sigma=1/8)
plot(D$x, col=sc)

#La verite
plot(D$x, col=D$classes)

#ARI
my_ari(D$classes,clusters_baseline)
my_ari(D$classes,clusters)
my_ari(D$classes,sc)

#Tester avec un library
library(mclust)
adjustedRandIndex(D$classes,clusters_baseline)
