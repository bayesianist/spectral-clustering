source('~/R/Projet Apprentissage automatique avancee/my_kmeans.R')
source('~/R/Projet Apprentissage automatique avancee/my_nngraph.R')

my_spclust <- function(X, similarity = c("linear","gaussian","polynomial"),
                       neighbor = c("seuil","knn","connexe"),
                       k=4, normalized = FALSE,
                       sigma=1.73,deg=2,theta=0.35,k_selection=60){
  #Matrice d'adjacence
  adjacency <- my_nngraph(X, similarity, neighbor,sigma,deg,theta,k_selection)
  
  #Matrice des degres
  summing_row <- apply(adjacency,1,sum)
  degree <- diag(summing_row,nrow(adjacency),ncol(adjacency))
  
  #Matrice laplacienne
  L <- (degree - adjacency)
  if (normalized == FALSE){
    laplacian <- L
  } else {
    #Matrice laplacienne normalisee
    #Fonction puissance d'une matrice
    puissance_matrice <- function(M, puissance)
      with(eigen(M), vectors %*% (values^puissance * solve(vectors)))
    laplacian <- puissance_matrice(degree,-1/2)  %*% L %*% puissance_matrice(degree,-1/2)
    #Si normalized = TRUE, il faut normer les vecteurs lignes de F
    laplacian <- apply(laplacian,1,function(x){x/sqrt(x%*%x)})
  }
  
  #La matrice des k derniers vecteurs propres de laplacian mis en colonne
  evL <- eigen(laplacian, symmetric=TRUE)
  F   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]
  
  
  #Envoyer dans le K-Means
  clusters <- my_kmeans(F,k)
  #clusters <- kmeans(F, centers=k, nstart=10)
  
  #Output
  return(clusters)
}



