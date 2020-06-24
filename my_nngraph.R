my_nngraph <- function(X, similarity = c("linear","gaussian","polynomial"),
                       neighbor = c("seuil","knn","connexe"),
                       sigma,deg,theta,k_selection){
  
  neighbor <- match.arg(neighbor)
  
  #Etape 1: Fonction mesure la similarite avec un noyau
  f_similarity <- function(X,similarity,sigma,deg){
    
    #Noyau gaussien
    noyau_gaussian <- function(x1,x2){
      return(exp(-(norm(as.matrix(x1-x2),type="F"))/2*sigma^2))
    }
    
    #Noyau linear
    noyau_linear <- function(x1,x2){
      return(x1%*%x2)
    }
    
    #Noyau polynomial
    noyau_polynomial <- function(x1,x2,deg=2){
      puissance_matrice <- function(M, puissance)
        with(eigen(M), vectors %*% (values^puissance * solve(vectors)))
      return(puissance_matrice(x1%*%x2 + 1,deg))
    }
    
    #Initialiser une matrice carree de taille nxn
    n <- nrow(X)
    S <- matrix(rep(NA,n*n), ncol=n)
    #Matrice de similiarite selon 3 choix
    for(i in 1:n) {
      for(j in 1:n) {
        if (similarity == 'linear'){
          S[i,j] <- noyau_linear(X[i,], X[j,])
        } else if (similarity == 'gaussian') {
          S[i,j] <- noyau_gaussian(X[i,], X[j,])
        } else {
          S[i,j] <- noyau_polynomial(X[i,], X[j,])
        }
      }
    }
    #Output: matrice de mesure de similiarite pour chaque paire d'observations
    return(S)
  }
  
  #Etape 2: Matrice d'adjacence selon knn
  adjacency_knn <- function(S,k=k_selection){
    n <- nrow(S)
    #Initialiser une matrice carree de taille nxn
    W <- matrix(rep(0,n*n), ncol=n)
    #Pour chaque ligne Xi, chercher l'ensemble des observations
    #ayant les plus forts valeurs de similiarite avec Xi
    for (i in 1:n){
      NN_X <- sort(S[i,],decreasing = TRUE)[1:k]
      for (s in NN_X) {
        #Filtrer des observations ayant les plus forts valeurs de similiarite avec Xi dans la matrice S
        j <- which(S[i,]==s)
        W[i,j] <- S[i,j]
        #Puisqu'on construit un graphe non-oriente, la matrice d'adjacence est symetrique
        W[j,i] <- S[i,j]
      }
    }
    return(W)
  }
  
  #Etape 3: Les resultats finaux
  #La matrice matrice de mesure de similiarite pour chaque paire d'observations
  S <- f_similarity(X,similarity,sigma,deg)
  #La matrice d'adjacence en fonction du choix du type de selection voisinage
  if (neighbor=='seuil'){
    W <- ifelse(S>=theta,S,0)
  } else if (neighbor=='connexe') {
    W <- ifelse(S>=0,S,0)
  } else {
    W <- adjacency_knn(S)
  }
  
  #Output: matrice carree donnant pour chaque paire d'objets la mesure de similarite de ses plus proches voisins
  return(W)
}


