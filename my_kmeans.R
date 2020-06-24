my_kmeans <- function(X,k){
  
  #Fonction de calcul des distances euclidienne
  my_dist <- function(x,y){
    return(sqrt(sum((x-y)^2)))
  }
  
  #Fonction de clustering en fonction de la distance minimale au barycentre
  my_clusters <- function(B,data=X){ #Input: dataframe de barycentres, datasets
    distance <- c()
    #Pour chaque barycentre, calcul la distance à tous les points
    for (i in 1:nrow(B)) {
      distance <- cbind(distance,apply(X,1,function(x){my_dist(x,B[i,])}))
    }
    #Output: vecteur des clusters
    cluster_id <- (apply(distance,1, function(x)which.min(x)))
    return(as.factor(cluster_id))
  } 
  
  #Fonction de calcul de barycentre à partir du vecteur des clusters.
  my_centroids <- function(C, data=X){ #Input: vecteur des clusters, datasets
    k = length(levels(C))
    centroid <- c()
    for (i in 1:k){
      clusters <- data[C==i,]
      centroid <- rbind(centroid,apply(clusters,2,mean))
    }
    return(centroid)
  }
  
  #1ere iteration avec k points aleatoires pris comme barycentres
  clusters_start <- (my_clusters(X[1:k,]))
  #Calculer les nouveaux barycentres
  centroids_end <- my_centroids(clusters_start)
  
  #Jusqu'à quand les barycentres ne bougent plus, on repete l'operation
  #Initialiser un vecteur pour mesurer le déplacement des nouveaux barycentres
  centroids_moves <- rep(1,k)
  while (max(centroids_moves)>0.001) {
    centroids_start <- centroids_end
    #Nouveaux clusters
    clusters_end <- my_clusters(centroids_start)
    #Nouveaux barycentres
    centroids_end <- my_centroids(clusters_end)
    #Distance entre les nouveaux barycentres et les precedents
    for (i in 1:k){
      centroids_moves[i] <- my_dist(centroids_start[i,],centroids_end[i,])
    }
  }
  #Output: vecteur donnant pour chaque individu l'id numerique de son cluster.
  return(clusters_end)
}
