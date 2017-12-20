max3 <- function(dat) {
 # dat : tableau de type 'table'
 # la fonction retourne la valeur maximale admissible pour le slider bar de l'interface graphique (definissant le nb d'individus par groupe)

 mins <- apply(dat[1:(nrow(dat)/2),], MARGIN=2, FUN=min) # les valeurs minimales (en effectifs) associees a chaque variable
 
 return(as.numeric(sort(mins, decreasing=TRUE)[2]))
}
