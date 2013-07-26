Mat_MMD <- function(Mat_eff, Mat_prop, corrFT) {
# Mat_eff : la matrice des effectifs
# Mat_prop : la matrice des proportions de présence
# corrFT : booléen indiquant s'il faut appliquer la correction de Freeman-Tukey pour les petits effectifs

## FORMULE DE LA TRANSFORMATION ANGULAIRE (Anscombe) :
theta <- function(n,p) { asin((n/(n+3/4))*(1-2*p)) }

## FORMULE SERVANT POUR LE CALCUL DE L'ECART-TYPE DES MMD :
sdform <- function(nA,nB) { (1/nA + 1/nB)^2 }

## FAUT-IL APPLIQUER LA CORRECTION DE FT ?
if (corrFT == TRUE) {
# Formule des MMD *avec* correction de Freeman-Tukey pour les petits effectifs
thetadiff <- function(nA,pA,nB,pB) { (theta(nA,pA) - theta(nB,pB))^2 - (1/(nA+0.5) + 1/(nB+0.5)) }
} else {
# Formule des MMD *sans* correction de Freeman-Tukey pour les petits effectifs
thetadiff <- function(nA,pA,nB,pB) { (theta(nA,pA) - theta(nB,pB))^2 }
}

## CONSTRUCTION DE LA MATRICE DE MMD :
MMDMatrix = matrix(0, nrow=nrow(Mat_eff), ncol=nrow(Mat_eff)) # MMDMatrix a autant de lignes et de colonnes qu'on a de groupes dans les données
dimnames(MMDMatrix) = list(rownames(Mat_eff), rownames(Mat_eff)) # On nomme les lignes et colonnes selon les noms de groupes

for (i in 1:nrow(MMDMatrix)) {
 for (j in 1:ncol(MMDMatrix)) { # on parcourt les cases (i,j) de MMDMatrix pour la remplir
 
  MMDVect = vector("numeric", length(Mat_eff[1,])) 
  if (j > i) { # on est au-dessus de la diagonale du tableau : on remplit avec les valeurs MMD
   for (k in 1:length(MMDVect)) { 
    MMDVect[k] = thetadiff(Mat_eff[i,k], Mat_prop[i,k], Mat_eff[j,k], Mat_prop[j,k]) 
   }
   MMDMatrix[i, j] = sum(MMDVect) / length(MMDVect) 
  } else if (i ==j) { # on est sur la diagonale du tableau
   MMDMatrix[i, j] = 0 # on affecte donc une valeur nulle
  } else { # donc i > j, on est sous la diagonale et on affecte l'écart-type du MMD
   for (k in 1:length(MMDVect)) { 
    MMDVect[k] = sdform(Mat_eff[i,k], Mat_eff[j,k]) 
   }
   MMDMatrix[i, j] = sqrt(sum(MMDVect)*2 / length(MMDVect)^2)
  }
  
 } 
}

return(round(MMDMatrix,3))
}
