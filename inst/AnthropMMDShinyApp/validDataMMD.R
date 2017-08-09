validDataMMD <- function(tab, type) {
 # verifie si le dataframe "tab", du type "type" (raw ou table) est bien valide pour AnthropMMD
 
 if (ncol(tab) <= 1) { # quel que soit son type, le fichier est invalide s'il n'a qu'une seule colonne (l'utilisateur s'est sans doute tromp\'e de separateur de champ) 
 	return(FALSE)
 } else {
 
	if (type=="raw") {
		tab[,1] <- NULL # on ne considere pas la colonne d'identifiant de groupe ici
		for (j in 1:ncol(tab)) { tab[,j] <- factor(tab[,j]) } # on convertit tout en facteurs 0/1
		niveaux <- apply(tab, MARGIN=2, FUN=function(x) return(nlevels(factor(x))))
		if (max(niveaux)>2) { return(FALSE) } # si un facteur quelconque a plus de deux niveaux (donc autre chose que 0/1, les donnees sont invalides)
		# on va aussi regarder s'il n'y a bien que des 0 et des 1 (pas des 3 et des 4 par exemple) :
		ok01 <- rep(NA, ncol(tab))
		for (j in 1:ncol(tab)) { ok01[j] <- all(levels(tab[,j]) %in% c("0","1")) }
		if (all(ok01)) { # si tous les facteurs ne prennent bien que les valeurs 0 et 1 :
			return(TRUE)		
		} else {
			return(FALSE)
		}
		
	} else if (type=="table") {
		nb_grps <- nrow(tab)/2 # le nombre de groupes
		noms <- rownames(tab)[1:nb_grps] # theoriquement de la forme N_Group1, N_Group2, ...
		if (all(substr(noms,1,2) == "N_") & all(apply(tab, MARGIN=2, FUN=mode)=="numeric")) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
 }
}
	
