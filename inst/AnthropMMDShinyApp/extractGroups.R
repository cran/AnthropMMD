extractGroups <- function(tab, type) {
 # il s'agit d'une fonction permettant d'extraire les groupes presents dans le tableau fourni par l'utilisateur
 # type : sont-ce des donnees brutes (raw) ou resumees (table) ?
 
	if (type == "raw") { ## POUR UN TABLEAU DE DONNEES BRUTES :
		return(levels(tab[,1]))
	} else if (type == "table") { # POUR UN TABLEAU DE DONNEES RESUMEES : 
		nb_grps <- nrow(tab)/2 # le nombre de groupes
		noms <- rownames(tab)[1:nb_grps] # de la forme N_Group1, N_Group2, ...
		return(substr(noms,3,nchar(noms)))
	}
}
