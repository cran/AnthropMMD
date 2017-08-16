fisherTestTab <- function(tab) {
# tab : table de format 'table'
# cette fonction teste si chaque trait presente une difference significative pour au moins deux des groupes etudies (Harris & Sjovold 2004).
# retourne le tableau reduit aux seules variables presentant au moins une difference significative

	nbGroupes <- nrow(tab)/2 # le nombre de groupes
	nbVars <- ncol(tab) # le nombre de variables
	MatRes <- matrix(NA, ncol=nbVars, nrow=nbGroupes*(nbGroupes-1)/2) # matrice de p-valeurs qui dira si chaque trait est informatif ou non pour chaque paire de groupes
	
	for (j in 1:nbVars) { # pour chaque trait...
		compteurLignes <- 1
		for (k in 1:(nbGroupes-1)) { # ... et chaque paire...
			for (l in (k+1):nbGroupes) { # ... de groupes...
				presGroupeA <- round(tab[k,j] * tab[(k+nbGroupes),j])
				presGroupeB <- round(tab[l,j] * tab[(l+nbGroupes),j])
				absGroupeA <- round(tab[k,j] * (1-tab[(k+nbGroupes),j]))
				absGroupeB <- round(tab[l,j] * (1-tab[(l+nbGroupes),j]))
				mat <- matrix(c(presGroupeA, presGroupeB, absGroupeA, absGroupeB), ncol=2)# ... on construit la matrice a tester...
				MatRes[compteurLignes , j] <- fisher.test(mat)$p.value # ... et on calcule sa p-valeur.
				compteurLignes <- compteurLignes + 1
			}		
		}
	}
	isInformative <- apply(MatRes, MARGIN=2, FUN= function(x) if (any(x<=0.05)) return(TRUE) else return(FALSE))
	return(tab[ , isInformative])
}
