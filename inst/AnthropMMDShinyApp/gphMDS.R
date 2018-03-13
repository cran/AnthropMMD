gphMDS <- function(mmdval, methodMDS, displayAxes=FALSE, displayStress=FALSE) {
# mmdval : matrice (symétrique et à diagonale nulle) de valeurs de MMD
# methodMDS : "MMDS" or "NMDS", dépend du choix de l'utilisateur dans l'interface graphique
# displayAxes : booléen récupéré depuis l'interface graphique
	mmdtoy <- mmdval; diag(mmdtoy) <- rep(1, nrow(mmdtoy))
	
	if (methodMDS=="MMDS") { # classical metric MDS
		coor <- cmdscale(mmdval, k=2) # coordonnées de la configuration de points
		if (ncol(coor)==2 & any(mmdval>0)) { # tout va bien, on affiche le graphique
			plot(coor[,1], coor[,2], pch=16, xlab="", ylab="", axes=displayAxes, main="Classical multidimensional scaling of MMD values", ylim=c(min(coor[,2]), 1.15*max(coor[,2])), asp=1)
			text(coor[,1], coor[,2], pos=3, labels=rownames(coor))
		} else if (ncol(coor)==2 & all(mmdval==0)) { # si la matrice d'entrée ne contient que des 0 -> message d'erreur
			plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
			text(x=0, y=0.5, labels="The MMD matrix contains only zeros.", col="black")
			text(x=0, y=-0.5, labels="Impossible to get a MDS plot.", col="black")
		} else { # ncol(coor)<2, le MDS n'a pas pu être calculé
			plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
			text(x=0, y=0, labels="The representation could not be computed in two dimensions.", col="black")
		}
	} else if (methodMDS!="MMDS" & all(mmdtoy>0)) { # nonmetric MDS possible, toutes les valeurs étant positives
			resNMDS <- smacofSym(as.dist(mmdval), type=methodMDS)
			plot(resNMDS, xlab="", ylab="", axes=displayAxes, main=paste("Multidimensional scaling of MMD values (", methodMDS, " type)", sep=""), ylim=c(min(resNMDS$conf[,2]), 1.15*max(resNMDS$conf[,2])), asp=1, cex=1, label.conf=list(cex=1))
			if (displayStress==TRUE) {
				legend("topleft", legend=paste("Stress=", round(resNMDS$stress,3), sep=""))
			}
	} else if (methodMDS!="MMDS" & any(mmdtoy<=0)) { # nonmetric MDS impossible, il y a des valeurs négatives ou nulles
			plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
			text(x=0, y=-0.5, labels="Impossible to get a non-metric MDS plot with a MMD matrix containing zero or negative dissimilarities.", col="black")
	}
}
