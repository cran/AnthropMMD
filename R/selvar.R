selvar <-
function(T) {
# T est un tableau binaire.
# La premiere colonne de T est un facteur determinant des groupes.
# Renvoie le descriptif des effectifs, variable par variable, groupe par groupe.

nvar = ncol(T)-1 # le nombre de variables
groupes = T[,1] # la premiere colonne est l'indicateur de groupes
nbgroupes = nlevels(groupes)
nomgroupe = levels(groupes)

sel = matrix(nrow=2*nbgroupes, ncol=nvar)
colnames(sel) = colnames(T)[-1]

for (j in 1:nvar) {
 x = split(T[,j+1], groupes)
 for (i in 1:nbgroupes) {
 sel[i,j] = length(x[[i]][(is.na(x[[i]])==FALSE)])
 }
 for (i in 1:nbgroupes) {
 sel[i+nbgroupes,j] = length(x[[i]][(is.na(x[[i]])==FALSE) & (x[[i]]==1)]) / sel[i,j]
 }
}

noms = rep(NA, 2*nbgroupes)
for (i in 1:nbgroupes) {
 noms[i] = paste("Eff_", nomgroupe[i], sep="")
}
for (i in 1:nbgroupes) {
 noms[i+nbgroupes] = paste("Prop1_", nomgroupe[i], sep="")
}

rownames(sel) = noms
sel = round(sel,3)

write.csv2(sel[1:nbgroupes, ], "effectifs_MMD.csv")
write.csv2(sel[(nbgroupes+1):(2*nbgroupes), ], "prop_MMD.csv")
return(sel)
}
