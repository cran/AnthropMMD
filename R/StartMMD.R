StartMMD <-
function(){

myenvg = new.env() # environnement privé au package ; contiendra les variables globales
GoOn <- 1 # variable globale indiquant si le programme doit continuer ou se terminer

## FENÊTRE DE CHOIX DU TYPE DES DONNÉES :
# Deux possibilités : l'utilisateur a-t-il un tableau de données binaires (brutes) ou un tableau d'effectifs et de proportions (données résumées) ?
type_data = Sel_type_data() # on récupère ici le choix de l'utilisateur

## FENÊTRE DE CHARGEMENT DES DONNÉES :
# Faire charger les données
fileName <- tclvalue(tkgetOpenFile(filetypes="{{CSV Files} {.csv}}"))
if (!nchar(fileName)) {
 tkmessageBox(message = "No file was selected!")
 GoOn <- 0  
}

## FENÊTRE DE MISE EN FORME DES DONNÉES :
# L'utilisateur donne des détails sur la mise en forme de ses données
if ((GoOn == 1) & (type_data=="raw_data")) { # si le programme peut continuer normalement et que l'utilisateur a chargé des données brutes
 details_fichier=datafile_details() # on lui demande des détails sur les en-têtes de son jeu de données
 bdd = read.csv(file=fileName, header=details_fichier[1], sep=";") # dans un premier temps on ne met pas row.names=1, pour vérifier que les noms   d'individus ne comportent pas de doublons
 if (anyDuplicated(bdd[,1])>0 & (details_fichier[2]==TRUE)) {
  tkmessageBox(title="Duplicated names", message = "Warning : there are duplicates in the names of your individuals. The program will continue normally anyway, but please check your data.", icon = "warning", type = "ok") # on prévient l'utilisateur si son fichier a des doublons
 }
 if (details_fichier[2]==TRUE) {bdd = bdd[,-1]} # on retire la première colonne (i.e. les noms d'individus) le cas echeant

} else if ((GoOn == 1) & (type_data=="summarized_data")) { # si le programme peut continuer normalement et que l'utilisateur a chargé un résumé
 bdd = read.csv(file=fileName, header=TRUE, row.names=1, dec=",", sep=";")
}

## FENÊTRE DE CHOIX DES GROUPES :
# L'utilisateur choisit les groupes à conserver
if ((GoOn == 1) & (type_data=="raw_data")) { # si le programme peut continuer normalement et que l'utilisateur a chargé des données brutes
 groups_to_keep = Group_selection(bdd)

 if (length(groups_to_keep)<2) { # si l'utilisateur a retenu moins de 2 groupes, le programme s'arrête
  tkmessageBox(title="Error", message = "You must select at least two groups.", icon = "error", type = "ok")
  GoOn <- 0 
 } else {
  groups_to_keep = levels(factor(bdd[,1]))[as.logical(as.numeric(groups_to_keep))]
  bdd = bdd[bdd[,1] %in% groups_to_keep, ] # la bdd est restreinte aux groupes sélectionnés par l'utilisateur
 }
} # si l'utilisateur a chargé des données résumées, tous les groupes sont retenus


## FENÊTRE DE CHOIX DES CARACTÈRES À RETENIR :
# L'utilisateur choisit de supprimer (ou pas) certaines variables, et définit à partir de quel nombre d'individus par groupe un caractère est étudiable.
if (GoOn == 1) {
 userchoice = Protocole_MMD() # on récupère ici le choix de l'utilisateur
} 

## ON LANCE DONC DÉSORMAIS LE CALCUL DU MMD :
if (GoOn == 1) {
 prep = Prepa_MMD(bdd, type=type_data, k=as.numeric(userchoice[3]), all_vars=as.logical(as.numeric(userchoice[1])), idiosync=as.logical(as.numeric(userchoice[1])))
}

if (!is.vector(prep)) {
 print(prep)
 resul = Mat_MMD(prep[1:(nrow(prep)/2), ], prep[(nrow(prep)/2 + 1):nrow(prep), ], as.logical(as.numeric(userchoice[4])))
} else {
 GoOn <- 0
 tkmessageBox(title="Invalid selection", message = "Error : no variable meets your selection criteria in the dataset. The program will stop : please try again with more flexible criteria.", icon = "error", type = "ok") # on prévient l'utilisateur si ses criteres de selection sont trop severes
}

if (GoOn == 1) {
 tkmessageBox(title = "End", message = "The MMD matrix has been successfully calculated. It will be saved in a CSV file.", icon = "info", type = "ok")
}


# A CE STADE, LA MATRICE RESUL CONTIENT DANS LA PARTIE TRIANGULAIRE SUPERIEURE LES VALEURS DE MMD,
# ET DANS LA PARTIE TRIANGULAIRE INFERIEURE LES SD DES MMD. ON CREE PLUSIEURS FICHIERS DE RESULTATS.
if (GoOn == 1) {
mmdvalues = resul # matrice qui contiendra les valeurs de MMD (symetrique et a diagonale nulle)
for (i in 1:nrow(resul)) {
 for (j in 1:ncol(resul)) {
  if (i > j) {mmdvalues[i,j] = resul[j, i]}
 }
}

signif = resul # matrice qui contiendra les valeurs de MMD (symetrique et a diagonale nulle)
for (i in 1:nrow(resul)) {
 for (j in 1:ncol(resul)) {
  if (i > j) {signif[i,j] = NA}
  else if(i < j) {signif[i,j] = ifelse(resul[i,j]>2*resul[j,i], "*", NA)}
  else {signif[i,j] = NA} 
 }
}

# ET ON AFFICHE LE MDS :
mds = cmdscale(mmdvalues)
plot(x=mds[,1], y=mds[,2], xlab="MDS_Axis_1", ylab="MDS_Axis_2", asp=1, axes=FALSE, pch=16, main="MDS performed on MMDs")
text(x=mds[,1], y=mds[,2], rownames(mds), pos=2)
}

# ET ON PROPOSE A L'UTILISATEUR DE SAUVEGARDER SA MATRICE DE MMD :
if (GoOn == 1) {
 print("Significant MMD are indicated by stars :")
 print(signif)
 write.csv2(resul, "Results_AnthropMMD_MmdValuesUp_SdDown.csv") # matrice MMD en haut et var en bas
 write.csv2(mmdvalues, "Results_AnthropMMD_MmdValuesSym.csv") # matrice MMD symetrique a diagonale nulle
 write.csv2(signif, "Results_AnthropMMD_SignifMmd.csv", na="") # matrice indiquant si les MMD sont significatives ou non
}

}
