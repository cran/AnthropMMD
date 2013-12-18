Sel_type_data <-
function(){
tt <- tktoplevel() # on cr\'ee une fenetre
tkwm.geometry(tt, "367x370")
tkwm.minsize(tt, 367, 370)
tktitle(tt) <- "About your dataset..." # Le titre qui s'affichera dans la barre de fenetre
fontHeading <- tkfont.create(family="arial",size=12,weight="bold")

##################################
# Declarations et configurations :
frameTypeData <- tkframe(tt, relief="groove", borderwidth=3) # declaration d'une frame

rb1 <- tkradiobutton(frameTypeData) # declaration d'un premier bouton radio
rb2 <- tkradiobutton(frameTypeData) # declaration d'un second bouton radio
rbValue <- tclVar("raw_data") # cette variable (initialis\'ee à Raw) contiendra au final le choix retenu par l'utilisateur via son bouton
tkconfigure(rb1,variable=rbValue,value="raw_data",text="Raw (binary) dataset...") # affectation d'une variable tcltk a chaque bouton radio
tkconfigure(rb2,variable=rbValue,value="summarized_data",text="Summarized data...")

cb1 <- tkcheckbutton(frameTypeData)
val_header <- tclVar("1")
tkconfigure(cb1,variable=val_header)
cb2 <- tkcheckbutton(frameTypeData)
val_rownames <- tclVar("1")
tkconfigure(cb2,variable=val_rownames)

#################
# Mise en forme :
tkgrid(frameTypeData)
tkgrid(tklabel(frameTypeData,text="Type of your dataset", font=fontHeading))
tkgrid(tklabel(frameTypeData,text="    ")) # ligne vide pour a\'erer
tkgrid(rb1, sticky="w")
tkgrid(tklabel(frameTypeData,text="... with column names"),cb1,sticky="e")
tkgrid(tklabel(frameTypeData,text="... with row names"),cb2,sticky="e")
tkgrid(tklabel(frameTypeData,text="    ")) # ligne vide pour a\'erer
tkgrid(rb2, sticky="w")
tkgrid(tklabel(frameTypeData,text="      ... row and column names are mandatory here."),sticky="e")

tkgrid(tklabel(tt,text="    ")) # ligne vide pour a\'erer
tkgrid(tklabel(tt,text="Information", font=fontHeading))
tkgrid(tklabel(tt,text="Please make sure that your data files are \n formated in .csv with a semicolon separator (;).\n The first variable must be a group indicator."))

tkgrid(tklabel(tt,text="    ")) # ligne vide pour a\'erer
OK.but <- tkbutton(tt,text="OK",command=function() tkdestroy(tt), relief="groove",borderwidth=3,width=7,bg="green4",fg="white")
tkgrid(OK.but)
tkfocus(tt)
tkwait.window(OK.but) # TRÈS IMPORTANT : l'ex\'ecution s'arrête ici tant que l'utilisateur n'a pas cliqu\'e sur OK 

user_choice = list(as.character(tclvalue(rbValue)), tclvalue(val_header), tclvalue(val_rownames))
names(user_choice) = c("Type_data", "Colnames", "Rownames")
return(user_choice)
}


StartMMD <-
function(){

myenvg = new.env() # environnement priv\'e au package ; contiendra les variables globales
GoOn <- 1 # variable globale indiquant si le programme doit continuer ou se terminer

## FENETRE DE CHOIX DU TYPE DES DONNEES :
# Deux possibilit\'es : l'utilisateur a-t-il un tableau de donn\'ees binaires (brutes) ou un tableau d'effectifs et de proportions (donn\'ees r\'esum\'ees) ?
type_data = Sel_type_data() # on r\'ecup\`ere ici le choix de l'utilisateur


## FENÊTRE DE CHARGEMENT DES DONNÉES :
# Faire charger les donn\'ees
fileName <- tclvalue(tkgetOpenFile(filetypes="{{CSV Files} {.csv}}"))
if (!nchar(fileName)) {
 tkmessageBox(message = "No file was selected!")
 GoOn <- 0  
}


## CHARGEMENT EFFECTIF DES DONNEES :
if ((GoOn == 1) & (type_data$Type_data=="raw_data")) { # si le programme peut continuer normalement et que l'utilisateur a charg\'e des donn\'ees brutes
 bdd = read.csv(file=fileName, header=(type_data$Colnames=="1"), sep=";") # dans un premier temps on ne met pas row.names=1, pour v\'erifier que les noms   d'individus ne comportent pas de doublons
 if (anyDuplicated(bdd[,1])>0 & (type_data$Rownames=="1")) {
  tkmessageBox(title="Duplicated names", message = "Warning : there are duplicates in the names of your individuals. The program will continue normally anyway, but please check your data.", icon = "warning", type = "ok") # on pr\'evient l'utilisateur si son fichier a des doublons
 }
 if (type_data$Colnames=="1") {bdd = bdd[,-1]} # on retire la premi\`ere colonne (i.e. les noms d'individus) le cas echeant

} else if ((GoOn == 1) & (type_data$Type_data=="summarized_data")) { # si le programme peut continuer normalement et que l'utilisateur a charg\'e un r\'esum\'e
 bdd = read.csv(file=fileName, header=TRUE, row.names=1, dec=",", sep=";")
}


## FENETRE DE PARAMETRAGE DE L'ANALYSE :
if (GoOn == 1) {
 UserSettings <- AnaSettings(bdd, type_data$Type_data)
}

if (length(UserSettings$Groupes_retenus)<2) { # si l'utilisateur a retenu moins de 2 groupes, le programme s'arrete
 tkmessageBox(title="Error", message = "You must select at least two groups.", icon = "error", type = "ok")
 GoOn <- 0 
} else if (type_data$Type_data=="raw_data") { # cas d'une base brute 0/1
 groups_to_keep = levels(factor(bdd[,1]))[UserSettings$Groupes_retenus]
 bdd = bdd[bdd[,1] %in% groups_to_keep, ] # la bdd est restreinte aux groupes s\'electionn\'es par l'utilisateur
 bdd[,1] = factor(bdd[,1])
} else if (type_data$Type_data=="summarized_data") { # cas d'une base r\'esum\'ee
 bdd = bdd[c(UserSettings$Groupes_retenus, UserSettings$Groupes_retenus+nrow(bdd)/2) ,]
}


## ON LANCE DONC DESORMAIS LE CALCUL DU MMD :
if (GoOn == 1) {
 prep = Prepa_MMD(bdd, type=type_data$Type_data, k=UserSettings$Nb_min_indiv, all_vars=UserSettings$All_vars, idiosync=UserSettings$All_BO_vars)
}

if (!is.vector(prep)) {
 print(prep)
 resul = Mat_MMD(Mat_eff=prep[1:(nrow(prep)/2), ], Mat_prop=prep[(nrow(prep)/2 + 1):nrow(prep), ], formule=UserSettings$Formula, corrFT=UserSettings$FTcor)
} else {
 GoOn <- 0
 tkmessageBox(title="Invalid selection", message = "Error : no variable meets your selection criteria in the dataset. The program will stop : please try again with more flexible criteria.", icon = "error", type = "ok") # on pr\'evient l'utilisateur si ses criteres de selection sont trop severes
}

# if (GoOn == 1) {
#  tkmessageBox(title = "End", message = "The MMD matrix has been successfully calculated. It will be saved in a CSV file.", icon = "info", type = "ok")
# }


# A CE STADE, LA MATRICE RESUL CONTIENT DANS LA PARTIE TRIANGULAIRE SUPERIEURE LES VALEURS DE MMD,
# ET DANS LA PARTIE TRIANGULAIRE INFERIEURE LES SD DES MMD. ON CREE PLUSIEURS FICHIERS DE RESULTATS.
if (GoOn == 1) {
mmdvalues = resul$MMDMatrix # matrice qui contiendra les valeurs de MMD (symetrique et a diagonale nulle)
for (i in 1:nrow(mmdvalues)) {
 for (j in 1:ncol(mmdvalues)) {
  if (i > j) {mmdvalues[i,j] = resul$MMDMatrix[j, i]}
 }
}

signif = resul$MMDMatrix # matrice qui contiendra les valeurs de MMD (symetrique et a diagonale nulle)
for (i in 1:nrow(signif)) {
 for (j in 1:ncol(signif)) {
  if (i > j) {signif[i,j] = NA}
  else if(i < j) {signif[i,j] = ifelse(resul$MMDMatrix[i,j]>2*resul$MMDMatrix[j,i], "*", NA)}
  else {signif[i,j] = NA} 
 }
}

# ET ON AFFICHE LE MDS :
if (ncol(resul$MMDMatrix)>2) {
 mds = cmdscale(mmdvalues, k=2)
 plot(x=mds[,1], y=mds[,2], xlab="MDS_Axis_1", ylab="MDS_Axis_2", asp=1, axes=FALSE, pch=16, main="MDS performed on MMDs")
 text(x=mds[,1], y=mds[,2], rownames(mds), pos=2)
} else {
 tkmessageBox(title="MDS", message = "Warning : MDS plot will not be displayed (more than two groups are necessary)", icon = "error", type = "ok") # on pr\'evient l'utilisateur si ses criteres de selection sont trop severes
}
}

# ET ON PROPOSE A L'UTILISATEUR DE SAUVEGARDER SA MATRICE DE MMD :
if (GoOn == 1) {
 cat("\n", "Significant MMD are indicated by stars :", signif)
 if(UserSettings$IDP) {
  cat("\n", "Individual Measures of Divergence for each variable :", resul$IndividualMD)
  write.csv2(resul$IndividualMD, "Results_AnthropMMD_IndividualMD.csv")
 }
 write.csv2(resul$MMDMatrix, "Results_AnthropMMD_MmdValuesUp_SdDown.csv") # matrice MMD en haut et var en bas
 write.csv2(mmdvalues, "Results_AnthropMMD_MmdValuesSym.csv") # matrice MMD symetrique a diagonale nulle
 write.csv2(signif, "Results_AnthropMMD_SignifMmd.csv", na="") # matrice indiquant si les MMD sont significatives ou non
}

}
