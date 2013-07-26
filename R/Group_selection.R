Group_selection <-
function(tab){
# tab : le tableau de donnees
# Cette fonction affiche un resume des effectifs par groupe et propose de ne retenir que certains d'entre eux

tt <- tktoplevel() # on crée une fenêtre
tktitle(tt) <- "Group selection" # titre de la fenêtre
tkgrid(tklabel(tt,text="Which groups do you want to retain? \n", font="bold")) # phrase d'intro de la fenêtre

nbgroups = nlevels(factor(tab[,1])) # Le nombre de groupes
list_buttons = as.list(1:nbgroups)
list_varstcl = as.list(1:nbgroups)

for (i in 1:nbgroups) {
 list_buttons[[i]] <- tkcheckbutton(tt) 
 list_varstcl[[i]] <- tclVar("1")
 tkconfigure(list_buttons[[i]], variable=list_varstcl[[i]])
}

for (i in 1:nbgroups) {
 tkgrid(tklabel(tt,text=paste("Keep the group", levels(factor(tab[,1]))[i])), list_buttons[[i]])
}
tkgrid(tklabel(tt,text="    ")) # ligne vide pour aérer

OK.but <- tkbutton(tt,text="OK",command=function() tkdestroy(tt), relief="groove",borderwidth=3,width=7,bg="green4",fg="white")
tkgrid(OK.but)
tkfocus(tt)
tkwait.window(OK.but) # TRÈS IMPORTANT : l'exécution s'arrête ici tant que l'utilisateur n'a pas cliqué sur OK 

res = lapply(list_varstcl, tclvalue)
names(res) = levels(factor(tab[,1]))
return(res)
}
