Protocole_MMD <-
function(){
tt <- tktoplevel() # on crée une fenêtre
tktitle(tt) <- "Variable selection" # titre de la fenêtre
tkgrid(tklabel(tt,text="Which traits do you want to retain? \n", font="bold")) # phrase d'intro de la fenêtre

cb1 <- tkcheckbutton(tt) # première tickbox
var_all <- tclVar("0")
tkconfigure(cb1,variable=var_all)

cb2 <- tkcheckbutton(tt) # seconde tickbox
var_abo <- tclVar("0")
tkconfigure(cb2,variable=var_abo)

cb3 <- tkcheckbutton(tt) # troisième tickbox
var_css <- tclVar("1")
tkconfigure(cb3,variable=var_css)

tkgrid(tklabel(tt,text="Keep the traits showing the same value for all the indiviuals of your dataset"),cb1)
tkgrid(tklabel(tt,text="Keep the traits showing the same value for all but one of the indiviuals of your dataset"),cb2)
textEntryVarTcl <- tclVar("5")
textEntryWidget <- tkentry(tt, width = 2, textvariable = textEntryVarTcl)
tkgrid(tklabel(tt, text = "Keep only the traits with this minimal number of individuals per group"), textEntryWidget)
tkgrid(tklabel(tt,text="    ")) # ligne vide pour aérer
tkgrid(tklabel(tt,text="Apply Freeman-Tukey correction for small samples"),cb3)
tkgrid(tklabel(tt,text="    ")) # ligne vide pour aérer

OK.but <- tkbutton(tt,text="OK",command=function() tkdestroy(tt), relief="groove",borderwidth=3,width=7,bg="green4",fg="white")
tkgrid(OK.but)
tkfocus(tt)
tkwait.window(OK.but) # TRÈS IMPORTANT : l'exécution s'arrête ici tant que l'utilisateur n'a pas cliqué sur OK 

res = c(tclvalue(var_all), tclvalue(var_abo), tclvalue(textEntryVarTcl), tclvalue(var_css))
names(res) = c("Retain_all_vars", "Retain_idiosync", "Min_nb_indiv", "F-T_corr")
return(as.numeric(res))
}
