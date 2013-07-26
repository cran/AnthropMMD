Sel_type_data <-
function(){
tt <- tktoplevel() # on crée une fenêtre
tktitle(tt) <- "About your data file(s)..."
rb1 <- tkradiobutton(tt) # premier bouton radio
rb2 <- tkradiobutton(tt) # second bouton radio
rbValue <- tclVar("raw_data") # cette variable (initialisée à une valeur idiote) contiendra au final le choix retenu par l'utilisateur via son bouton
tkconfigure(rb1,variable=rbValue,value="raw_data")
tkconfigure(rb2,variable=rbValue,value="summarized_data")
tkgrid(tklabel(tt,text="What is the type of your dataset? \n", font="bold"))
tkgrid(tklabel(tt,text="Raw (binary) dataset"),rb1)
tkgrid(tklabel(tt,text="Summarized data"),rb2)
tkgrid(tklabel(tt,text="    ")) # ligne vide pour aérer
tkgrid(tklabel(tt,text="[Info] Please make sure that your data files are formated in .csv with a semicolon separator (;).\n The first variable must be a group indicator."))
tkgrid(tklabel(tt,text="    ")) # ligne vide pour aérer
OK.but <- tkbutton(tt,text="OK",command=function() tkdestroy(tt), relief="groove",borderwidth=3,width=7,bg="green4",fg="white")
tkgrid(OK.but)
tkfocus(tt)
tkwait.window(OK.but) # TRÈS IMPORTANT : l'exécution s'arrête ici tant que l'utilisateur n'a pas cliqué sur OK 
return(as.character(tclvalue(rbValue)))
}
