shinyServer(function(input, output, session) {

	source("calcIMD.R")	
	source("calcMMD.R")
	source("extractGroups.R")
	source("fisherTestTab.R")
	source("max3.R")
	source("rawToTable.R")
	source("selectVars.R")
	source("tableToFreq.R")
	source("validDataMMD.R")

	myenvg = new.env() # environnement priv\'e au package ; contiendra le jeu de donnees (vu comme une variable globale)

	#########################################################
	# 1. CHARGER LE JEU DE DONNÉES FOURNI PAR L'UTILISATEUR #
	#########################################################
	observeEvent(input$loadData, {
		if (! is.null(input$file$name)) { # on verifie que l'utilisateur a bien choisi un fichier !
			if (input$typeData=="raw") { # si c'est un jeu de donnees brutes, binaires
				dat <- read.table(input$file$datapath, header=input$colNamesRaw, sep=input$fieldSepRaw, na.strings=input$charNA)
				if (input$rowNames) { # s'il y a les noms d'individus...
					dat[,1] <- NULL # on les supprime (ils ne servent a rien ici)
				}		
			} else if (input$typeData=="table") { # si c'est une table d'effectifs et frequences
				dat <- read.table(input$file$datapath, header=input$colNamesTable, row.names=1, sep=input$fieldSepTable)
			}
			if (validDataMMD(dat, type=input$typeData)) { # on regarde si le fichier est bien valide
				groups <- extractGroups(dat, type=input$typeData) # on extrait les groups presents dans les donnees
				updateSelectizeInput(session, "selectGroups", choices=groups, selected=groups, server=TRUE) # et on met a jour la liste de selection des groupes.
				output$text_title_summary <- renderText("Number of individuals and relative frequencies for each active variable within each group")
				output$text_table_MMD <- renderText("MMD values (upper triangular part) and associated SD values (lower triangular part)") # on calcule aussi le titre des tables...
				output$text_table_IMD <- renderText("Overall measure of divergence for each variable, sorted in decreasing order of discriminatory power") 
				output$text_table_MMDSym <- renderText("Symmetrical matrix of MMD values") 
				output$text_table_MMDSignif <- renderText("MMD values (upper triangular part) and their significance (indicated by a * in the lower part; 'NS'='non significant')") # ... jusqu'ici.
				if (input$typeData=="raw") { # si c'est un fichier de donnees brutes...
					dat <- rawToTable(dat) # on le convertit en table d'effectifs et frequences
				}
				dat <- tableToFreq(dat)
				assign("dat", dat, envir=myenvg) # on place le jeu de donnees (qui est desormais forcement de type table) dans l'environnement global
				#updateSliderInput(session, "minNbInd", value=min(c(10, max3(dat))), min=1, max=max3(dat)) # on empeche par la suite la saisie de valeurs "nb min individus" trop elevees au vu des donnees
			} else {
				showModal(modalDialog(title = "Error", "Invalid file. Please read the help page for 'StartMMD'.", easyClose = TRUE))
			}
		} else { # l'utilisateur avait oubli\'e de choisir un fichier
			showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
		}
	})

	##################
	# 2. ANALYSE MMD #
	##################
	
	dat <- reactive({ # ici, on insere une expression qui retournera en temps reel le jeu de donnees correspondant aux choix de filtrage de l'utilisateur
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de donnees a bien ete fourni et qu'il est valide !
						selectVars(get("dat", envir=myenvg), k=as.numeric(input$minNbInd), excludeTraits=as.character(input$exclusionStrategy), groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD))
					} else { # sinon, s'il n'y a pas de donnees ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant (evite l'affichage d'erreurs en rouge ou de "resultats vides" en l'absence de fichier correct)
					}
			}) # ce jeu de donnees est calcul\'e une fois pour toutes, et sera reutilis\'e a plusieurs reprises ci-dessous (evite des recalculs multiples a differents endroits)
	
	resultatsMMD <- reactive({ # meme chose avec les resultats des MMD sur le jeu de donnes obtenu ci-dessus
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de donnees a bien ete fourni et qu'il est valide !
						if (ncol(dat()$TableCalcMMD) > 0) { # et s'il reste encore des variables avec les criteres de selection definis !
							calcMMD(dat()$TableCalcMMD, formule=input$formuleMMD)
						} else { return() }
					} else {
						return()
					}
				})
	
	temp <- reactive({ # sera comme dat(), sauf qu'on ne filtre pas en fonction du nb d'individus, on filtre juste en fonction des variables
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { # si un jeu de donnees a bien ete fourni et qu'il est valide !
						selectVars(get("dat", envir=myenvg), k=1, excludeTraits=as.character(input$exclusionStrategy), groups=as.character(input$selectGroups), formule=as.character(input$formuleMMD))
					} else { # sinon, s'il n'y a pas de donnees ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant (evite l'affichage d'erreurs en rouge ou de "resultats vides" en l'absence de fichier correct)
					}
			}) # ce jeu de donnees est calcul\'e une fois pour toutes, et sera reutilis\'e a plusieurs reprises ci-dessous (evite des recalculs multiples a differents endroits)
	
	calcNbMaxReglette <- reactive({ # sert a calculer dynamiquement la borne max de la reglette *en fonction des groupes en presence* ! (ce nb peut changer en fonction des groupes actifs)
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) {
						max3(temp()$TableCalcMMD)
					} else {
						return(100)
					}
			})	
	
	output$regletteNbMinInd <- renderUI({
		sliderInput("minNbInd", label="Only retain the traits with this minimal number of individuals per group", value=min(c(10,calcNbMaxReglette())), min=1, max=calcNbMaxReglette())
	}) # la reglette est par defaut de 1 a 100, puis ensuite elle s'adapte dynamiquement aux groupes actifs du jeu de donnees
	
	####################################################################
	# 2.1. Remplir l'onglet "Summary" avec la table des donnees filtrees
	output$tableResume <- renderTable(dat()$TableCalcMMD, rownames=TRUE, digits=3)
	# explication (pour rappel) : cette sortie est cach\'ee tant que le fichier n'est pas charg\'e (grace a l'expression reactive ci-dessus)

	output$button_download_summary <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_summary", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_summary <- downloadHandler(filename='summary_active_groups.csv', content=function(file) {
		write.csv(dat()$TableCalcMMD, file)
	}) # la fonction declenchee par le bouton de telechargement

	#######################################################################################
	# 2.2. Remplir l'onglet "MMD Statistics" avec la matrice de MMD et les resultats divers
	# 2.2-a) Matrice de MMD "classique", avec SD :
	output$tableMMD <- renderTable(resultatsMMD()$MMDMatrix, rownames=TRUE, digits=3)
	
	output$button_download_tableMMD <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMD", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMD <- downloadHandler(filename='MMD_SD_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDMatrix, file)
	}) # la fonction declenchee par le bouton de telechargement

	# 2.2-b) Matrice des mesures individuelles de divergence :
	output$tableIMD <- renderTable(dat()$TableDisplayIMD, rownames=TRUE, digits=3)
	
	output$button_download_tableIMD <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableIMD", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableIMD <- downloadHandler(filename='Overall_MD_matrix.csv', content=function(file) {
		write.csv(dat()$TableDisplayIMD, file)
	}) # la fonction declenchee par le bouton de telechargement

	# 2.2-c) Matrice symetrique de MMD :
	output$tableMMDSym <- renderTable(resultatsMMD()$MMDSym, rownames=TRUE, digits=3)

	output$button_download_tableMMDSym <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMDSym", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMDSym <- downloadHandler(filename='MMD_Sym_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDSym, file)
	}) # la fonction declenchee par le bouton de telechargement

	# 2.2-d) Matrice de signif. des MMD :
	output$tableMMDSignif <- renderTable(resultatsMMD()$MMDSignif, rownames=TRUE, digits=3)
	output$button_download_tableMMDSignif <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>1) { 
						downloadButton("download_tableMMDSignif", "Download this table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_tableMMDSignif <- downloadHandler(filename='MMD_Signif_matrix.csv', content=function(file) {
		write.csv(resultatsMMD()$MMDSignif, file)
	}) # la fonction declenchee par le bouton de telechargement

	#############################################################################################
	# 2.3. Remplir l'onglet "MDS plot" avec un MDS *seulement s'il y a au moins de trois groupes*
	output$plotMDS <- renderPlot({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2) { # si un jeu de donnees valide a bien ete fourni et qu'on a au moins 3 groupes !
						mmdval <- resultatsMMD()$MMDSym
						coor <- cmdscale(mmdval, k=2)
						if (ncol(coor)==2 & any(mmdval>0)) {
							plot(coor[,1], coor[,2], pch=16, xlab="", ylab="", axes=FALSE, main="Multidimensional scaling of MMD values",
						     ylim=c(min(coor[,2]), 1.1*max(coor[,2])), asp=1)
							text(coor[,1], coor[,2], pos=3, labels=rownames(coor))
						} else if (ncol(coor)==2 & all(mmdval==0)) { 
							plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
							text(x=0, y=0.5, labels="The MMD matrix contains only zeros.", col="black")
							text(x=0, y=-0.5, labels="Impossible to get a MDS plot.", col="black")
													
						} else {
							plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
							text(x=0, y=0, labels="The representation could not be computed in two dimensions.", col="black")
						}
					} else { # sinon, s'il n'y a pas de donnees ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant.
					}
	})

	output$button_download_plotMDS <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2 ) { 
						mmdval <- resultatsMMD()$MMDSym
						coor <- cmdscale(mmdval, k=2)
						if (ncol(coor)==2 & any(mmdval>0)) {
							downloadButton("download_plotMDS", "Download this plot [PNG file]")
						} else {
							return()
						}
					} else { 
						return() 
					}
	})

	output$download_plotMDS <- downloadHandler(filename='MDS_plot.png', content=function(file) { # la fonction declenchee par le bouton de telechargement
					mmdval <- resultatsMMD()$MMDSym
					coor <- cmdscale(mmdval, k=2)
					png(file, width=900, height=900)
						par(cex=1.15)
						plot(coor[,1], coor[,2], pch=16, xlab="", ylab="", axes=FALSE, main="Multidimensional scaling of MMD values",
						     ylim=c(min(coor[,2]), 1.1*max(coor[,2])), asp=1)
						text(coor[,1], coor[,2], pos=3, labels=rownames(coor))
					dev.off()
	})
	
	###########################################################################################
	# 2.4. Remplir l'onglet "CAH" avec un dendro *seulement s'il y a au moins de trois groupes*
	output$plotCAH <- renderPlot({
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2) { # si un jeu de donnees valide a bien ete fourni et qu'on a au moins 3 groupes !
						if (any(resultatsMMD()$MMDSym>0)) {
							distances <- as.dist(resultatsMMD()$MMDSym) 
							plot(hclust(distances, method="ward.D2"), main="Hierarchical clustering", xlab="")
						} else { 
							plot(x=0, y=0, xlab="", ylab="", axes=FALSE, xlim=c(-2,2), ylim=c(-2,2), pch="")
							text(x=0, y=0.5, labels="The MMD matrix contains only zeros.", col="black")					
						} 
					} else { # sinon, s'il n'y a pas de donnees ou qu'elles sont non-valides,
						return() # on n'affiche rien pour l'instant.
					}
	})

	output$button_download_plotCAH <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a upload\'e les donnees et lanc\'e le calcul 
					if (input$loadData>0 & exists("dat", envir=myenvg) & length(input$selectGroups)>2 ) { 
						if (any(resultatsMMD()$MMDSym>0)) {
							downloadButton("download_plotCAH", "Download this plot [PNG file]")
						} else {
							return()
						}
					} else { 
						return() 
					}
	})

	output$download_plotCAH <- downloadHandler(filename='Hierarchical_clustering_MMD.png', content=function(file) { # la fonction declenchee par le bouton de telechargement
					distances <- as.dist(resultatsMMD()$MMDSym) 
					png(file, width=900, height=900)
						par(cex=1.15)
						plot(hclust(distances, method="ward.D2"), main="Hierarchical clustering", xlab="")
					dev.off()
	})	
})
