shinyUI(fluidPage(theme="kappa.css",
	
	titlePanel("AnthropMMD — A GUI for Smith's Mean Measure of Divergence"),
	#tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0098B6}")), # pour régler la couleur de la barre du slider
	tags$style(type = "text/css", "
		.irs-bar {width: 100%; background: #0098B6;}
		.irs-bar-edge {background: #0098B6;}
		.irs-single {color:white; background:#0098B6; font-weight: bold;}
 	"),
	sidebarLayout(
		#############################################################
		# I] Le menu de gauche, presentant les options de l'analyse :
		sidebarPanel(
			# 1.a) Choix du type de donnees :
			fileInput("file", label=h3("1. Data import"), accept=c(".csv", ".txt")),
			radioButtons("typeData", label=strong("Type of dataset"), choices=list("Raw binary dataset"="raw", "Table of n's and absolute frequencies for each group"="table")),
			# 1.b) Specification des info utiles pour l'import des donnees :
			conditionalPanel(condition="input.typeData == 'raw'", # panneau qui ne s'affiche que pour un "raw dataset"
				fluidRow( # on découpe en lignes et colonnes
					column(6,
						checkboxInput("colNamesRaw", label="Variable names in first line", value=TRUE),
						selectInput("fieldSepRaw", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" "))
					),
					column(6,
						checkboxInput("rowNames", label="Row names in first column", value=TRUE),
						textInput("charNA", label="Indicator for missing values", value="")
					)
				)
				#helpText("The first variable should be a group identifier. Only .csv and .txt files are accepted.")
			),
			conditionalPanel(condition="input.typeData == 'table'", # panneau qui ne s'affiche que pour une "table of n's and frequencies"
				fluidRow(
					column(6,
						selectInput("fieldSepTable", label="Field separator", choices=list("Semicolon (;)"=";", "Comma (,)"=",", "Tabulation"="\t", "Space"=" "))
					),
					column(6,
						checkboxInput("colNamesTable", label="Variable names in first line", value=TRUE)
					)
				),
				helpText("No missing values allowed here. Row names are mandatory.")
			),
			actionButton("loadData", "Load dataset"), # bouton de chargement de donnees (server.R attend un clic pour demarrer)
			br(),
			br(),
			
			# 2. Parametres divers de l'analyse :
			h3("2. Analysis settings"),
			selectizeInput("selectGroups", label=h4("Selection of active groups"), choices=NULL, multiple=TRUE),
			
			radioButtons("formuleMMD", label=h4("Formula"), choices=list("Use Anscombe formula"="Anscombe", "Use Freeman and Tukey formula"="Freeman"), inline=TRUE, selected="Anscombe"),
			
			h4("Trait selection"),
			fluidRow(
				column(6,
					radioButtons("exclusionStrategy", label="Exclusion strategy", choices=list("None"="none", "Exclude nonpolymorphic traits"="excludeNPT", "Exclude quasi-nonpolymorphic traits"="excludeQNPT", "Use Fisher's exact test (may be slow)"="keepFisher", "Exclude traits with overall MD lower than..."="excludeNOMD")),
					conditionalPanel(condition="input.exclusionStrategy == 'excludeNOMD'", # panneau qui ne s'affiche que pour le critère overall MD
						numericInput("OMDvalue", label=NULL, value=0, step=0.05, min=0)
					)
				),
				column(6,
					uiOutput("regletteNbMinInd")
				)
			)
		),

		############################################################
		# II] Le panneau principal, pour l'affichage des resultats :
		mainPanel(
			tabsetPanel(
				# 1. L'onglet d'affichage des donnees filtrees :
    				tabPanel("Summary",
    					br(),
    					strong(textOutput("text_title_summary")), # cet element est calcul\'e dans server.R seulement apr\`es l'importation du fichier : il ne s'affiche donc qu'a ce moment
    					br(),
    					div(style="overflow:auto; width:100%;", tableOutput("tableResume")), # le "div" sert à mettre la table dans une frame avec scrollbar, si elle est large
    					uiOutput("button_download_summary"), # le bouton de telechargement des resultats n'est calcul\'e / affich\'e qu'au bout du processus (cf. server.R)
						br(),
						strong(textOutput("text_title_pvalFisher")),
						br(),
						br(),
						div(style="overflow:auto; width:100%;", tableOutput("tablePval")),
						uiOutput("button_download_tablePval")
    				), 
    				# 2. L'onglet d'affichage du resultat des MMD :
				tabPanel("MMD Statistics",
					fluidRow( # grille 2x2 pourl'affichage des resultats
						column(6,
							br(),
    							strong(textOutput("text_table_MMDSym")), # cet element est calcul\'e dans server.R seulemt apr\`es l'importation du fichier
    							br(),
    							div(style="overflow:auto; width:100%;", tableOutput("tableMMDSym")),
    							uiOutput("button_download_tableMMDSym"),
    							br(),
    							br(),
    							strong(textOutput("text_table_IMD")), # cet element est calcul\'e dans server.R seulemt apr\`es l'importation du fichier
    							br(),
    							div(style="overflow:auto; height:210px; width:50%;", tableOutput("tableIMD")),
    							uiOutput("button_download_tableIMD")
    						),
    						column(6,
							br(),
							strong(textOutput("text_table_MMD")), # cet element est calcul\'e dans server.R seulemt apr\`es l'importation du fichier
							br(),
							div(style="overflow:auto; width:100%;", tableOutput("tableMMD")),
							uiOutput("button_download_tableMMD"),
							br(),
							br(),
							strong(textOutput("text_table_MMDSignif")), # cet element est calcul\'e dans server.R seulemt apr\`es l'importation du fichier
							br(),
							div(style="overflow:auto; width:100%;", tableOutput("tableMMDSignif")),
							uiOutput("button_download_tableMMDSignif")
								
    						)
    					)
				), 
				# 3. L'onglet d'affichage de l'eventuel graphique MDS :
				tabPanel("MDS plot", 
					helpText("A multidimensional scaling plot (MDS) is displayed below if and only if there are at least three active groups."),
					br(),
					selectInput("methodMDS", label="MDS method", choices=list("Classical metric MDS"="MMDS", "Kruskal's non-metric MDS"="NMDS"), selected="MMDS", multiple=FALSE),
					br(),
					plotOutput("plotMDS", width="80%"),
					br(),
					uiOutput("button_download_plotMDS") # le bouton de telechargement des resultats n'est calcul\'e / affich\'e qu'au bout du processus (cf. server.R)
				),
				# 4. L'onglet d'affichage de l'eventuel graphique CAH :
				tabPanel("Hierarchical clustering", 
					helpText("A hierarchical clustering using Ward's method is displayed below if and only if there are at least three active groups."),
					br(),
					selectInput("methodCAH", label="Agglomeration method", choices=list("Ward's method (squared distances)"="ward.D2", "Single linkage"="single", "Complete linkage"="complete", "Average linkage (UPGMA)"="average"), selected="ward.D2", multiple=FALSE),
					br(),
					plotOutput("plotCAH", width="80%"),
					br(),
					uiOutput("button_download_plotCAH") # le bouton de telechargement des resultats n'est calcul\'e / affich\'e qu'au bout du processus (cf. server.R)
				)
			)
		)
	)

))
