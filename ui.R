#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
#library(shiny.i18n)
library(ape)
library(ggthemes)
library(paletteer)
library(coda)
library(shinyMatrix)
library(corHMM)
library(shiny)
library(phytools)
library(reactable)
library(shinyjs)
library(rhandsontable)
library(shinyWidgets)


# Define UI for application that draws a histogram

theme <-bs_theme(version = 4, bootswatch = "minty"  )
#theme <-'bootstrap.min.css'

shinyUI(

  
  
  navbarPage(title = "GAMMA", theme=theme,
             tabPanel("Home",
                      includeHTML("www/home.html")
                      ),
             
             navbarMenu("Methods",
                        tabPanel("Independent Contrats"),
                        tabPanel("Ancestral States Reconstruction",
                                 fluidPage(theme=theme,
                                   tabsetPanel(type = "tabs",
                                                       tabPanel("Data",
                                                                fluidRow(
                                                                  column(3,wellPanel(selectInput("tree", "Load tree",c("Select" = "select","Example" = "examp","Import tree" = "treeFile")),
                                                                                     conditionalPanel(condition = "input.tree=='treeFile'",
                                                                                                      selectInput("format", "Tree format",c("Select" = "select","Nexus" = "Nexus","Newick" = "Newick")),
                                                                                                      fileInput("fileTree", "Load tree file")),
                                                                                     actionButton("importTree", "Import tree"), hr(),
                                                                                     selectInput("csvData", "Load csv",c("Select" = "select","Example" = "exampCSV","Import csv" = "DataFile")),
                                                                                     conditionalPanel(condition = "input.csvData == 'DataFile'",
                                                                                                      fileInput("fileCSV", "Load data file")),
                                                                                     actionButton("importCSV", "Import csv"), hr(),
                                                                                     checkboxInput("checknames", "Check tree and csv names"),
                                                                                     selectInput('dataVar','Select character',choices='Select', selected=NULL),
                                                                                     selectInput('typeChar','Confirm character type',choices=c('Select','Discrete','Continuous'), selected='Select'),hr(),
                                                                                     numericInput(inputId = "seed", label = "Set seed",value =  999, min = 1, max = 1000000)
                                                                                     )
                                                                         ),
                                                                  column(9,fluidRow(column(9,plotOutput(outputId = 'PhyloPlot', inline = T)),
                                                                                    column(3,wellPanel(checkboxInput("tipLabels", "Show tip labels"),
                                                                                                       conditionalPanel(condition = "input.tipLabels==1",
                                                                                                                        sliderInput("tipSize", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.5)),
                                                                                                       checkboxInput("branchLength", "Use edge length"),
                                                                                                       numericInput(inputId = 'PlotHeightDt',label = 'Plot height (px)',value =800,min = 20,max = 1500),
                                                                                                       numericInput(inputId = 'PlotWidthDt',label = 'Plot width (px)',value =400,min = 20,max = 1500),
                                                                                                       selectInput("plotType", "Plot type",
                                                                                                                   c("phylogram" = "phylogram","cladogram" = "cladogram","fan" = "fan", "unrooted" = "unrooted","radial" = "radial", "tidy" = "tidy" ),selected = "phylogram")
                                                                                                       )
                                                                                           )
                                                                                    ),hr(),
                                                                         fluidRow(column(12,verbatimTextOutput("objects")))
                                                                         )
                                                                  )
                                                                ),
                                                       tabPanel("Analysis",
                                                                conditionalPanel(condition = "input.typeChar == 'Continuous'",
                                                                                 tabsetPanel(id = "ContinuousCharacter",
                                                                                             tabPanel("Maxima Likelihood",
                                                                                                      fluidRow(column(3,wellPanel(radioButtons("transform1", "Transformation type",
                                                                                                                                               c("No transform" = "NoTrans","ln" = "ln","log10" = "log10",
                                                                                                                                                 "Square Root" = "squareRoot","Cube Root"="cubeRoot",
                                                                                                                                                 "Reciprocal"="reciprocal","Exponential"="exp1"),selected = "NoTrans"),
                                                                                                                                  checkboxInput("AncPIC", "Compute Phylogenetic Independent Contrast"),hr(),
                                                                                                                                  selectInput("ModelContinuous", "Fit model",c("BM" = "BM","OU" = "OU","EB" = "EB"),multiple = TRUE,selected = NULL),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('BM') > -1",
                                                                                                                                                   textInput('maxitBM', 'Max iter BM', value = '2000')),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('EB') > -1",
                                                                                                                                                  textInput('maxitEB', 'Max iter EB', value = '200')),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('OU') > -1",
                                                                                                                                                    textInput('maxitOU', 'Max iter OU', value = '20')),
                                                                                                                                  actionButton("runAncML", "run"),hr(),
                                                                                                                                  checkboxInput("MLModAIC", "Model selection AIC"),hr(),
                                                                                                                                  radioButtons("exportMLanc", "Export output",c("R object (RDS)" = "MLancRDS","TXT" = "MLancTXT")),
                                                                                                                                  downloadButton("downloadAnc", "Download")
                                                                                                                                  )
                                                                                                                      ),
                                                                                                               column(9,fluidRow(column(6,
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'PhyloPlot2', inline = T)))),
                                                                                                                                 column(6,
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'histo1'))),
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'QQ1'))))
                                                                                                                                 ),
                                                                                                                      wellPanel(fluidRow(column(4,selectInput('mapModelMl','Plot model',choices=NULL, selected=NULL),
                                                                                                                                                sliderInput("tipSizeContMl", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7)),
                                                                                                                                         column(4, numericInput(inputId = 'PlotHeightContMl',label = 'Plot height (px)',value =800,min = 20,max = 1500),
                                                                                                                                                numericInput(inputId = 'PlotWidthContMl',label = 'Plot width (px)',value =400,min = 20,max = 1500)
                                                                                                                                                ),
                                                                                                                                         column(4,checkboxInput('phenogramML', 'Plot phenogram'),
                                                                                                                                                actionButton('PlotEditorML', 'Plot Editor'))
                                                                                                                                         )
                                                                                                                                ),hr(),
                                                                                                                      fluidRow(column(12,verbatimTextOutput("infoPanelContinuousML"))))
                                                                                                               )
                                                                                                      ),
                                                                                             tabPanel("Stocastic State Mapping",fluidRow(column(3,wellPanel(selectInput("parametersBI", "Set parameters",
                                                                                                                                                                        c("Select" = "select","by default" = "defaultBI","Costumize" = "costumizeBI")),
                                                                                                                                                            conditionalPanel(condition = "input.parametersBI == 'costumizeBI'",
                                                                                                                                                                             textInput('sig2BI','Sig2: BM rate'),
                                                                                                                                                                             textInput('aBI','a: State at the root node'),
                                                                                                                                                                             textInput('sampleBI','Sample')
                                                                                                                                                                             ),
                                                                                                                                                            textInput('ngenBI','ngen',200000),
                                                                                                                                                            textInput('BurninBI','burnin',0.2),
                                                                                                                                                            actionButton("runAncBI", "Run"),hr(),
                                                                                                                                                            radioButtons("exportAncBI", "Export output",
                                                                                                                                                                         c("R object (RDS)" = "BIancRDS","TXT" = "BIancTXT")),
                                                                                                                                                            downloadButton("downloadAncBI", "Download")
                                                                                                                                                            )
                                                                                                                                                ),
                                                                                                                                         column(9,fluidRow(column(6,
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'PhyloPlot4',inline = T,click = "plot_click")))
                                                                                                                                                                  ),
                                                                                                                                                           column(6,
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'trace1'))),
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'desnsity1'))))
                                                                                                                                                           ),
                                                                                                                                                fluidRow(column(12, plotOutput(outputId = 'PhyloPlot5'))),
                                                                                                                                                wellPanel(fluidRow(column(4,selectInput('plotNodesBI','Plot posterior prob. by node',choices=NULL, selected=NULL,multiple = TRUE),
                                                                                                                                                                          sliderInput("tipSizeContBI", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7)),
                                                                                                                                                                   column(4, numericInput(inputId = 'PlotHeightContBI',label = 'Plot height (px)',value =800,min = 20,max = 1500),
                                                                                                                                                                          numericInput(inputId = 'PlotWidthContBI',label = 'Plot width (px)',value =400,min = 20,max = 1500)),
                                                                                                                                                                   column(4,checkboxInput('phenogramBI', 'Plot phenogram'),
                                                                                                                                                                          actionButton('PlotEditorBI', 'Plot Editor'))
                                                                                                                                                                   )
                                                                                                                                                          ),hr(),
                                                                                                                                                fluidRow(column(12,verbatimTextOutput("infoPanelContinuousBI")))
                                                                                                                                                )
                                                                                                                                         )
                                                                                                      )
                                                                                             )
                                                                                 ),
                                                                conditionalPanel(condition = "input.typeChar == 'Discrete'",
                                                                                 tabsetPanel(id= 'DiscreteCharacter',
                                                                                             tabPanel("Maxima Likelihood",
                                                                                                      fluidRow(column(3, wellPanel(selectInput("typeDisML", "Type of reconstruction",choices= c('joint'='joint', 'marginal'='marginal'),selected = 'marginal'),
                                                                                                                                   selectInput("ModelsDisML", "Set models",choices= NULL,multiple = TRUE),
                                                                                                                                   uiOutput("addModel3States"),
                                                                                                                                   conditionalPanel("input.AddModelDisML == 1",
                                                                                                                                                    br(),rHandsontableOutput("w1"),br(),
                                                                                                                                                    actionButton('SubmAddModel','Submit')),br(),
                                                                                                                                   actionButton('RunAnalyDisML','Run'), hr(),
                                                                                                                                   checkboxInput("DisMLModAIC", "Model selection AIC"),
                                                                                                                                   conditionalPanel("input.DisMLModAIC == 1",
                                                                                                                                                    checkboxInput("ModAverDisML", "Model averaging") ),
                                                                                                                                   conditionalPanel("input.ModAverDisML == 1",
                                                                                                                                                    selectInput("SetModAverDisML", "Set models",choices=NULL, selected=NULL,multiple = TRUE),
                                                                                                                                                    actionButton('RunModAverDisML','Run')),hr(),
                                                                                                                                   radioButtons("exportDisMLanc", "Export output",c("R object (RDS)" = "MLancRDS","TXT" = "MLancTXT")),
                                                                                                                                   selectInput("disCharOutputs", "Choose output",choices= c("Select"="Select","Fitted models"="FittedModels","AIC matrix"="AICmatrix", "Model averege"="ModelAverage")),
                                                                                                                                   downloadButton("downloadDisML", "Download")
                                                                                                                                   )
                                                                                                                      ),
                                                                                                               column(9, fluidRow(column(6,fluidRow(plotOutput(outputId = 'PhyloPlot8'))),
                                                                                                                                  column(6,fluidRow(plotOutput(outputId = 'PhyloPlot9')))
                                                                                                                                  ),
                                                                                                                      wellPanel(fluidRow(column(4,selectInput('plotModelDisML','Plot model',choices=NULL)),
                                                                                                                                         column(4,checkboxInput('bestState', 'Plot the most likely state')),
                                                                                                                                         column(4,actionButton('PlotEditorDisML', 'Plot Editor'))
                                                                                                                                         )
                                                                                                                                ),hr(),
                                                                                                                      fluidRow(column(12,verbatimTextOutput("infoPanelDiscreteML")
                                                                                                                                      )
                                                                                                                               )
                                                                                                                      )
                                                                                                               )
                                                                                                      ),
                                                                                             tabPanel("Stocastic State Mapping",
                                                                                                      fluidRow(column(3,wellPanel(selectInput("QmatrixBI", "Q matrix: rates",choices = c('Select' = 'select', 'empirical' = 'empirical', 'mcmc' = 'mcmc','costumize' = 'costumQmDiscBI')),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'costumQmDiscBI'",
                                                                                                                                                   br(),rHandsontableOutput(outputId = "putitamatrix"),br(),
                                                                                                                                                   actionButton('SubmQmatBI','Submit matrix')),br(),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'empirical' | input.QmatrixBI == 'mcmc'",
                                                                                                                                                   
                                                                                                                                                   selectInput("ModelsDisBI", "Set model",choices = NULL,multiple = F),
                                                                                                                                                   conditionalPanel("input.ModelsDisBI == 'Costumize'",
                                                                                                                                                                    br(), rHandsontableOutput("costuModelBI"),br(),
                                                                                                                                                                    actionButton('SubmAddModelBI','Submit model')),br()),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'mcmc'",
                                                                                                                                                   selectInput("mcmcParDisBI", "Set mcmc parameter", choices = c('by default' = 'bydefaultmcmcDisBI','costumize' = 'costummcmcDisBI')),
                                                                                                                                                   conditionalPanel("input.mcmcParDisBI == 'costummcmcDisBI'",
                                                                                                                                                                    selectInput("priorDisBI", "Prior: gamma parameters", choices = c('Select' = 'select','Use empirical' = 'useEmpirical','No use empirical'='noUseEmpirical')),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'useEmpirical'",
                                                                                                                                                                                     radioButtons("betavaluesDisBI", "Define Beta values",
                                                                                                                                                                                                  c("Fix one value per rate" = "oneBeta","Assign a value per rate " = "betaPerRate"),selected = character(0)),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'oneBeta'",
                                                                                                                                                                                                      rHandsontableOutput("matBetaValDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmBetaValDisBI','Submit beta')
                                                                                                                                                                                                      ),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'betaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matBetaValuePerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmBetaValuePerRateDisBI','Submit betas'))
                                                                                                                                                                                     ),br(),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'noUseEmpirical'",
                                                                                                                                                                                     radioButtons("alphaBetaValDisBI", "Define Alpha and Beta values",
                                                                                                                                                                                                  c("Fix equal values per rate" = "oneAlphaBeta","Assign a alpha and beta value per rate " = "alphaBetaPerRate"), character(0)),
                                                                                                                                                                                     conditionalPanel("input.alphaBetaValDisBI == 'oneAlphaBeta'",
                                                                                                                                                                                                      rHandsontableOutput("matOneAlphaBetaValDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmOneAlphaBetaValDisBI','Submit (a,b)')),
                                                                                                                                                                                     conditionalPanel("input.alphaBetaValDisBI == 'alphaBetaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matAlphaBetaPerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmAlphaBetaPerRateDisBI','Submit (a,b)*rate'))
                                                                                                                                                                                     ),br(),
                                                                                                                                                                    selectInput("vQDisBI", "vQ: variance per rate", choices = c('Select' = 'select','equal' = 'equalvQDisBI','Assign variances'='vQValPerRatesDisBI')),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'equalvQDisBI'",
                                                                                                                                                                                     rHandsontableOutput("matequalvQValDisBI"),br(),
                                                                                                                                                                                     actionButton('SubmequalvQValDisBI','Submit vQ')
                                                                                                                                                                                     ),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'vQValPerRatesDisBI'",
                                                                                                                                                                                     rHandsontableOutput("matvQValPerRatesDisBI"),br(),
                                                                                                                                                                                     actionButton('SubmbvQValPerRatesDisBI','Submit vQ*rates'))
                                                                                                                                                                    ),br(),
                                                                                                                                                   textInput('samfreqDisBI','Sample frequency',value = 10),
                                                                                                                                                   textInput('burninDisBI','Burn-in',value = 0.2)
                                                                                                                                                   ),
                                                                                                                                   selectInput("piBI", "Pi: prior prob. in root",choices = c('Select' = 'select', 'fitzjohn' = 'fitzjohn','stationary' = 'estimated', 'equal' = 'equal','Costumize' = 'costumPiDiscBI')),
                                                                                                                                  conditionalPanel("input.piBI == 'costumPiDiscBI'",
                                                                                                                                                   rHandsontableOutput("matPiBI"),br(),
                                                                                                                                                   actionButton('SubmPiBI','Submit')),br(),
                                                                                                                                   textInput('nsimDisBI','nsim',value = 100),
                                                                                                                                  actionButton('RunAnalyDisBI','Run'), hr()
                                                                                                                                  )),
                                                                                                               column(9,fluidRow(column(6,fluidRow(plotOutput(outputId = 'PhyloPlot10',height="800px"))),
                                                                                                                                 column(6,fluidRow(plotOutput(outputId = 'PhyloPlot11')),
                                                                                                                                        fluidRow(plotOutput(outputId = 'PhyloPlot12')))
                                                                                                               ),
                                                                                                               wellPanel(fluidRow(column(4,selectInput('plotModelDisBI','Plot outcome',choices = NULL)),
                                                                                                                                  column(4,selectInput('ploHPDDisBI','Plot transitions',choices = NULL)),
                                                                                                                                  column(4,actionButton('PlotEditorDisBI', 'Plot Editor'))
                                                                                                               )
                                                                                                               ),hr(),
                                                                                                               fluidRow(column(12,verbatimTextOutput("infoPanelDiscreteBI")
                                                                                                               )
                                                                                                               )))),
                                                                                             tabPanel("Parsimony")
                                                                                             )
                                                                                 )
                                                                )
                                                       )
                                           )
                                 ),
                        tabPanel("Basic Diversification Analysis"),
                        tabPanel("SSE Models")
                        ),
             tabPanel("Plot Edition"),
             tabPanel("R code", verbatimTextOutput("Rcode")
                      )
             )
  )
