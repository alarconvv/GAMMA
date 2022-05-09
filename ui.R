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



# Define UI for application that draws a histogram
shinyUI(
  navbarPage("GAMMA",theme = bs_theme(version = 4, bootswatch = "minty"),
             
             tabPanel("Home",
                      tags$style(HTML("
              .tabbable > .nav > li > a {margin-top:0px;}")),
                      
                      includeHTML("www/home.html")
                 
                      ),
             
             navbarMenu("Methods",
                        tabPanel("Independent Contrats"),
                        tabPanel("Ancestral States Reconstruction",
                                 fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Data",
                                                                fluidRow(
                                                                  column(3,wellPanel(selectInput("tree", "Load tree",c("Select" = "select","Example" = "examp","Import tree" = "treeFile")),
                                                                                     conditionalPanel(condition = "input.tree=='treeFile'",
                                                                                                      selectInput("format", "Tree format",c("Select" = "select","Nexus" = "Nexus","Newick" = "Newick")),
                                                                                                      fileInput("fileTree", "Load tree file")),
                                                                                     actionButton("importTree", "Import tree",style='padding:4px; font-size:80%'), hr(),
                                                                                     selectInput("csvData", "Load csv",c("Select" = "select","Example" = "exampCSV","Import csv" = "DataFile")),
                                                                                     conditionalPanel(condition = "input.csvData == 'DataFile'",
                                                                                                      fileInput("fileCSV", "Load data file")),
                                                                                     actionButton("importCSV", "Import csv",style='padding:4px; font-size:80%'), hr(),
                                                                                     selectInput('dataVar','Select character',choices='Select', selected=NULL),
                                                                                     selectInput('typeChar','Confirm character type',choices=c('Select','Discrete','Continuous'), selected='Select')
                                                                                     )
                                                                         ),
                                                                  column(9,fluidRow(column(9,checkboxInput("plottree", "Plot tree"),plotOutput(outputId = 'PhyloPlot')),
                                                                                    column(3,wellPanel(checkboxInput("tipLabels", "Show tip labels"),
                                                                                                       conditionalPanel(condition = "input.tipLabels==1",
                                                                                                                        sliderInput("tipSize", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.5)),
                                                                                                       checkboxInput("branchLength", "Use edge length"),checkboxInput("checknames", "Check tree and csv names")
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
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'PhyloPlot2'))),
                                                                                                                                        fluidRow(column(12, plotOutput(outputId = 'PhyloPlot3')))),
                                                                                                                                 column(6,
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'histo1'))),
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'QQ1'))))
                                                                                                                                 ),
                                                                                                                      wellPanel(fluidRow(column(6,selectInput('mapModelMl','Plot model',choices=NULL, selected=NULL)),
                                                                                                                                         column(6,actionButton('PlotEditorML', 'Plot Editor'))
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
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'PhyloPlot4'))),
                                                                                                                                                                  fluidRow(column(12, plotOutput(outputId = 'PhyloPlot5')))),
                                                                                                                                                           column(6,
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'trace1'))),
                                                                                                                                                                  fluidRow(column(12,plotOutput(outputId = 'desnsity1'))))
                                                                                                                                                           ),
                                                                                                                                                wellPanel(fluidRow(column(4,selectInput('plotNodesBI','Plot posterior prob. by node',choices=NULL, selected=NULL,multiple = TRUE)),
                                                                                                                                                                   column(4,checkboxInput('phenogramBI', 'Plot phenogram')),
                                                                                                                                                                   column(4,actionButton('PlotEditorBI', 'Plot Editor'))
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
                                                                                                                                   uiOutput("addModel3States"),br(),
                                                                                                                                   conditionalPanel("input.AddModelDisML > 0",
                                                                                                                                                    rHandsontableOutput("w1"),br(),
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
                                                                                                      fluidRow(column(3,wellPanel(selectInput("ModelsDisBI", "Set model",choices= NULL,multiple = TRUE),
                                                                                                                                  uiOutput("addModel3StatesBI"),br(),
                                                                                                                                  conditionalPanel("input.AddModelDisBI > 0",
                                                                                                                                                   rHandsontableOutput("modelBI"),br(),
                                                                                                                                                   actionButton('SubmAddModelBI','Submit')),br(),

                                                                                                                                  selectInput("QmatrixBI", "Q matri: rates",choices= c('Select'='select', 'empirical' = 'empirical', 'mcmc' = 'mcmc','costumize'='costumQmDiscBI')),
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'costumQmDiscBI'",
                                                                                                                                                   rHandsontableOutput("QmatBI"),br(),
                                                                                                                                                   actionButton('SubmQmatBI','Submit')),br(),
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'mcmc'",
                                                                                                                                                   selectInput("mcmcRunDisBI", "Set mcmc parameter", choices = c('by default' = 'bydefaultmcmcDisBI','costumize'='costummcmcDisBI')),
                                                                                                                                                   conditionalPanel("input.mcmcRunDisBI == 'costummcmcDisBI'",
                                                                                                                                                                    selectInput("priorDisBI", "Prior: gamma parameters", choices = c('Use empirical' = 'useEmpirical','No use empirical'='noUseEmpirical')),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'useEmpirical'",
                                                                                                                                                                                     radioButtons("betavaluesDisBI", "Define Beta values",
                                                                                                                                                                                                  c("Fix one value per rate" = "oneBeta","Assign a value per rate " = "betaPerRate"),selected = "oneBeta"),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'oneBeta'",
                                                                                                                                                                                                      textInput('betaValueDisBI','Beta',value = 0.1)),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'betaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matBetaValuePerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmbetaValuePerRateDisBI','Submit'))
                                                                                                                                                                                     ),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'noUseEmpirical'",
                                                                                                                                                                                     radioButtons("alphaBetavaluesDisBI", "Define Alpha and Beta values",
                                                                                                                                                                                                  c("Fix equal values per rate" = "oneAlphaBeta","Assign a alpha and beta value per rate " = "alphaBetaPerRate")),
                                                                                                                                                                                     conditionalPanel("input.alphaBetavaluesDisBI == 'oneAlphaBeta'",
                                                                                                                                                                                                      rHandsontableOutput("matOneAlphabetaValuePerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmOneAlphaBetaValuePerRateDisBI','Submit')),
                                                                                                                                                                                     conditionalPanel("input.alphaBetavaluesDisBI == 'alphaBetaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matAlphaBetaPerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmAlphaBetaPerRateDisBI','Submit'))
                                                                                                                                                                                     ),
                                                                                                                                                                    selectInput("vQDisBI", "vQ: variance per rate", choices = c('equal' = 'equalvQDisBI','Assign variances'='vQDisBI')),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'equalvQDisBI'",
                                                                                                                                                                                     textInput('equalvQValuesDisBI','Beta',value = 0.1)),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'vQDisBI'",
                                                                                                                                                                                     rHandsontableOutput("vQValuesPerRatesDisBI"),br(),
                                                                                                                                                                                     actionButton('SubmbvQValuesPerRatesDisBI','Submit'))
                                                                                                                                                                    ),
                                                                                                                                                   textInput('samfreqDisBI','Sample frequencies',value = 10)
                                                                                                                                                   ),
                                                                                                                                   selectInput("piBI", "Pi: prior prob. in root",choices = c('Select' = 'select', 'fitzJohn' = 'fitzJohn', 'equal' = 'equal','Costumize' = 'costumPiDiscBI')),
                                                                                                                                  conditionalPanel("input.piBI == 'costumPiDiscBI'",
                                                                                                                                                   rHandsontableOutput("PiBI"),br(),
                                                                                                                                                   actionButton('SubmPiBI','Submit')),br(),
                                                                                                                                   textInput('nsimDisBI','nsim',value = 10000)
                                                                                                                                  )),
                                                                                                               column(9,fluidRow(column(6,fluidRow(plotOutput(outputId = 'PhyloPlot10'))),
                                                                                                                                 column(6,fluidRow(plotOutput(outputId = 'PhyloPlot11')))
                                                                                                               ),
                                                                                                               wellPanel(fluidRow(column(4,selectInput('plotModelDisBI','Plot model',choices = NULL)),
                                                                                                                                  column(4,checkboxInput('bestStateBI', 'Plot the most likely state')),
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
