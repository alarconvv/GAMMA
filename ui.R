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
library(ape)
library(paletteer)
library(coda)
library(corHMM)
#devtools::install_github(repo = 'https://github.com/liamrevell/phytools',force = TRUE)
library(phytools)
library(shinyjs)
library(rhandsontable)
library(shinyWidgets)



theme <- bs_theme(version = 5, bootswatch =  "minty", primary = "#0BB48C",secondary = "#C1D9DB",
  "font-size-base" = "0.9rem", "enable-rounded" = T
) %>%
   bs_add_rules(
     '.navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom) {
    margin-bottom: 0px;}
     .container, .container-fluid, .container-xxl, .container-xl, .container-lg, .container-md, .container-sm {
         padding-left: 0.1rem;
         padding-right: 0.1rem;
     }
     .navbar-brand { margin-left: 1rem;}
     
     .row>* {padding-right: calc(var(--bs-gutter-x) * 0.7);
    padding-left: calc(var(--bs-gutter-x) * 0.7);}
    
    .nav-tabs .nav-link, .nav-tabs>li>a, .nav-tabs .nav-pills>li>a, .nav-tabs ul.nav.navbar-nav>li>a{
    margin-left: 0.45rem;
    margin-top: 0.5rem;
    }
     
     .nav-link, .nav-tabs>li>a, .nav-pills>li>a, ul.nav.navbar-nav>li>a{
         color: #5a5a5a;
     }
     
     .nav-tabs .nav-link.active, .nav-tabs>li>a.active, .nav-tabs .nav-pills>li>a.active, .nav-tabs ul.nav.navbar-nav>li>a.active, .nav-tabs .nav-item.show .nav-link, .nav-tabs .nav-item.in .nav-link, .nav-tabs .nav-item.show .nav-tabs>li>a, .nav-tabs .nav-item.in .nav-tabs>li>a, .nav-tabs .nav-item.show .nav-pills>li>a, .nav-tabs .nav-item.in .nav-pills>li>a, .nav-tabs>li.show .nav-link, .nav-tabs>li.in .nav-link, .nav-tabs>li.show .nav-tabs>li>a, .nav-tabs>li.in .nav-tabs>li>a, .nav-tabs>li.show .nav-pills>li>a, .nav-tabs>li.in .nav-pills>li>a, .nav-tabs .nav-pills>li.show .nav-link, .nav-tabs .nav-pills>li.in .nav-link, .nav-tabs .nav-pills>li.show .nav-tabs>li>a, .nav-tabs .nav-pills>li.in .nav-tabs>li>a, .nav-tabs .nav-pills>li.show .nav-pills>li>a, .nav-tabs .nav-pills>li.in .nav-pills>li>a, .nav-tabs .nav-item.show ul.nav.navbar-nav>li>a, .nav-tabs .nav-item.in ul.nav.navbar-nav>li>a, .nav-tabs>li.show ul.nav.navbar-nav>li>a, .nav-tabs>li.in ul.nav.navbar-nav>li>a, .nav-tabs .nav-pills>li.show ul.nav.navbar-nav>li>a, .nav-tabs .nav-pills>li.in ul.nav.navbar-nav>li>a, .nav-tabs ul.nav.navbar-nav>li.show:not(.dropdown) .nav-link, .nav-tabs ul.nav.navbar-nav>li.in:not(.dropdown) .nav-link, .nav-tabs ul.nav.navbar-nav>li.show:not(.dropdown) .nav-tabs>li>a, .nav-tabs ul.nav.navbar-nav>li.in:not(.dropdown) .nav-tabs>li>a, .nav-tabs ul.nav.navbar-nav>li.show:not(.dropdown) .nav-pills>li>a, .nav-tabs ul.nav.navbar-nav>li.in:not(.dropdown) .nav-pills>li>a, .nav-tabs ul.nav.navbar-nav>li.show:not(.dropdown) ul.nav.navbar-nav>li>a, .nav-tabs ul.nav.navbar-nav>li.in:not(.dropdown) ul.nav.navbar-nav>li>a {
     color: #0BB48C;
     }
     
     .navbar-light .navbar-nav .show>.nav-link, .navbar-light .navbar-nav .in>.nav-link, .navbar-light .navbar-nav .nav-tabs>li.show>a, .navbar-light .navbar-nav .nav-tabs>li.in>a, .navbar-light .navbar-nav .nav-pills>li.show>a, .navbar-light .navbar-nav .nav-pills>li.in>a, .navbar.navbar-default .navbar-nav .show>.nav-link, .navbar.navbar-default .navbar-nav .in>.nav-link, .navbar.navbar-default .navbar-nav .nav-tabs>li.show>a, .navbar.navbar-default .navbar-nav .nav-tabs>li.in>a, .navbar.navbar-default .navbar-nav .nav-pills>li.show>a, .navbar.navbar-default .navbar-nav .nav-pills>li.in>a, .navbar-light ul.nav.navbar-nav>li.show>a, .navbar-light ul.nav.navbar-nav>li.in>a, .navbar.navbar-default ul.nav.navbar-nav>li.show>a, .navbar.navbar-default ul.nav.navbar-nav>li.in>a, .navbar-light .navbar-nav .nav-link.active, .navbar-light .navbar-nav .nav-tabs>li>a.active, .navbar-light .navbar-nav .nav-pills>li>a.active, .navbar.navbar-default .navbar-nav .nav-link.active, .navbar.navbar-default .navbar-nav .nav-tabs>li>a.active, .navbar.navbar-default .navbar-nav .nav-pills>li>a.active, .navbar-light ul.nav.navbar-nav>li>a.active, .navbar.navbar-default ul.nav.navbar-nav>li>a.active {
     color: #0BB48C;
     }
     
     .navbar-brand{
     font-size: 1.2rem;
     font-weight: 500;
     }
     
     .navbar-light .navbar-brand, .navbar.navbar-default .navbar-brand {
    color: #67847d;}
     '
   )

shinyUI(
  navbarPage(title = "Guane", theme = theme,
             tabPanel("Home",
                      includeHTML("www/home.html"),
                      ),
             navbarMenu("Methods",
                        tabPanel("Independent Contrats"),
                        tabPanel("Ancestral character estimation",
                                 fluidPage(theme = theme,
                                   tabsetPanel(type = "tabs",
                                                       tabPanel("Data",
                                                                fluidRow(
                                                                  column(3,wellPanel(selectInput("tree", "Load tree",c("Select" = "select","Example" = "examp","Import tree" = "treeFile")),
                                                                                     conditionalPanel(condition = "input.tree=='treeFile'",
                                                                                                     # selectInput("format", "Tree format",c("Select" = "select","Nexus" = "Nexus","Newick" = "Newick")),
                                                                                                      fileInput("fileTree", "Load file")),
                                                                                     actionButton("importTree", "Import tree"), hr(),
                                                                                     selectInput("csvData", "Load csv", c("Select" = "select", "Example" = "exampCSV", "Import data" = "DataFile")),
                                                                                     conditionalPanel(condition = "input.csvData == 'DataFile'",
                                                                                                      fileInput("fileCSV", "Load file")),
                                                                                     actionButton("importCSV", "Import csv"), hr(),
                                                                                     checkboxInput("checknames", "Check tree & data names"),
                                                                                     selectInput('dataVar','Select a character',choices = 'Select', selected = NULL),
                                                                                     selectInput('typeChar', 'Confirm character type', choices = c('Select','Discrete','Continuous'), selected = 'Select'), hr(),
                                                                                     numericInput(inputId = "seed", label = "Set seed",value =  999, min = 1, max = 1000000)
                                                                                     )
                                                                         ),
                                                                  column(9,fluidRow(column(9,plotOutput(outputId = 'PhyloPlot', inline = T)),
                                                                                    column(3,wellPanel(checkboxInput("tipLabels", "Tip labels"),
                                                                                                       conditionalPanel(condition = "input.tipLabels==1",
                                                                                                                        sliderInput("tipSize", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.5)),
                                                                                                       checkboxInput("branchLength", "Edge length"),
                                                                                                       sliderInput("PlotWidthDt", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                       sliderInput("PlotHeightDt", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800),
                                                                                                       selectInput("plotType", "Tree type",
                                                                                                                   c("Phylogram" = "phylogram", "Cladogram" = "cladogram", "Fan" = "fan", "Unrooted" = "unrooted", "Radial" = "radial", "tidy" = "Tidy" ), selected = "phylogram")
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
                                                                                             tabPanel(title = "Maximum Likelihood", useShinyjs(),
                                                                                                      div(id = 'ResetConCharML',
                                                                                                      fluidRow(column(3,wellPanel(radioButtons("transform1", "Data transformation",
                                                                                                                                               c("Raw data" = "NoTrans","Natural logarithm" = "ln","Log base 10" = "log10",
                                                                                                                                                 "Square Root" = "squareRoot","Cube Root" = "cubeRoot",
                                                                                                                                                 "Reciprocal" = "reciprocal","Exponential" = "exp1"),selected = "NoTrans"),
                                                                                                                                  checkboxInput("AncPIC", "Phylogenetic independent contrast estimation"),hr(),
                                                                                                                                  selectInput("ModelContinuous", "Fit model",c("BM" = "BM","OU" = "OU","EB" = "EB"),multiple = TRUE,selected = NULL),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('BM') > -1",
                                                                                                                                                   textInput('maxitBM', 'Max iter BM', value = '2000')),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('EB') > -1",
                                                                                                                                                  textInput('maxitEB', 'Max iter EB', value = '200')),
                                                                                                                                  conditionalPanel(condition = "input.ModelContinuous.indexOf('OU') > -1",
                                                                                                                                                    textInput('maxitOU', 'Max iter OU', value = '20')),
                                                                                                                                  actionButton("runAncML", "run"),hr(),
                                                                                                                                  checkboxInput("MLModAIC", "Model selection - AIC"),hr(),
                                                                                                                                  radioButtons("exportMLanc", "Export output",c("R object (RDS)" = "MLancRDS","TXT" = "MLancTXT")),
                                                                                                                                  downloadButton("downloadAnc", "Download"),hr(),
                                                                                                                                  actionButton("ResetConMl", "Reset Panel")
                                                                                                                                  )
                                                                                                                      ),
                                                                                                               column(9,fluidRow(column(6,
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'PhyloPlot2', inline = T)))),
                                                                                                                                 column(6,
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'histo1'))),
                                                                                                                                        fluidRow(column(12,plotOutput(outputId = 'QQ1'))))
                                                                                                                                 ),
                                                                                                                      wellPanel(fluidRow(column(4,selectInput('mapModelMl','Plot model',choices = NULL, selected = NULL),
                                                                                                                                                sliderInput("tipSizeContMl", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7)),
                                                                                                                                         column(4, sliderInput("PlotWidthContMl", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                                                sliderInput("PlotHeightContMl", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)
                                                                                                                                                ),
                                                                                                                                         column(4,checkboxInput('phenogramML', 'Phenogram'),
                                                                                                                                                conditionalPanel('input.phenogramML == 1',
                                                                                                                                                                 checkboxInput('NormalScaleConML', 'Normal scale')),
                                                                                                                                                actionButton('PlotEditorML', 'Plot Editor'))
                                                                                                                                         )
                                                                                                                                ),hr(),
                                                                                                                      fluidRow(column(12,verbatimTextOutput("infoPanelContinuousML"))))
                                                                                                               )
                                                                                                      )
                                                                                                      ),
                                                                                             tabPanel("Bayesian MCMC",fluidRow(column(3,wellPanel(selectInput("parametersBI", "Set parameters",
                                                                                                                                                                        c("Select" = "select","By default" = "defaultBI","Costumize" = "costumizeBI")),
                                                                                                                                                            conditionalPanel(condition = "input.parametersBI == 'costumizeBI'",
                                                                                                                                                                             textInput('sig2BI','Sig2: Brownian Motion rate'),
                                                                                                                                                                             textInput('aBI','a: Root node state'),
                                                                                                                                                                             textInput('sampleBI','Sample')
                                                                                                                                                                             ),
                                                                                                                                                            textInput('ngenBI','Number of generations',200000),
                                                                                                                                                            textInput('BurninBI','Burn-in',0.2),
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
                                                                                                                                                wellPanel(fluidRow(column(4,selectInput('plotNodesBI','Plot posterior prob. by node',choices = NULL, selected = NULL,multiple = TRUE),
                                                                                                                                                                          sliderInput("tipSizeContBI", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7)),
                                                                                                                                                                   column(4, sliderInput("PlotWidthContBI", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                                                                          sliderInput("PlotHeightContBI", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)),
                                                                                                                                                                   column(4,checkboxInput('phenogramBI', 'Tree phenogram'),
                                                                                                                                                                          actionButton('PlotEditorBI', 'Tree Editor'))
                                                                                                                                                                   )
                                                                                                                                                          ),hr(),
                                                                                                                                                fluidRow(column(12,verbatimTextOutput("infoPanelContinuousBI")))
                                                                                                                                                )
                                                                                                                                         )
                                                                                                      )
                                                                                             )
                                                                                 ),
                                                                conditionalPanel(condition = "input.typeChar == 'Discrete'",
                                                                                 tabsetPanel(id = 'DiscreteCharacter',
                                                                                             tabPanel("Maximum Likelihood",
                                                                                                      fluidRow(column(3, wellPanel(selectInput("typeDisML", "Estimation",choices = c('Joint' = 'joint', 'Marginal' = 'marginal'),selected = 'marginal'),
                                                                                                                                   selectInput("ModelsDisML", "Set models",choices = NULL,multiple = TRUE),
                                                                                                                                   uiOutput("addModel3States"),
                                                                                                                                   conditionalPanel("input.AddModelDisML == 1",
                                                                                                                                                    br(), rHandsontableOutput("w1"), br(),
                                                                                                                                                    actionButton('SubmAddModel','Submit')),br(),
                                                                                                                                   actionButton('RunAnalyDisML','Run'), hr(),
                                                                                                                                   checkboxInput("DisMLModAIC", "Model selection - AIC"),
                                                                                                                                   conditionalPanel("input.DisMLModAIC == 1",
                                                                                                                                                    checkboxInput("ModAverDisML", "Model averaging") ),
                                                                                                                                   conditionalPanel("input.ModAverDisML == 1 & input.DisMLModAIC == 1",
                                                                                                                                                    selectInput("SetModAverDisML", "Set models",choices = NULL, selected = NULL, multiple = TRUE),
                                                                                                                                                    actionButton('RunModAverDisML','Run')), hr(),
                                                                                                                                   radioButtons("exportDisMLanc", "Export output",c("R object (RDS)" = "MLancRDS","TXT" = "MLancTXT")),
                                                                                                                                   selectInput("disCharOutputs", "Choose output",choices = c("Select" = "Select", "Fitted models" = "FittedModels", "AIC matrix" = "AICmatrix", "Model averaging" = "ModelAverage")),
                                                                                                                                   downloadButton("downloadDisML", "Download")
                                                                                                                                   )
                                                                                                                      ),
                                                                                                               column(9, fluidRow(column(6,fluidRow(plotOutput(outputId = 'PhyloPlot8',inline = T))),
                                                                                                                                  column(6,fluidRow(plotOutput(outputId = 'PhyloPlot9')))
                                                                                                                                  ),
                                                                                                                      wellPanel(fluidRow(column(4,selectInput('plotModelDisML','Plot model',choices = NULL),
                                                                                                                                                sliderInput("tipSizeDisML", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7)),
                                                                                                                                         column(4,sliderInput("PlotWidthDisML", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                                                sliderInput("PlotHeightDisML", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)),
                                                                                                                                         column(4,checkboxInput('bestState', 'The most likely state'),
                                                                                                                                                actionButton('PlotEditorDisML', 'Plot Editor'))
                                                                                                                                         )
                                                                                                                                ),hr(),
                                                                                                                      fluidRow(column(12,verbatimTextOutput("infoPanelDiscreteML")
                                                                                                                                      )
                                                                                                                               )
                                                                                                                      )
                                                                                                               )
                                                                                                      ),
                                                                                             tabPanel("Bayesian MCMC",
                                                                                                      fluidRow(column(3,wellPanel(selectInput("QmatrixBI", "Q matrix: rates",choices = c('Select' = 'select', 'Empirical' = 'empirical', 'MCMC' = 'mcmc','Costumize' = 'costumQmDiscBI')),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'costumQmDiscBI'",
                                                                                                                                                   br(),rHandsontableOutput(outputId = "putitamatrix"), br(),
                                                                                                                                                   actionButton('SubmQmatBI','Submit matrix')), br(),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'empirical' | input.QmatrixBI == 'mcmc'",
                                                                                                                                                   
                                                                                                                                                   selectInput("ModelsDisBI", "Set model",choices = NULL,multiple = F),
                                                                                                                                                   conditionalPanel("input.ModelsDisBI == 'Costumize'",
                                                                                                                                                                    br(), rHandsontableOutput("costuModelBI"),br(),
                                                                                                                                                                    actionButton('SubmAddModelBI','Submit model')),br()),
                                                                                                                                  
                                                                                                                                  conditionalPanel("input.QmatrixBI == 'mcmc'",
                                                                                                                                                   selectInput("mcmcParDisBI", "Set mcmc parameters", choices = c('By default' = 'bydefaultmcmcDisBI','Costumize' = 'costummcmcDisBI')),
                                                                                                                                                   conditionalPanel("input.mcmcParDisBI == 'costummcmcDisBI'",
                                                                                                                                                                    selectInput("priorDisBI", "Prior: Gamma parameters", choices = c('Select' = 'select','Use empirical' = 'useEmpirical','No use empirical' = 'noUseEmpirical')),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'useEmpirical'",
                                                                                                                                                                                     radioButtons("betavaluesDisBI", "Beta values",
                                                                                                                                                                                                  c("Fix one value per rate" = "oneBeta", "Assign a value per rate" = "betaPerRate"), selected = character(0)),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'oneBeta'",
                                                                                                                                                                                                      rHandsontableOutput("matBetaValDisBI"), br(),
                                                                                                                                                                                                      actionButton('SubmBetaValDisBI','Submit beta')
                                                                                                                                                                                                      ),
                                                                                                                                                                                     conditionalPanel("input.betavaluesDisBI == 'betaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matBetaValuePerRateDisBI"), br(),
                                                                                                                                                                                                      actionButton('SubmBetaValuePerRateDisBI','Submit betas'))
                                                                                                                                                                                     ),br(),
                                                                                                                                                                    conditionalPanel("input.priorDisBI == 'noUseEmpirical'",
                                                                                                                                                                                     radioButtons("alphaBetaValDisBI", "Alpha and Beta values",
                                                                                                                                                                                                  c("Fix equal values per rate" = "oneAlphaBeta","Alpha and beta value per rate " = "alphaBetaPerRate"), character(0)),
                                                                                                                                                                                     conditionalPanel("input.alphaBetaValDisBI == 'oneAlphaBeta'",
                                                                                                                                                                                                      rHandsontableOutput("matOneAlphaBetaValDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmOneAlphaBetaValDisBI','Submit (a,b)')),
                                                                                                                                                                                     conditionalPanel("input.alphaBetaValDisBI == 'alphaBetaPerRate'",
                                                                                                                                                                                                      rHandsontableOutput("matAlphaBetaPerRateDisBI"),br(),
                                                                                                                                                                                                      actionButton('SubmAlphaBetaPerRateDisBI','Submit (a,b)*rate'))
                                                                                                                                                                                     ),br(),
                                                                                                                                                                    selectInput("vQDisBI", "vQ: variance per rate", choices = c('Select' = 'select','Equal' = 'equalvQDisBI', 'Variances' = 'vQValPerRatesDisBI')),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'equalvQDisBI'",
                                                                                                                                                                                     rHandsontableOutput("matequalvQValDisBI"),br(),
                                                                                                                                                                                     actionButton('SubmequalvQValDisBI','Submit vQ')
                                                                                                                                                                                     ),
                                                                                                                                                                    conditionalPanel("input.vQDisBI == 'vQValPerRatesDisBI'",
                                                                                                                                                                                     rHandsontableOutput("matvQValPerRatesDisBI"),br(),
                                                                                                                                                                                     actionButton('SubmbvQValPerRatesDisBI','Submit vQ*rates'))
                                                                                                                                                                    ), br(),
                                                                                                                                                   textInput('samfreqDisBI','Sample frequency',value = 10),
                                                                                                                                                   textInput('burninDisBI','Burn-in',value = 0.2)
                                                                                                                                                   ),
                                                                                                                                   selectInput("piBI", "Pi: prior prob. in root",choices = c('Select' = 'select', 'Fitzjohn' = 'fitzjohn', 'Stationary' = 'estimated', 'Equal' = 'equal', 'Costumize' = 'costumPiDiscBI')),
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
                                                                                                               wellPanel(fluidRow(column(4,selectInput('plotModelDisBI','Plot outcome',choices = NULL),
                                                                                                                                         selectInput('ploHPDDisBI','Plot transitions',choices = NULL)),
                                                                                                                                  column(4,sliderInput("PlotWidthDisBI", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                                         sliderInput("PlotHeightDisBI", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)),
                                                                                                                                  column(4,sliderInput("tipSizeDisBI", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7),
                                                                                                                                         actionButton('PlotEditorDisBI', 'Plot Editor'))
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
                        tabPanel("Basic Diversification Analysis",
                                 fluidPage(theme = theme,
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Data",
                                                                fluidRow(column(3,wellPanel(selectInput("treeDiver", "Load tree",c("Select" = "select","Example" = "exampDiver", "Import tree" = "treeFileDiver")),
                                                                                            conditionalPanel(condition = "input.treeDiver =='treeFileDiver'",
                                                                                                             fileInput("fileTreeDiver", "Load file")),
                                                                                            actionButton("importTreeDiver", "Import tree"), hr(),
                                                                                            checkboxInput("ultrametricDiverDT", "Is ultrametric?"),
                                                                                            uiOutput("forceultrDiverDT"), hr(),
                                                                                            checkboxInput("BinaryDiverDT", "Is binary?"),
                                                                                            uiOutput("PoliDiverDT"),
                                                                                            conditionalPanel("input.PoliDiverDT == 1",
                                                                                                             selectInput("ResolveDiverDT", "How to resolve multichotomies", choices = c('Resolve randomly' = 'ramdom','how they appear in the tree' ))
                                                                                                             ),
                                                                                            hr(),
                                                                                            radioButtons("exportDiverDT", "Export output",c("R object (RDS)" = "RDSdiverDT","TXT" = "TXTdiverDT")),
                                                                                            downloadButton("downloadDiverDT", "Download"),hr(),
                                                                                            numericInput(inputId = "seedDiverDT", label = "Set seed",value =  999, min = 1, max = 1000000)
                                                                                            )
                                                                                ),
                                                                         column(9,fluidRow(column(9,plotOutput(outputId = 'PhyloPlotDiver1', inline = T, click = 'plot_clickDiverDT') ),
                                                                                           column(3, wellPanel(checkboxInput("tipLabelsDiverDT", "Tip labels"),
                                                                                                               conditionalPanel(condition = "input.tipLabelsDiverDT == 1",
                                                                                                                                sliderInput("tipSizeDiverDT", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.5)),
                                                                                                               checkboxInput("branchLengthDiverDT", "Edge length"),
                                                                                                               sliderInput("PlotWidthDiverDT", "Tree width (px)",step = 100,min = 100, max = 1000, value = 500),
                                                                                                               sliderInput("PlotHeightDiverDT", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800),
                                                                                                               selectInput("plotTypeDiverDT", "Tree type",
                                                                                                                           c("Phylogram" = "phylogram", "Cladogram" = "cladogram", "Fan" = "fan", "Unrooted" = "unrooted", "Radial" = "radial", "tidy" = "Tidy" ), selected = "phylogram")
                                                                                           )
                                                                                                  )
                                                                                           ), hr(),
                                                                                fluidRow(column(12,verbatimTextOutput("infoPanelDiverData")))
                                                                                )
                                                                         )
                                                                
                                                                
                                                                
                                                                ),
                                                       tabPanel("Analysis",
                                                                tabsetPanel(id = 'BasicDiverification',
                                                                            tabPanel("Lineage Throught of Time",
                                                                                     fluidRow(
                                                                                       column(3, wellPanel(p('Ltt plot'),
                                                                                                           checkboxInput('dropExtinct', 'Drop extinct tips from the tree',FALSE ),
                                                                                                           checkboxInput('logLineages', 'plot on log-linear', FALSE ),
                                                                                                           checkboxInput('GammaPybus', 'compute γ from Pybus & Harvey (2000;Proc.Roy.Soc.B)', FALSE ),
                                                                                                           actionButton("runDiverLtt", "Run"),hr(),
                                                                                                           p('MCCR test'),
                                                                                                           textInput(inputId = 'RhoMCCRdiverLtt', 'Rho: Sampling fraction', '0.9'),
                                                                                                           textInput(inputId = 'nsimMCCRdiverLtt', 'nsim', '1000'),
                                                                                                           actionButton("runMCCRDiverLtt", "Run")
                                                                                                           )
                                                                                              ),
                                                                                       column(9,fluidRow(column(6,fluidRow(plotOutput(outputId = 'PhyloPlotDiver2', inline = T))),
                                                                                                         column(6,fluidRow(plotOutput(outputId = 'PhyloPlotDiver3')),
                                                                                                                fluidRow(plotOutput(outputId = 'PhyloPlotDiver4')))
                                                                                       ),
                                                                                       wellPanel(fluidRow(column(4,selectInput('plotLttTree','Plot LTT over tree',choices = NULL),
                                                                                                                 selectInput('ploLttTreeLog','Plot log-lineage',choices = NULL)),
                                                                                                          column(4,sliderInput("PlotWidthDiverLtt", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                 sliderInput("PlotHeightDiverLtt", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)),
                                                                                                          column(4,sliderInput("tipSizeDiverLtt", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7),
                                                                                                                 actionButton('PlotEditorDiverLtt', 'Plot Editor'))
                                                                                       )
                                                                                       ),hr(),
                                                                                       fluidRow(column(12,verbatimTextOutput("infoPanelDiverLtt")
                                                                                       )
                                                                                       ))
                                                                                       )
                                                                                     ),
                                                                            tabPanel("Diversification models: ML",
                                                                                              fluidRow(
                                                                                                column(3, wellPanel(p('Set models'),
                                                                                                                    checkboxInput(inputId = 'yuleModML',label = 'Pure Birth'),
                                                                                                                    conditionalPanel(condition = 'input.yuleModML == 1',
                                                                                                                                     checkboxInput(inputId = 'rhoyuleModML', label = 'Sampling fraction'),
                                                                                                                                     conditionalPanel(condition = 'input.rhoyuleModML == 1', 
                                                                                                                                                      textInput(inputId = 'fractYuleModML',label = 'Rho',value = '0.9')),
                                                                                                                                     checkboxInput(inputId = 'unresolYuModML', label = 'Unresolve tips?'),
                                                                                                                                     textInput(inputId = 'BrateYuleModML',label = 'Speciation Rate',value = '0.1'),
                                                                                                                                     actionButton(inputId = 'addYuleModML', label = 'add'), br()),
                                                                                                                   checkboxInput(inputId = 'BDContModML',label = 'Birth-Death constant'),
                                                                                                                   conditionalPanel(condition = 'input.BDContModML == 1',
                                                                                                                                    textInput(inputId = 'fractBDContModML',label = 'Rho',value = '0.9'),
                                                                                                                                    checkboxInput(inputId = 'unresolBDContModML', label = 'Unresolve tips?'),
                                                                                                                                    textInput(inputId = 'RateBDContModML',label = 'Speciation & Exteictiong Rate',value = '0.1'),
                                                                                                                                    actionButton(inputId = 'addBDContModML', label = 'add'), br()),
                                                                                                                    checkboxInput(inputId = 'BDvarSpeModML',label = 'Birth-Death variable: speciation'),
                                                                                                                    checkboxInput(inputId = 'BDvarExtModML',label = 'Birth-Death variable: extinction'),
                                                                                                                    checkboxInput(inputId = 'BDvarSpeExtModML',label = 'Birth-Death variable: Spec. & Exti.'),
                                                                                                                    checkboxInput(inputId = 'CladoDepenModML',label = 'Clado-dependent model'),
                                                                                                                    checkboxInput(inputId = 'DiverDepentModML',label = 'Diversity-dependent model'))),
                                                                                                column(9,fluidRow(column(6, fluidRow(plotOutput(outputId = 'PhyloPlotDiver5', inline = T))),
                                                                                                                  column(6, fluidRow(plotOutput(outputId = 'PhyloPlotDiver6')))),
                                                                                                       wellPanel(fluidRow(column(6,sliderInput("PlotWidthDiverModML", "Tree width (px)",step = 100,min = 100, max = 1000, value = 400),
                                                                                                                                 sliderInput("PlotHeightDiverModML", "Tree height (px)",step = 100,min = 100, max = 1000, value = 800)),
                                                                                                                          column(6,sliderInput("tipSizeDiverModML", "Tip label size",step = 0.1,min = 0, max = 3, value = 0.7),
                                                                                                                                 actionButton('PlotEditorDiverModML', 'Plot Editor'))),hr(),
                                                                                                       fluidRow(column(12,verbatimTextOutput("infoPanelDiverModML")))))
                                                                                                )
                                                                                    
                                                                                  
                                                                            )
                                                                            )
                                                                
                                                                
                                                                )
                                 ))),
                        
                        tabPanel("SSE Models")
                        ),
             tabPanel("Plot Edition"),
             tabPanel("R code", verbatimTextOutput("Rcode")
                      )
             )
  )
