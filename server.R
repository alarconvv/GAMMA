#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




# Define server
# 
shinyServer(function(input, output, session) {

  ##############################################################################
  ##############################################################################
  #
  # Ancestral state reconstruction
  #
  ##############################################################################
  ##############################################################################  

  ##############################################################################
  #   Data panel
  ##############################################################################
  
  source(file = 'Server/AncestralStateReconsttruction/AncData.R',local = T)

  ##############################################################################
  #   Analysis panel
  ##############################################################################
  
  ##############################################################################
  #     Continuous Character : Maxima likelihood
  ##############################################################################

  source(file = 'Server/AncestralStateReconsttruction/Continuous/AncContML.R', local = T)
  
  ##############################################################################
  #     Continuous Character : Stocastic Mapping
  ##############################################################################

   source(file = 'Server/AncestralStateReconsttruction/Continuous/AncContBI.R', local = T)
  
  ##############################################################################
  # Discrete Character : Maxima Likelihood
  ##############################################################################
  
   source(file = 'Server/AncestralStateReconsttruction/Discrete/AncDiscML.R', local = T)

  ##############################################################################
  # Discrete Character : Stocastic Mapping
  ##############################################################################
 
  source(file = 'Server/AncestralStateReconsttruction/Discrete/AncDiscBI.R', local = T)
})















