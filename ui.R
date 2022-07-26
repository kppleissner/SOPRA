# SOPRA 
# ui.R  V.0.15 last edited by KPP: 2022/07/26

# The software has been implemented in R http://www.r-project.org.
# Software License:
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.

# -- Libraries. 
#




###########################################

# Define packages which are necessary
# Packages "magrittr","tidyverse","tidyr", are necessary for pipe operator 

packages <- c("magrittr","tidyverse","tidyr",
              "shiny", "shinyFiles","RColorBrewer",
              "NLP", "data.table", "mclust", 
              "wordcloud", "tm", "fs",
              "BiocManager")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# install maSigPro
BiocManager::install("maSigPro")


# Packages loading
library(shinyFiles)
library(maSigPro)
library(magrittr)
lapply(packages, library, character.only = TRUE) %>% invisible()


##################################################


# -- Initialation.
rm(list=ls())
options( warn = -1 )

script_dir = getwd()

inside_script=T


# --- User interface:

ui <- fluidPage(
  
  
  titlePanel(img(src="SOPRA.png", height = 100, width = 400)),
  
  tags$p(),
  
  titlePanel(h4("Single Object Profiles Regression Analysis")),
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  actionButton("showversioninfo", "Show version info"),
  
  
  titlePanel(img(src="SOPRA2.png", height = 100, width = 700)),
  
  actionButton("showauthorinfo", "Show author info"),
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  
  
  # Interactive input of input/output folder
  
  
  tags$h4(p(br(), strong(span("Folder selection", style="color:blue")))),
  
  sidebarPanel(
    tags$h5(p(br(), strong(span("Choose the input folder with the ScanR data files", style="color:brown")))),
    tags$p(),
    shinyDirButton("indirectory", "Select Input folder", "Please select an input folder with ScanR data"),
    mainPanel(
      tags$hr(),
      tags$h5( strong(span("Following input folder was selected", style="color:blue"))),
      textOutput("directorypath1")   
    )
  ),
  
  
  sidebarPanel(
    tags$h5(p(br(), strong(span("Choose the output folder for resulting data", style="color:brown")))),
    tags$p(),
    shinyDirButton("outdirectory", "Select or create Output folder", "Please select or create an output folder for  resulting data"),
    mainPanel(
      tags$hr(),
      tags$h5(strong(span("Following output folder was selected", style="color:blue"))),
      textOutput("directorypath2")
    ) 
  ), 
  
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  # interactive input of PlateList
  tags$h4(strong(span("File selection", style="color:blue"))),
  
  sidebarPanel(strong(span("Select Platelist file with information which files should be processed", style="color:brown")),
               tags$hr(),
               
               shinyFilesButton("PlateLi", "Choose Platelist-file", "Please select the Platelist-file", multiple = TRUE), 
               
               mainPanel(
                 tags$hr(),
                 tags$h5(strong(span("Following Platelist was choosen:", style="color:blue"))),
                 textOutput("PlateListOut")
               )
  ),
  
  
  
  # interactive input of PlateConfLookUp
  
  sidebarPanel(strong(span("Select PlateConfLookUp file with information on plate configuation", style="color:brown")),
               tags$hr(),
               #shinyFilesButton("PlateConfLookUp", "Choose PlateConfLookUp-file", "Please select the PlateConfLookUp-file", multiple = TRUE), 
               shinyFilesButton("PlateCon", "Choose PlateConfLookUp-file", "Please select the PlateConfLookUp-file", multiple = TRUE), 
               
               
               mainPanel(
                 tags$hr(),
                 tags$h5(strong(span("Following PlateConfLookUp was choosen:", style="color:blue"))),
                 textOutput("PlateConfLookUpOut")
               )
  ),
  
  
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  # -- Sopra 1of4 (Checkbox Preprocessing).
  
  actionButton("show1of4info", "Show SOPRA 1 of 4 information"),
  
  inputPanel(
    tags$div(class = "header", checked = NA,
             p(br(), strong(span("SOPRA 1 of 4: Preprocessing and well flagging", style="color:darkblue"))))
  ),
  inputPanel(
    checkboxInput(inputId ="preprocess",
                  label = "Check if you want to preprocess data",
                  value = FALSE)
  ),
  
  
  
  # interactive input of ScreenLog
  
  sidebarPanel(strong(span("Select ScreenLog file with information which files should be preprocessed", style="color:brown")),
               tags$hr(),
               shinyFilesButton("ScreenLo", "Choose ScreenLog-file", "Please select the ScreenLog-file", multiple = TRUE), 
               
               mainPanel(
                 tags$hr(),
                 tags$h5(strong(span("Following ScreenLog was choosen:", style="color:blue"))),
                 textOutput("ScreenLogOut")
               )  
               
               
  ),
  
  
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  # -- Sopra 2of4 (Checkbox Dataprocessing).
  
  actionButton("show2of4info", "Show SOPRA 2 of 4 information"),
  
  inputPanel(
    tags$div(class = "header", checked = NA,
             p(strong(span("SOPRA 2 of 4: Data gathering, median plate normalization, gating filters and data binning", style="color:darkblue"))), p())
    
  ),
  
  inputPanel(
    
    checkboxInput(inputId="dataprocess", 
                  label = "Check if you want to perform: data gathering,  median plate normalization, gating filters and data binning.",
                  value = TRUE)
  ),
  
  
  # -- Input Sopra 2of4.
  
  inputPanel(
    checkboxInput(inputId="normalize.plate", 
                  label = "Check if you want to median-normalize each plate?",
                  value = TRUE)
  ),
  
  fluidRow(
    
    column(3,
           sliderInput("number_of_bins", "Number_of_bins", 
                       min=3, max=9, value=7)),
    
    column(3, 
           selectInput("number_of_wells", 
                       label = tags$h5(strong(span("Wells_per_plate (have to match to selected PlateList)"))), 
                       choices = c(384, 96),
                       selected = 96)),
    
    column(3,
           sliderInput("left.quantile", "Left Quantile", 
                       min=0, max=20, value=5)),
    
    column(3,
           sliderInput("right.quantile", "Right Quantile", 
                       min=80, max=100, value=95))
  ),
  
  inputPanel(
    
    radioButtons("gating.filter",label = h5("Use gating filters:"),choices = list("Filter 1" = 1, "Filter 2" = 2, "Filter 3" = 3, "Use predefined option" =4, "No filters"=5), selected = 4),
    checkboxInput(inputId="table.out", label = "Do you want to export the interim data files?", value = TRUE),
    checkboxInput(inputId="pic.out", label = "Do you want to export the QC files?", value = TRUE)
    
  ),
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),
  
  # -- Sopra 3 of 4 (Checkbox maSigPro).
  
  actionButton("show3of4info", "Show SOPRA 3 of 4 information"),
  
  inputPanel(
    tags$div(class = "header", checked = NA,
             p(br(), strong(span("SOPRA 3 of 4: Identification of statistically significant density profiles using maSigPro", style="color:darkblue"))),
             tags$a(href = "https://bioconductor.org/packages/release/bioc/vignettes/maSigPro/inst/doc/maSigProUsersGuide.pdf" , target= "_blank", "Click for maSigPro Users Guide"),
             p() )
  ),
  
  
  
  
  inputPanel(
    checkboxInput(inputId="dataanalysis", label = "Check if you want to identify significant density profiles.", value = TRUE)),
  
  
  
  
  
  # -- Input Sopra 3of4.
  
  fluidRow(
    
    column(3,
           sliderInput("FDR", "Level of false discovery rate (FDR):", 
                       min=0, max=1, value=0.05)),
    
    column(3,
           sliderInput("alpha", "Threshold for p-value-alfa:", 
                       min=0, max=1, value=0.05)),
    
    column(3,
           sliderInput("RSQ", "Value for stepwise regression (RSQ):", 
                       min=0, max=1, value=0.6)),
    
    column(3,
           sliderInput("degree", "Degree of regression fit polynome:", 
                       min=1, max=5, value=3))    
  ),
  
  
  # -- Input Sopra Cluster 3of4.
  
  fluidRow(
    
    column(3,
           sliderInput("cluster", "Cluster number:", 
                       min=1, max=9, value=4)),
    
    
    
    column(3,selectInput("clustermethod", 
                         label = tags$h5(strong(span("Choose cluster method (Do not use hclust, if preprocessing was done!)"))), 
                         choices = list("hclust"=1,"kmeans" = 2,"Mclust" = 3), 
                         selected = 2)),
    
    column(3,selectInput("hclustmethod", 
                         label = tags$h5(strong(span("Cluster method only for hclust"))), 
                         choices = list("ward.D" = 1, "ward.D2" = 2, "single" = 3, "complete" = 4,  "average" = 5, "mcquitty" = 6, "median" = 7, "centroid" = 8 ), 
                         selected = 7)),
    
    column(3,selectInput("distmethod", 
                         label = tags$h5(strong(span("Distance method only for hclust"))), 
                         choices = list("cor" = 1, "euclidean" = 2), 
                         selected = 1))
  ),
  
  
  fluidRow(
    
    column(3,selectInput("stepmethod", 
                         label = tags$h5(strong(span("Choose step.method"))), 
                         choices = list("two.steps.backward" = 1, "backward" = 2, "forward" = 3, "two.steps.forward" = 4), 
                         selected = 1)),    
    
    column(3,
           sliderInput("maxiterations", "Choose max. Iterations", 
                       min=1, max=500000000, value=500000000)),
    
    
    column(3,selectInput("nvar.cor",
                         label = tags$h5(strong(span("Choose nvar.corr"))), 
                         choices = list("TRUE" = 1, "FALSE" = 2), 
                         selected = 1))    
  ),
  
  titlePanel(img(src="blueline.png",height = 20, width = 1500)),
  
  
  
  # -- Sopra Cluster 4of4 (Checkbox Postprocessing).
  
  actionButton("show4of4info", "Show SOPRA 4 of 4 information"),
  inputPanel(
    tags$div(class = "header", checked = NA,
             p(br(),
               strong(span("SOPRA 4 of 4: Converting of sig. siRNAs into gene name, cluster and frequency of genes in a cluster.", style="color:darkblue"))))
  ),
  titlePanel(img(src="blueline.png",height = 20, width = 1500)),
  
  # -- GO Action.
  
  
  inputPanel(
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("GO", "Run analysis!", style='padding:4px; font-size:150%'),
    p("Click the button to run the pipeline.")
  ),
  
  # -- STOP Action.
  inputPanel(
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("STOP", "Stop analysis!", style='padding:4px; font-size:100%'),
    p("Click the button to stop the script.This can take a while.")
  ),  
  
  titlePanel(img(src="blueline.png", height = 20, width = 1500)),  
  
  fluidPage(
    headerPanel(h4(" << Click on the tabs to see the results >>")),
    mainPanel(width = "100%",height = "125%",
              tabsetPanel(
                tabPanel(h4(style="color:brown","Exp.design"),tableOutput("Edesign")),
                tabPanel(h4(style="color:brown","QC plot"),imageOutput("image2", width=500,height = 500)), 
                tabPanel(h4(style="color:brown","clustered profiles of sign. siRNAs"),imageOutput("image3", width=800,height = 800)),
                tabPanel(h4(style="color:brown","sign. siRNAs with cluster "),tableOutput("sig_siRNA")), 
                tabPanel(h4(style="color:brown","sign. genes with cluster and frequency"),tableOutput("sig_genes")),  
                tabPanel(h4(style="color:brown","Genecount"),plotOutput("plot1", width=500,height = 500))
              )
    )
    
  ) # end of second fluidPage
  
) # end of first fluidPage 

# --- END of ui. 

