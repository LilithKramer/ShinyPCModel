#### ShinyPCModel ####

## Author: L. Kramer
## Date:   16 April 2020

## App cannot work with results that have differences in their selection of outputs!


##~~~~~~~~~~~~~~~~~~~~~
##==== Preparation ====
##~~~~~~~~~~~~~~~~~~~~~

##== legend =====

## ##      = comment
## #       = outcommented code
## #!#     = should be checked if path to script changes 
## #?#     = to do
## ##==    = block of info separator

## Objects
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 

## Info 
## multiple PCModel scenario's only contain a header once!


##== install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## nice layouts for shiny
# install.packages("tidyverse")
# install.packages("data.table")

library(tidyverse)
library(data.table)
library(shiny)         
library(shinydashboard)
library(gridExtra)


##== functions ======

fread_AddFilename <- function(df_singlerow){
  #df_singlerow <- input_list[[2]]
  input_data <- fread(df_singlerow[1, "datapath"])
  input_data[, filename := gsub(".txt", "", (df_singlerow[1, "name"]))]
  return(input_data)
  }

##== settings =======

options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 



##== set paths ========

# setwd("C:/Users/Lilith Kramer/Documents/PhD/Documenten/3.0. PhD inhoudelijk/3.8. R/ShinyPCModel")

#!# directories
#dirInp <- "input/"
## dirInpPCModel <- "C:/Users/Lilith Kramer/Documents/PhD/Documenten/3.0. PhD inhoudelijk/3.6. PCLake/PCModel-master/Licence_agreement/I_accept/PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/Txt"
#dirSet <- "settings/"
#dirOut <- "output/"


 
##==User Interface Start==================

header  <- dashboardHeader(title = "Shiny PCModel")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "side_tabs",
              menuItem(text = "Ready for action?", tabName = "start", icon = icon("home"))
  ))

## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body <- dashboardBody(
  
  tabItems(
    #===start_tab==============
    tabItem(tabName = "start",
            fluidRow(
              box(title = "Load data",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  p("Maximum file size is 30MB. Only .txt files are accepted.", br(), "Multiple files can be added, but", strong(" DO NOT "), "add files that differ in their output columns!"),
                  fileInput("input_data", NULL,
                            multiple = TRUE,
                            accept = c("text/txt"),
                            buttonLabel = "Choose .txt files",
                            placeholder = "No files have been selected yet."),
                  div(style = 'overflow-x: scroll', tableOutput("display_input_data")),
                  width = 12),
              box(title = "Show data",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  uiOutput("checkbox_geomlines"),
                  tags$div(align = 'left', 
                           class = 'multicol',
                           style = 'overflow-x: scroll',
                           uiOutput("checkbox_facets")),
                  div(style = 'overflow-y: scroll', uiOutput("plotgraph")),
                  width = 12
              )))))

ui <- dashboardPage(skin = "green", header, sidebar, body, 
                    tags$head(tags$style(HTML(" .multicol { 
                        height: 200px;
                        -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                        -moz-column-count: 5;    /* Firefox */ 
                        column-count: 5; 
                        -moz-column-fill: auto;
                        -column-fill: auto;
                  } "  ## https://www.geeksforgeeks.org/css-column-fill-property/
                                              )
                                         )
                              )
                    )

##==SET SERVER=========


server <- function(input, output, session) {
  
  ## load data
  input_files <- reactive({
    req(input$input_data)
    input_df <- as.data.frame(input$input_data)
    input_list <- split(input_df, f = input_df$name)
    return(input_list)
    })
  
  input_data <- reactive({

   read_datatables <- lapply(input_files(), fread_AddFilename)
   names_all_cols <- colnames(read_datatables[[which(sapply(read_datatables, function(y) "RunId" %in% colnames(y)))]])

   make_datatable <- rbindlist(read_datatables, use.names = F)
   if(colnames(make_datatable)[1] != "RunId"){colnames(make_datatable) <- names_all_cols}
   make_datatable[, which(colnames(make_datatable) == "-1"):= NULL]
   
   changeCols_numeric <- grep("^_", colnames(make_datatable), value = T) ## first character = _
   make_datatable[,(changeCols_numeric):= lapply(.SD, as.numeric), .SDcols = changeCols_numeric]
   changeCols_factor <- c("RunId", "filename")
   make_datatable[,(changeCols_factor):= lapply(.SD, FUN = function(x) as.factor(as.character(x))), .SDcols = changeCols_factor]
   return(make_datatable)
  
 })
 

  ## make output so user can see the if the data is correct
  output$display_input_data <- renderTable({
    input_data()[1:5]
    })
 
  molten_data <- reactive({
    totally_molten <- melt(input_data(), id.vars = c("RunId","Time", "filename"))
    setkey(totally_molten, filename, variable)
    return(totally_molten)
  })

  output$checkbox_facets <- renderUI({
    var_options <- unique(molten_data()$variable)
    checkboxGroupInput("variable_options", "Choose variables:", var_options, inline = FALSE)
  })
  
  output$checkbox_geomlines <- renderUI({
    file_options <- unique(molten_data()$filename)
    checkboxGroupInput("run_options", "Choose run:", file_options, selected = 1, inline = TRUE)
  })
  
  
  # this function defines a height of your plot
  plot_height <- function() {
    req(input$variable_options)
    amount_of_plots <- length(input$variable_options) * 200 
    return(amount_of_plots)
  }
  
  # this function defines your plot, which depends on input$dimension1In
  output$contents <- renderPlot({
    req(input$variable_options, input$run_options)
    
    # subset_data <- molten_data()[.(input$run_options, input$variable_options),]
    subset_data <- molten_data()[(filename %in% input$run_options) & (variable %in% input$variable_options),]
    
    
    
    plot <- ggplot(subset_data, aes(x = Time, y = value, color = filename)) +
      geom_line() +
      facet_wrap("variable", ncol = 1, scales = "free_y") +
      theme_bw()
    
  return(plot)
  })
  
  # wrap plotOutput in renderUI
  output$plotgraph <- renderUI({
    plotOutput("contents", height = plot_height(), width = "100%")
  })
  

  
}

shinyApp(ui, server)





