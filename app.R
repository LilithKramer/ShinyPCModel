#### ShinyPCModel ####

## Author: L. Kramer
## Date:   16 April 2020


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

addFilename <- function(filename) { cbind(fread(filename), name = paste(filename))}


##== settings =======

options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 



##== set paths ========

# setwd("C:/Users/Lilith Kramer/Documents/PhD/Documenten/3.0. PhD inhoudelijk/3.8. R/ShinyPCModel")

#!# directories
dirInp <- "input/"
## dirInpPCModel <- "C:/Users/Lilith Kramer/Documents/PhD/Documenten/3.0. PhD inhoudelijk/3.6. PCLake/PCModel-master/Licence_agreement/I_accept/PCModel1350/PCModel/3.00/Models/PCLake+/6.13.16/Txt"
dirSet <- "settings/"
dirOut <- "output/"

#!# files
fnSysrep0 <- "sysrep0.txt"
fnSysrep1 <- "sysrep1.txt"



##== load files =========


## read & load as list
# dtSysrep0 <- fread(paste(dirInp, fnSysrep0, sep = ""))
# test <-  list("input/sysrep1.txt", "input/sysrep0.txt")
# 
# read_datatables <- lapply(test, fread)
# lapply(read_datatables, FUN = function(x) which(colnames(x)=="RunId"))
# bind_datatables <- rbindlist(read_datatables, use.names = F)
# make_dt <- make_datatable[, which(colnames(make_datatable) == "-1"):=NULL]
# 
# dtSysrep0 <- fread(paste(dirInp, fnSysrep0, sep = ""))
# make_dt <- dtSysrep0[, (42):=NULL]
# 
# molten_data <- melt(make_dt, id.vars = c(1,2))
# plot <- ggplot(molten_data, aes(x = Time, y = variable, group = RunId)) +
#   geom_line() +
#   facet_wrap("variable", ncol = 4, scales = "free_y")
# plot

## add name to file 
 
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
                  p("Maximum file size is 30MB. Only .txt files are accepted. Multiple files can be added."),
                  fileInput("input_data", NULL,
                            multiple = TRUE,
                            accept = c("text/txt"),
                            buttonLabel = "Choose .txt files",
                            placeholder = "No files have been selected yet."),
                  div(style = 'overflow: scroll', tableOutput("display_input_data")),
                  width = 12),
              box(title = "Show data",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  tags$div(align = 'left', 
                           class = 'multicol',
                           # style = 'overflow: scroll',
                           uiOutput("checkbox_geomlines")),
                 # div(style = 'overflow: scroll_y', plotOutput("plot01")),
                  div(style = 'overflow: scroll_y', uiOutput("plotgraph")),
                  width = 12
              )))))

ui <- dashboardPage(skin = "green", header, sidebar, body, 
                    tags$head(tags$style(HTML(" .multicol { 
                        height: 250px;
                        -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                        -moz-column-count: 5;    /* Firefox */ 
                        column-count: 5; 
                        -moz-column-fill: balance;
                        -column-fill: balance;
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
    list_of_files <- input$input_data
    #browser()
    return(list_of_files)
    
  })
  
  
#  input_data <- reactive({
#   
#    loading <- function(inputfilelist_no) {
#      x <- fread(inputfilelist$datapath[inputfilelist_no])
#      return(x)
#    }
#    
#    read_datatables <- lapply(input_files(), loading)
#    make_datatable <- rbindlist(read_datatables, use.names = F)
#    make_dt <- make_datatable[, which(colnames(make_datatable) == "-1"):=NULL]
#    
#  })
 
  
 input_data <- reactive({
   req(input$input_data)
   
   list_of_files <- input$input_data
   
   read_datatables <- lapply(input$input_data$datapath, fread)
   
   
   make_datatable <- rbindlist(read_datatables, use.names = F)
   
   make_dt <- make_datatable[, which(colnames(make_datatable) == "-1"):=NULL]
   
   
 })
 
 
  ## make output so user can see the if the data is correct
  output$display_input_data <- renderTable({
    input_data()[1:5]
    })
 
  molten_data <- reactive({
    melt(input_data(), id.vars = c(1,2))
  })

  output$checkbox_geomlines <- renderUI({
    var_options <- unique(molten_data()$variable)
    checkboxGroupInput("variable_options", "Choose variables:", var_options, inline = TRUE)
  })
  
  
  ## make plot
 # output$plot01 <- renderPlot({
 # 
 #   plot <- ggplot(molten_data(), aes(x = Time, y = variable, color = RunId)) +
 #     geom_line() +
 #     facet_wrap("variable", ncol = 1, scales = "free_y")
 # 
 # return(plot)  
 # })
  
  
  # this function defines a height of your plot
  plot_height <- function() {
    req(input$variable_options)
    amount_of_plots <- length(input$variable_options) * 200 
    return(amount_of_plots)
  }
  
  # this function defines your plot, which depends on input$dimension1In
  output$contents <- renderPlot({
    req(input$variable_options)
    
    plot <- ggplot(molten_data()[variable %in% input$variable_options,], aes(x = Time, y = value, color = RunId)) +
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





