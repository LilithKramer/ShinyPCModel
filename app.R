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


## To Do
## Maybe add spinners for waiting time during loading
## Maybe make ggplots plotly's

##== install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## nice layouts for shiny
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("gtools")

library(tidyverse)
library(shiny)         
library(shinydashboard)
library(gridExtra)
library(plotly)
library(rlang)
library(data.table)
library(gtools)
library(scales)

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

#!# directories
dirHome <- "C:/Users/Lilith Kramer/Documents/PhD/Documenten/_01. Onderzoek/01.07. R/01.07.01. ShinyPCModel/"
dirSet <- "settings/"

fnSettings <- "output_names.csv"

## load settings file & rework for labelling and subsetting
dtOutNames <- data.table::fread(paste(dirSet, fnSettings, sep = ""))
dtOutNames$Rname <- gsub("_", "", dtOutNames$Excelname)
cOutNamesR <- unique(dtOutNames$Rname)
cOutNamesExcel <- unique(dtOutNames$Excelname)
dtOutNames$Unit <- gsub("_", "'", dtOutNames$Unit)
dtOutNames$Unit <- gsub("0", "'-'", dtOutNames$Unit)
dtOutNames$Unit <- gsub("\\*", "%\\*%", dtOutNames$Unit)
dtOutNames$prettyName <- paste(dtOutNames$Rname, " ~ (", dtOutNames$Unit, ")", sep = "")


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
              box(title = "Import data",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = FALSE,
                  p("Multiple files can be imported at the same time. Each file should contain one run only.
                    The files can be in the output format of either PCLake+ Excel or PCLake+ R. 
                    Only .txt files are accepted", br(), 
                    "If you choose the", strong(" Excel format"), ", the .txt files can miss headers. However, at least one file should contain the headers and", em("all files"), "should have the same output columns in the same order.
                    It is also expected that the 'RunId' column is the first column in the .txt file that contains the headers.", br(),
                    "If you choose the", strong(" R format "), ",", em(" all files"), " should contain headers. However, the order of the headers does not matter.", br(),
                    "Maximum file size is 30MB.",),
                  radioButtons("model_type", label = "Data source", choices = c("Excel", "R"), selected = "R"),
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
                  p(""),
                  uiOutput("checkbox_facets"),
                  uiOutput("timeslider"),
                  actionButton("make_a_plot", "Make graphs", icon("caret-right"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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

    if(input$model_type == "Excel"){
      
      validate(
        need(length(which(sapply(read_datatables, function(y) "RunId" %in% colnames(y)[1]))) > 0,
             "Please check the file format. You have selected Excel as the source of your text files. However, the RunId column name is either not the header of the first column or the RunId column name is missing in all input files.")
        )
      
      names_all_cols <- colnames(read_datatables[[which(sapply(read_datatables, function(y) "RunId" %in% colnames(y)))]])
      
      make_datatable <- rbindlist(read_datatables, use.names = F)
      if(colnames(make_datatable)[1] != "RunId"){colnames(make_datatable) <- names_all_cols}
      if("-1" %in% colnames(make_datatable)){make_datatable[, which(colnames(make_datatable) == "-1"):= NULL]}
      changeCols_numeric <- intersect(colnames(make_datatable), c(cOutNamesExcel, "Time", "time"))
      make_datatable[,(changeCols_numeric):= lapply(.SD, as.numeric), .SDcols = changeCols_numeric]
      changeCols_factor <- setdiff(colnames(make_datatable), changeCols_numeric)
      make_datatable[,(changeCols_factor):= lapply(.SD, FUN = function(x) as.factor(as.character(x))), .SDcols = changeCols_factor]
      
      
    } else {
      
      validate(
        need(length(which(sapply(read_datatables, function(y) "V1" %in% colnames(y)))) == 0,
             "You have selected R as the source of your text files. However, column headers are missing in at least one input file. Please check the file format.")
      )
      
      if(length(read_datatables)==1){
        make_datatable <- read_datatables[[1]]  
      }
      
      if(length(read_datatables)>1){

        common_colnames <- Reduce(intersect, lapply(read_datatables, colnames))
        dt_setkey <- lapply(read_datatables, function(x) setkeyv(data.table(x), common_colnames))
        make_datatable <- Reduce(function(...) merge(..., all=TRUE), dt_setkey)
        
       }
      
      if("-1" %in% colnames(make_datatable)){make_datatable[, which(colnames(make_datatable) == "-1"):= NULL]}
      changeCols_numeric <- intersect(colnames(make_datatable), c(cOutNamesR, "Time", "time")) ## first character = _
      make_datatable[,(changeCols_numeric):= lapply(.SD, as.numeric), .SDcols = changeCols_numeric]
      changeCols_factor <- setdiff(colnames(make_datatable), changeCols_numeric)
      make_datatable[,(changeCols_factor):= lapply(.SD, FUN = function(x) as.factor(as.character(x))), .SDcols = changeCols_factor]
      
      if("time" %in% colnames(make_datatable)){colnames(make_datatable)[which(colnames(make_datatable)=="time")] <- "Time"}
      
    }
     
    
     return(make_datatable)
  
 })
 

  ## make output so user can see the if the data is correct
  output$display_input_data <- renderTable({
    input_data()[1:5]
    })
 
  molten_data <- reactive({
    if(input$model_type == "Excel"){toMelt <- setdiff(colnames(input_data()), intersect(colnames(input_data()), cOutNamesExcel))}
    if(input$model_type == "R"){toMelt <- setdiff(colnames(input_data()), intersect(colnames(input_data()), cOutNamesR))}
    totally_molten <- melt(input_data(), id.vars = toMelt)
    totally_molten$filename <- factor(totally_molten$filename, levels = mixedsort(levels(totally_molten$filename))) ## reorder levels according to characters AND numericals so the sorting makes sense in the plots
    return(totally_molten)
  })

  # make colours for lines in plot
  line_colours <- reactive({
    
    cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7") ## color blind palette from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
    
    if(length(levels(molten_data()$filename)) - 8 > 0){
      line_colours <- c(cbp1, scales::hue_pal()(length(levels(molten_data()$filename)) - 8))}
    if(length(levels(molten_data()$filename)) - 8 <= 0){
      line_colours <- cbp1[1:length(levels(molten_data()$filename))]
    }   
    
    names(line_colours) <- levels(molten_data()$filename)
    return(line_colours)
  })
  
  
  output$checkbox_geomlines <- renderUI({
    file_options <- unique(molten_data()$filename)
    checkboxGroupInput("run_options", "Choose run:", file_options, selected = file_options[1], inline = TRUE)
  })
  
  output$timeslider <- renderUI({
    min_slider <- min(molten_data()$Time)
    max_slider <- max(molten_data()$Time)
    sliderInput("timeslider_true", label = "Choose timespan (days):", min_slider, max_slider, value = c(0, max_slider))
    })
  
  output$checkbox_facets <- renderUI({
    if(input$model_type == "Excel"){keepIn <- intersect(colnames(input_data()), cOutNamesExcel)}
    if(input$model_type == "R"){keepIn <- intersect(colnames(input_data()), cOutNamesR)}
    
    input_data_adj <- input_data()[, ..keepIn]
    varSelectInput("variable_options", "Choose variables:", input_data_adj, multiple = TRUE)
  })
  
  ## make the display of the plots dependent on the "Make Plot" button
  list_plot_input <- eventReactive(input$make_a_plot, {
    req(length(input$variable_options)>0, input$run_options)
    
    df_variables  <- input$variable_options
    df_run        <- input$run_options
    df_timeslider <- input$timeslider_true
    list_inputs <- list(var = df_variables, run = df_run, ts = df_timeslider)
    return(list_inputs)
    
  })
 
  # this function defines a height of your plot
  plot_height <- function() {
    req(input$variable_options)
    amount_of_plots <- max(1, length(list_plot_input()$var)) * 200 
    return(amount_of_plots)
  }
  
  
  # this function defines your plot, which depends on input$dimension1In
  output$contents <- renderPlot({
    
    ## varSelectInput makes a 'symbol list' that you cannot just 'unlist' (wrong class type)
    ## somehow I can't get the !! option to work (from rlang) that is mentioned on the function page of varSelectInput
    ## so... I made this ugly lapply thing below to unlist the variable options input
    var_op <- unlist(lapply(list_plot_input()$var, function(x) rlang::as_name(x))) ## niet chique
    
    ## get subset
    subset_data <- molten_data()[(filename %in% list_plot_input()$run) & 
                                   (variable %in% var_op) &
                                   (Time >= list_plot_input()$ts[1]) & 
                                   (Time <= list_plot_input()$ts[2]),]
    
    if(input$model_type == "Excel"){getPrettyName <- unique(dtOutNames[, c("Excelname", "prettyName")])}
    if(input$model_type == "R"){getPrettyName <- unique(dtOutNames[, c("Rname", "prettyName")])}
    colnames(getPrettyName)[which(colnames(getPrettyName) %in% c("Rname", "Excelname"))] <- "variable"
    subset_data_addNames <- subset_data[getPrettyName, on = 'variable', prettyName := i.prettyName]
  
    ## easy fix for problem with y-axis, that I would like to start at zero, except when the error balances are included
    ifelse(any(grepl("Error", var_op)), limit_y <- NA, limit_y <- 0)  ## if Error is present, unleash the y-axis from it's zero
    
    plot <- ggplot(subset_data_addNames, aes(x = Time, y = value, color = filename)) +
      geom_line() +
      scale_x_continuous(expand = c(0, 0), minor_breaks = seq(0, max(molten_data()$Time), 365/4), breaks = seq(0, max(molten_data()$Time), 365)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(limit_y, NA)) + ## this sets the y-axis to zero (otherwise there's a small gap below the axis); maybe remove this again as for the BalanceErrors the axes should go below zero
      facet_wrap("prettyName", labeller = label_parsed, ncol = 1, scales = "free_y") +
      theme_bw() +
      scale_color_manual(values = line_colours()) +
      geom_text(aes(y = value * 1.1, label = "")) + ## if the scale is set to expand = c(0,0), the max value can appear to be cut of. This is a workaround to fix that.
      theme(
        #panel.grid.major.x = element_line(color = "darkgrey"),
        panel.grid.minor.x = element_line(linetype = "dashed") #, color = "lightgrey" linetype = "dashed")
      )
    
    return(plot)
  })

  # wrap plotOutput in renderUI
  output$plotgraph <- renderUI({
    plotOutput("contents", height = plot_height(), width = "100%")
  })
  
}

shinyApp(ui, server)





