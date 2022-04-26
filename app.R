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


## Optional improvements
## Maybe add spinners for waiting time during loading
## Maybe make ggplots plotly's
## - Tested it quickly, plotly gives a warning about group_by_ that is deprecated. Don't know exactly why yet. 
## - Test also showed that plotly cannot handle 'prettynames', so it might be better to make another tab with a plotly in it
## Make timeslider easier to work with (convert days to years?)
## select all runs button / deselect all runs
## if I select more than 11 runs, the legend of the plot disappears off screen

##== install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## nice layouts for shiny
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("gtools")
# install.packages("DT")

library(DT)
library(tidyverse)
library(shiny)         
library(shinydashboard)
library(gridExtra)
library(plotly)
library(rlang)
library(data.table)
library(gtools)
library(scales)

# library(plotly)

##== functions ======

fread_AddFilename <- function(df_singlerow){
  #df_singlerow <- input_list[[2]]
  input_model_data <- fread(df_singlerow[1, "datapath"])
  input_model_data[, filename := gsub(".txt", "", (df_singlerow[1, "name"]))]
  return(input_model_data)
}


get_colours <- function(factor_vector){
  
  #factor_vector <- subset_data_addNames$node
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7") ## color blind palette from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
  
  if(length(levels(factor_vector)) - 8 > 0){
    line_colours <- c(cbp1, scales::hue_pal()(length(levels(factor_vector)) - 8))}
  if(length(levels(factor_vector)) - 8 <= 0){
    line_colours <- cbp1[1:length(levels(factor_vector))]
  }   
  
  names(line_colours) <- levels(factor_vector)
  return(line_colours)
}

##== settings =======

options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
options(shiny.maxRequestSize = 160*1024^2) ## maximum upload size is now 160Mb, standard for R shiny is 5Mb. 
## This pc cannot handle much more than 160Mb, due to processing steps and objects that will take up too much space. 



##== set paths ========

#!# directories
dirHome <- "C:/Users/Lilith Kramer/Documents/PhD/Documenten/_01. Onderzoek/01.06. Modellen/01.06.05. R/01.06.01. ShinyPCModel/"
dirSet <- "settings/"

fnSettings <- "output_names.csv" ## this file is made with the R script "make_output_names_csv.R". 

## load settings file & rework for labelling and subsetting
dtOutNames <- data.table::fread(paste(dirSet, fnSettings, sep = ""))
dtOutNames$Rname <- gsub("_", "", dtOutNames$Excelname)
cOutNamesR <- unique(dtOutNames$Rname)
cOutNamesExcel <- unique(dtOutNames$Excelname)
cOutNamesMeasurement <- unique(dtOutNames$Measurement)
dtOutNames$Unit <- gsub("_", "'", dtOutNames$Unit)
dtOutNames$Unit <- gsub("0", "'-'", dtOutNames$Unit)
dtOutNames$Unit <- gsub("\\*", " \\* ", dtOutNames$Unit)
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
                    "Maximum file size is 160MB.",),
                  radioButtons("model_type", label = "Data source", choices = c("Excel", "R"), selected = "Excel"),
                  conditionalPanel(condition = "input.model_type == 'R'",
                                   radioButtons("model_r_type", label = "Model type", choices = c("Single run", "Network"), selected = "Network")),
                  fileInput("input_model_data", NULL,
                            multiple = TRUE,
                            accept = c("text/txt"),
                            buttonLabel = "Choose .txt files",
                            placeholder = "No files have been selected yet."),
                  div(style = 'overflow-x: scroll', tableOutput("display_input_model_data")),
                  checkboxInput("measurement_data", label = "Add measurement data"),
                  conditionalPanel(condition = "input.measurement_data == true",
                                   fileInput("input_measurement_data", NULL,
                                             multiple = FALSE,
                                             accept = c("text/txt"),
                                             buttonLabel = "Choose .txt files",
                                             placeholder = "No files have been selected yet.")),
                  conditionalPanel(condition = "input.measurement_data == true",
                  div(style = 'overflow-x: scroll', tableOutput("display_input_measurement_data"))),
                  width = 12),
              box(title = "Show data",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  
                  uiOutput("checkbox_geomlines"),
                  conditionalPanel(condition = "input.model_type == 'R'",
                                   uiOutput("checkbox_nodes")),
                  # actionButton("select_all", "Select or deselect all runs"), ## look at shiny app diepe meren for Deltares
                  p(""),
                  uiOutput("checkbox_facets"),
                  uiOutput("timeslider"),
                  actionButton("make_a_plot", "Make graphs", icon("caret-right"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  div(style = 'overflow-y: scroll', uiOutput("plotgraph")),
                  p(""),
                  downloadButton(outputId = "download_plot_pdf", label = "Export pdf"),
                  downloadButton(outputId = "download_plot_png", label = "Export png"),
                  br(),
                  br(),
                  #actionButton("make_a_table", "Make table", icon("caret-right"), 
                  #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  div(style = 'overflow-y: scroll', DT::dataTableOutput("plottable")),
                  p(""),
                  downloadButton(outputId = "download_table_csv", label = "Export csv"),
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
  input_model_files <- reactive({
    req(input$input_model_data)
    input_df <- as.data.frame(input$input_model_data)
    input_list <- split(input_df, f = input_df$name)
    return(input_list)
    })
  
  
  input_model_data <- reactive({
    # browser()
    read_datatables <- lapply(input_model_files(), fread_AddFilename)
    if(input$model_type == "Excel"){
      validate(
        need(length(which(sapply(read_datatables, function(y) "RunId" %in% colnames(y)[1]))) > 0,
             "Please check the file format. You have selected Excel as the source of your text files. However, the RunId column name is either not the header of the first column or the RunId column name is missing in all input files.")
        )
      ## The excel version of PCLake outputs the system0, system1 txt files with only 1 header somewhere. Here I check in which file this header occurs and I save those names. 
      names_all_cols <- colnames(read_datatables[[which(sapply(read_datatables, function(y) "RunId" %in% colnames(y)))]])
      ## then I bind all the dataframes together
      make_datatable <- rbindlist(read_datatables, use.names = F)
      ## and give them the headers of the dataframe that originally had them
      if(colnames(make_datatable)[1] != "RunId"){colnames(make_datatable) <- names_all_cols}
      ## often there's a column with -1 in there, a left-over from the model run, this column is clutter and can be removed
      if("-1" %in% colnames(make_datatable)){make_datatable[, which(colnames(make_datatable) == "-1"):= NULL]}
      changeCols_numeric <- intersect(colnames(make_datatable), c(cOutNamesExcel, "Time", "time"))
      make_datatable[,(changeCols_numeric):= lapply(.SD, as.numeric), .SDcols = changeCols_numeric]
      changeCols_factor <- setdiff(colnames(make_datatable), changeCols_numeric)
      make_datatable[,(changeCols_factor):= lapply(.SD, FUN = function(x) as.factor(as.character(x))), .SDcols = changeCols_factor]
    } else {
      validate(
        need(!any(sapply(read_datatables, function(y) "V1" %in% colnames(y))),
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
      if(input$model_type != "Excel" & input$model_r_type == 'Network'){
        names_to_melt <- grep("^[[:alnum:]]+N[[:digit:]]$", colnames(make_datatable), invert = T, perl = T)
        molten_df <- melt(make_datatable, colnames(make_datatable)[names_to_melt])
        molten_df$node <- gsub("^([[:alnum:]]+)(N[[:digit:]]$)", "\\2", molten_df$variable, perl = T)
        molten_df$variable <- gsub("^([[:alnum:]]+)(N[[:digit:]]$)", "\\1", molten_df$variable, perl = T)
        unmolten_df <- dcast(molten_df, filename + node + time ~ variable)
        make_datatable <- unmolten_df
      }
      changeCols_numeric <- intersect(colnames(make_datatable), c(cOutNamesR, "Time", "time")) ## first character = _
      make_datatable[,(changeCols_numeric):= lapply(.SD, as.numeric), .SDcols = changeCols_numeric]
      changeCols_factor <- setdiff(colnames(make_datatable), changeCols_numeric)
      make_datatable[,(changeCols_factor):= lapply(.SD, FUN = function(x) as.factor(as.character(x))), .SDcols = changeCols_factor]
      if("time" %in% colnames(make_datatable)){colnames(make_datatable)[which(colnames(make_datatable)=="time")] <- "Time"}
    }
     return(make_datatable)
 })
 
  output$display_input_model_data <- renderTable({ ## make output so user can see the if the data is correct
    input_model_data()[1:5]
  })
  
  
  molten_data <- reactive({
    # browser()
    if(input$model_type == "Excel"){toMelt <- setdiff(colnames(input_model_data()), intersect(colnames(input_model_data()), cOutNamesExcel))}
    if(input$model_type == "R"){toMelt <- setdiff(colnames(input_model_data()), intersect(colnames(input_model_data()), cOutNamesR))}
    totally_molten <- melt(input_model_data(), id.vars = toMelt)
    totally_molten$filename <- factor(totally_molten$filename, levels = mixedsort(levels(totally_molten$filename))) ## reorder levels according to characters AND numericals so the sorting makes sense in the plots
    return(totally_molten)
  })
  
  ## input measurement data
  input_measurement_data <- reactive({
    req(input$input_measurement_data)
    input_measurement_data <- fread(input$input_measurement_data$datapath)
  })
  
  output$display_input_measurement_data <- renderTable({ ## make output so user can see the if the data is correct
    input_measurement_data()[1:5]
  })

  molten_measurement_data <- reactive({
    toMelt <- setdiff(colnames(input_measurement_data()), intersect(colnames(input_measurement_data()), cOutNamesMeasurement))
    totally_molten <- melt(input_measurement_data(), id.vars = toMelt)
    
    morePrettyNames <-  unique(dtOutNames[!Measurement %in% "", c("Measurement", "prettyName")])
    colnames(morePrettyNames)[which(colnames(morePrettyNames) %in% c("Measurement"))] <- "variable"
    totally_molten_addNames <- totally_molten[morePrettyNames, on = 'variable', prettyName := i.prettyName]
    colnames(totally_molten_addNames)[which(colnames(totally_molten_addNames) %in% "days")] <- 'Time'
    colnames(totally_molten_addNames)[which(colnames(totally_molten_addNames) %in% "locatie")] <- 'location'
    
    return(totally_molten_addNames)
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
  
  output$checkbox_nodes <- renderUI({
    node_options <- unique(molten_data()$node)
    checkboxGroupInput("node_options", "Choose node:", node_options, selected = node_options[1], inline = TRUE)
  })
  
  output$timeslider <- renderUI({
    min_slider <- min(molten_data()$Time)
    max_slider <- max(molten_data()$Time)
    sliderInput("timeslider_true", label = "Choose timespan (days):", min_slider, max_slider, value = c(0, max_slider),
                )
    })
  
  output$checkbox_facets <- renderUI({
    if(input$model_type == "Excel"){keepIn <- intersect(colnames(input_model_data()), cOutNamesExcel)}
    if(input$model_type == "R"){keepIn <- intersect(colnames(input_model_data()), cOutNamesR)}
    
    input_model_data_adj <- input_model_data()[, ..keepIn]
    varSelectInput("variable_options", "Choose variables:", input_model_data_adj, multiple = TRUE)
  })
  
  ## make the display of the plots dependent on the "Make Graphs" button
  list_plot_input <- eventReactive(input$make_a_plot, {
    # browser()
    if(input$model_type != "Excel" & input$model_r_type == 'Network'){req(length(input$variable_options)>0, input$run_options, length(input$node_options)>0)}
    if(input$model_type == "Excel" | input$model_r_type != 'Network'){req(length(input$variable_options)>0, input$run_options)}
    
    df_variables  <- input$variable_options
    df_run        <- input$run_options
    df_timeslider <- input$timeslider_true
    if(input$model_type != "Excel" & input$model_r_type == 'Network'){df_nodes <- input$node_options}
    ifelse(input$model_type == "Excel" | input$model_r_type != 'Network', 
           list_inputs <- list(var = df_variables, run = df_run, ts = df_timeslider),
           list_inputs <- list(var = df_variables, run = df_run, ts = df_timeslider, nodes = df_nodes))
    return(list_inputs)
  })
 
  # this function defines a height of your plot
  plot_height <- function() {
    req(input$variable_options)
    amount_of_plots <- max(1, length(list_plot_input()$var)) * 200 
    return(amount_of_plots)
  }
  
  
  # this function defines your plot, which depends on input$dimension1In
  # output$contents <- renderPlot({
    
    plot_func <- function(){
  
    # browser()
    ## varSelectInput makes a 'symbol list' that you cannot just 'unlist' (wrong class type)
    ## somehow I can't get the !! option to work (from rlang) that is mentioned on the function page of varSelectInput
    ## so... I made this ugly lapply thing below to unlist the variable options input
    var_op <- unlist(lapply(list_plot_input()$var, function(x) rlang::as_name(x))) ## niet chique
    
    ## get subset
    subset_data <- molten_data()[(filename %in% list_plot_input()$run) & 
                                 (variable %in% var_op) &
                                 (Time >= list_plot_input()$ts[1]) & 
                                 (Time <= list_plot_input()$ts[2]),]
    
    if(input$model_type != "Excel" & input$model_r_type == 'Network'){
      subset_data <- subset_data[node %in% list_plot_input()$nodes, ]
    }
    
  
    if(input$model_type == "Excel"){getPrettyName <- unique(dtOutNames[, c("Excelname", "prettyName")])}
    if(input$model_type == "R"){getPrettyName <- unique(dtOutNames[, c("Rname", "prettyName")])}
    colnames(getPrettyName)[which(colnames(getPrettyName) %in% c("Rname", "Excelname"))] <- "variable"
    subset_data_addNames <- subset_data[getPrettyName, on = 'variable', prettyName := i.prettyName]
  
    ## easy fix for problem with y-axis, that I would like to start at zero, except when the error balances are included
    ifelse(any(grepl("Error", var_op)), limit_y <- NA, limit_y <- 0)  ## if Error is present, unleash the y-axis from it's zero
    
    if(input$measurement_data == TRUE){
      subset_msm_data <- molten_measurement_data()[(prettyName %in% unique(subset_data_addNames$prettyName)) &
                                                   (Time >= list_plot_input()$ts[1]) & 
                                                   (Time <= list_plot_input()$ts[2]),]
    }
    
    # dev.new() ## open a new window to display & debug a plot
      
    # plot <- ggplot(subset_data_addNames, aes(x = Time, y = value, color = filename)) +
    plot <- ggplot(subset_data_addNames, aes(x = Time, y = value)) +
      {if(input$model_type != "Excel" & input$model_r_type == 'Network') geom_line(aes(color = node, linetype = filename))} +
      {if(input$model_r_type != 'Network' | input$model_type == "Excel") geom_line(aes(color = filename))} +
      {if(input$measurement_data == TRUE) geom_point(aes(x = Time, y = value, fill = location), data = subset_msm_data, size = 3, shape = 21, stroke = 0.1)} +
      scale_x_continuous(expand = c(0, 0), minor_breaks = seq(0, max(molten_data()$Time), 365/4), breaks = seq(0, max(molten_data()$Time), 365)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(limit_y, NA)) + ## this sets the y-axis to zero (otherwise there's a small gap below the axis); maybe remove this again as for the BalanceErrors the axes should go below zero
      facet_wrap("prettyName", labeller = label_parsed, ncol = 1, scales = "free_y") +
      theme_bw() +
      {if(input$model_type != "Excel" & input$model_r_type == 'Network') scale_color_manual(values = get_colours(subset_data_addNames$node))} +
      {if(input$model_r_type != 'Network' | input$model_type == "Excel") scale_color_manual(values = get_colours(subset_data_addNames$filename))} +
      geom_text(aes(y = value * 1.1, label = "")) + ## if the scale is set to expand = c(0,0), the max value can appear to be cut of. This is a workaround to fix that.
      theme(
        #panel.grid.major.x = element_line(color = "darkgrey"),
        panel.grid.minor.x = element_line(linetype = "dashed") #, color = "lightgrey" linetype = "dashed")
      )
    
    
    return(plot)
  }

    
  output$contents <- renderPlot({
      plot_func()
  })
  
  # wrap plotOutput in renderUI
  output$plotgraph <- renderUI({
    plotOutput("contents", height = plot_height(), width = "100%")
  })
  
  ## make table
  output$plottable <- DT::renderDataTable({
     
    var_op <- unlist(lapply(list_plot_input()$var, function(x) rlang::as_name(x))) ## niet chique
    
    ## get subset
    subset_data <- molten_data()[(filename %in% list_plot_input()$run) & 
                                   (variable %in% var_op) &
                                   (Time >= list_plot_input()$ts[1]) & 
                                   (Time <= list_plot_input()$ts[2]),]
    
    if(input$model_type != "Excel" & input$model_r_type == 'Network'){
      subset_data <- subset_data[node %in% list_plot_input()$nodes, ]
    }
    
    # browser()
    if(input$model_type != "Excel" & input$model_r_type == 'Network'){subset_data2 <- datatable(dcast(subset_data, filename + Time ~ variable + node, value.var="value"),
                                                                                                options = list(lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All')),
                                                                                                               pageLength = 10))} 
    if(input$model_type == "Excel" | input$model_r_type != 'Network'){
    subset_data2 <- datatable(dcast(subset_data, filename + Time ~ variable, value.var = "value"))}
    
    return(subset_data2)
  })
  

  
  #browser()

  ## export plots
  ## https://stackoverflow.com/questions/24557541/shiny-downloadhandler-doesnt-save-png-files
  ## https://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  ## downloadHandler contains 2 arguments as functions, namely filename, content
  output$download_plot_pdf <- downloadHandler(
    filename =  function(){paste0("shinyPCmodel_export_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")},
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file, plot_height() * 0.01, width = 10) # open the png device with the right dimensions
      print(plot_func()) # draw plot
      dev.off()  # turn the device off
     },
  contentType = "application/pdf")
  
  output$download_plot_png <- downloadHandler(
    filename =  function(){paste0("shinyPCmodel_export_", format(Sys.time(), "%Y%m%d_%H%M"), ".jpg")},
    content = function(file) {
    png(file, height = plot_height(), width = 800) # open the png device with the right dimensions
    print(plot_func()) # draw plot
    dev.off()  # turn the device off
    },
    contentType = 'image/png')
  
  output$download_table_csv <- downloadHandler(
    filename =  function(){paste0("shinyPCmodel_export_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")},
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      
      ## varSelectInput makes a 'symbol list' that you cannot just 'unlist' (wrong class type)
      ## somehow I can't get the !! option to work (from rlang) that is mentioned on the function page of varSelectInput
      ## so... I made this ugly lapply thing below to unlist the variable options input
      var_op <- unlist(lapply(list_plot_input()$var, function(x) rlang::as_name(x))) ## niet chique
      
      ## get subset
      subset_data <- molten_data()[(filename %in% list_plot_input()$run) & 
                                     (variable %in% var_op) &
                                     (Time >= list_plot_input()$ts[1]) & 
                                     (Time <= list_plot_input()$ts[2]),]
      
      if(input$model_type != "Excel" & input$model_r_type == 'Network'){
        subset_data <- subset_data[node %in% list_plot_input()$nodes, ]
      }
      
      ifelse(input$model_type != "Excel" & input$model_r_type == 'Network', 
        subset_data2 <- dcast(subset_data, filename + Time ~ variable + node, value.var="value"),
        subset_data2 <- dcast(subset_data, filename + Time ~ variable, value.var="value"))

      write.csv(subset_data2, file, row.names = F)
    },
    contentType = "text/plain")
  
  
}

shinyApp(ui, server)





