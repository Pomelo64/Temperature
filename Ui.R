
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#------ dropdownbuton func
# https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
        
        status <- match.arg(status)
        # dropdown button content
        html_ul <- list(
                class = "dropdown-menu",
                style = if (!is.null(width)) 
                        paste0("width: ", validateCssUnit(width), ";"),
                lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
        )
        # dropdown button apparence
        html_button <- list(
                class = paste0("btn btn-", status," dropdown-toggle"),
                type = "button", 
                `data-toggle` = "dropdown"
        )
        html_button <- c(html_button, list(label))
        html_button <- c(html_button, list(tags$span(class = "caret")))
        # final result
        tags$div(
                class = "dropdown",
                do.call(tags$button, html_button),
                do.call(tags$ul, html_ul),
                tags$script(
                        "$('.dropdown-menu').click(function(e) {
                        e.stopPropagation();
});")
  )
        }

#-----------------
# for data preparation that may be used in selectInput
# for deploying on shinyapps.io
data <- read.csv("all.csv")

cities <- unique(data$city)
cities <- c("Global",levels(cities))

#-----------------

shinyUI(
        fluidPage(
                
                titlePanel("SciMagViz Applet"),
                
                fluidRow(
                        column(2,wellPanel(
                                helpText("Data Filtering"),
                                # Checkbox with several selection for filtering the data
                                dropdownButton(
                                        label = "Select Cities", status = "default", width = "100%",
                                        tags$div(style='overflow-y: scroll; height: 200px;',
                                                 checkboxGroupInput(inputId = "selected_cities",
                                                                    label = "Choose",
                                                                    choices = cities,
                                                                    selected = "Global"))
                                        
                                ),
                                selectInput(inputId = "period",
                                            label = "Periods of Moving Average",
                                            choices = 1:100, 
                                            selected = 10
                                ),
                                
                                
                                
                                actionButton("plot_button","Plot")
                                
                                
                        )
                        ),
                        
                        
                        column(10,wellPanel(
                                
                                tabsetPanel(
                                        tabPanel("Data View",
                                                 
                                                 #plotOutput("temp_plot")
                                                 
                                                 plotOutput("temp_plot",width = "800px",height = "600px",
                                                            dblclick = "temp_plot_dblclick",
                                                            brush = brushOpts(
                                                                    id = "temp_plot_brush",
                                                                    resetOnNew = TRUE
                                                            )), 
                                                 downloadButton('download_plot', 'Download the Plot'),
                                                 tags$h3("PCA data table"),
                                                 DT::dataTableOutput("brush_info_pca", width = "800px")
                                        ),
                                        tabPanel("Correlations" 
                                                 #plotOutput("correlation_plot", width = "800px",height = "600px")
                                        ),
                                        tabPanel("Help",
                                                 tags$iframe(src = "SciMagVizHelp.html", style="height:600px; width:100%")
                                                 
                                        )
                                )
                                
                                
                        )) # End mainPanel
                        
                ) # End sidebar layout
        ) # end fluidPage
) # end shiny