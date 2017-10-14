
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(dplyr)
require(ggplot2)
require(TTR)
library(PerformanceAnalytics)
library(DT)

# reading the data
data <- read.csv("all.csv")
global <- read.csv("World.csv")
global$city <- "Global"
global$country <- "Global"
data <- rbind(global,data)

shinyServer(function(input, output) {
        
        # ------- Filtering the main dataset 
        dataset <- eventReactive(input$plot_button,{
                
                subset <- data %>% 
                        filter(year>=1900) %>%
                        filter(city %in% input$selected_cities)
                
                #print(head(subset))
                return(subset)
             }
        )
        
        # ---- adding simple moving average 
        
        dataset_sma <- reactive({

                dataset_sma <- dataset() %>% 
                        group_by(city) %>%
                        mutate(moving_avg = SMA(avg_temp, n = as.numeric(input$period)))
                
                
                return(dataset_sma)
                        
        })
        
        #--------- Visualization 
        
        temp_plot_func <- reactive({
                
                dataset <- as.data.frame(dataset_sma())
                
                #print(head(dataset))
                
                g<- ggplot(data = dataset) + 
                        geom_line(aes(x = year , y = avg_temp, color = city ),
                                  alpha = 0.3) + 
                        geom_line(aes(x = year, y = moving_avg, color = city)) + 
                        theme_linedraw() + 
                        coord_cartesian(xlim = brush_ranges$x,
                                        ylim = brush_ranges$y,
                                        expand = TRUE) 
                
                #ggsave(filename = "kos.png", plot = g , device = "png")
                
                return(g)
        })
        
        output$temp_plot <- renderPlot({
                temp_plot_func()
        })
        
        brush_ranges <- reactiveValues(x = NULL, y = NULL)
        
        observeEvent(input$temp_plot_dblclick, {
                brush <- input$temp_plot_brush
                if (!is.null(brush)) {
                        brush_ranges$x <- c(brush$xmin, brush$xmax)
                        brush_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        brush_ranges$x <- NULL
                        brush_ranges$y <- NULL
                }
        })

        
        # ------ Table for data view
        output$brush_info <- DT::renderDataTable({
                
                data <-as.data.frame(dataset_sma())
                
                res <- brushedPoints(data,input$temp_plot_brush)
                datatable(res)
                
        })
        
        
        
        #------ download handler
        output$download_plot <- downloadHandler(
                filename = "temperature_plot.png",
                content = function(file) {
                        g<- temp_plot_func()
                        ggsave(file, g ,device = "png", dpi = 450)
                        
                }
        )
      
        
        
        
       
        
      
        
        
        output$download_plot <- downloadHandler(
                filename = "SciMap_plot.png",
                content = function(file) {
                        g<- switch(EXPR = input$dim_reduct_method,
                                   "PCA" = biplot_func(),
                                   "MDS" = mds_plot_func()
                        )
                        ggsave(file, g ,device = "png", dpi = 450)
                        
                }
        )
        # ------ Category-Variable plot
        category_variable_plot_func <- reactive({
                category_plot_df <- sciMag.data %>%
                        group_by(Categories) %>%
                        summarize(mean(SJR),  mean(`H index`), mean(`Total Docs. (2016)`), mean(`Total Docs. (3years)`), mean(`Total Refs.`), mean(`Total Cites (3years)`), mean(`Citable Docs. (3years)`),mean(`Cites / Doc. (2years)`), mean(`Ref. / Doc.`) ) 
                
                category_plot_df <- data.frame(category_plot_df)
                choices <- paste0("mean(`", c("SJR","H index","Total Docs. (2016)", "Total Docs. (3years)","Total Refs.","Total Cites (3years)","Citable Docs. (3years)","Cites / Doc. (2years)","Ref. / Doc." ), "`)")
                colnames(category_plot_df) <- c("Categories", choices)
                
                var <- paste0("mean(`",input$category_variable,"`)")
                
                var_index <- match(var ,colnames(category_plot_df) )
                
                category_plot_df %>% data.frame() %>%
                        ggplot(aes(x = reorder(Categories,category_plot_df[,var_index]), y = category_plot_df[,var_index])) +
                        geom_bar(stat = "identity") +
                        coord_flip() + 
                        ylab(colnames(category_plot_df)[var_index]) + 
                        xlab("Category")
                
        })
        
        output$category_variable_plot <- renderPlot({
                category_variable_plot_func()
                
        })
        
        output$download_category_plot <- downloadHandler(
                filename = "category_plot.png",
                content = function(file) {
                        
                        ggsave(file, category_variable_plot_func() ,device = "png", dpi = 450)
                        
                }
        )
        
        
}) # end of shiny()