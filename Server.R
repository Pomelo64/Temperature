
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
                        theme_linedraw()
                
                #ggsave(filename = "kos.png", plot = g , device = "png")
                
                return(g)
        })
        
        output$temp_plot <- renderPlot({
                temp_plot_func()
        })
        
        # ------ Table for data view
        
        # for printing the filtered sciMag.data.filtered in the sciMag.data.filteredView tabset 
        output$table <- DT::renderDataTable({
                data<- dataset()
                datatable(data)
        })
        
        
        
       
        
        #------- PCA Biplot function
        
        pca_brush_ranges <- reactiveValues(x = NULL, y = NULL)
        
        biplot_func <- reactive({
                coordinates<- biplot_list()[[1]]
                vectors <- biplot_list()[[2]]
                
                g<- ggplot(data = coordinates) + 
                        geom_point(aes(x = PC1,
                                       y = PC2),
                                   size = input$point_size,
                                   alpha = input$point_alpha) + 
                        geom_segment(data=vectors, 
                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                     col="red") + 
                        geom_text_repel(data=vectors ,
                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                        col="red" 
                        )
                
                # shape yes 
                if (input$point_shape != "None"){ 
                        
                        #shape yes - color yes 
                        if (input$color_variable != "None" ) {
                                
                                
                                
                                shape_index <- match(input$point_shape,colnames(coordinates))
                                coordinates$shape <- coordinates[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(coordinates))
                                
                                coordinates$color <- coordinates[,color_index]
                                
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       shape = shape,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) +
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                                # shape yes- color no        
                        } else {
                                
                                index <- match(input$point_shape,colnames(coordinates))
                                coordinates$shape <- coordinates[,index]
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       shape = shape),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = coordinates,
                                                        aes(x = PC1 , y = PC2),
                                                        label = dataset$Title , 
                                                        color = "orange", alpha = 0.4 )
                        }
                        
                        # shape no 
                } else {
                        #shape no - color yes 
                        if (input$color_variable != "None" ) {
                                
                                
                                
                                #shape_index <- match(input$point_shape,colnames(coordinates))
                                #coordinates$shape <- coordinates[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(coordinates))
                                coordinates$color <- coordinates[,color_index]
                                
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) +
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                                # shape no- color no        
                        } else {
                                
                                #index <- match(input$point_shape,colnames(coordinates))
                                #coordinates$shape <- coordinates[,index]
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = coordinates,
                                                        aes(x = PC1 , y = PC2,label = dataset$Title ),
                                                        color = "orange", alpha = 0.8 )
                        }
                }
                
                g <- g + 
                        coord_cartesian(xlim = pca_brush_ranges$x,
                                        ylim = pca_brush_ranges$y,
                                        expand = TRUE) +
                        theme_linedraw(base_size = 16)
                
                
                
                return(g)
                
        })
        
        observeEvent(input$plot_dblclick, {
                brush <- input$plot_brush
                if (!is.null(brush)) {
                        pca_brush_ranges$x <- c(brush$xmin, brush$xmax)
                        pca_brush_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        pca_brush_ranges$x <- NULL
                        pca_brush_ranges$y <- NULL
                }
        })
        
        # for the DT table. it uses brush info
        output$brush_info_pca <- DT::renderDataTable({
                
                data <- biplot_list()[[1]]
                
                res <- brushedPoints(data,input$plot_brush)
                datatable(res)
                
        })
        
        
        
        
        
        ## MDS Part
        # ------- MDS coordinates dataset        
        # for MDS version of the plot 
        scimag_mds <- reactive({
                
                #numeric variables
                mds_dataset <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                mds_dataset <- scale(mds_dataset)
                mds_dist <- dist(mds_dataset)
                
                mds_model <- smacofSym(delta = mds_dist, ndim = 2 , type = "ratio")
                
                #the badnes-of-fit
                stress <- mds_model$stress
                
                coordinates <- dataset() %>% 
                        cbind(mds_model$conf) 
                
        })
        
        
        
        
        #------- MDS plot function
        mds_brush_ranges <- reactiveValues(x = NULL, y = NULL)
        
        mds_plot_func <- reactive({
                
                mds_dataset <- scimag_mds()
                
                
                g<- ggplot(data = mds_dataset) + 
                        geom_point(aes(x = D1,
                                       y = D2),
                                   size = input$point_size,
                                   alpha = input$point_alpha) 
                
                
                if (input$point_shape != "None"){ #yes shape 
                        
                        if (input$color_variable != "None" ) { #yes shape - yes color
                                
                                
                                
                                shape_index <- match(input$point_shape,colnames(mds_dataset))
                                mds_dataset$shape <- mds_dataset[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(mds_dataset))
                                
                                mds_dataset$color <- mds_dataset[,color_index]
                                
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       shape = shape,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) 
                                
                                
                                
                                
                        } else { #yes shape - no color
                                
                                index <- match(input$point_shape,colnames(mds_dataset))
                                mds_dataset$shape <- mds_dataset[,index]
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       shape = shape),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha)
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = mds_dataset,
                                                        aes(x = D1 , y = D2),
                                                        label = dataset$Title , 
                                                        color = "orange", size = 1, alpha = 0.4 )
                        }
                        
                        
                } else { #no shape 
                        
                        if (input$color_variable != "None" ) { #no shape - yes color
                                
                                
                                
                                #shape_index <- match(input$point_shape,colnames(mds_dataset))
                                #mds_dataset$shape <- mds_dataset[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(mds_dataset))
                                
                                mds_dataset$color <- mds_dataset[,color_index]
                                
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) 
                                
                                
                                
                                
                        } else { #no shape no color
                                
                                #index <- match(input$point_shape,colnames(mds_dataset))
                                #mds_dataset$shape <- mds_dataset[,index]
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2
                                        ),
                                        size = input$point_size,
                                        alpha = input$point_alpha)
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = mds_dataset,
                                                        aes(x = D1 , y = D2),
                                                        label = dataset$Title , 
                                                        color = "orange", size = 1, alpha = 0.4 )
                        }
                        
                }
                
                g <- g   + 
                        coord_cartesian(xlim = mds_brush_ranges$x,
                                        ylim = mds_brush_ranges$y,
                                        expand = TRUE) +
                        theme_linedraw()
                
                return(g)
                
                
        })
        
        #for brushing and zooming as well as DT
        observeEvent(input$plot_dblclick, {
                brush <- input$plot_brush
                if (!is.null(brush)) {
                        mds_brush_ranges$x <- c(brush$xmin, brush$xmax)
                        mds_brush_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        mds_brush_ranges$x <- NULL
                        mds_brush_ranges$y <- NULL
                }
        })
        
        #for DT output, it uses brush data
        output$brush_info <- DT::renderDataTable({
                data <- scimag_mds()
                
                res <- brushedPoints(data, input$plot_brush)
                datatable(res)
        })
        
        # -------- plot generation  
        
        output$sciMag_plot <- renderPlot({
                
                switch(EXPR = input$dim_reduct_method,
                       "PCA" = biplot_func(),
                       "MDS" = mds_plot_func()
                )
                
        })
        
        # ------ color UI 
        output$color_variable_select <- renderUI({
                selectInput(inputId = "color_variable",
                            label = "Point Color reflects:",
                            choices = c("None",input$selected_variable,"region","open.access","SJR Quartile")
                )
        })
        
        
        
        # ------- Correlation 
        
        output$correlation_plot <- renderPlot({
                corr_data <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                chart.Correlation(corr_data, histogram=TRUE, pch=19) 
        })
        
        # ----- download graph 
        
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