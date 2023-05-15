source("shiny_functions.R")
library(shinyjs)

shinyApp(
  ui = tagList(
    navbarPage("Attrition Clustering",
      tabPanel("Data Overview",
              
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overview",
                            h4("Table"),
                            tableOutput("table"),
                            verbatimTextOutput("nulls")
                   ),
                   
                   tabPanel("Correlations",
                            fluidPage(
                              useShinyjs(),
                              selectInput(inputId = "corr_plots",
                                          label = "Choose correlation plot:",
                                          choices = c("Multicollinearity Plot",
                                                      "Correlation Funnel")),
                              hidden(plotOutput("funnel")),
                              hidden(plotOutput("plot1"))
                            )
                
                   ),
                   tabPanel("Explore",
                            fluidPage(
                              useShinyjs(),
                              selectInput(inputId = "explore_plots",
                                          label = "Choose variables:",
                                          choices = setdiff(names(df), "Attrition")),
                              hidden(plotOutput("continuos_plot")),
                              hidden(plotOutput("factor_plot"))
                            )
                            )
                 )
               )
      ),
      
      tabPanel("Cluster Tuning", 
               
               fluidPage(
               plotOutput("sil.plot",
                                    width = "100%"
               ),
                 sliderInput("slider", "Top correlated features to use:", 2, 25, 10),
                 tableOutput("att.table")
               )
      ),
      
      tabPanel("Cluster Results", 
               mainPanel(
                 tabsetPanel(
                   tabPanel("one", "left blank"),
                   
                   tabPanel("two", 
                            fluidPage(
                              useShinyjs(),
                              selectInput(inputId = "explore_clusters",
                                          label = "Choose variables:",
                                          choices = setdiff(names(df), "Attrition")),
                              hidden(plotOutput("continuos_plot2")),
                              hidden(plotOutput("factor_plot2"))
                            )
                            )
                   ))
               )
      )
  ),
  
  
  
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$varType, input$slider/100, sep = ", ")
    })
    output$table <- renderTable({
      head(df)
    })
    output$nulls <- renderText({
      paste("Number of null values:", import.null.count(df))
    })
    output$plot1 <- renderPlot({
      corrplot(df.cor, type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 90)
    })
    output$funnel <- renderPlot({
      corr.tbl  %>%
        plot_correlation_funnel() 
    })
    output$sil.plot <- renderPlot({
      ggplot(import.sil.tbl(import.gower.dist(input$slider, corr.tbl)), 
             aes(x = k, y = sil_width)) +
        geom_point(size = 2) +
        geom_line() +
        scale_x_continuous(breaks = 2:10)
    })
    output$att.table <- renderTable({
      import.best.k(import.sil.tbl(import.gower.dist(input$slider, corr.tbl)),
                    import.gower.dist(input$slider, corr.tbl))
    })
    
    observeEvent(input$corr_plots,{
      if (input$corr_plots == "Multicollinearity Plot"){
        hide('funnel')
        show('plot1')
      }else{
        hide('plot1')
        show('funnel')
      }
    })
    
    selectedData <- reactive({
      input$explore_plots
    })
    
    output$continuos_plot <- renderPlot({
      continous.plot(selectedData())
    })
    
    output$factor_plot <- renderPlot({
      factor.plot(selectedData())
    })
    
    observeEvent(input$explore_plots,{
      if (all(class(df[, input$explore_plots]) == 'integer')){
        hide('factor_plot')
        show('continuos_plot')
      }else{
        hide('continuos_plot')
        show('factor_plot')
      }
    })
    
    
    selectedData2 <- reactive({
      input$explore_clusters
    })
    
    output$continuos_plot2 <- renderPlot({
      continous.plot2(import.cluster(import.sil.tbl(import.gower.dist(input$slider, corr.tbl)),
                                     import.gower.dist(input$slider, corr.tbl)), 
                      selectedData2())
    })
    
    output$factor_plot2 <- renderPlot({
      factor.plot2(import.cluster(import.sil.tbl(import.gower.dist(input$slider, corr.tbl)),
                                  import.gower.dist(input$slider, corr.tbl)), 
                   selectedData2())
    })
    
    observeEvent(input$explore_clusters,{
      if (all(class(df[, input$explore_clusters]) == 'integer')){
        hide('factor_plot2')
        show('continuos_plot2')
      }else{
        hide('continuos_plot2')
        show('factor_plot2')
      }
    })
    
  }
)






