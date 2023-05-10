source("shiny_functions.R")
library(shinyjs)

shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Attrition Clustering",
      tabPanel("Data Overview",
              
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overview",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("nulls"),
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
                   tabPanel("Explore", "This panel is intentionally left blank")
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
                   
                   tabPanel("two", "left blank")
                   ))
               )
      )
  ),
  
  
  
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$varType, input$slider/100, sep = ", ")
    })
    output$table <- renderTable({
      head(df, 4)
    })
    output$nulls <- renderText({
      names(df)
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
    
  }
)






