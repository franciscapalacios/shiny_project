source("shiny_functions.R")
library(shinyjs)

linebreaks <- function(n){HTML(strrep(br(), n))}

shinyApp(
  ui = tagList(
    navbarPage("Attrition Clustering",
      tabPanel("Data Overview",
              
               mainPanel(
                 tabsetPanel(
                   tabPanel("About",
                            fluidRow(
                              column(12, align = "justify", 
                                     h3("Understanding Employee Attrition"),
                                     p("Employee attrition, or turnover, is a critical challenge faced by organizations across industries. It is vital to comprehend the underlying patterns and factors contributing to attrition in order to develop effective strategies for employee retention and engagement."),
                                     br(),
                                     h3("Going through the app"),
                                     h5("Exploring the Dataset"),
                                     p("The app allows you to explore the dataset and gain a comprehensive understanding of the variables and characteristics it encompasses. You can examine employee demographics, job-related attributes, and various factors that may influence attrition. This exploration phase helps you become familiar with the dataset and choose relevant variables for the clustering analysis."),
                                     br(),
                                     h5("Identifying Attrition Clusters"),
                                     p("Once you have explored the dataset, the app guides you through the process of identifying attrition clusters using silhouette plots. Silhouette plots provide a visual representation of the quality and distinctness of each cluster, aiding in the determination of an optimal number of clusters. You can analyze the silhouette scores and select the number of clusters that reveals meaningful patterns and concentrated attrition."),
                                     br(),
                                     h5("Unveiling Insights and Decision-Making"),
                                     p("By uncovering distinct clusters of attrition, this app assists in discovering groups of employees who exhibit similar attrition tendencies. Analyzing these clusters can yield valuable insights into the underlying factors that contribute to attrition within specific employee groups. These insights can inform targeted retention strategies, organizational policies, and initiatives to address attrition and enhance employee satisfaction and engagement."),
                                     br(),
                                     h5("References"),
                                     uiOutput("ref1.link"),
                                     uiOutput("ref2.link"),
                                     uiOutput("ref3.link"),
                                     uiOutput("ref4.link"),
                                     br()
                                     
                              ))
                   ),
                 
                  tabPanel("Overview",
                            #h4("Table"),
                            linebreaks(1),
                            plotOutput("att.perc"),
                            tableOutput("table"),
                            verbatimTextOutput("nulls")
                   ),
                   
                   tabPanel("Correlations",
                            linebreaks(1),
                            fluidPage(
                              useShinyjs(),
                              selectInput(inputId = "corr_plots",
                                          label = "Choose correlation plot:",
                                          choices = c("Multicollinearity Plot",
                                                      "Correlation Funnel")),
                              hidden(plotOutput("funnel",
                                                height=800, width = 800)),
                              hidden(plotOutput("corr",
                                                height=800, width = 800))
                            )
                
                   ),
                   tabPanel("Explore",
                            fluidPage(
                              useShinyjs(),
                              linebreaks(1),
                              selectInput(inputId = "explore_plots",
                                          label = "Choose variables:",
                                          choices = setdiff(names(df), "Attrition")),
                              hidden(plotOutput("continuos_plot",
                                                height=500)),
                              hidden(plotOutput("factor_plot",
                                                height=500))
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
               uiOutput("sil.link"),
                 sliderInput("slider", "Top correlated features to use:", 2, 25, 10),
                 tableOutput("att.table"),
               verbatimTextOutput("txtout")
               )
      ),
      
      tabPanel("Cluster Results", 
               mainPanel(
                 tabsetPanel(
                   tabPanel("Review",
                            linebreaks(1),
                            plotOutput("k.sil.plot"),
                            uiOutput("sil2.link")
                            ),
                   
                   tabPanel("Explore", 
                            fluidPage(
                              useShinyjs(),
                              linebreaks(1),
                              selectInput(inputId = "explore_clusters",
                                          label = "Choose variables:",
                                          choices = setdiff(names(df), "Attrition")),
                              hidden(plotOutput("continuos_plot2",
                                                height=500)),
                              hidden(plotOutput("factor_plot2",
                                                height=560))
                            )
                            )
                   ))
               )
      )
  ),
  
  
  
  server = function(input, output) {
    
    gower.dist <- reactive({
      import.gower.dist(input$slider, corr.tbl)
    })
    
    sil.tbl <- reactive({
      import.sil.tbl(gower.dist())
    })
    
    cluster.fit <- reactive({
      import.cluster(sil.tbl(), gower.dist())
    })
    
    attr.tbl <- reactive({
      import.best.k(cluster.fit())
    })
    
    output$txtout <- renderText({
      max_row <- sapply(str_extract_all(attr.tbl()$Cluster_Turnover_Rate, '\\d+([.]\\d+)?'), function(x) max(as.numeric(x)))
      cluster <- as.character(attr.tbl()[which.max(max_row),][1,1])
      cluster_turnover <- as.character(attr.tbl()[which.max(max_row),][1,2])
      total_turnover <- as.character(attr.tbl()[which.max(max_row),][1,5])
      paste("When choosing the", input$slider, "most correlated features, we get", dim(attr.tbl())[1], "clusters. \nCluster", cluster, 
      "has a turnover rate of", cluster_turnover, "which accounts for", total_turnover, "of the total attrition.")
    })
    
    output$table <- renderTable({
      desc.df
    })
    
    output$nulls <- renderText({
      paste("Number of null values:", import.null.count(df))
    })
    
    output$corr <- renderPlot({
      corrplot(corr.mat, type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 90)
    })
    
    output$att.perc <- renderPlot({
      df %>% 
        group_by(Attrition) %>% 
        summarise(Percentage=n()/nrow(.)) %>% 
        ggplot(aes(x="", y=Percentage, fill=Attrition)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        geom_text(aes(x = 1.1, label = scales::percent(Percentage, accuracy = .1)), 
                  position = position_stack(vjust = .5), color = "white") +
        theme_void()
    })
    
    output$funnel <- renderPlot({
      corr.tbl  %>%
        plot_correlation_funnel() 
    })
    
    output$sil.plot <- renderPlot({
      ggplot(sil.tbl(), 
             aes(x = k, y = sil_width)) +
        geom_point(size = 2) +
        geom_line() +
        ylab("Silhouette Width") +
        scale_x_continuous(breaks = 2:10) +
        theme_minimal() +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14))
    })
    
    output$att.table <- renderTable({
      attr.tbl()
    })
    
    observeEvent(input$corr_plots,{
      if (input$corr_plots == "Multicollinearity Plot"){
        hide('funnel')
        show('corr')
      }else{
        hide('corr')
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
      continous.plot2(df %>%
                        mutate(cluster = cluster.fit()$clustering), 
                      selectedData2())
    })
    output$factor_plot2 <- renderPlot({
      factor.plot2(df %>%
                     mutate(cluster = cluster.fit()$clustering), 
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
    
    output$k.sil.plot <- renderPlot({
      sil = silhouette (cluster.fit()$clustering, gower.dist()) 
      fviz_silhouette(sil)
    })
    
    url <- a("What's a silhouette plot?", href="https://towardsdatascience.com/silhouette-method-better-than-elbow-method-to-find-optimal-clusters-378d62ff6891", 
             style = "font-size:11px")
    output$sil.link <- renderUI({
      tagList(url)
    })
    
    url2 <- a("What's a silhouette plot?", href="https://towardsdatascience.com/silhouette-method-better-than-elbow-method-to-find-optimal-clusters-378d62ff6891", 
             style = "font-size:11px")
    output$sil2.link <- renderUI({
      tagList(url2)
    })
    
    ref1 <- a("A People Analytics Tutorial on Unsupervised Machine Learning", href="https://www.linkedin.com/pulse/people-analytics-tutorial-unsupervised-machine-r-mckinnon-phd-/", 
              style = "font-size:11px")
    output$ref1.link <- renderUI({
      tagList(ref1)
    })
    
    ref2 <- a("Hierarchical Clustering on Categorical Data in R", href="https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995", 
              style = "font-size:11px")
    output$ref2.link <- renderUI({
      tagList(ref2)
    })
    
    ref3 <- a("CLUSTERING EXAMPLE: 4 STEPS YOU SHOULD KNOW", href="https://www.datanovia.com/en/blog/clustering-example-4-steps-you-should-know/", 
              style = "font-size:11px")
    output$ref3.link <- renderUI({
      tagList(ref3)
    })
    
    ref4 <- a("How to Use t-SNE Effectively", href="https://distill.pub/2016/misread-tsne/", 
              style = "font-size:11px")
    output$ref4.link <- renderUI({
      tagList(ref4)
    })
    
  }
)






