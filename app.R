library("shiny")
library("shinyvalidate")
library("corrplot")
library("ggplot2")
library("dplyr")

source("fit_distribution.R")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  fluidRow(
    column(3,
           fileInput("file", "Upload CSV file of target data",
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           numericInput("n_rows", 
                        "Number of synthetic data rows to generate (max. 100,000):",
                        value = 100, min = 1, max = 100000),
           selectInput("fit_method", "Distribution best fit method:",
                       c("AIC" = "aic",
                         "BIC" = "bic",
                         "Loglikelihood" = "loglik")
           )
    ),
    column(6,
           h4("Target data sample"),
           dataTableOutput("contents")
    ),
    column(3,
           h4("Download synthetic data"),
           downloadButton("download", "Download")
    )
  ),
  fluidRow(
    column(12, 
           h3("Correlations", align="center")
    )
  ),
  fluidRow(
    column(4,
           h4("Target", align="center"),
           plotOutput(outputId = "corr_plot_target")
    ),
    column(4,
           h4("Synthetic", align="center"),
           plotOutput(outputId = "corr_plot_synth")
    ),
    column(4,
           h4("Difference", align="center"),
           plotOutput(outputId = "corr_plot_diff")
    )
  ),
  fluidRow(
    column(12, 
           h3("Density Plots", align="center")
    )
  ),
  fluidRow(
    column(12,
           plotOutput(outputId = "density")
    )
  ),
  fluidRow(
    column(12, 
           h3("Categorical Percentage", align="center")
    )
  ),
  fluidRow(
    column(12,
           plotOutput(outputId = "percent")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # validate inputs
  iv <- InputValidator$new()
  iv$add_rule("file1", sv_required())
  iv$add_rule("n_rows", sv_required())
  iv$add_rule("n_rows", sv_integer())
  iv$add_rule("n_rows", sv_between(1, 100000))
  iv$enable()
  
  dist_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, sep = ",")
  })
  
  dist_corr <- reactive({
    req(dist_data())
    cor(chars_to_ints(dist_data()))
  })
  
  synth_data <- reactive({
    req(dist_data())
    fit_distribution(dist_data(), input$fit_method, input$n_rows, debug_mode=FALSE)
  })
  
  synth_corr <- reactive({
    req(synth_data()) 
    cor(chars_to_ints(synth_data()))
  })
  
  output$contents <- renderDataTable(head(dist_data()),
                                     options = list(
                                       rownames = FALSE,
                                       scrollX = TRUE,
                                       paging = FALSE,
                                       searching = FALSE,
                                       info = FALSE
                                     )
  )
  
  output$corr_plot_target <- renderPlot({
    req(dist_corr())
    corrplot(dist_corr(), method='color')
  })
  
  output$corr_plot_synth <- renderPlot({
    req(synth_corr())
    corrplot(synth_corr(), method='color')
  })
  
  output$corr_plot_diff <- renderPlot({
    req(dist_corr(), synth_corr())
    corrplot(synth_corr()-dist_corr(), method='color')
  })
  
  output$density <- renderPlot({
    req(dist_data(), synth_data())
    
    df1_cont <- select_if(dist_data(), is.numeric)
    df2_cont <- select_if(synth_data(), is.numeric)
    
    # Combine the two datasets into one
    df_combined <- bind_rows(df1_cont, df2_cont, .id = "dataset")
    
    # Reshape the data into long format for easier plotting
    df_long <- pivot_longer(df_combined, cols = -c(dataset), names_to = "variable", values_to = "value")
    
    #Categorize to Real and Synthetic by creating a new row
    df_long <- df_long %>% 
      mutate(dataset = ifelse(dataset == 1, "Real", "Synthetic"))
    
    # Plot the density plots grouped by variable name
    ggplot(df_long, aes(x = value, fill = dataset)) +
      geom_density(alpha = 0.25) +
      facet_wrap(~ variable, scales = "free") +
      labs(x = "Value", y = "Density") +
      theme_void() + 
      theme(legend.position = "bottom")
  })
  
  output$percent <- renderPlot({
    req(dist_data(), synth_data())
    
    #select discrete variables
    df1_cat <- select_if(dist_data(), is.character)
    df2_cat <- select_if(synth_data(), is.character)
    
    #combine two datasets into one
    df_combined <- bind_rows(df1_cat, df2_cat, .id = "dataset")
    
    # Reshape the data into long format for easier plotting
    df_long <- pivot_longer(df_combined, cols = -c(dataset), names_to = "variable", values_to = "value")
    
    #Categorize to Real and Synthetic by creating a new row
    df_long <- df_long %>% 
      mutate(dataset = ifelse(dataset == 1, "Real", "Synthetic"))
    
    #Group Real dataset and calculate the percentage
    df_long_real <- df_long %>%
      group_by(value, dataset, variable) %>%
      filter(dataset == "Real") %>%
      summarise(n = n(),  .groups = "drop") %>%
      mutate(percentage = n / sum(n) * 100)
    
    #Group Synthetic dataset and calculate the percentage
    df_long_syn <- df_long %>%
      group_by(value, dataset, variable) %>%
      filter(dataset == "Synthetic") %>%
      summarise(n = n(),  .groups = "drop") %>%
      mutate(percentage = n / sum(n) * 100)
    
    #Combine two datasets into one
    df_long <- rbind(df_long_real, df_long_syn)
    
    #Plot Discrete dataset grouped by variable name
    ggplot(df_long, aes(x = value, y = percentage, group = dataset, color = dataset)) +
      geom_point() + geom_line() +
      facet_wrap(~ variable, scales = "free") +
      labs(x = "Value", y = "Percentage") +
      theme_void() + 
      theme(legend.position = "bottom")
  })
  
  
  # Downloadable csv of dataset
  output$download <- downloadHandler(
    filename = function(){"synthetic_data.csv"}, 
    content = function(fname){
      write.csv(synth_data(), fname, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
