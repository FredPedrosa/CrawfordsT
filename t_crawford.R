library(shiny)
library(dplyr)
library(psycho)
library(ggplot2)

# Function to interpret the results
interpret_results <- function(t_test, bayes_test) {
  interpretation <- paste(
    "<b>Interpretation of Crawford's t-test:</b><br>",
    t_test$text, "<br>",
    "<b>Interpretation of the Bayesian test:</b><br>",
    bayes_test$text, "<br>",
    "<br><br>",
    "<b>Citation:</b><br>",
    "Pedrosa, F. G. (2025). Estimation of Crawford's t-test and Bayesian test. [Software]. https://vmsfue-frederico-pedrosa.shinyapps.io/t_crawford<br>",
    "<b>Using:</b><br>",
    "Crawford, J. R., & Garthwaite, P. H. (2007). Comparison of a single case to a control or normative sample in neuropsychology: development of a Bayesian approach. Cognitive neuropsychology, 24(4), 343–372. https://doi.org/10.1080/02643290701290146<br>",
    "Crawford, J. R., & Howell, D. C. (1998). Comparing an individual's test score against norms derived from small samples. Clinical Neuropsychologist, 12(4), 482–486. https://doi.org/10.1076/clin.12.4.482.7241<br>",
    "Makowski, D. (2018). The Psycho Package: An Efficient and Publishing-Oriented Workflow for Psychological Science. Journal of Open Source Software, 3(22), 470. Available from https://github.com/neuropsychology/psycho.R<br>",
    "Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York."
  )
  
  return(HTML(interpretation))
}

# UI
ui <- fluidPage(
  titlePanel("Estimation of Crawford's t-test and Bayesian test"),
  sidebarLayout(
    sidebarPanel(
      numericInput("patient_score", "Patient's Score:", value = 0),
      radioButtons("input_type", "Input Type:", choices = c("Mean and Standard Deviation", "Individual Scores")),
      conditionalPanel(
        condition = "input.input_type == 'Mean and Standard Deviation'",
        numericInput("mean_controls", "Mean of Controls:", value = 0),
        numericInput("sd_controls", "Standard Deviation of Controls:", value = 0),
        numericInput("n_controls", "Number of Controls:", value = 0)
      ),
      conditionalPanel(
        condition = "input.input_type == 'Individual Scores'",
        textAreaInput("scores", "Control Scores (comma-separated):", value = "")
      ),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      plotOutput("densityPlot"),
      htmlOutput("interpretation")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$calculate, {
    patient <- input$patient_score
    
    if (input$input_type == "Mean and Standard Deviation") {
      mean_controls <- input$mean_controls
      sd_controls <- input$sd_controls
      n_controls <- input$n_controls
      
      # Verificar se os valores não estão ausentes
      if (!is.na(mean_controls) && !is.na(sd_controls) && !is.na(n_controls)) {
        set.seed(123)  # Fixar a semente para resultados consistentes
        scores <- rnorm(n_controls, mean = mean_controls, sd = sd_controls)  # Generate fictitious scores for visualization
      } else {
        scores <- NULL
      }
    } else {
      scores <- as.numeric(unlist(strsplit(input$scores, ",")))
      mean_controls <- mean(scores, na.rm = TRUE)
      sd_controls <- sd(scores, na.rm = TRUE)
      n_controls <- length(scores)
    }
    
    if (!is.null(scores) && length(scores) > 0) {
      t_test <- crawford.test.freq(patient = patient, controls = scores)
      bayes_test <- crawford.test(patient = patient, mean = mean_controls, sd = sd_controls, n = n_controls)
      
      output$densityPlot <- renderPlot({
        lines_data <- data.frame(
          x = c(patient, mean_controls),
          y = c(0, 0),
          label = c("Patient score", "Control mean score"),
          linetype = c("dotted", "dashed"),
          color = c("blue", "black")
        )
        
        ggplot() +
          geom_density(aes(x = scores), fill = "lightblue", alpha = 0.7) +
          geom_vline(data = lines_data, aes(xintercept = x, color = label, linetype = label), size = 1) +
          scale_color_manual(values = c("Patient score" = "blue", "Control mean score" = "black")) +
          scale_linetype_manual(values = c("Patient score" = "dotted", "Control mean score" = "dashed")) +
          #annotate("text", x = mean_controls + 5, y = 0.01, label = paste("t =", round(t_test$summary$t, 3), "\np =", format(round(t_test$summary$p, 3), nsmall = 3)), color = "black") +
          labs(title = "Crawford's t-test", x = "Scores", y = "Density", color = "Legend", linetype = "Legend") +
          theme_classic() +
          theme(legend.key = element_blank(),  # Remove the point in the legend
                legend.position = "right",  # Position the legend to the right
                legend.title = element_text(size = 12, face = "bold"),  # Adjust the legend title
                legend.text = element_text(size = 10),  # Adjust the legend text
                axis.line = element_line(linetype = "solid"),  # Solid axis line
                panel.grid.major = element_line(linetype = "dashed", size = 0.5, color = "grey80"))  # Adjust the size of the dashed line segments
      })
      
      output$interpretation <- renderUI({
        interpret_results(t_test, bayes_test)
      })
    } else {
      output$interpretation <- renderUI({
        HTML("Please provide valid control scores or parameters.")
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)