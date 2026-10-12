library(shiny)

ui <- fluidPage(
  titlePanel("🎓 Student Performance Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("hours", "Study Hours:",
                   5, min = 0, max = 15),
      numericInput("attendance", "Attendance (%):",
                   80, min = 0, max = 100),
      actionButton("predict", "Predict Marks")
    ),
    
    mainPanel(
      h3("Result"),
      textOutput("result"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  data <- data.frame(
    hours = c(1,2,3,4,5,6,7,8),
    attendance = c(60,65,70,75,80,85,90,95),
    marks = c(40,45,52,58,65,72,82,90)
  )
  
  model <- lm(marks ~ hours + attendance,
              data = data)
  
  prediction <- eventReactive(input$predict, {
    predict(model,
            data.frame(
              hours = input$hours,
              attendance = input$attendance
            ))
  })
  
  output$result <- renderText({
    paste("Predicted Marks:",
          round(prediction(), 2))
  })
  
  output$plot <- renderPlot({
    plot(data$hours, data$marks,
         type = "o",
         pch = 19,
         xlab = "Study Hours",
         ylab = "Marks",
         main = "Student Performance")
  })
}

shinyApp(ui, server)