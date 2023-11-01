# Install and load necessary packages
# install.packages("shiny")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)

df<-read.csv('aw_fb_data.csv')

ui <- fluidPage(
  titlePanel("Cumulative Steps Ribbon Plot"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x1_range", "Select X1 Range", min = 0, max = 76, value = c(0, 76))
    ),
    mainPanel(
      plotOutput("ribbon_plot", click = "plot_click"),
      verbatimTextOutput("note_display")
    )
  )
)

server <- function(input, output, session) {
  # Initialize note data
  note_data <- reactiveVal(data.frame(x = numeric(0), y = numeric(0), note = character(0)))
  
  # Handle adding notes
  observeEvent(input$plot_click, {
    x <- input$plot_click$x
    y <- input$plot_click$y
    showModal(modalDialog(
      title = "Add a Note",
      textAreaInput("note_text", "Note:"),
      footer = tagList(
        actionButton("save_note", "Save"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$save_note, {
    note <- input$note_text
    if (note != "") {
      notes <- note_data()
      x_scaled <- input$plot_click$x * (input$x1_range[2] - input$x1_range[1])
      # Set a minimum limit for y-coordinate to prevent -Inf
      #y_scaled <- max(0, input$plot_click$y) * (max(df$Cumulative_Steps))
      filtered_df <- df[df$X1 >= input$x1_range[1] & df$X1 <= input$x1_range[2] & df$X <= 76, ]
      filtered_df$Cumulative_Steps <- cumsum(filtered_df$steps)
      y_scaled <- input$plot_click$y * max(filtered_df$Cumulative_Steps)
      new_note <- data.frame(x = x_scaled, y = y_scaled, note = note)
      notes <- rbind(notes, new_note)
      note_data(notes)
    }
    removeModal()
  })
  
  output$ribbon_plot <- renderPlot({
    filtered_df <- df[df$X1 >= input$x1_range[1] & df$X1 <= input$x1_range[2] & df$X <= 76, ]
    filtered_df$Cumulative_Steps <- cumsum(filtered_df$steps)
    
    p <- ggplot(filtered_df, aes(x = X1, y = Cumulative_Steps)) +
      geom_ribbon(aes(ymin = 0, ymax = Cumulative_Steps), fill = "lightblue") +
      geom_line() +
      labs(x = "X1", y = "Cumulative Steps") +
      theme_minimal()
    
    # Add notes to the plot at the scaled click location
    for (i in 1:nrow(note_data())) {
      p <- p + annotate(
        "text",
        x = note_data()$x[i],
        y = note_data()$y[i],
        label = note_data()$note[i],
        hjust = 0,
        vjust = 1
      )
    }
    
    print(p)
  })
  
  output$note_display <- renderPrint({
    paste("Notes:", paste(paste("X:", note_data()$x, "Y:", note_data()$y, "Note:", note_data()$note, sep = " "), collapse = "\n"))
  })
}

shinyApp(ui, server)

