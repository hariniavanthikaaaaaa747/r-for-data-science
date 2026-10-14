library(shiny); library(ggplot2)
d <- read.csv("data/students_cleaned.csv")
ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(selectInput("section", "Section", c("All", sort(unique(d$section))))),
    mainPanel(plotOutput("scatter"), tableOutput("summary"))
  )
)
server <- function(input, output, session) {
  sel <- reactive(if (input$section == "All") d else d[d$section == input$section, ])
  output$scatter <- renderPlot(ggplot(sel(), aes(study_hours, average)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x))
  output$summary <- renderTable(data.frame(Students = nrow(sel()), Mean_Average = round(mean(sel()$average), 1)))
}
shinyApp(ui, server)
