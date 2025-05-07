
server <- function(input, output) {
  
  # Put in date of date data download
  last_updated <- "01/05/2025"
  
  # For scatter plot: only "Yes" consent
  filtered_scatter_data <- reactive({
    cpf_data %>%
      filter(
        Consent == "Yes",
        `Consent Year` >= input$year_select[1],
        `Consent Year` <= input$year_select[2],
        `Consent Month` %in% input$month_select
      ) %>%
      group_by(`Consent Year`, `Consent Month`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(`Consent Month` = factor(`Consent Month`, levels = month.abb))
  })
  
  # Scatter plot for Consented Patients per Month
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_scatter_data(), aes(
      x = `Consent Month`,
      y = Count,
      group = `Consent Year`,
      color = factor(`Consent Year`),
      text = paste("Month:", `Consent Month`, "<br>Year:", `Consent Year`, "<br>Count:", Count)
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = "Consented Patients per Month",
        x = "Month",
        y = "Number of Patients",
        color = "Year"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Consent box plot
  output$consentBoxplot <- renderPlotly({
    ggplotly(consent_boxplot)
  })
  
  # Consent bar chart
  output$consentBreakdown <- renderPlotly({
    ggplotly(total_consent_breakdown_p, tooltip = "text")
  })
  
  # GMA breakdown bar chart
  output$gmaBreakdown <- renderPlotly({
    ggplotly(gma_breakdown_p, tooltip = "text")
  })
  
  # Timeline HRCP/CP
  output$timelinePlot <- renderPlotly({
    ggplotly(hrcpcp_timeline, tooltip = "text")
  })
  
  # Last updated
  output$lastUpdated <- renderText({
    paste("Last Updated: ", last_updated)
  })
}

ui <- dashboardPage(
  dashboardHeader(
    title = "CPF Study Data Dashboard",
    # Add last updated information with an icon to the header
    tags$li(class = "dropdown",
            style = "padding: 8px 15px; color: white; font-size: 16px;",
            icon("calendar"), # Icon (you can use any other icon, like "clock")
            textOutput("lastUpdated")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Consent by Month", tabName = "consentMonth", icon = icon("line-chart")),
      menuItem("Time to Consent", tabName = "overview", icon = icon("dashboard")),
      menuItem("Consent Breakdown", tabName = "consentBreakdown", icon = icon("bar-chart")),
      menuItem("GMA", tabName = "gma", icon = icon("video")),
      menuItem("HRCP to CP Timeline", tabName = "timeline", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "consentMonth",
              fluidRow(
                box(
                  title = "Filter",
                  status = "info",
                  solidHeader = TRUE,
                  width = 3,
                  # Year and Month Select Inputs
                  sliderInput("year_select", "Select Year(s):", 
                              min = min(cpf_data$`Consent Year`, na.rm = TRUE), 
                              max = max(cpf_data$`Consent Year`, na.rm = TRUE), 
                              value = c(min(cpf_data$`Consent Year`, na.rm = TRUE), 
                                        max(cpf_data$`Consent Year`, na.rm = TRUE)), 
                              step = 1,
                              sep = ""),
                  
                  selectInput("month_select", "Select Month(s):",
                              choices = month.abb,
                              selected = month.abb,
                              multiple = TRUE)
                ),                
                box(
                  title = "Consented Patients per Month",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("scatterPlot")
                )
                
              ),
              
      ),
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Consent Boxplot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("consentBoxplot")
                )
              )
      ),
      tabItem(tabName = "consentBreakdown",
              fluidRow(
                box(
                  title = "Consent Breakdown",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("consentBreakdown")
                )
              )
      ),
      tabItem(tabName = "gma",
              fluidRow(
                box(
                  title = "GMA",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("gmaBreakdown")
                )
              )
      ),
      tabItem(tabName = "timeline",
              fluidRow(
                box(
                  title = "Timeline: HRCP to CP Diagnosis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("timelinePlot")
                )
              )
      )
    )
  )
)

shinyApp(ui, server)
