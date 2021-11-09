#
# This is a Shiny app to take the size and sex of interest and output
# candidates within our database who fit that criteria

library(shiny)
library(tidyverse)
library(vroom)
library(DT)

#Conversion from a US size to a range of lengths


# Define UI for application that draws a histogram
ui <- fluidPage(
    fileInput("file", NULL, accept = c(".csv")),
    sliderInput("MinLen", label = h3("Select min foot length"), min = 21, 
                max = 30, value = 25),
    sliderInput("MaxLen", label = h3("Select max foot length"), min = 21, 
                max = 30, value = 26),
    DT::dataTableOutput("mytable"),
    mainPanel(
        plotOutput("SummaryPlot", width = "100%", height = "400px")
    )
)
# Define server logic 
server <- function(input, output) {

    data <- reactive({
        req(input$file)
        
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               csv = vroom::vroom(input$file$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
    

    df_subset <- reactive({
        a <- subset(data(), Length >= input$MinLen & Length <= input$MaxLen)
        return(a)
    })
    
    output$mytable = DT::renderDataTable({
         df_subset()
     })
    
    output$SummaryPlot <- renderPlot({
        ggplot(data = df_subset(), mapping=aes(x = Instep, color = Sex, fill = Sex)) +
            geom_density() + facet_wrap(~Sex)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
