library(shiny)


numInputRow<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,...))
}

shinyUI(fluidPage(
  # headerPanel("Plot for GenePix"),
  titlePanel(h1("COVID on campus")),
  
  fluidRow(
    column(
      4,
      
      wellPanel(
        
        fileInput("choose_source", "Choose your own file",
                  multiple = F,
                  accept = c(".xlsx")),
        
        # uiOutput("choose_columns")
        # selectInput("choose_columns")
        
        # bootstrapPage(
        #   numInputRow(inputId="xlimitsmin", label="x-min", value = 0.0, class="input-mini"),
        #   numInputRow(inputId="xlimitsmax", label="x-max", value = 0.5, class="input-mini")
        # ),
        # 
        # 
        # bootstrapPage(width = 12, title = "A Box in a Fluid Row I want to Split", 
        #     splitLayout(
        #       numericInput("num1", label = h4("N1"), value = 1),
        #       numericInput("num2", label = h4("N2"), value = 2)
        #     )
        # )
        
        tags$form(
          class="form-horizontal",
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period1", br(), "Period 1"),
            column(width = 4, numericInput(inputId = "pos1", label = "Positive", value = 0)),
            column(width = 4, numericInput(inputId = "test1", label = "Tested", value = 300))
          ),
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period2", "Period 2"),
            column(width = 4, numericInput(inputId = "pos2", label = NULL, value = 46)),
            column(width = 4, numericInput(inputId = "test2", label = NULL, value = 300))
          ),
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period3", "Period 3"),
            column(width = 4, numericInput(inputId = "pos3", label = NULL, value = 58)),
            column(width = 4, numericInput(inputId = "test3", label = NULL, value = 300))
          ),
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period4", "Period 4"),
            column(width = 4, numericInput(inputId = "pos4", label = NULL, value = 64)),
            column(width = 4, numericInput(inputId = "test4", label = NULL, value = 300))
          ),
          
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period5", "Period 5"),
            column(width = 4, numericInput(inputId = "pos5", label = NULL, value = 64)),
            column(width = 4, numericInput(inputId = "test5", label = NULL, value = 300))
          ),
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period5", "Period 5"),
            column(width = 4, numericInput(inputId = "pos6", label = NULL, value = 67)),
            column(width = 4, numericInput(inputId = "test6", label = NULL, value = 300))
          ),
          
          tags$div(
            class="form-group",
            tags$label(class = "col-sm-4 control-label", `for` = "Period5", "Period 5"),
            column(width = 4, numericInput(inputId = "pos7", label = NULL, value = 70)),
            column(width = 4, numericInput(inputId = "test7", label = NULL, value = 300))
          )
        )
        
      )
      # br(),
    ),
    column(8,
           tabsetPanel(
             
             tabPanel("Case Curve",
                      
                      h2("Number of cases"), plotOutput("plot1", height = "720px")
                      
                      
             ),
             
             tabPanel("Percent Curve",
                      
                      h2("Percent cases"), plotOutput("plot2", height = "720px")
                      
                      
             )
             
           )
           
    )
  )
  
))


