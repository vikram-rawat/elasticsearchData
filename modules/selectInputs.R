

# ui ----------------------------------------------------------------------

selectUI <- function(id) {
  # define ns ---------------------------------------------------------------
  ns <- NS(id)
  
  tagList(uiOutput(ns("selectCourse")),
          uiOutput(ns("selectSkills")))
}

# server ------------------------------------------------------------------

selectServer <- function(input,
                         output,
                         session,
                         mainStorage) {
  # define ns ---------------------------------------------------------------
  
  ns <- session$ns
  
  # observe Events -----------------------------------------------------------
  
  observeEvent({
    input$courseTitle
    input$keyskills
    },{
      mainStorage$courseTitle <- input$courseTitle
      mainStorage$keyskill <- input$keyskills
    }
  )
  
  # select input ------------------------------------------------------------
  
  SelectData <- reactive({
    elastic(
      cluster_url = mainStorage$elasticUrl,
      index = "platform",
      doc_type = ""
    ) %search%
      (query('{
                "match_all": {}
              }') +
         select_fields('{
                  "includes": [
                        "skills",
                        "title"
                  ]
                }')) %>%
      data.table()
  })
  
  output$selectCourse <- renderUI({
    selectInput(
      inputId = ns("courseTitle"),
      label = "Select the Course",
      choices = SelectData()$title
    )
  })
  
  output$selectSkills <- renderUI({
    req(input$courseTitle)
    
    data <- SelectData()[title == input$courseTitle,]
    
    selectInput(
      inputId = ns("keyskills"),
      label = "Select the skills",
      choices = data$skills %>%
        unlist()
    )
  })
  
}