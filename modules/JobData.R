jobUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("jobData"),
                 width = "100%"),
    plotlyOutput(ns("locations"),
                 width = "100%")
  )
  
}

jobServer <- function(input,
                      output,
                      session,
                      mainStorage){

  # define ns ---------------------------------------------------------------

  ns <- session$ns  
  
  # job Query ---------------------------------------------------------------
  
  QueryParamJob <- reactive({
    
    validate(
      need(mainStorage$keyskill, "choose a skill")
    )
    
    queryBuild <- sprintf(
      '{
        "match" : {
              "keySkills.other.label" : {
              "query": "%s",
              "fuzziness": "0"
              }
        }
      }', mainStorage$keyskill
    )
    
    query(queryBuild)
    
  })
  
  MainDataJob <- reactive({
    elastic(cluster_url = elasticUrl,
            index = "jobid",
            doc_type = "") %search%
      QueryParamJob() %>% 
      data.table()
  })
  
  output$locations <- renderPlotly({
    MainDataJob()[, .(locations = 
                        lapply(locations, function(x){
                          x$label
                        }) %>% 
                        unlist()
    )] %>% 
      plot_ly(x = ~locations) %>% 
      add_histogram() %>% 
      layout(
        xaxis = list(
          title = "Locations"
        ),
        yaxis = list(
          title = paste0("Jobs Posting ", mainStorage$keyskill)
        ),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      ) %>% 
      config(
        displayModeBar = FALSE
      )
  })
  
  output$jobData <- renderPlotly({
    
    req(mainStorage$keyskill)
    
    MainDataJob()[order(salaryDetails.maximumSalary)] %>% 
      plot_ly(y = ~salaryDetails.maximumSalary,
              x = ~companyDetails.name) %>% 
      add_bars() %>% 
      layout(
        xaxis = list(
          title = "Company Names"
        ),
        yaxis = list(
          title = "maximum Salary"
        ),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      ) %>% 
      config(
        displayModeBar = FALSE
      )
  })
  
  return(MainDataJob)

}