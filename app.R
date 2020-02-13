# load packages -----------------------------------------------------------

library(shiny)
library(magrittr)
library(data.table)
library(elasticsearchr)
library(bs4Dash)
# library(bootstraplib)
library(stringi)
library(plotly)

# set defaults ------------------------------------------------------------

setDTthreads(0L)

elasticUrl <- "http://54.255.234.199:9200"

# bs_theme_new()
# 
# bs_theme_add_variables(
#   "body-bg" = "salmon",
#   "body-color" = "white",
#   "primary" = "#428bab",
#   "secondary" = "lime"
# )

# source files ------------------------------------------------------------

source(file = "modules/selectInputs.R")
source(file = "modules/JobData.R")

# ui ----------------------------------------------------------------------

ui <- bs4DashPage(
  old_school = FALSE,
  sidebar_min = FALSE,
  sidebar_collapsed = TRUE,
  controlbar_collapsed = TRUE,
  controlbar_overlay = FALSE,
  title = "Basic Dashboard",
  navbar = bs4DashNavbar(),
  sidebar = bs4DashSidebar(disable = TRUE),
  controlbar = bs4DashControlbar(disable = TRUE),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    # bootstrap(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectUI("selectValues") 
      ),
      mainPanel = mainPanel(
        fluidRow(
          bs4InfoBoxOutput("infoJobVacancies"),
          bs4InfoBoxOutput("infoTopCorp"),
          bs4InfoBoxOutput("infoMaxSalary")
        ),
        fluidRow(
          jobUI("jobPlot")
        )
      ))
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {
  

# Top Data Store per Session ----------------------------------------------

  mainStorage <- reactiveValues(elasticUrl = elasticUrl)

  callModule(selectServer, "selectValues", mainStorage)
  
  MainDataJob <- callModule(jobServer, "jobPlot", mainStorage)
  
  countOfJobs <- reactive({
    MainDataJob()[,sum(vacancy)]
  })
  
  MaxJobsCorporate <- reactive({
    MainDataJob()[is.na(vacancy), vacancy := 1
                  ][, .(vacancy = sum(vacancy)), 
                    companyDetails.name
                    ][order(-vacancy),
                      ][1]
  })
  
  MaxSalary <- reactive({
    MainDataJob()[,.(
      maxSalary = max(as.numeric(
        salaryDetails.maximumSalary
      ),na.rm = TRUE
      ))
      ]
  })
  
  output$infoJobVacancies <- renderbs4InfoBox({
    req(mainStorage$keyskill)
    
    bs4InfoBox(title = "Total Vacancies",
               value = countOfJobs(),
               icon = "users",
               gradientColor = "primary" 
              )
  })  
  
  output$infoTopCorp <- renderbs4InfoBox({
    req(mainStorage$keyskill)
    
    bs4InfoBox(title = "Top Corporate",
               value = MaxJobsCorporate(),
               icon = "industry",
               gradientColor = "primary" 
               )
  })

  output$infoMaxSalary <- renderbs4InfoBox({
    req(mainStorage$keyskill)
    
    bs4InfoBox(title = "Top Salary",
               value = MaxSalary(),
               icon = "money",
               gradientColor = "primary" 
               )
  })
  
# Course Query ------------------------------------------------------------

  QueryParamCourse <- reactive({
    
    validate(
      need(mainStorage$keyskills, "choose a skill")
    )
    
    queryBuild <- sprintf(
      '{
        "match" : {
            "skills" : {
              "query": "%s",
              "fuzziness": "0"
            }
        }
      }', mainStorage$keyskill
    )
    
    query(queryBuild)
  })
  
  MainDataCourse <- reactive({
    elastic(cluster_url = elasticUrl,
            index = "platform",
            doc_type = "") %search%
      QueryParamCourse() %>% 
      data.table()
  })

  output$courseData <- renderDataTable({
    req(mainStorage$keyskill)
    MainDataCourse()
  })

}

# run App -----------------------------------------------------------------
shinyApp(ui, server)
