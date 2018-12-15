# Initializing all packages

library(shinydashboard)
library(dplyr)
library(bupaR)
library(shiny)
library(processmapR)
library(processmonitR)
library(DT)
library(DiagrammeR)
library(shinyalert)

OTC <- patients %>% select(handling,patient,employee,handling_id,registration_type,time) %>% 
       mutate(Key_Event = paste(handling,patient,employee,handling_id,registration_type,time))

names(OTC) <- c("Event","Case_ID","Resource","Event_ID","Status","Timestamp","Order","Key_Event")

OTC$Resource <- as.character(OTC$Resource)

OTC$Resource[OTC$Resource == "r1"] <- "Katherine"     # Renaming the resources
OTC$Resource[OTC$Resource == "r2"] <- "Jonathan"
OTC$Resource[OTC$Resource == "r3"] <- "Andrea"
OTC$Resource[OTC$Resource == "r4"] <- "Steven"
OTC$Resource[OTC$Resource == "r5"] <- "Julia"
OTC$Resource[OTC$Resource == "r6"] <- "Amy"
OTC$Resource[OTC$Resource == "r7"] <- "Jerry"

OTC <- unique.data.frame(OTC)

# UI side design

ui <- dashboardPage(skin = "blue", 
                    
                    dashboardHeader(title = "Process Mining", 
                                    
                                    dropdownMenu(type = "notifications", badgeStatus = "danger",
                                                 
                                                 notificationItem(text = "12 new users today",
                                                                  icon("users")),
                                                 
                                                 notificationItem(text = "31 items delivered",
                                                                  icon("truck"),
                                                                  status = "success"),
                                                 
                                                 notificationItem(text = "Server load at 34%",
                                                                  icon("exclamation-triangle"),
                                                                  status = "warning")
                                                 ),
                                    
                                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                                 
                                                 taskItem(value = 90, color = "green","Documentation"),
                                                 
                                                 taskItem(value = 17, color = "aqua","Omega BI implementation"),
                                                 
                                                 taskItem(value = 78, color = "yellow","Server deployment"),
                                                 
                                                 taskItem(value = 84, color = "red", "Project progress")
                                                 
                                                 )
                                    ),
                    
                    dashboardSidebar(
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                       
                                              selectInput("R_ID", h5(strong(em("RESOURCES"))), 
                                                   
                                                          choices = c("All", unique(as.character(OTC$Resource))),
                                                   
                                                          selected = "All", multiple = TRUE))
                               )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("EVENT", h5(strong(em("EVENTS"))),
                                                          
                                                          choices = c("All", unique(as.character(OTC$Event))),
                                                          
                                                          selected = "All", multiple = TRUE))
                               )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("LEVEL", h5(strong(em("LEVEL OF ANALYSIS"))),
                                                          
                                                          choices = unique(c("activity","log","trace","case","resource","resource-activity")),
                                                          
                                                          selected = "activity", multiple = FALSE))
                               )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("UNITS", h5(strong(em("UNIT OF TIME"))),
                                                          
                                                          choices = unique(c("days","hours","mins")),
                                                          
                                                          selected = "days", multiple = FALSE))
                               )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("TYPE", h5(strong(em("TRACE TYPE"))),
                                                          
                                                          choices = unique(c("frequent","infrequent")),
                                                          
                                                          selected = "infrequent", multiple = FALSE))
                               )),
                      
                      fluidRow(column(12, div(style = "height:100px, padding:1px",
                                              
                                              dateRangeInput("TIME", h5(strong(em("DATE RANGE"))), 
                                                             
                                                             start = min(substr(OTC$Timestamp,0,10)),
                                                             
                                                             min = min(substr(OTC$Timestamp,0,10)),
                                                             
                                                             max = max(substr(OTC$Timestamp,0,10)),
                                                             
                                                             end = max(substr(OTC$Timestamp,0,10))))
                               )),
                      
                      fluidRow(column(12, offset = 2, useShinyalert(),
                                      
                                      actionButton(inputId = "PLOT", label = "PLOT", width = "40%", height = "40%",
                                                   
                                      style = "color: #fff; background-color: #337ab7;border-color: #2e6da4")
                               
                               )),
                      
                      fluidRow(column(12, div(style = "height:200px")
                      
                               )),

                      fluidRow(column(12, div(strong(em("This app has been designed by Shiivong Birla, a Data Science
                                                                            Postgraduate at Monash University, Melbourne.")))))
                      
                        
                    ),
                    
                    dashboardBody( 
                      
                      tags$head(tags$style(HTML('
                                                .main-header .logo {
                                                font-family: "Georgia", Times,
                                                "Times New Roman",
                                                font-weight: bold;
                                                font-size: 24px;
                                                font-style: italic;
                                                }
                                                '))),
                      
                      fluidRow(
                        
                        tabBox(height = "1100px", width = "1000px",
                               
                               tabPanel(title = tagList(icon("project-diagram", class = "fas fa-project-diagram")
                                                        
                                                        , "PROCESS SUMMARY"),
                                        
                                        box(grVizOutput("Pr_map"), status = "primary", solidHeader = TRUE,
                                            
                                            title = "PROCESS MAP", width = 8, height = 612, collapsible = TRUE),
                                        
                                        box(tableOutput("n_activities"), status = "primary",
                                            
                                            title = "# of Activities", width = 3, height = 85, collapsible = TRUE),
                                        
                                        box(tableOutput("n_activity_instances"), status = "primary",
                                            
                                            title = "# of Activity Instances", width = 3, height = 85,
                                            
                                            collapsible = TRUE),
                                        
                                        box(tableOutput("n_cases"), status = "primary",
                                            
                                            title = "# of Cases", width = 3, height = 85, collapsible = TRUE),
                                        
                                        box(tableOutput("n_events"), status = "primary",
                                            
                                            title = "# of Events", width = 3, height = 85, collapsible = TRUE),
                                        
                                        box(tableOutput("n_resources"), status = "primary",
                                            
                                            title = "# of Resources", width = 3, height = 85, collapsible = TRUE),
                                        
                                        box(tableOutput("n_traces"), status = "primary",
                                            
                                            title = "# of Traces", width = 3, height = 85, collapsible = TRUE)
                               ),
                               
                              
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"), "TABLE OUTPUT"),
                                        
                                        DT::dataTableOutput("table")
                               ),
                               
                               tabPanel(title = tagList(icon("user", class = "fas fa-users"), "RESOURCE ANALYSIS"),
                                        
                                        box(title = "PRECEDENCE MATRIX",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("prcn_Matr")),
                                        
                                        box(title = "RESOURCE FREQUENCY",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("resource_frequency")),
                                        
                                        box(grVizOutput("resource_map"), status = "primary", solidHeader = TRUE,
                                            
                                            title = "RESOURCE MAP", collapsible = TRUE),
                                        
                                        box(title = "RESOURCE SPECIALIZATION",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("resource_specialization"))
                               ),
                               
                               tabPanel(title = tagList(icon("clock", class = "fas fa-clock"), "TIME ANALYSIS"),
                                        
                                        box(title = "PROCESSING TIME",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("process_time"), width = 97),
                                        
                                        box(title = "IDLE TIME",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("idle_time")),
                                        
                                        box(title = "THROUGHPUT TIME",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("throughput_time"))
                               ),
                               
                               tabPanel(title = tagList(icon("sort-by-attributes-alt", lib = "glyphicon"), 
                                                        "ACTIVITY ANALYSIS"),
                                        
                                        box(title = "START ACTIVITIES", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("start_activity")),
                                        
                                        box(title = "END ACTIVITIES", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("end_activity")),
                                        
                                        box(title = "ACTIVITY FREQUENCY", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("activity_frequency")),
                                        
                                        box(title = "ACTIVITY PRESENCE", status = "primary", solidHeader = TRUE,
                                            collapsible = TRUE, plotOutput("activity_presence")
                                        )
                               ),
                               
                               tabPanel(title = tagList(icon("trace", class = "fas fa-map-marked-alt"), 
                                                        "TRACE ANALYSIS"),
                                        
                                        box(title = "TRACE EXPLORER", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("trace_explorer"), width = 97),
                                        
                                        box(title = "TRACE COVERAGE", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("trace_coverage")),
                                        
                                        box(title = "TRACE LENGTH", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("trace_length")
                                        )
                               )
                               
                               )
                      )
                    )) 

# Server side design

server <- function(input, output) {
  
  output$mytabs <- renderUI({
    
    nTabs = input$nTabs
    
    myTabs = lappy(paste("Tab", 1:nTabs), tabPanel)
    
    do.call(tabsetPanel, myTabs)
  
    })
  
  observeEvent(input$PLOT, {
    
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())
    
    progress$set(message = "Computing plot values", value = 0)
    
    n <- 10
    
    for (i in 1:n) {
      
      progress$inc(1/n, detail = paste(i*10,"% completed"))
      
      Sys.sleep(0.5)
      
    } 
    
    progress$set(message = "Plot values computed successfully!")
    
    
    Resource_filtered <- input$R_ID
    
    Time <- input$TIME
    
    Event_filtered <- input$EVENT
    
    Activity_frequency_level <- input$LEVEL
    
    Activity_unit_level <- input$UNITS
    
    Trace_type <- input$TYPE
    
    OTC_Filtered <- OTC
    
    if(Resource_filtered != "All") {
      
      OTC_Filtered <- OTC %>% filter(Resource == Resource_filtered)
      
    }
    
    if(Event_filtered != "All") {
      
      OTC_Filtered <- OTC %>% filter(Event == Event_filtered)
      
    }
    
    OTC_Filtered <- as.data.frame(OTC_Filtered)
    
    O2C <- bupaR::eventlog(eventlog = OTC_Filtered,
                           case_id = "Case_ID",
                           activity_id = "Event",
                           activity_instance_id = "Key_Event",
                           timestamp = "Timestamp",
                           lifecycle_id = "Status",
                           resource_id = "Resource",
                           order = "auto")
    
    O2C <- O2C %>% filter_time_period(interval = Time)
    
    if(nrow(O2C) == 0) {
      
      output$act_frq <- renderPlot(plot.new())
      output$Pr_map <- renderGrViz(plot.new())
      output$prcn_Matr <- renderPlot(plot.new())
      output$table <- renderDataTable(plot.new())
      
      shinyalert("Data not sufficient. Make some other selection!", type = "warning")
      
    }
    
    else {
      
      
      output$table <- renderDataTable({ O2C[1:6] })
      
      output$activity_frequency <- renderPlot(
        
        if(nrow(O2C) == 0) {
          
          plot.new()
          
        }
        
        else {
          
          validate(
            
            need(Activity_frequency_level == c("log", "trace","case","activity"), "Please select LEVEL OF ANALYSIS as either log, trace, case or activity!")
            
          )
          
          O2C %>% activity_frequency(level = Activity_frequency_level) %>% plot()
          
        }
        
      )
      
      output$Pr_map <- renderGrViz({
        
        O2C %>% process_map(rankdir = "TB", fixed_edge_width = FALSE, render = TRUE)
        
      })
      
      output$prcn_Matr <- renderPlot({
        
        O2C %>% precedence_matrix() %>% plot()
      
      })
      
      output$n_activities <- renderText({
        
        O2C %>% n_activities()
        
      })
      
      output$n_activity_instances <- renderText({
        
        O2C %>% n_activity_instances()
      
      })
      
      output$n_cases <- renderText({
        
        O2C %>% n_cases()
        
      })
      
      output$n_events <- renderText({
        
        O2C %>% n_events()
        
      })
      
      output$n_resources <- renderText({
        
        O2C %>% n_resources()
        
      })
      
      output$n_traces <- renderText({
        
        O2C %>% n_traces()
        
      })
      
      output$idle_time <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("resource", "log", "case"), "Please select LEVEL OF ANALYSIS as resource, log or case!")
        
          )
        
        O2C %>% idle_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot()
        
      })
      
      output$process_time <- renderPlot({
        
        patients$employee <- as.character(patients$employee)
        
        patients$employee[patients$employee == "r1"] <- "Katherine"     # Renaming resources again
        patients$employee[patients$employee == "r2"] <- "Jonathan"
        patients$employee[patients$employee == "r3"] <- "Andrea"
        patients$employee[patients$employee == "r4"] <- "Steven"
        patients$employee[patients$employee == "r5"] <- "Julia"
        patients$employee[patients$employee == "r6"] <- "Amy"
        patients$employee[patients$employee == "r7"] <- "Jerry"
        
        patients %>% processing_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot() 
        
      })
      
      output$throughput_time <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("log","case"), "Please select LEVEL OF ANALYSIS as either log or case!")
          
        )
        
        O2C %>% group_by(resource_id = Resource) %>% throughput_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot()
        
      })
      
     output$resource_frequency <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("log","resource","case","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either log, resource, resource-activity, activity or case!")
         
       )
       
       O2C %>% resource_frequency(level = Activity_frequency_level) %>% plot()
       
     })
     
     output$resource_map <- renderGrViz({
       
       O2C %>% resource_map()
       
     })
     
     output$resource_specialization <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("resource","activity","log"), "Please select LEVEL OF ANALYSIS as either resource, log or activity!")
         
       )
       
       O2C %>% resource_specialisation(level = Activity_frequency_level) %>% plot()
       
     })
     
     output$start_activity <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("resource","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either resource, resource-activity, or activity!")
         
       )
       
       O2C %>% start_activities(level = Activity_frequency_level) %>% plot()
       
     })
     
     output$end_activity <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("resource","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either resource, resource-activity, or activity!")
         
       )
       
       O2C %>% end_activities(level = Activity_frequency_level) %>% plot()
       
     })
     
     output$activity_presence <- renderPlot({
       
       O2C %>% activity_presence() %>% plot()
       
     })
     
     output$trace_explorer <- renderPlot({
       
       O2C %>% trace_explorer(type = Trace_type) %>% plot()
       
     })
     
     output$trace_coverage <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("trace","log","case"), "Please select LEVEL OF ANALYSIS as either log, trace or case!")
         
       )
       
       O2C %>% trace_coverage(level = Activity_frequency_level) %>% plot()
       
     })
     
     output$trace_length <- renderPlot({
       
       validate(
         
         need(Activity_frequency_level == c("case","log","trace"), "Please select LEVEL OF ANALYSIS as either log, trace or case!")
         
       )
       
       O2C %>% trace_length(level = Activity_frequency_level) %>% plot()
       
     })
     
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
