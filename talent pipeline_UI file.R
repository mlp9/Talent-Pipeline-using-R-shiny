library(shiny)
library(plotly)
library(dplyr)
library(data.table)
source('jayhanuman3pavitraconditional.R')
library(shinydashboard)
library(tidyverse)
#library(DT)
library(hrbrthemes)

ui <- dashboardPage( skin = "blue",
  header<-dashboardHeader(title='CEEI TALENT PIPELINE PLATFORM', titleWidth = 450),
  
  sidebar <- dashboardSidebar(
                
    sidebarMenu(width=350,
                
        menuItem("CEEI", icon = icon("send",lib='glyphicon'), 
               href = "http://www.theceei.com"),
      
    fileInput("file","Choose csv:",multiple="false"),
    selectInput("type","select type",choices = c("","Compare with Employee","compare with benchmarks")),
    conditionalPanel(
      condition = "input.type=='Compare with Employee'",
             # numericInput('begin','Competency  Beginning Index', value = ''),
             # numericInput('end','Competency Ending Index', value = ''),
             # uiOutput("subselect1"),
             # uiOutput("subselect2"),
      selectInput("weighttype","Do you want to add weight to competencies?",choices = c("","Yes","NO")),
      conditionalPanel(condition = "input.weighttype=='Yes'",
            textInput("competencyweight","Enter Competency Weights(comma seperated)",value = "" )),
                       
      
             numericInput('empid','Enter Employee ID', value = ''),
             uiOutput('menuitems'),
             uiOutput('fun'),
             numericInput('head1','No of Candidates', value = '5')),
  #conditionalPanel(condition = "input.type==''"),
  conditionalPanel(condition = "input.type=='compare with benchmarks'",
                   # numericInput('begins','Competency  Beginning Index', value = ''),
                   # numericInput('ends','Competency Ending Index', value = ''),
                   # uiOutput("subselect3"),
                   # uiOutput("subselect4"),
                   textInput("competencyvalue","Enter Competency values(comma seperated)",value = "" ),
                   uiOutput('menuitems2'),
                   uiOutput('fun1'),
                   numericInput('head2','No of Candidates', value = '5')
                   )),
  selectInput("chart","select plot type",choices = c("","Lollipop Plot","Radar chart"))
),
dashboardBody(tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
  ),
  h1("Welcome to CEEI Talent Pipleline Platform"),
  conditionalPanel(condition = "input.type=='Compare with Employee'",
                   div(style="display:inline-block",uiOutput("subselect1")),

                   div(style="display:inline-block",uiOutput("subselect2")),
                   
                   h3("Top matching Candidates"),
                   dataTableOutput("dist1"),
                   div(style = 'overflow-x: scroll',dataTableOutput("inter_dist1"))
  ),

  conditionalPanel(condition = "input.type=='compare with benchmarks'",
                   div(style="display:inline-block",uiOutput("subselect3")),
                   
                   div(style="display:inline-block",uiOutput("subselect4")),
                   dataTableOutput("benchmark_dist1"),
                   plotlyOutput("barplot_benchmark")
  ),
  conditionalPanel(condition = "input.chart=='Lollipop Plot'",
                   
                   plotlyOutput("barplot_emp")
  ),
  conditionalPanel(condition = "input.chart=='Radar chart'",
                   
                   plotlyOutput("radar")
  )
   )
)

#################################SERVER PART#######################################
server <- function(input, output) { 
  
  text_ip<-reactive({
    a<-as.data.frame(as.numeric(unlist(strsplit(input$competencyvalue,","))))
    a
  })
  file11 <- reactive({
    inFile <- input$file
   stren <- read.csv(inFile$datapath)}
   )
  
  output$subselect1 <- renderUI({
    stren=file11()
    #x <- read.csv(input$file$datapath)
    selectInput("subid1","Select competencies:",choices=names(stren[,1:ncol(stren)]),multiple = T, width = "500px") 
  })
  output$subselect2 <- renderUI({
    stren=file11()
    #x <- read.csv(input$file$datapath)
    selectInput("subid2","Select Sub-competencies:",choices=names(stren[,1:ncol(stren)])[!names(stren[,1:ncol(stren)]) %in% input$subid1],multiple = T, width = "500px") 
  })
  
  output$menuitems <- renderUI({
    #x=file11()
    x<-level(input$file$datapath)
    selectInput('level',choices = x,label ='Select Employee  Level')
  })
  output$menuitems2 <- renderUI({
    #x=file11()
    x2<-level(input$file$datapath)
    selectInput('level2',choices = x2,label ='Select Employee  Level')
  })

  output$fun <- renderUI({
    #x=file11()
    f<-fun(input$file$datapath)
    selectInput('fun',choices = f,label ='Select Employee  function')
  })

  output$subselect3 <- renderUI({
    stren=file11()
    #x <- read.csv(input$file$datapath)
    selectInput("subids1","Select competencies:",choices=names(stren[,1:ncol(stren)]),multiple = T, width = "500px") 
  })
  output$subselect4 <- renderUI({
    stren=file11()
    #x <- read.csv(input$file$datapath)``
    selectInput("subids2","Select features:",choices=names(stren[,1:ncol(stren)])[!names(stren[,1:ncol(stren)]) %in% input$subids1],multiple = T, width = "500px") 
  })
  
  output$fun1 <- renderUI({
    #x=file11()
    f<-fun(input$file$datapath)
    selectInput('funs',choices = f,label ='Select Employee  function')
  })
  
  
  output$dist1<-renderDataTable({
      stren=file11()
      df<-dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,input$empid,input$head1)
      df
  })
    
    output$inter_dist1<-renderDataTable({
      stren=file11()
      df<-inter_dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,input$empid,input$head1)
      df
    })
    output$benchmark_dist1<-renderDataTable({
      stren=file11()
      ab=text_ip()
      df<-benchmark_dist(stren,input$file$datapath,input$subids1,input$subids2,input$level2,input$funs,ab,input$head2)
      df
    })
    
    ####################################
    output$barplot_benchmark<-renderPlotly({
      stren=file11()
      ab=text_ip()
      #df<-dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,","seperated numbers,input$head)
      df<-benchmark_dist(stren,input$file$datapath,input$subids1,input$subids2,input$level2,input$funs,ab,input$head2)
      result_1<-df[,2]
      name_1<-df[,1]
      p<-plot(result_1,name_1,xlab="Matching Candidates",ylab="distance", type="p" , main="Lollipop plot")
      p
    })  
    
    
    output$barplot_emp<-renderPlotly({
      stren=file11()
      ab=text_ip()
      #df<-dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,input$empid,input$head)
      df<-dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,input$empid,input$head1)
      result_1<-df[,2]
      name_1<-df[,1]
      p<-plot(result_1,name_1)
      p
    })  
    
    #########################1stfebtrialforradarchart
    output$radar<-renderPlotly({
        p <- plot_ly(
          type = 'scatterpolar',
          fill = 'toself'
        ) %>%
          add_trace(
            r = c(input$subid1,input$subid2),
            theta = c(input$subid1,input$subid2),
            name = 'Group A'
          ) %>%
          add_trace(
            r = c(input$subid1,input$subid2),
            theta = c(input$subid1,input$subid2),
            name = 'Group B'
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(1,5)
              )
            )
          )
        p
    })  
    
}
shinyApp(ui, server)