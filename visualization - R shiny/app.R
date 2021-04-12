library(shiny)#https://github.com/david-cortes/isotree
library(data.table)
library(modiscloud)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(rpivotTable)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse)
library(dplyr)#tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(isotree)
library(shinyBS)
library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
library(highcharter)
library(shiny)
library(neuralnet)
library(nnet)
library(DT)
require(xts)
library(ggplot2)
library(solitude)
library(Gmisc)
library(isofor)
library(ggrepel)
library(timeDate)
library(tidyr)
library(gbm, quietly=TRUE)
library(pROC, quietly=TRUE)
library(microbenchmark, quietly=TRUE)
library(shinycssloaders)
library(xgboost, quietly=TRUE)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(prophet)
library(lubridate)
library(dplyr)
library(plotly)
library(leaflet)
library(timeDate)
library(tidyr)
# load("app.rda")
# app()
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(prophet)
library(lubridate)
library(dplyr)
library(plotly)
library(shiny)#https://github.com/david-cortes/isotree
library(data.table)
library(modiscloud)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(rpivotTable)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse)
library(dplyr)#tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(isotree)
library(shinyBS)
library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
library(highcharter)
library(shiny)
library(neuralnet)
library(nnet)
library(DT)
require(xts)
library(ggplot2)
library(solitude)
library(Gmisc)
library(isofor)
library(ggrepel)
library(timeDate)
library(tidyr)
library(gbm, quietly=TRUE)
library(pROC, quietly=TRUE)
library(microbenchmark, quietly=TRUE)
library(shinycssloaders)
library(xgboost, quietly=TRUE)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(prophet)
library(lubridate)
library(dplyr)
library(plotly)

library(timeDate)
library(tidyr)
# load("app.rda")
# app()
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(prophet)
library(lubridate)
library(dplyr)
library(plotly)
library(prophet)
library(dygraphs)



gm= tags$h3(strong("Good Morning",style="color:#446e9b"))
ga= tags$h3(strong("Good Afternoon",style="color:#446e9b"))
ge= tags$h3(strong("Good Evening",style="color:#446e9b"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
  
  ## LOAD PACKAGE
  require(lubridate, quietly = T)
  
  ## ISOLATE currHour
  currhour = hour(now())
  
  
  ## RUN LOGIC
  if(currhour < 12){
    return(gm)
  } 
  else if(currhour >= 12 & currhour < 17){
    return(ga)
  }
  else if( currhour >= 17){
    return(ge)  
  }
}

gm= tags$h3(strong("Good Morning",style="color:#446e9b"))
ga= tags$h3(strong("Good Afternoon",style="color:#446e9b"))
ge= tags$h3(strong("Good Evening",style="color:#446e9b"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
  
  ## LOAD PACKAGE
  require(lubridate, quietly = T)
  
  ## ISOLATE currHour
  currhour = hour(now())
  
  
  ## RUN LOGIC
  if(currhour < 12){
    return(gm)
  } 
  else if(currhour >= 12 & currhour < 17){
    return(ga)
  }
  else if( currhour >= 17){
    return(ge)  
  }
}



## STARTING LOGGED VALUE; LET'S CHANGE THAT!
Logged = FALSE;

#tags$div(img(src="wel.png",height='50%',width='50%'),align="center"),
#====
# UI
#====
## make login screen
ui1 <- function(){
  
  tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#446e9b}')),
          div(id="container",align="center",
              div(id = "login",
                  # make login panel
                  wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                            
                            HTML(paste0('
                                <h2>
                                Hello, ', 
                                        good_time() ,
                                        '</h2>',
                                        '<h3>
                                <br>You are in Admin page.</br>
                                </h3>')
                            ),
                            br(),
                            br(),
                            tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                            br(),
                            tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                            br(),
                            # button
                            tags$div(actionButton("Login", "Log in"),align="center"),
                            # login error message
                            tags$div(uiOutput("message"),align="center")
                  )
                  
              )
          ),
          # css for container
          tags$style(type = "text/css", 
                     "#container{
                   display: flex;
                   justify-content: center;
                   margin-top: 150px;
                   }"),
          # css for login well panel
          tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
          # well panel
          tags$style(type="text/css",
                     "#well{
                    padding: 50px;
                    background: white;
                   border: 1px;
                   box-shadow: ;}"),
          # welcome text css
          tags$style(type = 'text/css',
                     "h2, h3{
                   color: #525252;}"),
          # input fields
          tags$style(type="text/css",
                     "#userName, #passwd{
                        box-shadow: none;
                        outline:none;
                        border: none;
                        padding-left: 0;
                        border-bottom: 2px solid #446e9b;
                        border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
          # button css
          tags$style(type='text/css',
                     "#Login{
                    outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #446e9b;
                   color: #446e9b;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: #446e9b;
                   color: white;}"),
          # error box - fadeOut animation
          tags$style(type="text/css",
                     "@-webkit-keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }
                    @keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }"),
          tags$style(type="text/css",
                     "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
  )
}


ui <- fluidPage(
  tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
   theme = shinytheme("spacelab"),
  
  tags$style(".glyphicon-ok-sign {color:#2b8ee5}
              .glyphicon-question-sign {color:#f4e107}
              .glyphicon-exclamation-sign {color:#e5413b}
              .glyphicon-flag, .glyphicon-trash {color:#28b728}"),
  
  
  tags$head(tags$style("#modal1 .modal-body {padding: 10px;background-color: white;}
                       #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                       #modal1 .modal-dialog { width: 95%; display: inline-block; text-align: left; vertical-align: top;}
                       #modal1 .modal-header {background-color: white; border-top-left-radius: 6px; border-top-right-radius: 6px}
                       #modal1 .modal { text-align: center; padding-right:10px; padding-top: 24px;}
                       #modal1 .close { font-size: 16px;}")),
  
  tags$head(tags$style("#modal2 .modal-body {padding: ;background-color: white;}
                       
                       #modal2 .modal-dialog { height:500px;width: 95%; display: inline-block; text-align: left; vertical-align: top;}
                       #modal2 .modal-header {background-color: white; border-top-left-radius: 6px; border-top-right-radius: 6px}
                       #modal2 .modal { text-align: center; padding-right:10px; padding-top: 24px;}
                       #modal2 .close { font-size: 16px;}")),
  
  #setBackgroundImage(src = "w.jpg"),
  tags$head(
    tags$style(type = 'text/css', 
               HTML('
                                      .navbar-default .navbar-brand{color: ;}
                                      .tab-panel{ background-color: #; color: #}
                                      .navbar-default .navbar-nav > .active > a, 
                                      .navbar-default .navbar-nav > .active > a:focus, 
                                      .navbar-default .navbar-nav > .active > a:hover {
                                      color: #e6e6e6;
                                      background-color: #;
                                      
                                      }')
    )
  ),
  
  tags$style(HTML(".navbar  {
                                    background-color:#; }
                                    
                                    .navbar .navbar-nav {float: right; margin-right: 35px;
                                    margin-top: 1px;
                                    color: #; 
                                    font-size: 16px; 
                                    background-color: #; }
                                    
                                    .navbar.navbar-default.navbar-static-top{ 
                                    color: #; 
                                    font-size: 23px; 
                                    background-color: # ;}
                                    
                                    .navbar .navbar-header {
                                    float: left;
                                    background-color: # ;}
                                    
                                    .navbar-default .navbar-brand { color: black; 
                                    margin-top: 8px;
                                    font-size: 18px; 
                                    background-color: # ;} 
                                    
                                    ")),
  tags$style(type="text/css",
             "#well0{
                               padding: 100px;
                               background: white;
                               border: 1px;
                               box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well2{
                               padding: 100px;
                               background: #;
                               border: 1px;
                               box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well8{
                               padding: 100px;
                               background: #;
                               border: 1px;
                               box-shadow: 2px 2px;}"),
  tags$style(type="text/css",
             "#rrr{
                               padding: 100px;
                               background: #;
                               border: 0px;
                               box-shadow: 0px 0px;}"),
  tags$head(
    tags$style(HTML("
                                      input[type=\"number\"] {
                                      font-size: 20px;height:50px;
                                      }
                                      
                                      "))
  ),
  tags$head(tags$style( type = 'text/css',  '#OverallPivot{ overflow-x: scroll; }')),
  
  #tags$style(type='text/css','#qq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  
  tags$head(HTML("<title>Anomaly Analysis</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
  navbarPage(id="tabset",tags$li(class = "dropdown",
                                 tags$style(".navbar {min-height:# }")
  ),
  #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
  title = tags$div(img(src="log.png","Visualization", style="margin-top: -9px;margin-left: 10px;", height = 38)),
  position = "fixed-top",selected = actionBttn(inputId = "mkv",label =div("Upload",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
  inverse = F,
  
  #navbarMenu(actionBttn(inputId = "mks",label =div(strong("Data"),style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
  
  
  navbarMenu(actionBttn(inputId = "mk1",label =div("File",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
              
                
             tabPanel(title = actionBttn(inputId = "mkv",label =div("Upload",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
                     uiOutput("commonui")
                      )),
  
#navbarMenu(actionBttn(inputId = "mk2",label =div("Edit",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
#navbarMenu(actionBttn(inputId = "mk3",label =div("Import",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
navbarMenu(actionBttn(inputId = "mk4",label =div("Data",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
           tabPanel( actionBttn(inputId = "jj",label ="Count,%,Total ",icon = icon("square-root-alt"),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning"),
                     br(),br(),br(),tabsetPanel(type = "tabs",
                                                tabPanel(tags$div(h4(strong(""),style="color:black;font-size:100%"),align="center"),br(),
                                                         splitLayout(tags$div(h4(column(12,htmlOutput('datasum'))),align="center"),
                                                         tags$div(h4(column(12,htmlOutput('datasum2'))),align="center")),
                                                         br(),br(),
                                                      column(1),
                                                         tags$div(column(4,uiOutput("count1")),align="center"),
                                                         tags$div(column(3,tableOutput('tablex')),align="center"),
                                                      tags$div(h4(column(3,htmlOutput('datasum333'))),align="center"),
                                                      column(1)
                                                         
                                                         )
                     )
                     ),
           # tabPanel( actionBttn(inputId = "jjj",label ="Total",icon = icon("square-root-alt"),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning"),
           #           br(),br(),br(),tabsetPanel(type = "tabs",
           #                                      tabPanel(tags$div(h4(strong("Total"),style="color:black;font-size:100%"),align="center"),br(),
           #                                               tags$div(column(6,uiOutput("total1")),align="center"),
           #                                               # column(6,tableOutput('tabley')),
           #                                               tags$div(h4(column(6,htmlOutput('datasum3'))),align="left")
           #                                                           
           #                                      )
           #           )
           #           ),
           tabPanel(actionBttn(inputId = "jjjj",label ="Statistics" ,icon = list(icon("chart-line")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger"),
                    br(),br(),br(),tabsetPanel(type = "tabs",
                                               tabPanel(tags$div(h4(strong("Summary Statistics"),style="color:black;font-size:100%"),align="center"),br(),
                                                        tags$div(uiOutput("sssl"),align="center"),
                                                        plotlyOutput("summary1"))
                                                        
                                               )
                    ),
           tabPanel(actionBttn(inputId = "pivots",label ="Pivot Table" ,icon = list(icon("table")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary"),
                    br(),br(),br(), tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Pivot Table"),style="color:black;font-size:100%"),align="center"),
                                                         rpivotTableOutput("OverallPivot", width = "100%", height = "500px"))
                    )),
           tabPanel(actionBttn(inputId = "jjjjjjjjjj",label ="Cross Tabulate" ,icon = list(icon("copyright")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success"),
                    br(),br(),br(),tabsetPanel(type = "tabs",
                                               tabPanel(tags$div(h4(strong("Cross Tabulate"),style="color:black;font-size:100%"),align="center"),br(),
                                                        uiOutput("crossdown"),br(),
                                                        uiOutput("crossui"))
                    ))
          
           ),

navbarMenu(actionBttn(inputId = "mk",label =div("Analyze",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
           
           # tabPanel( actionBttn(inputId = "jj",label ="Count",icon = icon("plus"),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary")),
           # tabPanel( actionBttn(inputId = "jjj",label ="Total",icon = icon("square-root-alt"),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning")),
           # tabPanel(actionBttn(inputId = "jjjj",label ="Statistics" ,icon = list(icon("chart-line")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger")),
           # tabPanel(actionBttn(inputId = "jjjjj",label ="Profile" ,icon = list(icon("angellist")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success")),
           # tabPanel(actionBttn(inputId = "jjjjjj",label ="Summarize" ,icon = list(icon("calculator")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary")),
           #actionBttn(inputId = "jjjjjjj",label ="Stratify" ,icon = list(icon("calendar")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger")),
           # tabPanel(actionBttn(inputId = "jjjjjjjj",label ="Classify" ,icon = list(icon("code")),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning")),
           # tabPanel(actionBttn(inputId = "jjjjjjjjj",label ="Age" ,icon = list(icon("project-diagram")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success")),
           # tabPanel(actionBttn(inputId = "jjjjjjjjjjj",label ="Benford" ,icon = list(icon("shield-alt")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary")),
           # tabPanel(actionBttn(inputId = "jjjjjjjjjjjj",label ="Sequence" ,icon = list(icon("stream")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger")),
           # tabPanel(actionBttn(inputId = "jjjjjjjjjjjjj",label ="Gaps" ,icon = list(icon("coins")),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning")),
            tabPanel(actionBttn(inputId = "duplic",label ="Duplicates" ,icon = list(icon("exchange-alt")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success"),
                    br(),br(),br(),  tabsetPanel(type = "tabs",
                                                 tabPanel(tags$div(h4(strong("Duplicate Verify"),style="color:black;font-size:100%"),align="center"),br(),
                                                          column(4,uiOutput("dupsel")),
                                                          column(4,uiOutput("odown")),
                                                          column(4,uiOutput("odown3")),
                                                          dataTableOutput('duplicate'))
                    )),
           
           tabPanel(actionBttn(inputId = "jjjjj",label ="Ageing" ,icon = list(icon("angellist")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary"),
                    br(),br(),br(),tabsetPanel(type = "tabs",
                                               tabPanel(tags$div(h4(strong("Ageing"),style="color:black;font-size:100%"),align="center"),br(),
                                                        uiOutput("ageing1"))
                                               
                    )),
           tabPanel(actionBttn(inputId = "jjjjjj",label ="Benford Law" ,icon = list(icon("calculator")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger"),
                    br(),br(),br(),tabsetPanel(type = "tabs",
                                               tabPanel(tags$div(h4(strong("Benford Law"),style="color:black;font-size:100%"),align="center"),br(),
                                                        uiOutput("benford1"))
                                               
                    ))
           ),

navbarMenu(actionBttn(inputId = "mkg",label =div("Outlier",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
           
           tabPanel(title =  actionBttn(inputId = "cont",label ="Control Chart..." ,icon = list(icon("chart-line")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary"),
                    br(),br(),br(), tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Control-chart"),style="color:black;font-size:100%"),align="center"),
                                                         br(), column(12, column(4,tags$div(uiOutput("inmy"),align="center")),
                                                                      column(4,tags$div(uiOutput("lev1dow"),align="center")),
                                                                      # column(3,tags$div(uiOutput("limits21"),align="center")),
                                                                      # column(3, tags$div(uiOutput("limits22"),align="center")),
                                                         column(4, tags$div(uiOutput("lev2con"),align="center"))),
                                                         
                                                         plotOutput("contc", width = "100%", height = "600px")))
           ),
           
           
           
           tabPanel(title =  actionBttn(inputId = "unibox",label ="Single Factor Box Plot..." ,icon = list(icon("cog")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger"),
                    br(),br(),br(), tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Box-Plot"),style="color:black;font-size:100%"),align="center"),
                                                         br(), column(12,
                                                                      column(4,tags$div(uiOutput("boxin"),align="center")),
                                                                      column(4,tags$div(uiOutput("downunibox"),align="center")),
                                                                      column(4,tags$div(uiOutput("lev2box"),align="center"))),br(),br(),br(),
                                                         plotlyOutput("boxu", width = "100%", height = "600px")))
           ),
           
           tabPanel( actionBttn(inputId = "facsin",label ="Univariate Outliers (Non-time series)" ,icon =  list(icon("angellist")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success"),
                     br(),br(),br(),tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Univariate Outliers"),style="color:black;font-size:120%"),align="center"), 
                                                         br(),  column(12,
                                                                       column(4,tags$div(uiOutput("boxsin"),align="center")),
                                                                       column(4,tags$div(uiOutput("downsin"),align="center")),
                                                                       column(4,tags$div(uiOutput("lev2uni"),align="center"))),br(),br(),br(),
                                                         plotlyOutput("sinfac"))))
           
           # tabPanel( actionBttn(inputId = "yyy2",label ="Summary (control-box-univariate outliers)" ,icon =  list(icon("cog")),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning"),
           #           br(),br(),br(),tabsetPanel(type = "tabs", 
           #                                      tabPanel(tags$div(h4(strong("Summary"),style="color:black;font-size:120%"),align="center"), 
           #                                               br(),
           #                                               tags$div(uiOutput("comdown"),align="center"),
           #                                               column(12,
           #                                                             column(4,plotlyOutput("xxx1")),
           #                                                             column(4,plotlyOutput("xxx2")),
           #                                                             column(4,plotlyOutput("xxx3"))),br(),br(),
           #                                               column(12,
           #                                                      column(4,plotlyOutput("xxx4")),
           #                                                      column(4,plotlyOutput("xxx5")),
           #                                                      column(4,plotlyOutput("xxx6")))
           #                                               )))
                ),
#navbarMenu(actionBttn(inputId = "mk6",label =div("Share",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
#navbarMenu(actionBttn(inputId = "mk7",label =div("Tools",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
navbarMenu(actionBttn(inputId = "mk8",label =div("Apps",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
           tabPanel( actionBttn(inputId = "analyse4r",label ="Multivariate Outliers" ,icon = list(icon("stream")),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning"),
                     br(),br(),br(),  tabsetPanel(type = "tabs",  
                 tabPanel(tags$div(h4(strong("Multi Factor Outliers"),style="color:black;font-size:120%"),align="center"),br(),
                      column(12,column(2),column(2,uiOutput("eee")),column(3,uiOutput("sum")),column(3,uiOutput("sum2")),column(1)),
                         column(12,column(2),column(2,uiOutput("mod")),column(3,uiOutput("mod1")),column(3,uiOutput("mod2")),column(1)),
                         #dataTableOutput("ccc"),
                         column(12,column(4,uiOutput("numeric")),column(4,uiOutput("factor")),column(4,uiOutput("date"))),br(),br(),br(),
                      uiOutput("button")
                         
                     ))),
           tabPanel(title =  actionBttn(inputId = "analyse2r",label ="Multi Factor Box Plot..." ,icon = list(icon("angellist")),style = "jelly",size = "sm",no_outline = F,block = F,color = "success"),
                    br(),br(),br(), tabsetPanel(type = "tabs", 
                                                tabPanel(tags$div(h4(strong("Box-Plot"),style="color:black;font-size:100%"),align="center"),
                                                         br(), uiOutput("down2"), plotlyOutput('one2',height = "500px")))),
          
           
           tabPanel( actionBttn(inputId = "fore",label ="Forecasting next set of values   " ,icon = list(icon("chart-line")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary"),
                     br(),br(),br(),uiOutput("forecas")),
           tabPanel( actionBttn(inputId = "analyse3r",label ="Univariate Outliers (Time series)" ,icon = list(icon("cog")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger"),
                     br(),br(),br(),  tabsetPanel(type = "tabs", 
                                                  
                                                  tabPanel(tags$div(h4(strong("Anomaly-Plot"),style="color:black;font-size:120%"),align="center"), plotlyOutput('PI210A.PV_plot',height = "800px")),
                                                  tabPanel(tags$div(h4(strong("Control-Chart"),style="color:black;font-size:120%"),align="center"), plotOutput('PI210A.PV_cplot',height = "800px")),
                                                  tabPanel(tags$div(h4(strong("Box-Plot"),style="color:black;font-size:120%"),align="center"), plotOutput('PI210A.PV_boxplot',height = "800px")),
                                                  tabPanel(tags$div(h4(strong("Comparison-Plot"),style="color:black;font-size:120%"),align="center"),br(),
                                                           tags$div(downloadBttn(outputId = "down4",label = "Download Data..!",color = "success",size = "md"),align="center"),
                                                           br(),
                                                           column(12,
                                                                  
                                                                  column(4,plotlyOutput("anopast")),
                                                                  
                                                                  
                                                                  column(4,plotlyOutput("outcontrolpast")),
                                                                  
                                                                  
                                                                  column(4,plotlyOutput("outlierpast"))),
                                                           
                                                           column(12,
                                                                  
                                                                  column(4,plotlyOutput("c1p")),
                                                                  
                                                                  
                                                                  column(4,plotlyOutput("c2p")),
                                                                  
                                                                  
                                                                  column(4,plotlyOutput("c3p")))),

                                                  tabPanel(tags$div(uiOutput("foot1"),align="center"))
                     ))
         
                     #uiOutput("timeunivariate"))
    ),
# navbarMenu(actionBttn(inputId = "mk9",label =div("Server",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
 #navbarMenu(actionBttn(inputId = "mk9",label =div("Window",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
#navbarMenu(actionBttn(inputId = "mk10",label =div("Admin",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default")),
navbarMenu(actionBttn(inputId = "mk11",label =div("Help",style="color:black") ,icon = NULL,style = "simple",size = "sm",no_outline = F,block = F,color = "default"),
           tabPanel(tags$a(href="https://github.com/sailogeshh",target="_blank",actionBttn(inputId = "about",label ="About" ,icon = list(icon("coins")),style = "jelly",size = "sm",no_outline = F,block = F,color = "primary"))),
           tabPanel(tags$a(href="https://github.com/sailogeshh",target="_blank",actionBttn(inputId = "website",label ="Website" ,icon = list(icon("stream")),style = "jelly",size = "sm",no_outline = F,block = F,color = "warning"))),
           tabPanel(tags$a(href="https://github.com/sailogeshh",target="_blank",actionBttn(inputId = "contact",label ="Contact" ,icon = list(icon("angellist")),style = "jelly",size = "sm",no_outline = F,block = F,color = "danger")))),
tabPanel(
  title = tags$a(href="javascript:history.go(0)",tags$div(bsButton("ss",strong("Logout"),style = "success",size="small"),
                                                          style="color:white;margin-top: -12px;font-weight:100%;",align="center"),style="color:white;margin-top: -19px;")
)

                                            
))


server <- function(input, output, session) {
  qcc <- function(data, type = c("xbar", "R", "S", "xbar.one", "p", "np", "c", "u", "g"), sizes, center, std.dev, limits, data.name, labels, newdata, newsizes, newdata.name, newlabels, nsigmas = 3, confidence.level, rules = shewhart.rules, plot = TRUE, ...)
  {
    call <- match.call()
    
    if (missing(data))
      stop("'data' argument is not specified")
    
    if(identical(type, eval(formals(qcc)$type)))
    { type <- as.character(type)[1]
    warning("chart 'type' not specified, assuming \"", type, "\"",
            immediate. = TRUE) }
    if(!exists(paste("stats.", type, sep = ""), mode="function") |
       !exists(paste("sd.", type, sep = ""), mode="function") |
       !exists(paste("limits.", type, sep = ""), mode="function"))
      stop(paste("invalid", type, "control chart. See help(qcc) "))
    
    if (missing(data.name)) 
      data.name <- deparse(substitute(data))
    data <- data.matrix(data)
    if (missing(sizes)) 
    { if (any(type==c("p", "np", "u")))
      stop(paste("sample 'sizes' must be given for a", type, "Chart"))
      else
        sizes <- apply(data, 1, function(x) sum(!is.na(x)))  }
    else
    { if (length(sizes)==1)
      sizes <- rep(sizes, nrow(data))
    else if (length(sizes) != nrow(data))
      stop("sizes length doesn't match with data") }
    
    if (missing(labels))
    { if (is.null(rownames(data))) labels <- 1:nrow(data)
    else                         labels <- rownames(data) }
    
    stats <- paste("stats.", type, sep = "")
    if (!exists(stats, mode="function"))
      stop(paste("function", stats, "is not defined"))
    stats <- do.call(stats, list(data, sizes))
    statistics <- stats$statistics
    if (missing(center)) center <- stats$center
    
    sd <- paste("sd.", type, sep = "")
    if (!exists(sd, mode="function"))
      stop(paste("function", sd, "is not defined!"))
    missing.std.dev <- missing(std.dev)
    if (missing.std.dev)
    { std.dev <- NULL
    std.dev <- switch(type, 
                      "xbar" = { if(any(sizes > 25)) "RMSDF"
                        else                "UWAVE-R" },
                      "xbar.one" = "MR",
                      "R" = "UWAVE-R",
                      "S" = "UWAVE-SD",
                      NULL)
    std.dev <- do.call(sd, list(data, sizes, std.dev)) }
    else 
    { if (is.character(std.dev))
    { std.dev <- do.call(sd, list(data, sizes, std.dev)) }
      else
      { if (!is.numeric(std.dev))
        stop("if provided the argument 'std.dev' must be a method available or a numerical value. See help(qcc).")  }
    }
    
    names(statistics) <-  rownames(data) <-  labels
    names(dimnames(data)) <- list("Group", "Samples")
    
    object <- list(call = call, type = type, 
                   data.name = data.name, data = data, 
                   statistics = statistics, sizes = sizes, 
                   center = center, std.dev = std.dev)
    # check for new data provided and update object
    if (!missing(newdata))
    {   if (missing(newdata.name))
    {newdata.name <- deparse(substitute(newdata))}
      newdata <- data.matrix(newdata)
      if (missing(newsizes))
      { if (any(type==c("p", "np", "u")))
        stop(paste("sample sizes must be given for a", type, "Chart"))
        else
          newsizes <- apply(newdata, 1, function(x) sum(!is.na(x))) }
      else
      { if (length(newsizes)==1)
        newsizes <- rep(newsizes, nrow(newdata))
      else if (length(newsizes) != nrow(newdata))
        stop("newsizes length doesn't match with newdata") }
      stats <- paste("stats.", type, sep = "")
      if (!exists(stats, mode="function"))
        stop(paste("function", stats, "is not defined"))
      newstats <- do.call(stats, list(newdata, newsizes))$statistics
      if (missing(newlabels))
      { if (is.null(rownames(newdata)))
      { start <- length(statistics)
      newlabels <- seq(start+1, start+length(newstats)) }
        else
        { newlabels <- rownames(newdata) }
      }
      names(newstats) <- newlabels
      object$newstats <- newstats
      object$newdata  <- newdata
      object$newsizes <- newsizes
      object$newdata.name <- newdata.name
      statistics <- c(statistics, newstats)
      sizes <- c(sizes, newsizes)
    }
    
    conf <- nsigmas
    if (!missing(confidence.level))
      conf <- confidence.level
    if (conf >= 1)
    { object$nsigmas <- conf }
    else
      if (conf > 0 & conf < 1)
      { object$confidence.level <- conf } 
    
    # get control limits
    if (missing(limits))
    { limits <- paste("limits.", type, sep = "")
    if (!exists(limits, mode="function"))
      stop(paste("function", limits, "is not defined"))
    limits <- do.call(limits, list(center = center, std.dev = std.dev,
                                   sizes = sizes, conf = conf)) 
    }
    else 
    { if (!missing.std.dev)
      warning("'std.dev' is not used when limits is given")
      if (!is.numeric(limits))
        stop("'limits' must be a vector of length 2 or a 2-columns matrix")
      limits <- matrix(limits, ncol = 2)
      dimnames(limits) <- list(rep("",nrow(limits)), c("LCL ", "UCL"))
    }
    
    lcl <- limits[,1]
    ucl <- limits[,2]
    object$limits <- limits
    if (is.function(rules)) violations <- rules(object)
    else                    violations <- NULL
    object$violations <- violations
    
    class(object) <- "qcc"
    if(plot) plot(object, ...) 
    return(object)
  }
  
  
  blues.colors <- function (n) 
  {
    palette <- grDevices::colorRampPalette(c("#03396c", "#005b96", "#6497b1", "#b3cde0"), 
                                           space = "Lab")
    palette(n)
  }
  
  
  #----------------------------------------------------------------------------#
  # print a short version of a matrix by allowing to select the number of 
  # head/tail rows and columns to display
  
  .printShortMatrix <- function(x, head = 2, tail = 1, chead = 5, ctail = 1, ...)
  { 
    x <- as.matrix(x)
    nr <- nrow(x)
    nc <- ncol(x)
    if(is.na(head <- as.numeric(head))) head <- 2
    if(is.na(tail <- as.numeric(tail))) tail <- 1
    if(is.na(chead <- as.numeric(chead))) chead <- 5
    if(is.na(ctail <- as.numeric(ctail))) ctail <- 1
    
    if(nr > (head + tail + 1))
    { rnames <- rownames(x)
    if(is.null(rnames)) 
      rnames <- paste("[", 1:nr, ",]", sep ="")
    x <- rbind(x[1:head,,drop=FALSE], 
               rep(NA, nc), 
               x[(nr-tail+1):nr,,drop=FALSE])
    rownames(x) <- c(rnames[1:head], "...", rnames[(nr-tail+1):nr])
    }
    if(nc > (chead + ctail + 1))
    { cnames <- colnames(x)
    if(is.null(cnames)) 
      cnames <- paste("[,", 1:nc, "]", sep ="")
    x <- cbind(x[,1:chead,drop=FALSE], 
               rep(NA, nrow(x)), 
               x[,(nc-ctail+1):nc,drop=FALSE])
    colnames(x) <- c(cnames[1:chead], "...", cnames[(nc-ctail+1):nc])
    }
    
    print(x, na.print = "", ...)
  }
  
  # old version
  # .printShortMatrix <- function(x, head = 2, tail = 1, ...)
  # { 
  #   x <- as.matrix(x)
  #   nr <- nrow(x)
  #   nc <- ncol(x)
  #   if(nr > 4)
  #     { rnames <- rownames(x)
  #       if(is.null(rnames)) 
  #         rnames <- paste("[", 1:nr, ",]", sep ="")
  #       x <- rbind(x[1:head,], rep(NA, nc), x[(nr-tail+1):nr,])
  #       rownames(x) <- c(rnames[1:head], "...", rnames[(nr-tail+1):nr])
  #       print(x, na.print = "", ...)
  #     }
  #   else
  #     { print(x, ...)}  
  # }
  
  #-------------------------------------------------------------------#
  #                                                                   #
  #
  # Options retrieval and setting
  #
  
  qcc.options <- function (...)
  {
    current <- .qcc.options
    if(nargs() == 0) return(current)
    #  if(is.character(...))
    #       temp <- eval(parse(text = paste(c("list(", ..., ")"))))
    #  else temp <- list(...)
    temp <- list(...)
    if(length(temp) == 1 && is.null(names(temp))) 
    { arg <- temp[[1]]
    switch(mode(arg),
           list = temp <- arg,
           character = return(.qcc.options[[arg]]),
           stop(paste("invalid argument:", sQuote(arg)))) }
    if(length(temp) == 0) return(current)
    name <- names(temp)
    if(is.null(name)) stop("options must be given by name")
    changed <- current[name]
    current[name] <- temp
    env <- if(sys.parent() == 0) asNamespace("qcc") 
    else                  parent.frame()
    assign(".qcc.options", current, envir = env)
    invisible(current)
  }
  
  ".qcc.options" <- list(exp.R.unscaled = c(NA, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931),
                         se.R.unscaled = c(NA, 0.8525033, 0.8883697, 0.8798108, 0.8640855, 0.8480442, 0.8332108, 0.8198378, 0.8078413, 0.7970584, 0.7873230, 0.7784873, 0.7704257, 0.7630330, 0.7562217, 0.7499188, 0.7440627, 0.7386021, 0.7334929, 0.7286980, 0.7241851, 0.7199267, 0.7158987, 0.7120802, 0.7084528, 0.7050004, 0.7017086, 0.6985648, 0.6955576, 0.6926770, 0.6899137, 0.6872596, 0.6847074, 0.6822502, 0.6798821, 0.6775973, 0.6753910, 0.6732584, 0.6711952, 0.6691976, 0.6672619, 0.6653848, 0.6635632, 0.6617943, 0.6600754, 0.6584041, 0.6567780, 0.6551950, 0.6536532, 0.6521506),
                         beyond.limits = list(pch=19, col="red"),
                         violating.runs = list(pch=19, col="black"),
                         run.length = 7,
                         # bg.margin = "lightgrey",
                         bg.margin = "#E5E5E5",
                         bg.figure = "white",
                         cex = 1,
                         font.stats = 1,
                         cex.stats = 0.9)
  
  .onAttach <- function(library, pkg)
  {
    ## we can't do this in .onLoad
    unlockBinding(".qcc.options", asNamespace("qcc"))
    description <- readLines(system.file("DESCRIPTION", package = "qcc"))
    version <- grep("Version:", description, ignore.case = TRUE, value = TRUE)
    version <- gsub(pattern = "Version:", replacement = "", version, ignore.case = TRUE)
    version <- gsub(pattern = " ", replacement = "", version)
    packageStartupMessage("Package 'qcc', version ", version)
    packageStartupMessage("Type 'citation(\"qcc\")' for citing this R package in publications.")
    invisible()
  }
  
  print.qcc <- function(x, ...) str(x,1)
  
  summary.qcc <- function(object, digits =  getOption("digits"), ...)
  {
    #object <- x   # Argh.  Really want to use 'object' anyway
    cat("\nCall:\n",deparse(object$call),"\n\n",sep="")
    data.name <- object$data.name
    type <- object$type
    cat(paste(type, "chart for", data.name, "\n"))
    statistics <- object$statistics
    cat("\nSummary of group statistics:\n")
    print(summary(statistics), digits = digits, ...)
    sizes <- object$sizes
    if(length(unique(sizes))==1)
      sizes <- sizes[1]
    if(length(sizes) == 1)
      cat("\nGroup sample size: ", format(sizes))
    else {
      cat("\nSummary of group sample sizes: ")
      tab <- table(sizes)
      print(matrix(c(as.numeric(names(tab)), tab), 
                   ncol = length(tab), byrow = TRUE, 
                   dimnames = list(c("  sizes", "  counts"),
                                   character(length(tab)))), 
            digits = digits, ...)
    }
    cat("\nNumber of groups: ", length(statistics))
    
    center <- object$center
    if(length(center) == 1)
    { cat("\nCenter of group statistics: ", format(center, digits = digits)) }
    else
    { out <- paste(format(center, digits = digits))
    out <- out[which(cumsum(nchar(out)+1) < getOption("width")-40)]      
    out <- paste0(paste(out, collapse = " "), " ...")
    cat("\nCenter of group statistics: ", out, sep = "")
    }
    
    sd <- object$std.dev
    if(length(sd) == 1)
    { cat("\nStandard deviation: ", format(sd, digits = digits), "\n") }
    else
    { out <- paste(format(sd, digits = digits))
    out <- out[which(cumsum(nchar(out)+1) < getOption("width")-40)]
    out <- paste0(paste(out, collapse = " "), " ...")
    cat("\nStandard deviation: ", out, "\n", sep = "")
    }
    
    newdata.name <- object$newdata.name
    newstats <- object$newstats
    if (!is.null(newstats)) 
    { cat(paste("\nSummary of group statistics in ", 
                newdata.name, ":\n", sep = ""))
      print(summary(newstats), digits = digits, ...)
      newsizes <- object$newsizes
      if (length(unique(newsizes)) == 1)
        newsizes <- newsizes[1]
      if (length(newsizes) == 1)
        cat("\nGroup sample size: ", format(newsizes))
      else 
      { cat("\nSummary of group sample sizes:\n")
        new.tab <- table(newsizes)
        print(matrix(c(as.numeric(names(new.tab)), new.tab),
                     ncol = length(new.tab), byrow = TRUE, 
                     dimnames = list(c("  sizes", "  counts"),
                                     character(length(new.tab)))), 
              digits = digits, ...)
      }
      cat("\nNumber of groups: ", length(newstats), "\n")
    }
    
    limits <- object$limits
    if (!is.null(limits)) 
    { cat("\nControl limits:\n")
      .printShortMatrix(limits, digits = digits, ...) }
    
    invisible()
  }
  
  
  plot.qcc <- function(x, add.stats = TRUE, chart.all = TRUE, 
                       label.limits = c("LCL ", "UCL"),
                       title, xlab, ylab, ylim, axes.las = 0,
                       digits =  getOption("digits"),
                       restore.par = TRUE, ...) 
  {
    object <- x  # Argh.  Really want to use 'object' anyway
    if ((missing(object)) | (!inherits(object, "qcc")))
      stop("an object of class `qcc' is required")
    
    # collect info from object
    type <- object$type
    std.dev <- object$std.dev
    data.name <- object$data.name
    center <- object$center
    stats <- object$statistics
    limits <- object$limits 
    lcl <- limits[,1]
    ucl <- limits[,2]
    newstats <- object$newstats
    newdata.name <- object$newdata.name
    violations <- object$violations
    if(chart.all) 
    { statistics <- c(stats, newstats)
    indices <- 1:length(statistics) }
    else
    { if(is.null(newstats))
    { statistics <- stats
    indices <- 1:length(statistics) }
      else
      { statistics <- newstats 
      indices <- seq(length(stats)+1, length(stats)+length(newstats)) }
    }
    
    if (missing(title))
    { if (is.null(newstats))
      main.title <- paste(type, "Chart\nfor", data.name)
    else if (chart.all)
      main.title <- paste(type, "Chart\nfor", data.name, 
                          "and", newdata.name)
    else main.title <- paste(type, "Chart\nfor", newdata.name) 
    }
    else main.title <- paste(title)
    
    oldpar <- par(no.readonly = TRUE)
    if(restore.par) on.exit(par(oldpar))
    mar <- pmax(oldpar$mar, c(4.1,4.1,3.1,2.1))
    par(bg  = qcc.options("bg.margin"), 
        cex = oldpar$cex * qcc.options("cex"),
        mar = if(add.stats) pmax(mar, c(7.6,0,0,0)) else mar)
    
    # plot Shewhart chart
    plot(indices, statistics, type="n",
         ylim = if(!missing(ylim)) ylim 
         else range(statistics, limits, center),
         ylab = if(missing(ylab)) "Group summary statistics" else ylab,
         xlab = if(missing(xlab)) "Group" else xlab, 
         axes = FALSE)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = qcc.options("bg.figure"))
    axis(1, at = indices, las = axes.las,
         labels = if(is.null(names(statistics))) 
           as.character(indices) else names(statistics))
    axis(2, las = axes.las)
    box()
    top.line <- par("mar")[3]-length(capture.output(cat(main.title)))
    top.line <- top.line - if(chart.all & (!is.null(newstats))) 0.1 else 0.5
    mtext(main.title, side = 3, line = top.line,
          font = par("font.main"), 
          cex  = qcc.options("cex"), 
          col  = par("col.main"))
    
    lines(indices, statistics, type = "b", pch=20) 
    
    if(length(center) == 1)
      abline(h = center)
    else lines(indices, center[indices], type="s")
    
    if(length(lcl) == 1) 
    { abline(h = lcl, lty = 2)
      abline(h = ucl, lty = 2)
      abline(h=input$uslimit0,lty=1,col="green",lwd=3)
      abline(h=input$lslimit0,lty=1,col="green",lwd=3)}
    else 
    { lines(indices, lcl[indices], type="s", lty = 2)
      lines(indices, ucl[indices], type="s", lty = 2) }
    mtext(label.limits, side = 4, at = c(rev(lcl)[1], rev(ucl)[1]), 
          las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
    mtext("CL", side = 4, at = rev(center)[1], 
          las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
    
    if(is.null(qcc.options("violating.runs")))
      stop(".qcc.options$violating.runs undefined. See help(qcc.options).")
    if(length(violations$violating.runs))
    { v <- violations$violating.runs
    if(!chart.all & !is.null(newstats))
    { v <- v - length(stats) 
    v <- v[v>0] }
    points(indices[v], statistics[v], 
           col = qcc.options("violating.runs")$col, 
           pch = qcc.options("violating.runs")$pch) 
    }
    
    if(is.null(qcc.options("beyond.limits")))
      stop(".qcc.options$beyond.limits undefined. See help(qcc.options).")
    if(length(violations$beyond.limits))
    { v <- violations$beyond.limits
    if(!chart.all & !is.null(newstats))
    { v <- v - length(stats) 
    v <- v[v>0] }
    points(indices[v], statistics[v], 
           col = qcc.options("beyond.limits")$col, 
           pch = qcc.options("beyond.limits")$pch) 
    }
    
    if(chart.all & (!is.null(newstats)))
    { len.obj.stats <- length(object$statistics)
    len.new.stats <- length(statistics) - len.obj.stats
    abline(v = len.obj.stats + 0.5, lty = 3)
    mtext(# paste("Calibration data in", data.name),
      "Past Data", cex = par("cex")*0.8,
      at = len.obj.stats/2, line = 0, adj = 0.5)
    mtext(# paste("New data in", object$newdata.name),  
      "Forecast Data", cex = par("cex")*0.8, 
      at = len.obj.stats + len.new.stats/2, line = 0, adj = 0.5)
    }
    
    if(add.stats) 
    { 
      # computes the x margins of the figure region
      plt <- par()$plt; usr <- par()$usr
      px <- diff(usr[1:2])/diff(plt[1:2])
      xfig <- c(usr[1]-px*plt[1], usr[2]+px*(1-plt[2]))
      at.col <- xfig[1] + diff(xfig[1:2])*c(0.10, 0.40, 0.65)
      top.line <- 4.5
      # write info at bottom
      mtext(paste("Number of Observations = ", length(statistics), sep = ""), 
            side = 1, line = top.line, adj = 0, at = at.col[1],
            font = qcc.options("font.stats"),
            cex = par("cex")*qcc.options("cex.stats"))
      center <- object$center
      if(length(center) == 1)
      { mtext(paste("Center = ", signif(center[1], digits), sep = ""),
              side = 1, line = top.line+1, adj = 0, at = at.col[1],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      else 
      { mtext("Center is variable",
              side = 1, line = top.line+1, adj = 0, at = at.col[1],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      
      if(length(std.dev) == 1)
      { mtext(paste("StdDev = ", signif(std.dev, digits), sep = ""),
              side = 1, line = top.line+2, adj = 0, at = at.col[1],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }      
      else
      { mtext("StdDev is variable",
              side = 1, line = top.line+2, adj = 0, at = at.col[1],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      
      if(length(unique(lcl)) == 1)
      { mtext(paste("LCL = ", signif(lcl[1], digits), sep = ""), 
              side = 1, line = top.line+1, adj = 0, at = at.col[2],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      else 
      { mtext("LCL is variable", 
              side = 1, line = top.line+1, adj = 0, at = at.col[2],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      
      if(length(unique(ucl)) == 1)
      { mtext(paste("UCL = ", signif(ucl[1], digits), sep = ""),
              side = 1, line = top.line+2, adj = 0, at = at.col[2],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats")) 
      }
      else 
      { mtext("UCL is variable", 
              side = 1, line = top.line+2, adj = 0, at = at.col[2],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
      }
      
      if(!is.null(violations))
      { mtext(paste("Number beyond limits =",
                    length(unique(violations$beyond.limits))), 
              side = 1, line = top.line+1, adj = 0, at = at.col[3],
              font = qcc.options("font.stats"),
              cex = par("cex")*qcc.options("cex.stats"))
        # mtext(paste("Number in warning Zone =",
        #             length(unique(violations$violating.runs))), 
        #       side = 1, line = top.line+2, adj = 0, at = at.col[3],
        #       font = qcc.options("font.stats"),
        #       cex = par("cex")*qcc.options("cex.stats"))
      }
    }
    
    invisible()
  }
  
  #
  #  Functions used to compute Shewhart charts statistics
  #
  
  .qcc.c4 <- function(n)
  { sqrt(2/(n - 1)) * exp(lgamma(n/2) - lgamma((n - 1)/2)) }
  
  # xbar
  
  stats.xbar <- function(data, sizes)
  {
    if (missing(sizes))
      sizes <- apply(data, 1, function(x) sum(!is.na(x)))
    statistics <- apply(data, 1, mean, na.rm=TRUE)
    center <- sum(sizes * statistics)/sum(sizes)
    list(statistics = statistics, center = center)
  }
  
  sd.xbar <- function(data, sizes, std.dev = c("UWAVE-R", "UWAVE-SD", "MVLUE-R", "MVLUE-SD", "RMSDF"))
  {
    if (!is.numeric(std.dev))
      std.dev <- match.arg(std.dev)
    if (missing(sizes))
      sizes <- apply(data, 1, function(x) sum(!is.na(x)))
    if (any(sizes == 1))
      stop("group sizes must be larger than one")
    c4 <- .qcc.c4
    if (is.numeric(std.dev))
    { sd <- std.dev }
    else
    { switch(std.dev, 
             "UWAVE-R" = {  R <- apply(data, 1, function(x) 
               diff(range(x, na.rm = TRUE)))
             d2 <- qcc.options("exp.R.unscaled")[sizes]
             sd <- sum(R/d2)/length(sizes) }, 
             "UWAVE-SD" = { S <- apply(data, 1, sd, na.rm = TRUE)
             sd <- sum(S/c4(sizes))/length(sizes) },
             "MVLUE-R"  = { R <- apply(data, 1, function(x) 
               diff(range(x, na.rm = TRUE)))
             d2 <- qcc.options("exp.R.unscaled")[sizes]
             d3 <- qcc.options("se.R.unscaled")[sizes]
             w  <- (d2/d3)^2
             sd <- sum(R/d2*w)/sum(w) }, 
             "MVLUE-SD" = { S <- apply(data, 1, sd, na.rm = TRUE)
             w  <- c4(sizes)^2/(1-c4(sizes)^2)
             sd <- sum(S/c4(sizes)*w)/sum(w) },
             "RMSDF" =    { S <- apply(data, 1, sd, na.rm = TRUE)
             w  <- sizes-1
             sd <- sqrt(sum(S^2*w)/sum(w))/c4(sum(w)+1) }
    )
    }
    #  if (missing(std.dev))
    #     var.within <- apply(data, 1, var, na.rm=TRUE)
    #  else 
    #     var.within <- std.dev^2
    #  var.df <- sum(sizes - 1)
    #  if (equal.sd) 
    #     { std.dev <- sqrt(sum((sizes - 1) * var.within)/var.df) / c4(var.df + 1) }
    #  else 
    #     { c <- c4(sizes)/(1 - c4(sizes)^2)
    #       std.dev <- sum(c * sqrt(var.within))/sum(c * c4(sizes)) }
    return(sd)
  }
  
  limits.xbar <- function(center, std.dev, sizes, conf)
  {
    if (length(unique(sizes))==1)
      sizes <- sizes[1]
    se.stats <- std.dev/sqrt(sizes)
    if (conf >= 1) 
    { lcl <- center - conf * se.stats
    ucl <- center + conf * se.stats
    }
    else 
    { if (conf > 0 & conf < 1) 
    { nsigmas <- qnorm(1 - (1 - conf)/2)
    lcl <- center - nsigmas * se.stats
    ucl <- center + nsigmas * se.stats
    }
      else stop("invalid 'conf' argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    return(limits)
  }
  
  # S chart
  
  stats.S <- function(data, sizes)
  {
    if (missing(sizes))
      sizes <- apply(data, 1, function(x) sum(!is.na(x)))
    if(ncol(data)==1) 
    { statistics <- as.vector(data) }
    else 
    { statistics <- sqrt(apply(data, 1, var, na.rm=TRUE)) }
    if (length(sizes == 1))
      sizes <- rep(sizes, length(statistics))
    center <- sum(sizes * statistics)/sum(sizes)
    list(statistics = statistics, center = center)
  }
  
  sd.S <- function(data, sizes, std.dev = c("UWAVE-SD", "MVLUE-SD", "RMSDF"))
  {
    if (!is.numeric(std.dev))
      std.dev <- match.arg(std.dev)
    sd.xbar(data, sizes, std.dev)
  }
  
  limits.S <- function(center, std.dev, sizes, conf)
  {
    if (length(unique(sizes))==1)
      sizes <- sizes[1]
    c4 <- .qcc.c4
    se.stats <- std.dev * sqrt(1 - c4(sizes)^2)
    if (conf >= 1) 
    { lcl <- pmax(0, center - conf * se.stats)
    ucl <- center + conf * se.stats
    }
    else 
    { if (conf > 0 & conf < 1) 
    { ucl <- std.dev * sqrt(qchisq(1 - (1 - conf)/2, sizes - 1)/
                              (sizes - 1))
    lcl <- std.dev * sqrt(qchisq((1 - conf)/2, sizes - 1)/
                            (sizes - 1))
    }
      else stop("invalid conf argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    limits
  }
  
  # R Chart 
  
  stats.R <- function(data, sizes)
  {
    if (missing(sizes))
      sizes <- apply(data, 1, function(x) sum(!is.na(x)))
    if(ncol(data)==1) 
    { statistics <- as.vector(data) }
    else 
    { statistics <- apply(data, 1, function(x) diff(range(x, na.rm=TRUE))) }
    if (length(sizes == 1))
      sizes <- rep(sizes, length(statistics))
    center <- sum(sizes * statistics)/sum(sizes)
    list(statistics = statistics, center = center)
  }
  
  sd.R <- function(data, sizes, std.dev = c("UWAVE-R", "MVLUE-R"))
  {
    if (!is.numeric(std.dev))
      std.dev <- match.arg(std.dev)
    sd.xbar(data, sizes, std.dev)
  }
  
  limits.R <- function(center, std.dev, sizes, conf)
  {
    if (length(unique(sizes))==1)
      sizes <- sizes[1]
    se.R.unscaled <- qcc.options("se.R.unscaled")
    Rtab <- length(se.R.unscaled)
    if (conf >= 1) 
    { if (any(sizes > Rtab))
      stop(paste("group size must be less than", 
                 Rtab + 1, "when giving nsigmas"))
      se.R <- se.R.unscaled[sizes] * std.dev
      lcl <- pmax(0, center - conf * se.R)
      ucl <- center + conf * se.R
    }
    else 
    { if (conf > 0 && conf < 1) 
    { ucl <- qtukey(1 - (1 - conf)/2, sizes, 1e100) * std.dev
    lcl <- qtukey((1 - conf)/2, sizes, 1e100) * std.dev
    }
      else stop("invalid conf argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    return(limits)
  }
  
  # xbar Chart for one-at-time data
  
  stats.xbar.one <- function(data, sizes)
  {
    statistics <- as.vector(data)
    center <- mean(statistics)
    list(statistics = statistics, center = center)
  }
  
  sd.xbar.one <- function(data, sizes, std.dev = c("MR", "SD"), k = 2)
  {
    data <- as.vector(data)
    n <- length(data)
    if(!is.numeric(std.dev)) 
      std.dev <- match.arg(std.dev)
    c4 <- .qcc.c4
    if(is.numeric(std.dev)) 
    { sd <- std.dev }
    else
    { switch(std.dev, 
             "MR" = { d2 <- qcc.options("exp.R.unscaled")
             if(is.null(d2))
               stop(".qcc.options$exp.R.unscaled is null")
             d <- 0
             for(j in k:n)
               d <- d+abs(diff(range(data[c(j:(j-k+1))])))
             sd <- (d/(n-k+1))/d2[k] },
             "SD" = { sd <- sd(data)/c4(n) },
             sd <- NULL)
    }
    return(sd)
  }
  
  
  limits.xbar.one <- function(center, std.dev, sizes, conf)
  {
    se.stats <- std.dev
    if (conf >= 1) 
    { lcl <- center - conf * se.stats
    ucl <- center + conf * se.stats
    }
    else 
    { if (conf > 0 & conf < 1) 
    { nsigmas <- qnorm(1 - (1 - conf)/2)
    lcl <- center - nsigmas * se.stats
    ucl <- center + nsigmas * se.stats
    }
      else stop("invalid conf argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    return(limits)
  }
  
  
  # p Chart
  
  stats.p <- function(data, sizes)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    pbar <- sum(data)/sum(sizes)
    list(statistics = data/sizes, center = pbar)
  }
  
  sd.p <- function(data, sizes, ...)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    pbar <- sum(data)/sum(sizes)
    std.dev <- sqrt(pbar * (1 - pbar))
    return(std.dev)
  }
  
  limits.p <- function(center, std.dev, sizes, conf)
  { 
    limits.np(center * sizes, std.dev, sizes, conf) / sizes
  }
  
  # np Chart
  
  stats.np <- function(data, sizes)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    pbar <- sum(data)/sum(sizes)
    center <- sizes * pbar
    if (length(unique(center)) == 1)
      center <- center[1]
    list(statistics = data, center = center)
  }
  
  sd.np <- function(data, sizes, ...)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    pbar <- sum(data)/sum(sizes)
    std.dev <- sqrt(sizes * pbar * (1 - pbar))
    if (length(unique(std.dev)) == 1)
      std.dev <- std.dev[1]
    return(std.dev)
  }
  
  limits.np <- function(center, std.dev, sizes, conf)
  { 
    sizes <- as.vector(sizes)
    if (length(unique(sizes)) == 1)
      sizes <- sizes[1]
    pbar <- mean(center / sizes)
    if (conf >= 1)
    { tol <- conf * sqrt(pbar * (1 - pbar) * sizes)
    lcl <- pmax(center - tol, 0)
    ucl <- pmin(center + tol, sizes)
    }
    else
    { if (conf > 0 & conf < 1)
    { lcl <- qbinom((1 - conf)/2, sizes, pbar)
    ucl <- qbinom((1 - conf)/2, sizes, pbar, lower.tail = FALSE)
    }
      else stop("invalid conf argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    return(limits)
  }
  
  # c Chart
  
  stats.c <- function(data, sizes)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    if (length(unique(sizes)) != 1)
      stop("all sizes must be be equal for a c chart")
    statistics <- data
    center <- mean(statistics)
    list(statistics = statistics, center = center)
  }
  
  sd.c <- function(data, sizes, ...)
  {
    data <- as.vector(data)
    std.dev <- sqrt(mean(data))
    return(std.dev)
  }
  
  limits.c <- function(center, std.dev, sizes, conf)
  {
    if (conf >= 1) 
    { lcl <- center - conf * sqrt(center)
    lcl[lcl < 0] <- 0
    ucl <- center + conf * sqrt(center)
    }
    else 
    { if (conf > 0 & conf < 1) 
    { ucl <- qpois(1 - (1 - conf)/2, center)
    lcl <- qpois((1 - conf)/2, center)
    }
      else stop("invalid conf argument. See help.")
    }
    limits <- matrix(c(lcl, ucl), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    return(limits)
  }
  
  # u Chart
  
  stats.u <- function(data, sizes)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    statistics <- data/sizes
    center <- sum(sizes * statistics)/sum(sizes)
    list(statistics = statistics, center = center)
  }
  
  sd.u <- function(data, sizes, ...)
  {
    data <- as.vector(data)
    sizes <- as.vector(sizes)
    std.dev <- sqrt(sum(data)/sum(sizes))
    return(std.dev)
  }
  
  limits.u <- function(center, std.dev, sizes, conf)
  {
    sizes <- as.vector(sizes)
    if (length(unique(sizes))==1)
      sizes <- sizes[1]
    limits.c(center * sizes, std.dev, sizes, conf) / sizes
  }
  
  #
  # Functions used to signal points out of control 
  #
  
  shewhart.rules <- function(object, limits = object$limits, run.length = qcc.options("run.length"))
  {
    # Return a list of cases beyond limits and violating runs
    bl <- beyond.limits(object, limits = limits)
    vr <- violating.runs(object, run.length = run.length)
    list(beyond.limits = bl, violating.runs = vr)
  }
  
  beyond.limits <- function(object, limits = object$limits)
  {
    # Return cases beyond limits
    statistics <- c(object$statistics, object$newstats) 
    lcl <- limits[,1]
    ucl <- limits[,2]
    index.above.ucl <- seq(along = statistics)[statistics > ucl]
    index.below.lcl <- seq(along = statistics)[statistics < lcl]
    return(c(index.above.ucl, index.below.lcl))
  }
  
  violating.runs <- function(object, run.length = qcc.options("run.length"))
  {
    # Return indices of points violating runs
    if(run.length == 0)
      return(numeric())
    center <- object$center
    statistics <- c(object$statistics, object$newstats)
    cl <- object$limits
    diffs <- statistics - center
    diffs[diffs > 0] <- 1
    diffs[diffs < 0] <- -1
    runs <- rle(diffs)
    vruns <- rep(runs$lengths >= run.length, runs$lengths)
    vruns.above <- (vruns & (diffs > 0))
    vruns.below <- (vruns & (diffs < 0))
    rvruns.above <- rle(vruns.above)
    rvruns.below <- rle(vruns.below)
    vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
      (rvruns.above$lengths - run.length)[rvruns.above$values]
    vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
    vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
      (rvruns.below$lengths - run.length)[rvruns.below$values]
    vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
    violators <- numeric()
    if (length(vbeg.above)) 
    { for (i in 1:length(vbeg.above))
      violators <- c(violators, vbeg.above[i]:vend.above[i]) }
    if (length(vbeg.below)) 
    { for (i in 1:length(vbeg.below))
      violators <- c(violators, vbeg.below[i]:vend.below[i]) }
    return(violators)
  }
  
  #-------------------------------------------------------------------#
  #                                                                   #
  #          Operating Characteristic Function                        #
  #                                                                   #
  #-------------------------------------------------------------------#
  
  oc.curves <- function(object, ...)
  {
    # Draws the operating characteristic curves for the qcc object 
    
    if ((missing(object)) | (!inherits(object, "qcc")))
      stop("an object of class 'qcc' is required")
    
    size <- unique(object$sizes)
    if (length(size)>1)
      stop("Operating characteristic curves available only for equal sample sizes!")
    
    beta <- switch(object$type,
                   xbar = oc.curves.xbar(object, ...),
                   R    = oc.curves.R(object, ...),
                   S    = oc.curves.S(object, ...),
                   np   =,
                   p    = oc.curves.p(object, ...),
                   u    =,
                   c    = oc.curves.c(object, ...))
    if (is.null(beta))
      stop("Operating characteristic curves not available for this type of chart.")
    
    invisible(beta)
  }
  
  oc.curves.xbar <- function(object, n, c = seq(0, 5, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
  {
    # Draw the operating-characteristic curves for the xbar-chart with nsigmas
    # limits. The values on the vertical axis give the probability of not detecting
    # a shift of c*sigma in the mean on the first sample following the shift.
    
    if (!(object$type=="xbar"))
      stop("not a `qcc' object of type \"xbar\".")
    
    size <- unique(object$sizes)
    if (length(size) > 1)
      stop("Operating characteristic curves available only for equal sample sizes!")
    if (missing(n))
      n <- unique(c(size, c(1,5,10,15,20)))
    if (is.null(nsigmas))
      nsigmas <- qnorm(1 - (1 - object$confidence.level) / 2)
    
    beta <- matrix(as.double(NA), length(n), length(c))
    for (i in 1:length(n))
      beta[i,] <- pnorm(nsigmas-c*sqrt(n[i])) - pnorm(-nsigmas-c*sqrt(n[i]))
    rownames(beta) <- paste("n=",n,sep="")
    colnames(beta) <- c
    
    oldpar <- par(no.readonly = TRUE)
    if(restore.par) on.exit(par(oldpar))
    par(bg  = qcc.options("bg.margin"), 
        cex = oldpar$cex * qcc.options("cex"),
        mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
    
    plot(c, beta[1,], type="n",
         ylim = c(0,1), xlim = c(0,max(c)),
         xlab = "Process shift (std.dev)",
         ylab = "Prob. type II error ")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = qcc.options("bg.figure"))
    box()
    mtext(paste("OC curves for", object$type, "Chart"), 
          side = 3, line = par("mar")[3]/3,
          font = par("font.main"), 
          cex  = qcc.options("cex"), 
          col  = par("col.main"))
    for(i in 1:length(n))
      lines(c, beta[i,], type = "l", lty=i)
    beta <- t(beta)
    names(dimnames(beta)) <- c("shift (std.dev)", "sample size")
    
    if (identify)
    { cs <- rep(c,length(n))
    betas <- as.vector(beta)
    labels <- paste("c=", formatC(cs, 2, flag="-"), 
                    ": beta=", formatC(betas, 4, flag="-"), 
                    ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
    i <- identify(cs, betas, labels, pos=4, offset=0.2)
    apply(as.matrix(labels[i$ind]), 1, cat, "\n")
    }
    else
    { legend(max(c), 1, legend = paste("n =", n), 
             bg = qcc.options("bg.figure"),
             lty = 1:length(n), xjust = 1, yjust = 1)
    }
    invisible(beta)
  }
  
  oc.curves.p <- function(object, nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
  {
    if (!(object$type=="p" | object$type=="np"))
      stop("not a `qcc' object of type \"p\" or \"np\".")
    
    size <- unique(object$sizes)
    if (length(size) > 1)
      stop("Operating characteristic curves available only for equal sample sizes!")
    
    if (is.null(object$limits))
      stop("the `qcc' object does not have control limits!")
    limits <- object$limits
    p <- seq(0, 1, length=101)
    
    if (object$type=="p") 
    { UCL <- min(floor(size*limits[,2]), size)
    LCL <- max(floor(size*limits[,1]), 0) }
    else
    { UCL <- min(floor(limits[,2]), size)
    LCL <- max(floor(limits[,1]), 0) }
    beta <- pbinom(UCL, size, p) - pbinom(LCL-1, size, p)
    names(beta) <- p
    
    oldpar <- par(no.readonly = TRUE)
    if(restore.par) on.exit(par(oldpar))
    par(bg  = qcc.options("bg.margin"), 
        cex = oldpar$cex * qcc.options("cex"),
        mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
    
    plot(p, beta, type = "n", 
         ylim = c(0,1), xlim = c(0,1),
         xlab = expression(p), 
         ylab = "Prob. type II error ")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = qcc.options("bg.figure"))
    box()
    mtext(paste("OC curves for", object$type, "Chart"), 
          side = 3, line = par("mar")[3]/3,
          font = par("font.main"), 
          cex  = qcc.options("cex"), 
          col  = par("col.main"))
    
    lines(p, beta)
    lines(rep(p[which.max(beta)], 2), c(0, max(beta)), lty = 2)
    
    warning("Some computed values for the type II error have been rounded due to the discreteness of the binomial distribution. Thus, some ARL values might be meaningless.")
    
    if (identify)
    { labels <- paste("p=", formatC(p, 2, flag="-"), 
                      ": beta=", formatC(beta, 4, flag="-"), 
                      ", ARL=", formatC(1/(1-beta), 2, flag="-"), sep="")
    i <- identify(p, beta, labels, pos=4, offset=0.2)
    apply(as.matrix(labels[i$ind]), 1, cat, "\n")
    }
    invisible(beta)  
  }
  
  oc.curves.c <- function(object, nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
  {
    type <- object$type
    if (!(object$type=="c" | object$type=="u"))
      stop("not a `qcc' object of type \"c\" or \"u\".")
    
    size <- unique(object$sizes)
    if (length(size) > 1)
      stop("Operating characteristic curves available only for equal sample size!")
    
    if (is.null(object$limits))
      stop("the `qcc' object does not have control limits!")
    limits <- object$limits
    CL  <- object$center
    std.dev <- object$std.dev
    if (object$type=="c") 
    { max.lambda <- ceiling(CL+10*std.dev)
    UCL <- floor(limits[1,2])
    LCL <- floor(limits[1,1])
    }
    else
    { max.lambda <- ceiling(CL*size+10*std.dev)[1]
    UCL <- floor(size*limits[1,2])
    LCL <- floor(size*limits[1,1])
    }
    lambda <- seq(0, max.lambda)
    beta <- ppois(UCL, lambda) - ppois(LCL-1, lambda)
    names(beta) <- lambda
    
    oldpar <- par(no.readonly = TRUE)
    if(restore.par) on.exit(par(oldpar))
    par(bg  = qcc.options("bg.margin"), 
        cex = oldpar$cex * qcc.options("cex"),
        mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
    
    plot(lambda, beta, type = "n", 
         ylim = c(0,1), xlim = range(lambda),
         xlab = "Mean", 
         ylab = "Prob. type II error ")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
         col = qcc.options("bg.figure"))
    box()
    mtext(paste("OC curves for", object$type, "Chart"), 
          side = 3, line = par("mar")[3]/3,
          font = par("font.main"), 
          cex  = qcc.options("cex"), 
          col  = par("col.main"))
    
    lines(lambda, beta)     
    lines(rep(lambda[which.max(beta)], 2), c(0, max(beta)), lty = 2)
    
    warning("Some computed values for the type II error have been rounded due to the discreteness of the Poisson distribution. Thus, some ARL values might be meaningless.")
    
    if (identify)
    { labels <- paste("lambda=", formatC(lambda, 0, flag="-"), 
                      ": beta=", formatC(beta, 4, flag="-"), 
                      ", ARL=", formatC(1/(1-beta), 2, flag="-"), sep="")
    i <- identify(lambda, beta, labels, pos=4, offset=0.2)
    apply(as.matrix(labels[i$ind]), 1, cat, "\n")
    }
    invisible(beta)
  }
  
  oc.curves.R <-
    function(object, n, c = seq(1, 6, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
    {
      # Draw the operating-characteristic curves for the R-chart with nsigmas
      # limits. The values on the vertical axis give the probability of not detecting
      # a change from sigma to c*sigma on the first sample following the change.
      
      if (!(object$type=="R"))
        stop("not a `qcc' object of type \"R\".")
      
      size <- unique(object$sizes)
      if (length(size) > 1)
        stop("Operating characteristic curves available only for equal sample sizes!")
      if (missing(n))
        n <- unique(c(size, c(2,5,10,15,20)))
      if (is.null(nsigmas))
      { tail.prob <- (1 - object$confidence.level) / 2
      beta.fun1 <- function(c, n, p)
      {
        lcl <- qtukey(p, n, Inf)
        ucl <- qtukey(p, n, Inf, lower.tail = FALSE)
        ptukey(ucl / c, n, Inf) - ptukey(lcl / c, n, Inf)
      }
      beta <- outer(c, n, beta.fun1, tail.prob)
      }
      else
      { exp.R.unscaled <- qcc.options("exp.R.unscaled")
      se.R.unscaled <- qcc.options("se.R.unscaled")
      Rtab <- min(length(exp.R.unscaled), length(se.R.unscaled))
      if (any(n > Rtab))
        stop(paste("group size must be less than",
                   Rtab + 1, "when giving nsigmas"))
      beta.fun2 <- function(c, n, conf)
      {
        d2 <- exp.R.unscaled[n]
        d3 <- se.R.unscaled[n]
        lcl <- pmax(0, d2 - conf * d3)
        ucl <- d2 + conf * d3
        ptukey(ucl / c, n, Inf) - ptukey(lcl / c, n, Inf)
      }
      beta <- outer(c, n, beta.fun2, nsigmas)
      }
      
      colnames(beta) <- paste("n=",n,sep="")
      rownames(beta) <- c
      
      oldpar <- par(no.readonly = TRUE)
      if(restore.par) on.exit(par(oldpar))
      par(bg  = qcc.options("bg.margin"), 
          cex = oldpar$cex * qcc.options("cex"),
          mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
      
      plot(c, beta[,1], type="n",
           ylim = c(0,1), xlim = c(1,max(c)),
           xlab = "Process scale multiplier",
           ylab = "Prob. type II error ")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
           col = qcc.options("bg.figure"))
      box()
      mtext(paste("OC curves for", object$type, "Chart"), 
            side = 3, line = par("mar")[3]/3,
            font = par("font.main"), 
            cex  = qcc.options("cex"), 
            col  = par("col.main"))
      
      matlines(c, beta, lty = 1:length(n), col = 1)
      
      names(dimnames(beta)) <- c("scale multiplier", "sample size")
      
      if (identify)
      { cs <- rep(c,length(n))
      betas <- as.vector(beta)
      labels <- paste("c=", formatC(cs, 2, flag="-"),
                      ": beta=", formatC(betas, 4, flag="-"),
                      ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
      i <- identify(cs, betas, labels, pos=4, offset=0.2)
      apply(as.matrix(labels[i$ind]), 1, cat, "\n")
      }
      else
      { legend(max(c), 1, legend = paste("n =", n),
               bg = qcc.options("bg.figure"),
               lty = 1:length(n), xjust = 1, yjust = 1)
      }
      invisible(beta)
    }
  
  oc.curves.S <- function(object, n, c = seq(1, 6, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
  {
    # Draw the operating-characteristic curves for the S-chart with nsigmas
    # limits. The values on the vertical axis give the probability of not detecting
    # a change from sigma to c*sigma on the first sample following the change.
    
    if (!(object$type=="S"))
      stop("not a `qcc' object of type \"S\".")
    
    size <- unique(object$sizes)
    if (length(size) > 1)
      stop("Operating characteristic curves available only for equal sample sizes!")
    if (missing(n))
      n <- unique(c(size, c(2,5,10,15,20)))
    if (is.null(nsigmas))
    { tail.prob <- (1 - object$confidence.level) / 2
    beta.fun1 <- function(c, n, p)
    {
      ucl <- sqrt(qchisq(1 - p, n - 1) / (n - 1))
      lcl <- sqrt(qchisq(p, n - 1) / (n - 1))
      pchisq((n - 1) * (ucl / c)^2, n - 1) - pchisq((n - 1)* (lcl / c)^2, n - 1)
    }
    beta <- outer(c, n, beta.fun1, tail.prob)
    }
    else
    { c4 <- .qcc.c4
    beta.fun2 <- function(c, n)
    {
      center <- c4(n)
      tol <- sqrt(1 - c4(n)^2)
      lcl <- pmax(0, center - nsigmas * tol)
      ucl <- center + nsigmas * tol
      pchisq((n - 1) * (ucl / c)^2, n - 1) - pchisq((n - 1) * (lcl / c)^2, n - 1)
    }
    beta <- outer(c, n, beta.fun2)
    }
    
    colnames(beta) <- paste("n=",n,sep="")
    rownames(beta) <- c
    
    oldpar <- par(no.readonly = TRUE)
    if(restore.par) on.exit(par(oldpar))
    par(bg  = qcc.options("bg.margin"), 
        cex = oldpar$cex * qcc.options("cex"),
        mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
    
    plot(c, beta[,1], type="n",
         ylim = c(0,1), xlim = c(1,max(c)),
         xlab = "Process scale multiplier",
         ylab = "Prob. type II error ")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = qcc.options("bg.figure"))
    box()
    mtext(paste("OC curves for", object$type, "Chart"), 
          side = 3, line = par("mar")[3]/3,
          font = par("font.main"), 
          cex  = qcc.options("cex"), 
          col  = par("col.main"))
    matlines(c, beta, lty = 1:length(n), col = 1)
    
    names(dimnames(beta)) <- c("scale multiplier", "sample size")
    
    if (identify)
    { cs <- rep(c,length(n))
    betas <- as.vector(beta)
    labels <- paste("c=", formatC(cs, 2, flag="-"),
                    ": beta=", formatC(betas, 4, flag="-"),
                    ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
    i <- identify(cs, betas, labels, pos=4, offset=0.2)
    apply(as.matrix(labels[i$ind]), 1, cat, "\n")
    }
    else
    { legend(max(c), 1, legend = paste("n =", n),
             bg = qcc.options("bg.figure"),
             lty = 1:length(n), xjust = 1, yjust = 1)
    }
    invisible(beta)
  }
  
  #-------------------------------------------------------------------#
  #                                                                   #
  # Miscellaneous functions                                           #
  #                                                                   #
  #-------------------------------------------------------------------#
  
  qcc.groups <- function(data, sample)
  {
    if(length(data)!=length(sample))
      stop("data and sample must be vectors of equal length")
    x <- lapply(split(data, sample), as.vector)
    lx <- sapply(x, length)
    for(i in which(lx != max(lx)))
      x[[i]] <- c(x[[i]], rep(NA, max(lx)-lx[i]))
    x <- t(sapply(x, as.vector))
    return(x)
  }
  
  qcc.overdispersion.test <- function(x, size, 
                                      type=ifelse(missing(size), "poisson", "binomial"))
  {
    type <- match.arg(type, c("poisson", "binomial"))
    if (type=="binomial" & missing(size))
      stop("binomial data require argument \"size\"")
    if (!missing(size))
      if (length(x) != length(size))   
        stop("arguments \"x\" and \"size\" must be vector of same length")
    
    n <- length(x)
    obs.var <- var(x)
    if (type=="binomial")
    { p <- sum(x)/sum(size)
    theor.var <- mean(size)*p*(1-p) }
    else if (type=="poisson")
    { theor.var <- mean(x) }
    else
      stop("invalid \"type\" argument. See help.")
    
    D <- (obs.var * (n-1)) / theor.var
    p.value <- 1-pchisq(D, n-1)
    
    out <- matrix(c(obs.var/theor.var, D, signif(p.value,5)), 1, 3)
    rownames(out) <- paste(type, "data")
    colnames(out) <- c("Obs.Var/Theor.Var", "Statistic", "p-value") 
    names(dimnames(out)) <- c(paste("Overdispersion test"), "")
    return(out)
  }
  
  #########################################################################common UI ###################################################################################################
  output[["commonui"]] <- renderUI({
  
    fluidPage(
      
      
      tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                                    overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
      tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                                    overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
      
      
      # br(),
      # br(),
      # 
      # 
      # column(7,
      #        
      #        # tags$h3(strong(em("Aim of this Analysi(s):")),style="text-align:center;color:#004264;font-size:180%"),br(),
      #        # tags$div(h4("The identification of rare items, events or observations which raise suspicions",style="text-align:center;color:dimgrey"),align="center"),
      #        # tags$div(h4("by differing significantly from the majority of the data.",style="text-align:center;color:dimgrey"),align="center"),
      #        br(),br(),br(),br(),br(),
      #        tags$div(id = 'logo1',img(src="ee.jpg",height='90%',width='90%'),align="center")
      #        
      # ),
      # 
      # br(),
    br(),br(),
    
    column(12,
           
           
           bootstrapPage(useShinyjs(),
                         br(),
                         
                         
                         tags$h3(strong(em("Statistical Analysis")),style="text-align:center;color:#034e91;font-size:180%"),
                                        br(),
                                        
                                        tags$div(id = 'logo20',img(src="c.png",height='23%',width='23%'),align="center"),
                                        uiOutput("button20"),
                                        
                                        br(),
                                        uiOutput('fileupload20'), uiOutput("bspop20"),
                                        uiOutput('checkbox20'),
                                     
                                        br(),
                                        br(),
                                        bsPopover(id="check20",title = "",content = "Note: I accept Terms & Conditions.. Show the Analyse button",placement = "right"),
                                        tags$div(bsButton("reset20", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                        
                                        br(),
                                        
                                        tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                 tags$tbody("Need Help ?"),
                                                 tags$a(href = "https://github.com/sailogeshh", "Contact ")
                                        )
                         )
                         )
                         
                         
                         
           )
})
  
  observeEvent(input$reset20,{
    reset(id = "file20")
  })
  
  output[["fileupload20"]] <- renderUI({
    input$reset20
    tags$div(fileInput("file20",label = tags$h4(strong(em("Upload data..")),style="color:#034e91;font-size:160%"),
                       accept=c('csv','comma-seperated-values','.csv')),align="center")#
    
  })
  
  output[["checkbox20"]] <- renderUI({
    input$reset20
    tags$div(checkboxInput("check20",tags$a(href = "https://github.com/sailogeshh", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
    
  })
  
  output[["bspop20"]] <- renderUI({
    bsPopover(id="fileupload20",title = "",content = "To get results, click the Lets go! button...",placement = "top")
  })
  

  data20 <-reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    # data=data.frame(abs(data))
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data
    
  })
########################################################################## COUNT UI & SERVER #########################################################################################################################################

  output[["count1"]] <- renderUI({ 
      
      file1 <- input$file20
      data0 <- data.frame(read.csv(file1$datapath))
      selectInput(inputId ="columnx",
                  label = "Choose Column:",
                  choices = names(data0),
                  selected = character(0))
    
    
  })


  dt_f <- reactive({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    req(input$columnx)
    df %>% 
      group_by(!! sym(input$columnx)) %>%
      summarise (Count = n(),percent = paste(round(100 * n() / nrow( df ),2),"%"))# %>% #, yield = round(mean(value)*100, 1)) %>%
      #mutate(Pct_of_value = paste0(round(100 * value/sum(value), 0), "%"))
  })
  
  
  output$datasum333 <- renderText({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    dd=input$columnx
    data=df[dd]
    colnames(data)="y"
    paste0("Total ",dd," :  ",round(data %>% summarise(Total = sum(y) ),2))
  })

  output$tablex <- renderTable(dt_f())
  output$datasum <- renderText({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    paste0("Total No.of Rows:  ",dim(df)[1])
  })
  
  
  output$datasum2 <- renderText({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    paste0("Total No.of Cloumns: ",dim(df)[2])
  })
  
  ########################################################################## Total UI & SERVER #########################################################################################################################################
  
  output[["total1"]] <- renderUI({ 
    
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    data01 <- dplyr::select_if(data0, is.numeric)
    selectInput(inputId ="columny",
                label = "Choose Column for Total",
                choices = names(data01),
                selected = character(0))
    
    
  })
  
  
  dt_y <- reactive({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    dd=input$columny
    data=df[dd]
    colnames(data)="y"
    
    #sum(data)
     data %>% summarise(Total = sum(y) )
     
    #   group_by(!! sym(input$columny)) %>%
    #   sum(df)# %>% #, yield = round(mean(value)*100, 1)) %>%
    # #mutate(Pct_of_value = paste0(round(100 * value/sum(value), 0), "%"))
  })
  
  output$tabley <- renderTable(dt_y())
  
  output$datasum3 <- renderText({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    dd=input$columny
    data=df[dd]
    colnames(data)="y"
    paste0("Total :  ",round(data %>% summarise(Total = sum(y) ),2))
  })

########################################################################## summary UI & SERVER #########################################################################################################################################
  get_var_names234 <- reactive({
    file <- input$file20
    var_names <- names(read.csv(file$datapath))
    var_names
    
  })
  output$sssl <- renderUI({
    choices <- get_var_names234()
    file <- input$file20
    if( !is.null(file) )    {
      selectInput(inputId = "selsum", label = "Choose Variable:", choices = choices)
      
    }
  })
  
  output$summary1 <- renderPlotly({
    
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    dd=input$selsum
    data=data0[dd]
    x=data.frame(do.call(cbind, lapply(data, summary)))
    x2=data.frame(round(x,2))
    df<- data.frame(x=c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum"),x2,row.names = NULL)
    colors <- c('#1b78be', '#f78018', '#28a327','#d22b23', '#9764c3', '#8c564a')
    fig <- plot_ly(
      x = df[,1],
      y = df[,2],
      text =  df[,2],
      marker = list(color = colors), 
      textposition = 'auto',
      name = "SF Zoo",
      type = "bar"
    )
    
    fig
    # L <- c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum")
    # # data=read.csv("weight-height3.csv")
    # x=data.frame(do.call(cbind, lapply(data, summary)))
    # x2=data.frame(round(x,2))
    # df<- data.frame(x=colnames(x2),as.data.frame(t(x2)),row.names = NULL)
    # colnames(df)<- c("x","Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum")
    # df$x <- factor(df$x, levels = df[["x"]])
    # P <- plot_ly(data = df)
    # for(k in 1:length(L)) {
    #   dfk <- data.frame(y=df[[L[k]]], x=df$x)
    #   P <- add_trace(P, y=~y, x=~x,  data=dfk, 
    #                  type="bar", name = L[k])
    # }
    # P
    
  })
  

########################################################################## BENFORD UI & SERVER #########################################################################################################################################
  
  output[["benford1"]] <- renderUI({ 
    fluidPage(
     
      # h3("Benford's Law:"),# US Census Data I",HTML("&ndash;"),"Population Estimates"),
      # 
      # HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
      
      fluidRow(
        column(4,wellPanel(
          
          uiOutput("chos2"),
          uiOutput("chosbut"),
          
          br(), 
          
          uiOutput("dpo")
          
         
        ) # Close wellPanel
        ),
      column(8,wellPanel(
             fluidRow(
        column(4,
               p(tags$b("Goodness of Fit Test:")),
               verbatimTextOutput("goodness.cens")
        ),
        column(8,
               plotlyOutput("cens.pmf")
        )#closes column-8
      )),#closes fluidRow
      HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
      
    )
    ))
    
    
  })
  
output$chos2 <- renderUI({
  file1 <- input$file20
  df <- data.frame(read.csv(file1$datapath))
  selectInput("sel.var.cens", label = h5("Select Variable:"),
              choices =names(df),
              selected = character(0))
  
})

output$dpo <- renderUI({
  div("App by", a(href="https://github.com/sailogeshh",target="_blank",
                "Sailogesh"),align="right", style = "font-size: 8pt")
})

output$chosbut <- renderUI({
  div(actionButton("chosesub",label = "Submit",icon = icon("refresh")),align="right")
})

goodness.test <- function(sequence){
  
  string <- substr(as.character(sequence),1,1)
  
  first.digits <- as.integer(string)
  
  # In the event any of the 9 possible digits is not observed in the string,
  # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
  #
  # NAME     1  2  3  4  5  6  7  9
  # FREQ    12 11  9  8  6  5  4  2
  #
  # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
  # To correct for this, initialize obs to a zero vector, loop through NAME,
  # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
  # This will also work for the case when all 9 possible digits are observed.
  
  Obs <- seq(0,0,,9)
  
  for (i in as.numeric(names(table(first.digits))))  {
    pos <- which(as.numeric(names(table(first.digits)))==i)
    Obs[i]<- as.numeric(table(first.digits))[pos]
  }
  i <- seq(1,9)
  
  Exp <- sum(Obs)*log10(1+1/i)
  
  Digit <- seq(1,9)
  
  #print(info)
  
  chisq <- sum(((Obs-Exp)**2)/Exp)
  
  Exp <- round(Exp,3)
  info <- cbind(Digit,Obs,Exp)
  rownames(info)<-rep("",nrow(info))
  
  #print(chisq)
  
  p.value <- 1-pchisq(chisq,8)
  
  text.chi <- paste("Chi-Square = ",format(round(chisq,3),nsmall=3),sep="")
  names(text.chi)<-""
  
  text.pval <- paste("P-Value = ",format(round(p.value,3),nsmall=3),sep="")
  names(text.pval)<-""
  
  data.frame(info,x=c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine"))
  
}
observeEvent(input$chosesub,{
output[["goodness.cens"]] <- renderPrint({
 
  goodness.test <- function(sequence){
    
    string <- substr(as.character(sequence),1,1)
    
    first.digits <- as.integer(string)
    
    # In the event any of the 9 possible digits is not observed in the string,
    # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
    #
    # NAME     1  2  3  4  5  6  7  9
    # FREQ    12 11  9  8  6  5  4  2
    #
    # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
    # To correct for this, initialize obs to a zero vector, loop through NAME,
    # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
    # This will also work for the case when all 9 possible digits are observed.
    
    Obs <- seq(0,0,9)
    
    for (i in as.numeric(names(table(first.digits))))  {
      pos <- which(as.numeric(names(table(first.digits)))==i)
      Obs[i]<- as.numeric(table(first.digits))[pos]
    }
    i <- seq(1,9)
    
    Exp <- sum(Obs)*log10(1+1/i)
    
    Digit <- seq(1,9)
    
    #print(info)
    
    chisq <- sum(((Obs-Exp)**2)/Exp)
    
    Exp <- round(Exp,3)
    info <- cbind(Digit,Obs,Exp)
    rownames(info)<-rep("",nrow(info))
    
    #print(chisq)
    
    p.value <- 1-pchisq(chisq,8)
    
    text.chi <- paste("Chi-Square = ",format(round(chisq,3),nsmall=3),sep="")
    names(text.chi)<-""
    
    text.pval <- paste("P-Value = ",format(round(p.value,3),nsmall=3),sep="")
    names(text.pval)<-""
    
    print(info)
    print(text.chi,quote=F)
    print(text.pval,quote=F)
    
  }
  withProgress(message = 'Performing Goodness of Fit', style = 'notification', value = 0.75, {
    Sys.sleep(0.6)
  file1 <- input$file20
  df <- data.frame(read.csv(file1$datapath))
  dd=input$sel.var.cens
  df2=data.frame(df[dd])
  goodness.test(df2[,1])
  })
  })
})
observeEvent(input$chosesub,{
output[["cens.pmf"]] <- renderPlotly({
  
  withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    Sys.sleep(0.6)
  
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    dd=input$sel.var.cens
    df2=data.frame(df[dd])
  d=goodness.test(df2[,1])
  d$x <- factor(d$x, levels = d[["x"]])
  fig <- plot_ly(d, x = ~x  , y = ~Obs, type = 'bar',name = "Observed")
  fig <- fig %>% add_trace(y = ~Exp,name = "Benford Expected")
  fig <- fig %>% layout(yaxis = list(title = 'Count'),xaxis = list(title = 'Digits'), barmode = 'group')
  })
  fig
})
})

########################################################################## AGEING UI & SERVER #########################################################################################################################################


output[["ageing1"]] <- renderUI({ 
  fluidPage(
    
    # h3("Benford's Law:"),# US Census Data I",HTML("&ndash;"),"Population Estimates"),
    # 
    # HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
    
    fluidRow(
      column(3,wellPanel(
        uiOutput("chos0"),
        uiOutput("chos3"),
        uiOutput("asof"),
        uiOutput("daate"),
        uiOutput("chos4"),
        uiOutput("chos5"),
        uiOutput("chosbut3"),
        uiOutput("tiptool"),
        
        br(), 
        
        uiOutput("dpo2")
        
        
      ) # Close wellPanel
      ),
      column(9,wellPanel(
        fluidRow(
         
                 p(tags$b("Accounts Receivable Aging method:")),
                
                 #dataTableOutput("goodness.cens2")
                 uiOutput("downage"),br(),
                 column(12, column(6,plotlyOutput("cens.pmf4")),
                        column(6,plotlyOutput("cens.pmf5"))),
                 
                 
          column(12,br(),column(6,plotlyOutput("cens.pmf2")),
                 column(6,plotlyOutput("cens.pmf3"))),
         
          )#closes column-8
        ),#closes fluidRow
        HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
        
      )
    ))
  
  
})

output$chos0 <- renderUI({
  file1 <- input$file20
  df <- data.frame(read.csv(file1$datapath))
  selectInput("sel.var.cens0", label = h5("Customer Ref.ID:"),
              choices =names(df),
              selected = character(0))
  
})

output$asof <- renderUI({
   selectInput("selasof", label = h5("Ageing by:"),
              choices =c("Today","Particular Date"),
              selected = character(0))
  
})

output$daate <- renderUI({
  if (!input$selasof=="Particular Date")return()
    dateInput("seldate", label = h5("Select Date:"),value= today())

  })

output$chos3 <- renderUI({
  file1 <- input$file20
  df <- data.frame(read.csv(file1$datapath))
  selectInput("sel.var.cens3", label = h5("Inv.Date (dd-mm-YYYY):"),
              choices =names(df),
              selected = character(0))
  
})

output$chos4 <- renderUI({
   numericInput("sel.var.cens4", label = h5("Inv.Due (days):"),
              value = 30)
  
})

output$chos5 <- renderUI({
  file1 <- input$file20
  df <- data.frame(read.csv(file1$datapath))
  selectInput("sel.var.cens5", label = h5("Inv.Amount:"),
              choices =names(df),
              selected = character(0))
  
})



output$dpo2 <- renderUI({
  div("App by", a(href="https://github.com/sailogeshh",target="_blank",
                  "sailogesh"),align="right", style = "font-size: 8pt")
})

output$chosbut3 <- renderUI({
  div(actionButton("chosesub2",label = "Submit",icon = icon("refresh")),align="right")
})
output$tiptool <- renderUI({
  bsPopover("chosbut3",title = "Note:",content = "Due Date is compared with Today..")
})



observeEvent(input$chosesub2,{
output$downage <- renderUI({
  tags$div(downloadButton("datadownage",label = "Download..!"),align="center")
})
})

output$datadownage <- downloadHandler(
  filename = function() {
    paste("Ageing output.csv", sep = "")
  },
  content = function(file) {
    
    write.csv(ageout(), file, row.names = FALSE)
  }
)

ageout <- reactive({
  file1 <- input$file20
  data <- data.frame(read.csv(file1$datapath))
  
  one <- input$sel.var.cens0
  two <- input$sel.var.cens3
  #three <- input$sel.var.cens4
  four <- input$sel.var.cens5
  
  df <- data.frame(data[,c(one,two,four)])
  colnames(df) <- c("ID","indate","amount")
  #df=read.csv("AP_Files_2016.csv")
  
  df$indate <- as.Date(df$indate,format="%d-%m-%Y")
  df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
  
  if (input$selasof=="Today") {
      df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
      }
  if (input$selasof=="Particular Date") {
      df$Days_Outstanding <-  ifelse(input$seldate<df$Due_Date,0,input$seldate-df$Due_Date)
      }
    
  df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
  df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
  df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
  df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
  df$Due_91to120days <- ifelse(df$Days_Outstanding <=120 ,ifelse(df$Days_Outstanding >90,df$amount,0),0)
  df$Due_121to180days <- ifelse(df$Days_Outstanding <=180 ,ifelse(df$Days_Outstanding >120,df$amount,0),0)
  df$Due_181to365days <- ifelse(df$Days_Outstanding <=365 ,ifelse(df$Days_Outstanding >180,df$amount,0),0)
  df$Due_morethan_365days <- ifelse(df$Days_Outstanding >=365 ,df$amount,0)
  colnames(df) <- c(one,two,four,"Due Date","Days Outstanding","No Due","Due 1-30days","Due 31-60days","Due 61-90days",
                    "Due 91-120days","Due 121-180days","Due 181-365days",">365days")
  df
})


# observeEvent(input$chosesub2,{
# output$goodness.cens2 <- renderDataTable({
#   file1 <- input$file20
#   data <- data.frame(read.csv(file1$datapath))
#   
#   one <- input$sel.var.cens0
#   two <- input$sel.var.cens3
#   #three <- input$sel.var.cens4
#   four <- input$sel.var.cens5
#   
#   df <- data.frame(data[,c(one,two,four)])
#   colnames(df) <- c("ID","indate","amount")
#   #df=read.csv("AP_Files_2016.csv")
#   
#   df$indate <- as.Date(df$indate,format="%d-%m-%Y")
#   df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
#   df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
#   df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
#   df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
#   df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
#   df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
#   df$Due_morethan_90days <- ifelse(df$Days_Outstanding >=90 ,df$amount,0)
#   colnames(df) <- c(one,two,four,"Due Date","Days Outstanding","No Due","Due 1-30days","Due 31-60days","Due 61-90days",">90days")
#   df
# })
# })
observeEvent(input$chosesub2,{
  output$cens.pmf2 <- renderPlotly({
    
    file1 <- input$file20
    data <- data.frame(read.csv(file1$datapath))
    
    one <- input$sel.var.cens0
    two <- input$sel.var.cens3
    #three <- input$sel.var.cens4
    four <- input$sel.var.cens5
    
    df <- data.frame(data[,c(one,two,four)])
    colnames(df) <- c("ID","indate","amount")
    #df=read.csv("AP_Files_2016.csv")
    
    df$indate <- as.Date(df$indate,format="%d-%m-%Y")
    df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
    if (input$selasof=="Today") {
      df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
    }
    if (input$selasof=="Particular Date") {
      df$Days_Outstanding <-  ifelse(input$seldate<df$Due_Date,0,input$seldate-df$Due_Date)
    }
    df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
    df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
    df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
    df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
    df$Due_91to120days <- ifelse(df$Days_Outstanding <=120 ,ifelse(df$Days_Outstanding >90,df$amount,0),0)
    df$Due_121to180days <- ifelse(df$Days_Outstanding <=180 ,ifelse(df$Days_Outstanding >120,df$amount,0),0)
    df$Due_181to365days <- ifelse(df$Days_Outstanding <=365 ,ifelse(df$Days_Outstanding >180,df$amount,0),0)
    df$Due_morethan_365days <- ifelse(df$Days_Outstanding >=365 ,df$amount,0)
    
    # df$Due_1to30days[df$Due_1to30days==0] <- NA
    # df$Due_31to60days[df$Due_31to60days==0] <- NA
    # df$Due_61to90days[df$Due_61to90days==0] <- NA
    # df$Due_91to120days[df$Due_91to120days==0] <- NA
    # df$Due_121to180days[df$Due_121to180days==0] <- NA
    # df$Due_181to365days[df$Due_181to365days==0] <- NA
    # df$Due_morethan_365days[df$Due_morethan_365days==0] <- NA
    
    dfp=data.frame(x=c("1-30","31-60","61-90",
                       "91-120","121-180","181-365",
                       ">365"),
                   value=rbind(sum(df$Due_1to30days),sum(df$Due_31to60days),sum(df$Due_61to90days),
                               sum(df$Due_91to120days),sum(df$Due_121to180days),sum(df$Due_181to365days),
                               sum(df$Due_morethan_365days))       )
    percent=round(dfp$value/sum(dfp$value)*100,2)
    dfplot=data.frame(dfp,percent)
    dfplot$x <- factor(dfplot$x, levels = dfplot[["x"]])
    # df1 <-data.frame(df[complete.cases(df$Due_1to30days),])
    # df2 <-data.frame(df[complete.cases(df$Due_31to60days),])
    # df3 <-data.frame(df[complete.cases(df$Due_61to90days),])
    # df4 <-data.frame(df[complete.cases(df$Due_morethan_90days),])
    
    plot_ly( x = dfplot$x, y = dfplot$value, type = 'bar',
             marker = list(color = 'rgba(219, 64, 82, 0.7)',
                           line = list(color = 'rgba(219, 64, 82, 1.0)',
                                       width = 2)))%>%
      add_annotations(text=round(dfplot$value,0),showarrow = TRUE) %>%
      layout(title = 'Amount Dues',
             yaxis = list(title = paste0("Due Amount"),showticklabels = FALSE),
             xaxis = list(title = paste0("Days")), barmode = 'group')
    
  })
})

observeEvent(input$chosesub2,{
  output$cens.pmf3 <- renderPlotly({
    
    file1 <- input$file20
    data <- data.frame(read.csv(file1$datapath))
    
    one <- input$sel.var.cens0
    two <- input$sel.var.cens3
    #three <- input$sel.var.cens4
    four <- input$sel.var.cens5
    
    df <- data.frame(data[,c(one,two,four)])
    colnames(df) <- c("ID","indate","amount")
    #df=read.csv("AP_Files_2016.csv")
    
    df$indate <- as.Date(df$indate,format="%d-%m-%Y")
    df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
    if (input$selasof=="Today") {
      df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
    }
    if (input$selasof=="Particular Date") {
      df$Days_Outstanding <-  ifelse(input$seldate<df$Due_Date,0,input$seldate-df$Due_Date)
    }
    df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
    df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
    df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
    df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
    df$Due_91to120days <- ifelse(df$Days_Outstanding <=120 ,ifelse(df$Days_Outstanding >90,df$amount,0),0)
    df$Due_121to180days <- ifelse(df$Days_Outstanding <=180 ,ifelse(df$Days_Outstanding >120,df$amount,0),0)
    df$Due_181to365days <- ifelse(df$Days_Outstanding <=365 ,ifelse(df$Days_Outstanding >180,df$amount,0),0)
    df$Due_morethan_365days <- ifelse(df$Days_Outstanding >=365 ,df$amount,0)
    
    # df$Due_1to30days[df$Due_1to30days==0] <- NA
    # df$Due_31to60days[df$Due_31to60days==0] <- NA
    # df$Due_61to90days[df$Due_61to90days==0] <- NA
    # df$Due_91to120days[df$Due_91to120days==0] <- NA
    # df$Due_121to180days[df$Due_121to180days==0] <- NA
    # df$Due_181to365days[df$Due_181to365days==0] <- NA
    # df$Due_morethan_365days[df$Due_morethan_365days==0] <- NA
    
    dfp=data.frame(x=c("1-30","31-60","61-90",
                       "91-120","121-180","181-365",
                       ">365"),
                   value=rbind(sum(df$Due_1to30days),sum(df$Due_31to60days),sum(df$Due_61to90days),
                               sum(df$Due_91to120days),sum(df$Due_121to180days),sum(df$Due_181to365days),
                               sum(df$Due_morethan_365days))       )
    percent=round(dfp$value/sum(dfp$value)*100,2)
    dfplot=data.frame(dfp,percent)
    dfplot$x <- factor(dfplot$x, levels = dfplot[["x"]])
    # df1 <-data.frame(df[complete.cases(df$Due_1to30days),])
    # df2 <-data.frame(df[complete.cases(df$Due_31to60days),])
    # df3 <-data.frame(df[complete.cases(df$Due_61to90days),])
    # df4 <-data.frame(df[complete.cases(df$Due_morethan_90days),])
    
    plot_ly( x = dfplot$x, y = dfplot$percent, type = 'bar',
             marker = list(color = 'rgba(219, 64, 82, 0.7)',
                           line = list(color = 'rgba(219, 64, 82, 1.0)',
                                       width = 2)))%>%
      add_annotations(text=paste0(round(dfplot$percent,0),"%"),showarrow = TRUE) %>%
      layout(title = 'Amount Dues(%)',
             yaxis = list(title = paste0("Due Amount"),showticklabels = FALSE),
             xaxis = list(title = paste0("Days")), barmode = 'group')
  })
})

observeEvent(input$chosesub2,{
  output$cens.pmf4 <- renderPlotly({

    file1 <- input$file20
    data <- data.frame(read.csv(file1$datapath))

    one <- input$sel.var.cens0
    two <- input$sel.var.cens3
    #three <- input$sel.var.cens4
    four <- input$sel.var.cens5

    df <- data.frame(data[,c(one,two,four)])
    colnames(df) <- c("ID","indate","amount")
    #df=read.csv("AP_Files_2016.csv")

    df$indate <- as.Date(df$indate,format="%d-%m-%Y")
    df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
    if (input$selasof=="Today") {
      df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
    }
    if (input$selasof=="Particular Date") {
      df$Days_Outstanding <-  ifelse(input$seldate<df$Due_Date,0,input$seldate-df$Due_Date)
    }
    df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
    df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
    df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
    df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
    df$Due_91to120days <- ifelse(df$Days_Outstanding <=120 ,ifelse(df$Days_Outstanding >90,df$amount,0),0)
    df$Due_121to180days <- ifelse(df$Days_Outstanding <=180 ,ifelse(df$Days_Outstanding >120,df$amount,0),0)
    df$Due_181to365days <- ifelse(df$Days_Outstanding <=365 ,ifelse(df$Days_Outstanding >180,df$amount,0),0)
    df$Due_morethan_365days <- ifelse(df$Days_Outstanding >=365 ,df$amount,0)
    
    df$Due_1to30days[df$Due_1to30days==0] <- NA
    df$Due_31to60days[df$Due_31to60days==0] <- NA
    df$Due_61to90days[df$Due_61to90days==0] <- NA
    df$Due_91to120days[df$Due_91to120days==0] <- NA
    df$Due_121to180days[df$Due_121to180days==0] <- NA
    df$Due_181to365days[df$Due_181to365days==0] <- NA
    df$Due_morethan_365days[df$Due_morethan_365days==0] <- NA
   
    df1 <-data.frame(df[complete.cases(df$Due_1to30days),])
    df2 <-data.frame(df[complete.cases(df$Due_31to60days),])
    df3 <-data.frame(df[complete.cases(df$Due_61to90days),])
    df4 <-data.frame(df[complete.cases(df$Due_91to120days),])
    df5 <-data.frame(df[complete.cases(df$Due_121to180days),])
    df6 <-data.frame(df[complete.cases(df$Due_181to365days),])
    df7 <-data.frame(df[complete.cases(df$Due_morethan_365days),])
    
    
    
    dfp=data.frame(x=c("1-30","31-60","61-90",
                       "91-120","121-180","181-365",
                       ">365"),
                   value=rbind(dim(df1)[1],dim(df2)[1],dim(df3)[1],
                               dim(df4)[1],dim(df5)[1],dim(df6)[1],
                               dim(df7)[1])       )
    percent=round(dfp$value/sum(dfp$value)*100,2)
    dfplot=data.frame(dfp,percent)
    dfplot$x <- factor(dfplot$x, levels = dfplot[["x"]])

    plot_ly( x = dfplot$x, y = dfplot$value, type = 'bar',
             marker = list(color = 'rgba(55, 128, 191, 0.7)',
                           line = list(color = 'rgba(55, 128, 191, 0.7)',
                                       width = 2)))%>%
      add_annotations(text=dfplot$value,showarrow = TRUE) %>%
      layout(title = 'No.of Pending Customers',
             yaxis = list(title = paste0("Customers"),showticklabels = FALSE),
             xaxis = list(title = paste0("Days")), barmode = 'group')


  })
})

observeEvent(input$chosesub2,{
  output$cens.pmf5 <- renderPlotly({

    file1 <- input$file20
    data <- data.frame(read.csv(file1$datapath))

    one <- input$sel.var.cens0
    two <- input$sel.var.cens3
    #three <- input$sel.var.cens4
    four <- input$sel.var.cens5

    df <- data.frame(data[,c(one,two,four)])
    colnames(df) <- c("ID","indate","amount")
    #df=read.csv("AP_Files_2016.csv")

    df$indate <- as.Date(df$indate,format="%d-%m-%Y")
    df$Due_Date <- as.Date(df$indate + input$sel.var.cens4,format="%d-%m-%Y")
    if (input$selasof=="Today") {
      df$Days_Outstanding <- ifelse(today()<df$Due_Date,0,today()-df$Due_Date)
    }
    if (input$selasof=="Particular Date") {
      df$Days_Outstanding <-  ifelse(input$seldate<df$Due_Date,0,input$seldate-df$Due_Date)
    }
    df$No_Due <- ifelse(df$Days_Outstanding==0,df$amount,0)
    df$Due_1to30days <- ifelse(df$Days_Outstanding <=30 ,ifelse(df$Days_Outstanding >0,df$amount,0),0)
    df$Due_31to60days <- ifelse(df$Days_Outstanding <=60 ,ifelse(df$Days_Outstanding >30,df$amount,0),0)
    df$Due_61to90days <-ifelse(df$Days_Outstanding <=90 ,ifelse(df$Days_Outstanding >60,df$amount,0),0)
    df$Due_91to120days <- ifelse(df$Days_Outstanding <=120 ,ifelse(df$Days_Outstanding >90,df$amount,0),0)
    df$Due_121to180days <- ifelse(df$Days_Outstanding <=180 ,ifelse(df$Days_Outstanding >120,df$amount,0),0)
    df$Due_181to365days <- ifelse(df$Days_Outstanding <=365 ,ifelse(df$Days_Outstanding >180,df$amount,0),0)
    df$Due_morethan_365days <- ifelse(df$Days_Outstanding >=365 ,df$amount,0)
    
    df$Due_1to30days[df$Due_1to30days==0] <- NA
    df$Due_31to60days[df$Due_31to60days==0] <- NA
    df$Due_61to90days[df$Due_61to90days==0] <- NA
    df$Due_91to120days[df$Due_91to120days==0] <- NA
    df$Due_121to180days[df$Due_121to180days==0] <- NA
    df$Due_181to365days[df$Due_181to365days==0] <- NA
    df$Due_morethan_365days[df$Due_morethan_365days==0] <- NA
    
    df1 <-data.frame(df[complete.cases(df$Due_1to30days),])
    df2 <-data.frame(df[complete.cases(df$Due_31to60days),])
    df3 <-data.frame(df[complete.cases(df$Due_61to90days),])
    df4 <-data.frame(df[complete.cases(df$Due_91to120days),])
    df5 <-data.frame(df[complete.cases(df$Due_121to180days),])
    df6 <-data.frame(df[complete.cases(df$Due_181to365days),])
    df7 <-data.frame(df[complete.cases(df$Due_morethan_365days),])
    
    
    
    dfp=data.frame(x=c("1-30","31-60","61-90",
                       "91-120","121-180","181-365",
                       ">365"),
                   value=rbind(dim(df1)[1],dim(df2)[1],dim(df3)[1],
                               dim(df4)[1],dim(df5)[1],dim(df6)[1],
                               dim(df7)[1])       )
    percent=round(dfp$value/sum(dfp$value)*100,2)
    dfplot=data.frame(dfp,percent)
    dfplot$x <- factor(dfplot$x, levels = dfplot[["x"]])
    
    plot_ly( x = dfplot$x, y = dfplot$percent, type = 'bar',
             marker = list(color = 'rgba(55, 128, 191, 0.7)',
                           line = list(color = 'rgba(55, 128, 191, 0.7)',
                                       width = 2)))%>%
      add_annotations(text=paste0(round(dfplot$percent,0),"%"),showarrow = TRUE) %>%
      layout(title = 'No.of Pending Customers(%)',
             yaxis = list(title = paste0("Customers"),showticklabels = FALSE),
             xaxis = list(title = paste0("Days")), barmode = 'group')


  })
})
########################################################################## Time Univariate UI & SERVER #########################################################################################################################################
# 
#   output[["timeunivariate"]] <- renderUI({
#     
#      fluidPage(tags$head(tags$style("#modal1 .modal-body {padding: 10px;background-color: white;}
#                        #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
#                        #modal1 .modal-dialog { width: 95%; display: inline-block; text-align: left; vertical-align: top;}
#                        #modal1 .modal-header {background-color: white; border-top-left-radius: 6px; border-top-right-radius: 6px}
#                        #modal1 .modal { text-align: center; padding-right:10px; padding-top: 24px;}
#                        #modal1 .close { font-size: 16px;}")),
#                      
#                      # 
#                      # tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}
#                      #            #modal1 .modal-dialog { width: 1400px;}
#                      #            #modal1 .modal-content  {background-color:#;}"), 
#                      
#                      tags$style(type="text/css",
#                                 ".shiny-output-error { visibility: hidden; }",
#                                 ".shiny-output-error:before { visibility: hidden; }"
#                      ),
#                      tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
#                                             overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
#                      tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
#                                             overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
#                      
#                     
#                      
#                      br(),br(),
#                      
#                      column(12,
#                             
#                             
#                             bootstrapPage(
#                               
#                               br(),
#                               tags$div(id = 'logo2',img(src="logo.png",height='20%',width='20%'),align="center"),br(),
#                               
#                               tags$div(h4(strong(em("Risk Analytics")),style="color:#2e5cb8;font-size:200%"),align="center"),
#                               
#                               
#                               
#                               br(),
#                               uiOutput('fileupload4'),
#                               #uiOutput("bss"),br(),
#                               uiOutput('checkbox4'),
#                               uiOutput("button4"),
#                               uiOutput("helptext4"),
#                               br(),
#                               br(),
#                               bsPopover(id = "dummy0004",title = "Note:",content = "XXX",placement = "right"),
#                               bsPopover(id="check4",title = "",content = "Note: I accept the erms & Conditions.. Show the Analyse button",placement = "right"),
#                               tags$div(bsButton("reset4", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
#                               
#                               
#                               #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
#                               br(),
#                               
#                               tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
#                                        tags$tbody("Need Help ?"),
#                                        tags$a(href = "https://github.com/sailogeshh", "Contact Us...", target="_blank")
#                               ),tags$div(actionLink("reset2",""),align="center"),
#                               br()
#                             )
#                      )
#                      
#            ) })
#   
#   
#   
#   
#   output[["fileupload4"]] <- renderUI({
#     input$reset4
#     tags$div(fileInput("file4",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"), accept=c('csv','comma-seperated-values','.csv')),align="center")
#     
#   })
#   
#   # output[["bss"]] <- renderUI({  
#   #   tags$div(h5("Note: Once the data was uploaded go to next tab for outputs..!",style="color:#2e5cb8;font-size:100%"),align="center")
#   # })
#   
#   
#   
#   output[["checkbox4"]] <- renderUI({
#     input$reset4
#     tags$div(checkboxInput("check4",tags$a(href = "h/", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
#     
#   })
#   
#   output[["button4"]] <- renderUI({
#     if (is.null(input$file4)) return()
#     tags$div(bsButton("analyse4",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
#              style="color:white;font-weight:100%;",align="center")
#     
#   })
#   
  output[["foot1"]] <- renderUI({
    tags$div(bsButton("level2",strong("Go for Level-2 Analysis..!"),icon = icon("refresh"),style = "primary",size="medium"),
             style="color:white;font-weight:100%;",align="right")

  })
  # 
  # observeEvent(input$analyse4, {
  #   confirmSweetAlert(
  #     session = session,
  #     inputId = "confirmation4",
  #     type = "success",
  #     title = "Make sure that the data was uploaded..!",
  #     btn_labels = c("Cancel..!", "Go for Level-1 Analysis..!"),
  #     danger_mode = TRUE
  #   )
  # })
  
  
  
  # observeEvent(input$confirmation4, {
  #   if(input$confirmation4==TRUE){
  #     showModal(tags$div(id="modal1", modalDialog(
  #       inputId = 'Dialog1',
  #       title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Level-1 Analysis<span>
  #                  <button type = "button" class="close" data-dismiss="modal" ">
  #                  <span style="color:white; ">x <span>
  #                  </button> '),
  #       size = "l",
  #       
  #       tabsetPanel(type = "tabs", 
  #                   tabPanel(tags$div(h4(strong("Anomaly-Plot"),style="color:black;font-size:120%"),align="center"), plotlyOutput('PI210A.PV_plot',height = "800px")),
  #                   tabPanel(tags$div(h4(strong("Control-Chart"),style="color:black;font-size:120%"),align="center"), plotOutput('PI210A.PV_cplot',height = "800px")),
  #                   tabPanel(tags$div(h4(strong("Box-Plot"),style="color:black;font-size:120%"),align="center"), plotOutput('PI210A.PV_boxplot',height = "800px")),
  #                   tabPanel(tags$div(h4(strong("Comparison-Plot"),style="color:black;font-size:120%"),align="center"),br(),
  #                            tags$div(downloadBttn(outputId = "down4",label = "Download Data..!",color = "success",size = "md"),align="center"),
  #                            br(),
  #                            column(12,
  #                                   
  #                                   column(4,plotlyOutput("anopast")),
  #                                   
  #                                   
  #                                   column(4,plotlyOutput("outcontrolpast")),
  #                                   
  #                                   
  #                                   column(4,plotlyOutput("outlierpast"))),
  #                            
  #                            column(12,
  #                                   
  #                                   column(4,plotlyOutput("c1p")),
  #                                   
  #                                   
  #                                   column(4,plotlyOutput("c2p")),
  #                                   
  #                                   
  #                                   column(4,plotlyOutput("c3p")))),
  #                   tabPanel(
  #                     title = tags$div(h4(strong("User Manual"),style="color:black;font-size:120%"),align="center"),
  #                     
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-01.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-02.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-03.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-04.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-05.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-06.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-07.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-08.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-09.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-10.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-11.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-12.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-13.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-14.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-15.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-16.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-17.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-18.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-19.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-20.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-21.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-22.jpg",height='100%',width='100%'),align="center"),
  #                     tags$div(id = 'rr',img(src="User Manual - fraud analytics-23.jpg",height='100%',width='100%'),align="center")
  #                     
  #                   )
  #       ),
  #       footer=  tagList(uiOutput("foot1"),
  #                        modalButton("Cancel")),
  #       easyClose = FALSE
  #     )))
  #   }
  # })
  
  observeEvent(input$level2, {
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1',
      title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Level-2 Analysis<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      size = "l",
      
      tabsetPanel(type = "tabs", 
                  tabPanel(tags$div(h4(strong("Anomaly-Plot"),style="color:black;font-size:120%"),align="center"), plotlyOutput('sea1',height = "800px")),
                  tabPanel(tags$div(h4(strong("Control-Chart"),style="color:black;font-size:120%"),align="center"), plotOutput('sea2',height = "800px")),
                  tabPanel(tags$div(h4(strong("Box-Plot"),style="color:black;font-size:120%"),align="center"), plotOutput('sea3',height = "800px")),
                  tabPanel(tags$div(h4(strong("Comparison-Plot"),style="color:black;font-size:120%"),align="center"), br(),
                           column(12,
                                  
                                  column(4,tags$div(downloadBttn(outputId = "downq1",label = "Download Data..!",color = "success",size = "md"),align="center")),
                                  
                                  
                                  column(4,tags$div(downloadBttn(outputId = "downq2",label = "Download Data..!",color = "success",size = "md"),align="center")),
                                  
                                  
                                  column(4,tags$div(downloadBttn(outputId = "downq3",label = "Download Data..!",color = "success",size = "md"),align="center"))),
                           
                           column(12,br()),
                           
                           column(12,
                                  
                                  column(4,plotlyOutput("anopast2")),
                                  
                                  
                                  column(4,plotlyOutput("outcontrolpast2")),
                                  
                                  
                                  column(4,plotlyOutput("outlierpast2"))),
                           
                           column(12,
                                  
                                  column(4,plotlyOutput("c1p2f")),
                                  
                                  
                                  column(4,plotlyOutput("c2p2f")),
                                  
                                  
                                  column(4,plotlyOutput("c3p2f")))
                           
                  )
      ),
      footer = modalButton("OK"),
      easyClose = FALSE
    )))
  })
  
  
  ############################################# Data ###############################################################################  
  
  data4 <-reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data=data.frame(data)
    data
    
  })
  
  
  data24 <-reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data
  })
  
  
  
  p1=function(){
    tbl_df(as.tibble(na.omit(datejointdata4()[,c(1,2)]))) %>% 		
      time_decompose(2,frequency="auto",trend="auto") %>%		
      anomalize(remainder) %>%		
      time_recompose()
  }
  #data4=read.csv("converted_temp - Copy.csv")
  datejointdata4 <- function(){
    data.frame(Date=seq.Date(from = as.Date( "2012-01-01",format="%Y-%m-%d"),by="day",length.out = dim(data4())[1]),data4()[2])
  } 
  
  observeEvent(input$file20, {
    output[["PI210A.PV_plot"]]<- renderPlotly({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      d=data.frame(datejointdata4()[,c(1,2)],data4()[1])
      d[,1] <- as.Date(d[,1],format="%Y-%m-%d")
      d[,3] <- as.Date(d[,3],format="%d-%m-%Y")
      data=data.frame(d,anomaly=p1()[,c(8)])
      colnames(data) <- c("Date","temperature","Observation","anomaly")
      plot_ly(data, x = ~Observation, y = ~temperature,symbol = ~anomaly ,color = ~anomaly ,type = 'scatter', 
              mode = 'markers',colors =  c("#009900","red"),symbols = c('circle','x'),marker = list(size = 10))
      })
      
    })
  })
  
  
  observeEvent(input$file20, {
    output[["PI210A.PV_boxplot"]]<- renderPlot({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      qplot(data=data4(),y=data4()[,2],x=1,
            geom = "boxplot",outlier.colour="red",
            xlim = c(0,2),main = "box plot",xlab = "",
            ylab = NULL)+
        geom_text(aes(label=ifelse(data4()[,2] %in%
                                     boxplot.stats(data4()[,2])$out,
                                   as.character(data4()[,1]),"")),hjust=1.5)+
        geom_boxplot(fill = "#0e75b8", color = "black",outlier.colour="red")+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white",colour = "black",size = 0.5, linetype = "solid"),
              axis.text.x = element_text(face = "bold", color = "black",size = 10,angle = NULL),
              axis.text.y = element_text(face = "bold", color = "black",size = 10,angle = NULL),
              axis.line.y = element_blank(),#element_line( color = "#404040", size = 1, linetype = "solid"),
              axis.line.x = element_blank()#element_line(color = "#404040", size = 1, linetype = "solid")
        )
      })
      
    })
  })
  
  observeEvent(input$file20, {
    output[["PI210A.PV_cplot"]]<- renderPlot({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      qcc(na.omit(data4()[,2]),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
    }) })
  })
  
  
  
  #j=data.frame(s.no=1:dim(data.frame(data=c(mtcars$cyl,mtcars$mpg)))[1],data=c(mtcars$cyl,mtcars$mpg))
  #   dim(j)[2]
  # 1:dim(j)[1]
  # j[2]
  
  #dataj=reactive({ data.frame(s.no=1:dim(joint())[1],data=joint()[1]) })
  out1 <- function() { 
    d=data.frame(s.no=1:dim(data.frame(level2ano()))[1],level2ano()[,c(2,3)],anomaly=p1le2()[,c(8)])
    colnames(d) <- c("S.no",colnames(data4()[1]),colnames(data4()[2]),"Anomaly")
    d
  }
  
  out2 <- function() {
    s=qcc(level2control()[2],type = "xbar.one",nsigmas = 3,std.dev = "SD",title="")
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    d=data.frame(s.no=1:dim(data.frame(level2control()))[1],level2control(),outofcontrol=out$outofcontrol)
    colnames(d) <- c("S.no",colnames(data4()[1]),colnames(data4()[2]),"Process_Out_of_Control")
    d
  }
  
  out3 <- function() {
    u=boxplot(level2box()[2])
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    count=dim(level2box())[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    box2=data.frame(rbind(box,dummy))
    box22 = merge(box2[, c(2,3)], 
                  level2box()[, c(2,1)])
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[2],box22[3])
    boxdata=data.frame(level2box()[,1],outlier=rep("No",dim(data.frame(level2box()))[1]))
    colnames(boxdata) <- c(colnames(level2box()[1]),"outlier")
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be[2]),]
    outbox=bdf[!duplicated(bdf[2]),]
    d=data.frame(s.no=1:dim(data.frame(level2box()))[1],level2box(),outlier=outbox$outlier)
    colnames(d) <- c("S.no",colnames(data4()[1]),colnames(data4()[2]),"Outlier")
    d
  }
  
  
  
  output$down4 <- downloadHandler(
    filename = function() {
      paste("overall.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(final2(), file, row.names = FALSE)
    }
  )
  
  output$downq1 <- downloadHandler(
    
    filename = function() {
      paste("anomaly.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(out1(), file, row.names = FALSE)
    }
  )
  
  output$downq2 <- downloadHandler(
    filename = function() {
      paste("process_control.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(out2(), file, row.names = FALSE)
    }
  )
  
  output$downq3 <- downloadHandler(
    
    filename = function() {
      paste("outlier.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(out3(), file, row.names = FALSE)
    }
  )
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # library(readxl)
  #data12 = function(){read_excel("converted_temp - Copy.xlsx")}
  
  final2 <- function()
  {
    
    d=data.frame(data4()[,c(1,2)])
    #d[,1] <- as.Date(d[,1])
    data=data.frame(d,anomaly=p1()[,c(8)])
    s=qcc(data[2],type = "xbar.one",nsigmas = 3,std.dev = "SD",title="")
    #s$violations$beyond.limits
    #s$statisticsm
    
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    #r2=data.frame(s.no=s$violations$violating.runs,outofcontrol=rep("No",dim(data.frame(s$violations$violating.runs))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    final=data.frame(s.no=1:dim(data.frame(data))[1],data,outofcontrol=out$outofcontrol)
    
    #----------------------------------------------------------------------------------------------------------------
    u=boxplot(data[2])
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c(colnames(data[1]),colnames(data)[2],"outlier")
    count=dim(data)[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c(colnames(data[1]),colnames(data)[2],"outlier")
    box2=data.frame(rbind(box,dummy))
    box22 = merge(box2[, c(2,3)], 
                  final[, c(3,5,1)])
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[4],box22[2])
    boxdata=data.frame(s.no=1:dim(data.frame(data))[1],outlier=rep("No",dim(data.frame(data))[1]))
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be$s.no),]
    outbox=bdf[!duplicated(bdf[c("s.no")]),]
    final2=data.frame(s.no=1:dim(data.frame(data))[1],final[,c(2,3,4,5)],outlier=outbox$outlier)
    final2
  }
  
  
  
  #----------------------------------------------------------------------------------------------------------------------
  
  tab <- function() { table(final2()$anomaly)}
  anomaly_pi <- function() { data.frame(tab(),label=paste0("Anomaly - ",rownames(tab())))[,c(3,2)] }
  
  tab2 <- function() {table(final2()$outofcontrol) }
  outofcontrol_pi <- function() {data.frame(tab2(),label=paste0("out of control - ",rownames(tab2())))[,c(3,2)] } 
  
  tab3 <- function() {table(final2()$outlier)}
  outlier_pi <- function() {data.frame(tab3(),label=paste0("outlier - ",rownames(tab3())))[,c(3,2)]}
  
  
  tab_1_2_3 <- function() { data.frame(label=c("Yes","No"),Anomaly=c(anomaly_pi()$Freq[2],anomaly_pi()$Freq[1]),outofcontrol=outofcontrol_pi()$Freq,outlier=outlier_pi()$Freq) }
  
  #--------------------------------------------------------------------------------------------------------------------
  Yesonly <- function() { data.frame(filter(final2(), anomaly == "Yes" & outofcontrol == "Yes" & outlier == "Yes"),row.names = NULL)[,c(2,3,4,5,6)] }
  allthreeyes <- function() {  data.frame(lab=c("All Three are YES","Remaining"),value=c(dim(Yesonly())[1],dim(data4())[1]-dim(Yesonly())[1])) }
  
  output$anopast <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    x=plot_ly(anomaly_pi(), labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'Anomaly Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
  
  
  output$outcontrolpast <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    plot_ly(outofcontrol_pi(), labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'Process Control Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })})
  
  output$outlierpast <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    plot_ly(outlier_pi(), labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'outlier Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })})
  
  output$c1p <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    plot_ly(tab_1_2_3(), x = ~label, y = ~Anomaly, type = 'bar', name = 'Anomaly') %>%
      add_trace(y = ~outofcontrol , name = 'Process In Control') %>%
      add_trace(y = ~outlier , name = 'Outlier') %>%
      layout(yaxis = list(title = 'Count'),xaxis = list(title = ''), barmode = 'group')
    })
  })
  
  output$c2p <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    plot_ly(tab_1_2_3(), x = ~label, y = ~Anomaly, type = 'bar', name = 'Anomaly') %>%
      add_trace(y = ~outofcontrol , name = 'Process In Control') %>%
      add_trace(y = ~outlier , name = 'Outlier') %>%
      layout(yaxis = list(title = 'Count'),xaxis = list(title = ''), barmode = 'stack')
  }) })
  
  output$c3p <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    plot_ly(allthreeyes(), x = ~lab, y = ~value, type = 'bar', name = 'Anomaly',text=~value,textposition = 'auto',
            marker = list(color = c('red','darkgreen')))%>%
      layout(yaxis = list(title = 'count'),xaxis = list(title = ''), barmode = 'stack') 
    
  })})
  
  
  #-----------------------------------------------------------level-2-------------------------------------------------------------------------------
  #x=read.csv("ee.csv")
  
  level2ano <- function(){
    x=data.frame(final2()[,-1])
    ano = data.frame(filter(x, anomaly == "No"))
    ano2 = ano[,c(1,2)]
    ano2[,1]=as.Date(ano2[,1],format="%d-%m-%Y")
    data.frame(Date=seq.Date(from = as.Date( "2012-01-01",format="%Y-%m-%d"),by="day",length.out = dim(ano2)[1]),ano2)
  }
  #names(x)
  
  level2control <- function(){
    x=data.frame(final2()[,-1])
    con=data.frame(filter(x, outofcontrol == "No"))
    data.frame(con[,c(1,2)])
  }
  
  
  level2box <- function(){
    x=data.frame(final2()[,-1])
    box=data.frame(filter(x, outlier == "No"))
    data.frame(box[,c(1,2)])
  }
  
  
  p1le2 = function(){
    tbl_df(as.tibble(na.omit(level2ano()[,c(1,3)]))) %>% 		
      time_decompose(2,frequency="auto",trend="auto") %>%		
      anomalize(remainder) %>%		
      time_recompose()
  }
  
  observeEvent(input$file20, {
    output[["sea1"]]<- renderPlotly({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      d=data.frame(level2ano())
      d[,1] <- as.Date(d[,1],format="%Y-%m-%d")
      data=data.frame(d,anomaly=p1le2()[,c(8)])
      colnames(data) <- c("Date","ID","Value","anomaly")
      plot_ly(data, x = ~ID, y = ~Value,symbol = ~anomaly ,color = ~anomaly ,type = 'scatter', 
              mode = 'markers',colors =  c("#009900","red"),symbols = c('circle','x'),marker = list(size = 10))
    })
    })
  })
  
  
  observeEvent(input$file20, {
    output$sea3 <- renderPlot({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      qplot(data=level2box(),y=level2box()[,2],x=1,
            geom = "boxplot",outlier.colour="red",
            xlim = c(0,2),main = "box plot",xlab = "",
            ylab = NULL)+
        geom_text(aes(label=ifelse(level2box()[,2] %in%
                                     boxplot.stats(level2box()[,2])$out,
                                   as.character(level2box()[,1]),"")),hjust=1.5)+
        geom_boxplot(fill = "#0e75b8", color = "black",outlier.colour="red")+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white",colour = "black",size = 0.5, linetype = "solid"),
              axis.text.x = element_text(face = "bold", color = "black",size = 10,angle = NULL),
              axis.text.y = element_text(face = "bold", color = "black",size = 10,angle = NULL),
              axis.line.y = element_blank(),#element_line( color = "#404040", size = 1, linetype = "solid"),
              axis.line.x = element_blank()#element_line(color = "#404040", size = 1, linetype = "solid")
        )
      
    })})
  })
  
  observeEvent(input$file20, {
    output[["sea2"]]<- renderPlot({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      qcc(na.omit(level2control()[,2]),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
    }) })
  })
  
  
  # final3 <- function(){
  #   dd=data.frame(level2ano())
  #   dd[,1] <- as.Date(dd[,1],format="%Y-%m-%d")
  #   dataano2=data.frame(dd,anomaly=p1le2()[,c(8)])
  #   colnames(dataano2) <- c("Date","ID","Value","anomaly")
  #   
  #   #---------------------------------------------------------------------------------------------------------------------
  #   s3=qcc(na.omit(level2control()[,2]),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="")
  #   #s$violations$beyond.limits
  #   #s$statisticsm
  #   
  #   r3=data.frame(s.no=s3$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s3$violations$beyond.limits))[1]))
  #   #r2=data.frame(s.no=s$violations$violating.runs,outofcontrol=rep("No",dim(data.frame(s$violations$violating.runs))[1]))
  #   qccdata3=data.frame(s.no=1:dim(data.frame(s3$statistics))[1],outofcontrol=rep("No",dim(data.frame(s3$statistics))[1]))
  #   qe3=data.frame(rbind(r3,qccdata3))
  #   df3=qe3[order(qe3$s.no),]
  #   out3=df3[!duplicated(df3[c("s.no")]),]
  #   
  #   
  #   nacount = max(dim(out3)[1],dim(dataano2)[1])-min(dim(out3)[1],dim(dataano2)[1])
  #   
  #   #---------------------------------------------------------------------------------------------------------------------
  #   
  
  #   
  # }
  # 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  # tab_1_2_3 <- function() { data.frame(label=c("Yes","No"),Anomaly=c(anomaly_pi()$Freq[2],anomaly_pi()$Freq[1]),outofcontrol=outofcontrol_pi()$Freq,outlier=outlier_pi()$Freq) }
  # 
  # #--------------------------------------------------------------------------------------------------------------------
  # Yesonly <- function() { data.frame(filter(final2(), anomaly == "Yes" & outofcontrol == "Yes" & outlier == "Yes"),row.names = NULL)[,c(2,3,4,5,6)] }
  # allthreeyes <- function() {  data.frame(lab=c("All Three are YES","Remaining"),value=c(dim(Yesonly())[1],dim(data4())[1]-dim(Yesonly())[1])) }
  # 
  
  
  output$anopast2 <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    dataano=data.frame(level2ano(),anomaly=p1le2()[,c(8)])
    tab12 = table(dataano$anomaly)
    
    anomaly_pi12 <- data.frame(tab12,label=paste0("Anomaly - ",rownames(tab12)))[,c(3,2)] 
    
    plot_ly(anomaly_pi12, labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'Anomaly Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
  
  
  output$outcontrolpast2 <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    s=qcc(level2control()[2],type = "xbar.one",nsigmas = 3,std.dev = "SD",title="")
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    datacontrol=data.frame(s.no=1:dim(data.frame(level2control()))[1],level2control(),outofcontrol=out$outofcontrol)
    
    tab21 <- table(datacontrol$outofcontrol) 
    outofcontrol_pi <- data.frame(tab21,label=paste0("out of control - ",rownames(tab21)))[,c(3,2)] 
    
    plot_ly(outofcontrol_pi, labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'Process Control Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })})
  
  output$outlierpast2 <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    u=boxplot(level2box()[2])
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    count=dim(level2box())[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    box2=data.frame(rbind(box,dummy))
    box22 = merge(box2[, c(2,3)], 
                  level2box()[, c(2,1)])
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[2],box22[3])
    boxdata=data.frame(level2box()[,1],outlier=rep("No",dim(data.frame(level2box()))[1]))
    colnames(boxdata) <- c(colnames(level2box()[1]),"outlier")
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be[2]),]
    outbox=bdf[!duplicated(bdf[2]),]
    databox=data.frame(s.no=1:dim(data.frame(level2box()))[1],level2box(),outlier=outbox$outlier)
    
    tab31 <- table(databox$outlier)
    outlier_pi <- data.frame(tab31,label=paste0("outlier - ",rownames(tab31)))[,c(3,2)]
    
    plot_ly(outlier_pi, labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
      layout(title = 'outlier Percentage Plot',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })})
  
  output$c1p2f <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    dataano=data.frame(level2ano(),anomaly=p1le2()[,c(8)])
    tab12 = table(dataano$anomaly)
    
    anomaly_pi12 <- data.frame(tab12,label=paste0("Anomaly - ",rownames(tab12)))[,c(3,2)] 
    
    plot_ly(anomaly_pi12, x = ~label, y = ~Freq, type = 'bar', name = 'Anomaly',text=~Freq,textposition = 'auto')%>%
      layout(yaxis = list(title = 'count'),xaxis = list(title = ''), barmode = 'stack') 
    
    # plot_ly(anomaly_pi12, x = ~label, y = ~Freq, type = 'bar') %>%
    #   layout(title = 'Anomaly Percentage Plot',
    #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
  
  output$c2p2f <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    s=qcc(level2control()[2],type = "xbar.one",nsigmas = 3,std.dev = "SD",title="")
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    datacontrol=data.frame(s.no=1:dim(data.frame(level2control()))[1],level2control(),outofcontrol=out$outofcontrol)
    
    tab21 <- table(datacontrol$outofcontrol) 
    outofcontrol_pi <- data.frame(tab21,label=paste0("out of control - ",rownames(tab21)))[,c(3,2)] 
    
    plot_ly(outofcontrol_pi, x = ~label, y = ~Freq, type = 'bar', name = 'Anomaly',text=~Freq,textposition = 'auto')%>%
      layout(yaxis = list(title = 'count'),xaxis = list(title = ''), barmode = 'stack') 
    
    # plot_ly(outofcontrol_pi, x = ~label, y = ~Freq, type = 'bar') %>%
    #   layout(title = 'Process Control Percentage Plot',
    #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })})
  
  output$c3p2f <- renderPlotly({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    u=boxplot(level2box()[2])
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    count=dim(level2box())[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c(colnames(level2box()[1]),colnames(level2box())[2],"outlier")
    box2=data.frame(rbind(box,dummy))
    box22 = merge(box2[, c(2,3)], 
                  level2box()[, c(2,1)])
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[2],box22[3])
    boxdata=data.frame(level2box()[,1],outlier=rep("No",dim(data.frame(level2box()))[1]))
    colnames(boxdata) <- c(colnames(level2box()[1]),"outlier")
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be[2]),]
    outbox=bdf[!duplicated(bdf[2]),]
    databox=data.frame(s.no=1:dim(data.frame(level2box()))[1],level2box(),outlier=outbox$outlier)
    
    tab31 <- table(databox$outlier)
    outlier_pi <- data.frame(tab31,label=paste0("outlier - ",rownames(tab31)))[,c(3,2)]
    
    plot_ly(outlier_pi, x = ~label, y = ~Freq, type = 'bar', name = 'Anomaly',text=~Freq,textposition = 'auto')%>%
      layout(yaxis = list(title = 'count'),xaxis = list(title = ''), barmode = 'stack')   
    
    # plot_ly(outlier_pi, x = ~label, y = ~Freq, type = 'bar') %>%
    #   layout(title = 'outlier Percentage Plot',
    #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })   
  })
  
################################################################################### Multi variate UI & SERVER ########################################################################################################################################

  
  
#   output[['multivariates']] = renderUI({
#            
#            fluidPage(
#              
#              tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
#              
#              tags$style(type="text/css",
#                         ".shiny-output-error { visibility: hidden; }",
#                         ".shiny-output-error:before { visibility: hidden; }"
#              ),
#              tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
#                                                     overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
#              tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
#                                                     overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
#              
#              
#              
#              br(),
#              br(),
#              
#              column(12,
#                     
#                     
#                     bootstrapPage(useShinyjs(),
#                                   br(),
#                                   
#                                   tags$h3(strong(em("Outlier Detection Analysis (Multivariate)")),style="text-align:center;color:#034e91;font-size:190%"),
#                                   
#                                   
#                                   tags$div(id = 'logo2',img(src="cc.png",height='20%',width='20%'),align="center"),
#                                   
#                                   
#                                   br(),
#                                   uiOutput('fileupload'), uiOutput("bspop"),
#                                   uiOutput('checkbox'),#br(),
#                                   # uiOutput("buttonmulti"),
#                                   
#                                   br(),
#                                   br(),
#                                   bsPopover(id="check",title = "",content = "Note: I accept the  Terms & Conditions.. Show the Analyse button",placement = "right"),
#                                   tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
#                                   
#                                   
#                                   #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
#                                   br(),
#                                   
#                                   tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
#                                            tags$tbody("Need Help ?"),
#                                   )
#                     )
#              )
#              
#              
#              
#            )
# })
  
  # output[["buttonmulti"]] <- renderUI({
  #   if (!is.null(input$file)) {
  #     tags$div(bsButton("analyset", label = strong("Lets Go..!"), icon =   icon("refresh",lib = "glyphicon"),block = F, style="primary",size = "default"),align="center")
  #   }
  # })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      showModal(tags$div(id="modal1", modalDialog(
        inputId = 'Dialog1', 
        title = HTML('<span style="color:black; font-size: 20px; font-weight:bold; font-family:sans-serif ">Output<span>
                       <button type = "button" class="close" data-dismiss="modal" ">
                       <span style="color:white; ">x <span>
                       </button> '),
        footer = modalButton("Close"),
        size = "l",br(),
        uiOutput("down"),br(),
        uiOutput("frf"),
        plotlyOutput('one',height = "500px"),
        easyClose = T
      )))
    }
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){  
      output[["frf"]] <- renderUI({
        data3 <- data.frame(table(pred()[,2]))
        tags$div(h5(strong(em(paste("Total No.of Outlier --> ",data3[2,2],"Out of ",sum(data3[,2])))),style="color:black;font-size:150%"),align="center")
      })
    }
  })
  
  # observeEvent(input$file, {
  #   showModal(tags$div(id="modal2", modalDialog(
  #     inputId = 'Dialog1', 
  #     title = HTML('<span style="color:black;text-align:center; font-size: 20px; font-weight:bold; font-family:sans-serif ">Select Data Type..!<span>
  #                      <button type = "button" class="close" data-dismiss="modal" ">
  #                      <span style="color:white; ">x <span>
  #                      </button> '),
  #     
  #     size = "l",#uiOutput("nnn"),
  #     column(12,column(2),column(2,uiOutput("eee")),column(3,uiOutput("sum")),column(3,uiOutput("sum2")),column(1)),
  #     column(12,column(2),column(2,uiOutput("mod")),column(3,uiOutput("mod1")),column(3,uiOutput("mod2")),column(1)),
  #     #dataTableOutput("ccc"),
  #     column(12,column(4,uiOutput("numeric")),column(4,uiOutput("factor")),column(4,uiOutput("date"))),br(),br(),br(),
  #     br(),br(),br(),br(),br(),
  #     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
  #     br(),br(),br(),br(),br(),br(),br(),
  #     footer = tagList(uiOutput("button"),
  #                      modalButton("Cancel")),
  #     easyClose = T
  #   )))
  #   
  # })
  
  fieldsMandatory <- c("select_num","select_fac")
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "analyse", condition = mandatoryFilled)
  })
  
  # output[["ccc"]] <- renderDataTable({
  #   pred()
  # })
  
  output[["eee"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(h5(strong(em(paste("Uploaded --> "))),style="color:black;font-size:150%"),align="center")
  })
  
  output[["sum"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(h5(strong(em(paste("No.of Rows :",dim(data3)[1]))),style="color:black;font-size:150%"),align="center")
  })
  
  output[["sum2"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(h5(strong(em(paste("No.of Columns :",dim(data3)[2]))),style="color:black;font-size:150%"),align="center")
  })
  
  # output[["nnn"]] <- renderUI({
  #   tags$div(h5(strong(em(paste(pred()))),style="color:black;font-size:150%"),align="center")
  # })
  
  output[["mod"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(h5(strong(em(paste("Modified --> "))),style="color:black;font-size:150%"),align="center")
  })
  
  output[["mod1"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)[input$select_num]
    data4 <- read.csv(file1$datapath,header = T)[input$select_fac]
    data5 <- read.csv(file1$datapath,header = T)[input$select_dat]
    data0 <- data.frame(data3,data4,data5)
    tags$div(h5(strong(em(paste("No.of Rows :",dim(data0)[1]))),style="color:black;font-size:150%"),align="center")
  })
  
  output[["mod2"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)[input$select_num]
    data4 <- read.csv(file1$datapath,header = T)[input$select_fac]
    data5 <- read.csv(file1$datapath,header = T)[input$select_dat]
    data0 <- data.frame(data3,data4,data5)
    tags$div(h5(strong(em(paste("No.of Columns :",dim(data0)[2]))),style="color:black;font-size:150%"),align="center")
  })
  
  
  output[["numeric"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(prettyCheckboxGroup(inputId = "select_num",
                                 label = tags$h4(strong(em("Select Numeric Variables")),style="color:#034e91;font-size:160%"),
                                 icon = icon("check"),inline = F,
                                 choices =  names(data3),
                                 animation = "tada", status = "default"),align="center")
  })
  
  output[["factor"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(prettyCheckboxGroup(inputId = "select_fac",
                                 label = tags$h4(strong(em("Select Factor Variables")),style="color:#034e91;font-size:160%"),
                                 icon = icon("check"),inline = F,
                                 choices =  names(data3),
                                 animation = "tada", status = "default"),align="center")
  })
  
  output[["date"]] <- renderUI({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    tags$div(prettyCheckboxGroup(inputId = "select_dat",
                                 label = tags$h4(strong(em("Select Date Variables")),style="color:#034e91;font-size:160%"),
                                 icon = icon("check"),inline = F,
                                 choices =  names(data3),
                                 animation = "tada", status = "default"),align="center")
  })
  
  college_clean <- reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    #data3 <- read.csv("insurance data.csv",header = T)
    data5 <- read.csv(file1$datapath,header = T)[input$select_num]
    data6 <- read.csv(file1$datapath,header = T)[input$select_fac]
    data7 <- read.csv(file1$datapath,header = T)[input$select_dat]
    data0 <- data.frame(data5,data6,data7)
    data1 <- lapply(data5, function(x) as.numeric(as.character(x)))
    data2 <-  lapply(data6, function(x) as.factor(as.character(x)))
    college_clean <-function(){ data.frame(unlist_df(data1),unlist_df(data2)) }
    college_clean()
  })
  
  pred <- reactive({
    set.seed(1000)
    iso <- isolation.forest(college_clean(), ntrees = 100, nthreads = 1)
    pred <- predict(iso, college_clean())
    outlier <- ifelse(pred >0.50, "outlier", "normal")
    df=data.frame(pred,outlier)
    df
  })
  
  outx <- reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    data.frame(college_clean(),Pred=pred()[,1],Prediction=pred()[,2])
  })
  
  # observeEvent(input$reset,{
  #   reset(id = "file")
  # })
  # 
  # output[["fileupload"]] <- renderUI({
  #   input$reset
  #   tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#034e91;font-size:160%"),
  #                      accept=c('csv','comma-seperated-values','.csv')),align="center")#
  #   
  # })
  # 
  
  # 
  # output[["checkbox"]] <- renderUI({
  #   input$reset
  #   
  # })
  
  output[["button"]] <- renderUI({
    input$file20
      tags$div(bsButton("analyse",strong("Submit..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="right")
    
  })
  # 
  # output[["bspop"]] <- renderUI({
  #   bsPopover(id="fileupload",title = "",content = "To get results, click the Lets go! button...",placement = "top")
  # })
  # 
  
  
  observe(addHoverAnim(session, 'logo1', 'pulse'))
  observe(addHoverAnim(session, 'logo2', 'pulse'))
  observe(addHoverAnim(session, 'analyse', 'shake'))
  observe(addHoverAnim(session, 'reset', 'shake'))
  
  observeEvent(input$analyse, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation",
      type = "success",
      title = "Submitted successfully..!",
      btn_labels = c("Cancel..!", "Go for Output..!"),
      danger_mode = TRUE
    )
  })
  
  
  
  output[["down"]]<-renderUI({
    tags$div(downloadButton("downloadData","Download Data..!"),align="center")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("multivar output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(outx(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output[["one"]]<- renderPlotly({
        
        Noax <- list(
          title = "",
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          showgrid = FALSE
        )
        
        Noax2 <- list(
          title = "",
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        
        progress$set(message = 'Please Wait...!')
        withProgress(message='Loading Plot..!',value=30,style = "notification",
                     {
                       
                       n<-10
                       
                       for(i in 1:n){
                         incProgress(1/n,detail="")#paste("Doing Part", i, "out of", n))
                         datax = data.frame(s.no=seq(1:nrow(pred())),pred())
                         pal <- c( "darkgreen","red")
                         out <- plot_ly(data = datax, x = ~s.no, y = ~pred, color = ~outlier,type = "scatter",colors = pal)%>%
                           layout(xaxis = Noax,yaxis = Noax2)
                       }
                     })
        
        return(out)
      })
    }
  })
  
###########################################################################Single factor outlier non time series###################################################################################################################################################################    

  get_var_names23 <- reactive({
    file <- input$file20
    var_names <- names(read.csv(file$datapath))
    var_names
    
  })
  output$boxsin <- renderUI({
    choices <- get_var_names23()
    file <- input$file20
    if( !is.null(file) )    {
      selectInput(inputId = "sinfactor", label = "Choose Y:", choices = choices)
      
    }
  })
  
  output[["downsin"]]<-renderUI({
    tags$div(downloadButton("downloadsinbox","Download Data..!"),align="center")
  })
  
  college_clean2 <- reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    #data3 <- read.csv("insurance data.csv",header = T)
    data5 <- read.csv(file1$datapath,header = T)[input$sinfactor]
    data0 <- data.frame(data5)
    data1 <- lapply(data5, function(x) as.numeric(as.character(x)))
    college_clean <-function(){ data.frame(unlist_df(data1)) }
    college_clean()
  })
  
  pred2 <- reactive({
    set.seed(1000)
    #college_clean2=read.csv("cc.csv")
    iso <- isolation.forest(college_clean2(), ntrees = 100, nthreads = 1)
    pred <- predict(iso, college_clean2())
    outlier <- ifelse(pred >0.50, "outlier", "normal")
    df=data.frame(pred,outlier)
    df
  })
  
  outx2 <- reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data3 <- read.csv(file1$datapath,header = T)
    data.frame(s.no=seq(1:nrow(pred2())),college_clean2(),Prediction=pred2()[,2])
  })
  
  
  output$downloadsinbox <- downloadHandler(
    filename = function() {
      paste("single variable-level1 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(outx2(), file, row.names = FALSE)
    })
  
  output[["sinfac"]]<- renderPlotly({
    
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE
    )
    
    Noax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = 'Please Wait...!')
    withProgress(message='Loading Plot..!',value=30,style = "notification",
                 {
                   
                   n<-10
                   
                   for(i in 1:n){
                     incProgress(1/n,detail="")#paste("Doing Part", i, "out of", n))
                     datax =data.frame(outx2()) #data.frame(s.no=seq(1:nrow(pred2())),pred2(),college_clean2())
                     colnames(datax) <- c("s.no","value","outlier")
                     pal <- c( "darkgreen","red")
                     #datax=read.csv("ww.csv")
                     out <- plot_ly( x = datax$s.no, y = datax$value, color = datax$outlier,type = "scatter",colors = pal)%>%
                       layout(xaxis = Noax,yaxis = Noax2)
                   }
                 })
    
    return(out)
  })
    
  output[["lev2uni"]] <- renderUI({
    tags$div(bsButton("lev2unip",strong("Go for Level-2 Analysis..!"),icon = icon("refresh"),style = "primary",size="medium"),
             style="color:white;font-weight:100%;",align="right")
    
  })
  
  observeEvent(input$lev2unip, {
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1',
      title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Level-2 Analysis<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      size = "l",
      
      tabsetPanel(type = "tabs", 
                  tabPanel(tags$div(h4(strong("Univariate outliers"),style="color:black;font-size:120%"),align="center"),
                           br(),tags$div(downloadButton("lev2unidown","Download..!"),align="center"),
                           br(),plotlyOutput('lev2uniplot',height = "600px"))
                  
      ),
      footer = modalButton("OK"),
      easyClose = FALSE
    )))
  })
  
  
  pred23 <- reactive({
    set.seed(1000)
    college_clean2=data.frame(filter(outx2(), Prediction == "normal"))
    college_clean23=data.frame(college_clean2[,c(1,2)])
    iso <- isolation.forest(college_clean23, ntrees = 100, nthreads = 1)
    pred <- predict(iso, college_clean23)
    outlier <- ifelse(pred >0.50, "outlier", "normal")
    df=data.frame(college_clean23[c(1,2)],pred,outlier)
    df
  })
  
  outx23 <- reactive({
    x=data.frame(s.no=1:dim(pred23())[1],pred23()[,2],Prediction=pred23()[,4])
    colnames(x)<- c("s.no",paste(input$sinfactor),"outlier")
    x
  })
  
  output$lev2unidown <- downloadHandler(
    filename = function() {
      paste("single variable-level2 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(outx23(), file, row.names = FALSE)
    })
  
  
  output[["lev2uniplot"]]<- renderPlotly({
    
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE
    )
    
    Noax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = TRUE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = 'Please Wait...!')
    withProgress(message='Loading Plot..!',value=30,style = "notification",
                 {
                   
                   n<-10
                   
                   for(i in 1:n){
                     incProgress(1/n,detail="")#paste("Doing Part", i, "out of", n))
                     datax =data.frame(outx23()) #data.frame(s.no=seq(1:nrow(pred2())),pred2(),college_clean2())
                     colnames(datax) <- c("s.no","value","outlier")
                     pal <- c( "darkgreen","red")
                     #datax=read.csv("ww.csv")
                     out <- plot_ly( x = datax$s.no, y = datax$value, color = datax$outlier,type = "scatter",colors = pal)%>%
                       layout(xaxis = Noax,yaxis = Noax2)
                   }
                 })
    
    return(out)
  })

###########################################################################PIVOT TABLE###################################################################################################################################################################    
  
  # observeEvent(input$pivots, {
  #   showModal(tags$div(id="modal1", modalDialog(
  #     inputId = 'Dialog1',
  #     title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif "><span>
  #                  <button type = "button" class="close" data-dismiss="modal" ">
  #                  <span style="color:white; ">x <span>
  #                  </button> '),
  #     tabsetPanel(type = "tabs", 
  #                 tabPanel(tags$div(h4(strong("Pivot Table"),style="color:black;font-size:120%"),align="center"),
  #                          rpivotTableOutput("OverallPivot", width = "100%", height = "500px"))
  #     ),
  #     size = "m"
  #    
  #   )))
  # })
  
  output$OverallPivot <- renderRpivotTable({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    s <- read.csv(file1$datapath)
    rpivotTable(data=s, onRefresh=htmlwidgets::JS("function(config) {  Shiny.onInputChange('myPivotData', config); }"))
    
  })

####################################################### DUPLICATE FIND ########################################################################################    
  
  
  # observeEvent(input$duplic, {
  #   showModal(tags$div(id="modal1", modalDialog(
  #     inputId = 'Dialog1',
  #     title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif "><span>
  #                  <button type = "button" class="close" data-dismiss="modal" ">
  #                  <span style="color:white; ">x <span>
  #                  </button> '),
  #     tabsetPanel(type = "tabs", 
  #                 tabPanel(tags$div(h4(strong("Duplicate Verify"),style="color:black;font-size:120%"),align="center"),br(),
  #                          uiOutput("odown"),
  #                          dataTableOutput('duplicate'))
  #     ),
  #     size = "m"
  #     
  #   )))
  # })
  
  dup <- reactive({
    file1 <- input$file20
    data0=read.csv(input$file20$datapath)
    ee <- input$dupselect
    data2 <- data.frame(data0[ee])
    data0[!duplicated(data2),]
  })
  
  dup2 <- reactive({
    file1 <- input$file20
    data0=read.csv(input$file20$datapath)
    ee <- input$dupselect
    data2 <- data.frame(data0[ee])
    data0[duplicated(data2),]
  })
  
  output[["odown"]] <- renderUI({
    if(!is.null(input$file20))
      tags$div(downloadButton("odown2",strong("Duplicate removed data..!"),icon = icon("refresh")),
               style="color:white;font-weight:100%;",align="center")
  })
  
  output[["odown3"]] <- renderUI({
    if(!is.null(input$file20))
      tags$div(downloadButton("odown4",strong("Duplicated data..!"),icon = icon("refresh")),
               style="color:white;font-weight:100%;",align="center")
  })
  
  
  
  output$odown2 <- downloadHandler(
    filename = function() {
      paste("Duplicate removed data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dup(), file, row.names = FALSE)
    })
  
  output$odown4 <- downloadHandler(
    filename = function() {
      paste("Duplicate data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dup2(), file, row.names = FALSE)
    })
  
  output[['duplicate']] = DT::renderDataTable({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data0=read.csv(input$file20$datapath)
    ee <- input$dupselect
    data2 <- data.frame(data0[ee])
    datatable(data0[duplicated(data2),], 
              extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
              options = list(
                # dom = 't',
                # deferRender = TRUE,
                searching = TRUE,
                autoWidth = TRUE,
                # scrollCollapse = TRUE,
                rownames = FALSE,
                scroller = TRUE,
                scrollX = TRUE,
                scrollY = "500px",
                fixedHeader = TRUE,
                class = 'cell-border stripe',
                fixedColumns = list(
                  leftColumns = 3,
                  heightMatch = 'none'
                )
              )
    )
  })
  
  
  get_var_names2345 <- reactive({
    file1 <- input$file20
    var_names <- names(read.csv(file1$datapath))
    var_names
  })
  get_var_namesnew <- reactive({
    file1 <- input$file20
    var_names <- names(read.csv(file1$datapath))
    var_names
  })
  output$dupsel <- renderUI({
    choices <- get_var_names2345()
    file <- input$file20
    if( !is.null(file) )    {
      tags$div(selectInput(inputId = "dupselect", label = "Choose Variable:", choices = choices,multiple = T),
               align="center")
    }
  })
  
################################################################### Multi Factor Box Plot UI & SEVER #######################################################################################################
  
  #   output[["univariate"]] <- renderUI({
  #    
  #      fluidPage(
  #       
  #       
  #       tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
  #       
  #       tags$style(type="text/css",
  #                  ".shiny-output-error { visibility: hidden; }",
  #                  ".shiny-output-error:before { visibility: hidden; }"
  #       ),
  #       tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
  #                                                     overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
  #       tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
  #                                                     overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
  #       
  #       
  #       br(),br(),
  #       
  #       column(12,
  #              
  #              
  #              bootstrapPage(useShinyjs(),
  #                            br(),
  #                            
  #                            tags$h3(strong(em("Outlier Detection Analysis (Univariate)")),style="text-align:center;color:#034e91;font-size:150%"),
  #                            br(),
  #                            
  #                            tags$div(id = 'logo2',img(src="c.png",height='20%',width='20%'),align="center"),
  #                            uiOutput("button2"),
  #                            
  #                            br(),
  #                            uiOutput('fileupload2'), uiOutput("bspop2"),
  #                            uiOutput('checkbox2'),br(),uiOutput("buttonuni"),
  #                            # uiOutput("drop"),
  #                            br(),
  #                            br(),
  #                            bsPopover(id="check2",title = "",content = "Note: I accept the  Terms & Conditions.. Show the Analyse button",placement = "right"),
  #                            tags$div(bsButton("reset2", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
  #                            
  #                            
  #                            #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
  #                            br(),
  #                            
  #                            tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
  #                                     tags$tbody("Need Help ?"),
  #       
  #     )
  #     
  #   })
  #   
  # output[["buttonuni"]] <- renderUI({
  #   if (!is.null(input$file2)) {
  #   tags$div(bsButton("analyse2", label = strong("Lets Go..!"), icon =   icon("refresh",lib = "glyphicon"),block = F, style="primary",size = "default"),align="center")
  #   }
  #     })
  #     
  #   observeEvent(input$confirmation2, {
  #     if(input$confirmation2==TRUE){
  #       showModal(tags$div(id="modal1", modalDialog(
  #         inputId = 'Dialog1', 
  #         title = HTML('<span style="color:black; font-size: 20px; font-weight:bold; font-family:sans-serif ">Output<span>
  #                        <button type = "button" class="close" data-dismiss="modal" ">
  #                        <span style="color:white; ">x <span>
  #                        </button> '),
  #         footer = modalButton("Close"),
  #         size = "l",br(),
  #         uiOutput("down2"),
  #         plotlyOutput('one2',height = "500px"),
  #         easyClose = T
  #       )))
  #     }
  #   })
  #   
  #   
  #   
  #   observeEvent(input$reset2,{
  #     reset(id = "file2")
  #   })
  #   
  #   output[["fileupload2"]] <- renderUI({
  #     input$reset2
  #     tags$div(fileInput("file2",label = tags$h4(strong(em("Upload data..")),style="color:#034e91;font-size:160%"),
  #                        accept=c('csv','comma-seperated-values','.csv')),align="center")#
  #     
  #   })
  #   
  #   output[["checkbox2"]] <- renderUI({
  #     input$reset2
  #     
  #   })
  #   
  #   gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
  # 
  #   output[["bspop2"]] <- renderUI({
  #     bsPopover(id="fileupload2",title = "",content = "To get results, click the Lets go! button...",placement = "top")
  #   })
  #   
  #   
  #   
  #   observeEvent(input$analyse2, {
  #     confirmSweetAlert(
  #       session = session,
  #       inputId = "confirmation2",
  #       type = "warning",
  #       title = "Are you sure the Data was uploaded..?",
  #       btn_labels = c("Nope", "Yep"),
  #       danger_mode = TRUE
  #     )
  #   })
  #   
  #   
  # data3 <-reactive({
  #   file1 <- input$file2
  #   if(is.null(file1)) {return(NULL)}
  #   data <- read.csv(file1$datapath)
  #   # data=data.frame(abs(data))
  #   #data=data.frame(readxl::read_excel("ega.xlsx"))
  #   data
  #   
  # })
  # 
  # 
  # outlier <- function (x,method="mean",addthres=FALSE){
  #   if (method=="mean") {
  #     avrg <- mean(x)
  #     stdev <-sd(x)
  #     dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-avrg)>2*stdev)
  #     midp <<- avrg
  #     lower <<- avrg-2*stdev
  #     upper <<- avrg+2*stdev
  #     outliern <<- length(which(dtf=="TRUE"))
  #   } else {}
  #   if (method=="median") {
  #     med <- median(x)
  #     MAD <-median(abs(med-x))
  #     dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-med)>2*(MAD/0.6745))
  #     midp <<- med
  #     lower <<- med-2*(MAD/0.6745)
  #     upper <<- med+2*(MAD/0.6745)
  #     outliern <<- length(which(dtf=="TRUE"))
  #   } else {}
  #   if (method=="boxplot") {
  #     Q1 <- quantile(x, 0.25)
  #     Q3 <- quantile(x, 0.75)
  #     IntQ <-Q3-Q1
  #     dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=x<Q1-1.5*IntQ | x>Q3+1.5*IntQ)
  #     midp <<- median(x)
  #     lower <<- Q1-1.5*IntQ
  #     upper <<- Q3+1.5*IntQ
  #     outliern <<- length(which(dtf=="TRUE"))
  #   } else {}
  #   if (addthres==TRUE) {
  #     p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", method, ")"), y="observation value") + geom_hline(yintercept = midp, colour="black", linetype = "longdash") + geom_hline(yintercept = lower, colour="black", linetype = "longdash") + geom_hline(yintercept = upper, colour="black", linetype = "longdash")
  #   } else {
  #     p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), y="observation value") #requires 'ggrepel'
  #   }
  #   return(p)
  # }
  # 
  
  
  output[["down2"]]<-renderUI({
    tags$div(downloadButton("downloadData2","Download Data..!"),align="center")
  })
  
  
  download11 = reactive({
    
    outl = list()
    outl1 = list()
    file1 <- input$file20
    
    data1 = read.csv(file1$datapath)
    data2 = data1[,-1]
    data3 = unlist(lapply(data2, is.numeric))
    data3 = data2[,data3]
    
    
    
    for(i in colnames(data3)){
      gfgf = outlier(data3[,i], method = "median", addthres = TRUE)
      dd = data.frame(gfgf$data[,3])
      names(dd) = c(i)
      outl[[i]] = dd
      outl1[[i]] = gfgf
    }
    
    final = do.call('cbind',outl)
    
    library(sjmisc)
    
    hsdjkh = row_count(final, count = TRUE, append = FALSE)
    
    final = data.frame(final, hsdjkh)
    
    Percentage = round((final$rowcount/(dim(final)[2]-1))*100, 2)
    
    final = data.frame(final, Percentage)
    
    final = data.frame(data1[,1],final)
    
    names(final)[1] = c(names(data1)[1])
    
    final1 = final[order(final$Percentage,decreasing = TRUE), ]
    
    final1
    
    final2 = final1 %>% filter(rowcount >=1)
    
    dim2 = dim(final1)[1]/10
    
    if(dim(final2)[1]<=dim2){
      dddd = final2
    }else{
      dddd = final2[1:dim2,]
    }
    
    dddd
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("univariatebox-level1 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(download11(), file, row.names = FALSE)
    })
  
  output[['finaldata']] =  renderDataTable({
    download11()
    
  })
  
  # 
  # observeEvent(input$confirmation2, {
  #   if(input$confirmation2==TRUE){
  output[["one2"]]<- renderPlotly({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = 'Please Wait...!')
    withProgress(message='Loading Plot..!',value=30,style = "notification",
                 {
                   
                   n<-10
                   
                   for(i in 1:n){
                     incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                     file1 <- input$file20
                     data <- read.csv(file1$datapath)
                     # data=data.frame(abs(data))
                     #data=data.frame(read.csv("eee - Copy (2).csv"))
                     data = data[,-1]
                     
                     data1 = unlist(lapply(data, is.numeric))
                     data1 = data[,data1]
                     
                     dkk = dim(data1)[2]
                     
                     p = plot_ly(type = 'box')
                     
                     for (i in 1:dkk) {
                       p = add_boxplot(p, y = data1[,i],name = names(data1)[i])
                     }
                     
                   }
                 })
    
    return(p)
  })
  #   }
  # })
  
  
################################################################### single Factor Box Plot UI & SEVER #######################################################################################################
  
  output[["downunibox"]]<-renderUI({
    tags$div(downloadButton("downloadunibox","Download Data..!"),align="center")
  })
  
  
  download112 = reactive({
    
    outl = list()
    outl1 = list()
    file1 <- input$file20
    # data1 = data3()
    #data0=data.frame(read.csv("eee - Copy (2).csv"))
    data0 <- data.frame(read.csv(file1$datapath))
    
    ff<- input$boxiny
    data3 <- data.frame(data0[ff])
    #names(data) <-"y"
    # data=data.frame(abs(data))
    #data0=data.frame(read.csv("eee - Copy (2).csv"))
    #data = data.frame(data0[,2])
    names(data3)=paste(input$boxiny)
    
    u=boxplot(data3)
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c("s.no",paste(input$boxiny),"outlier")
    count=dim(data0)[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c("s.no",paste(input$boxiny),"outlier")
    box2=data.frame(rbind(box,dummy))
    data00=data.frame(s.no=1:dim(data0)[1],data0)
    box22 = merge(box2[, c(2,3)], 
                  data00)
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[3],outlier=box22[2])
    boxdata=data.frame(s.no=1:dim(data.frame(data0))[1],outlier=rep("No",dim(data.frame(data0))[1]))
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be$s.no),]
    outbox=bdf[!duplicated(bdf[c("s.no")]),]
    final2=data.frame(1:dim(data.frame(data0))[1],data3,outbox$outlier)
    colnames(final2)=c("S.no",paste(input$boxiny),"outlier")
    final2
    
  })
  
  output$downloadunibox <- downloadHandler(
    filename = function() {
      paste("univariatebox-level1 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(download112(), file, row.names = FALSE)
    })
  
get_var_names <- reactive({
    file <- input$file20
    var_names <- names(read.csv(file$datapath))
    var_names
    
  })
  
  output$boxin <- renderUI({
    choices <- get_var_names()
    file <- input$file20
    if( !is.null(file) )
    {
      selectInput(inputId = "boxiny", label = "Choose Y:", choices = choices)
      
    }
  })
  
  
  
  
  output[["boxu"]]<- renderPlotly({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = 'Please Wait...!')
    withProgress(message='Loading Plot..!',value=30,style = "notification",
                 {
                   
                   n<-10
                   
                   for(i in 1:n){
                     incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                     file1 <- input$file20
                     data0 <- data.frame(read.csv(file1$datapath))
                     
                     ff<- input$boxiny
                     data <- data.frame(y=data0[ff])
                     #names(data) <-"y"
                     # data=data.frame(abs(data))
                     #data0=data.frame(read.csv("eee - Copy (2).csv"))
                     #data = data.frame(data0[,2])
                     names(data)=paste(input$boxiny)
                     
                     dkk = dim(data)[2]
                     
                     p = plot_ly(type = 'box',color = I("blue"))
                     
                     for (i in 1:dkk) {
                       p = add_boxplot(p, y = data[,i],name = names(data)[i])
                     }
                     
                   }
                 })
    
    return(p)
  })
  
  output[["lev2box"]] <- renderUI({
    tags$div(bsButton("lev2boxp",strong("Go for Level-2 Analysis..!"),icon = icon("refresh"),style = "primary",size="medium"),
             style="color:white;font-weight:100%;",align="right")
    
  })
  
  observeEvent(input$lev2boxp, {
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1',
      title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Level-2 Analysis<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      size = "l",
      
      tabsetPanel(type = "tabs", 
                  tabPanel(tags$div(h4(strong("Box-plot"),style="color:black;font-size:120%"),align="center"),
                           tags$div(downloadButton("lev2boxdown","Download..!"),align="center"),
                           plotlyOutput('lev2boxplot',height = "600px"))
                  
      ),
      footer = modalButton("OK"),
      easyClose = FALSE
    )))
  })
  
  observeEvent(input$file20, {
    output[["lev2boxplot"]]<- renderPlotly({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      progress$set(message = 'Please Wait...!')
      withProgress(message='Loading Plot..!',value=30,style = "notification",
                   {
                     
                     n<-10
                     
                     for(i in 1:n){
                       incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                       data=allfinalbox()
                       
                       dkk = dim(data)[2]
                       
                       p = plot_ly(type = 'box',color = I("blue"))
                       
                       for (i in dkk) {
                         p = add_boxplot(p, y = data[,i],name = names(data)[i])
                       }
                       
                     }
                   })
      
      return(p)
       })
  })
  
  
  allfinalbox <- function()
  {
    
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    
    ff<- input$boxiny
    data <- data.frame(y=data0[ff])
    #names(data) <-"y"
    # data=data.frame(abs(data))
    #data0=data.frame(read.csv("eee - Copy (2).csv"))
    #data = data.frame(data0[,2])
    names(data)=paste(input$boxiny)
    u=boxplot(data)
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c("s.no",paste(input$boxiny),"outlier")
    count=dim(data)[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c("s.no",paste(input$boxiny),"outlier")
    box2=data.frame(rbind(box,dummy))
    f=data.frame(s.no=1:dim(data)[1],data)
    box22 = merge(box2[, c(2,3)], 
                  f[, c(1,2)])
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[3],box22[2])
    boxdata=data.frame(s.no=1:dim(data.frame(data))[1],outlier=rep("No",dim(data.frame(data))[1]))
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be$s.no),]
    outbox=bdf[!duplicated(bdf[c("s.no")]),]
    final2=data.frame(s.no=1:dim(data.frame(data))[1],data,outlier=outbox$outlier)
    con=data.frame(filter(final2, outlier == "No"))
    data.frame(con[,c(1,2)])
  }
  
  lev2boxdowndata = reactive({
    
   
    data3 <- data.frame(allfinalbox())
    #names(data) <-"y"
    # data=data.frame(abs(data))
    #data0=data.frame(read.csv("eee - Copy (2).csv"))
    #data = data.frame(data0[,2])
    
    
    u=boxplot(data3[,2])
    box=data.frame(s.no=1:dim(data.frame(u$out))[1],u$out,outlier=rep("Yes",dim(data.frame(u$out))[1]))
    colnames(box) <-c("s.no",paste(input$boxiny),"outlier")
    count=dim(data3)[1]-dim(box)[1]
    dummy=data.frame(s.no=1:count,rep(0,count),outlier=rep("No",count))
    colnames(dummy) <-c("s.no",paste(input$boxiny),"outlier")
    box2=data.frame(rbind(box,dummy))
    data00=data.frame(s.no=1:dim(data3)[1],data3[,2])
    box22 = merge(box2[, c(1,3)], 
                  data00)
    #v=data.frame(box22[4],box22[1],box22[2])
    v2=data.frame(box22[1],outlier=box22[2])
    boxdata=data.frame(s.no=1:dim(data.frame(data3))[1],outlier=rep("No",dim(data.frame(data3))[1]))
    be=data.frame(rbind(v2,boxdata))
    bdf=be[order(be$s.no),]
    outbox=bdf[!duplicated(bdf[c("s.no")]),]
    final2=data.frame(1:dim(data.frame(data3))[1],data3[,2],outbox$outlier)
    colnames(final2)=c("S.no",paste(input$boxiny),"outlier")
    final2
    
  })
  
  output$lev2boxdown <- downloadHandler(
    filename = function() {
      paste("univariatebox-level2 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(lev2boxdowndata(), file, row.names = FALSE)
    })
########################################################## Control- Chart ######################################################################################################  
 
  get_var_names2 <- reactive({
    file <- input$file20
    var_names <- names(read.csv(file$datapath))
    var_names
    
  })
   output$inmy <- renderUI({
    choices <- get_var_names2()
    file <- input$file20
    if( !is.null(file) )    {
      selectInput(inputId = "myiny", label = "Choose Y:", choices = choices)
      
    }
  })
  
  output$limits21 <- renderUI({
    file <- input$file20
    if( !is.null(file) )
    {
      numericInput(inputId = "uslimit0",
                   label = "Upper Specification limits:",
                   value = character(0),
      )
    }
  })
  
  output$limits22 <- renderUI({
    file <- input$file20
    if( !is.null(file) )
    {
      numericInput(inputId = "lslimit0",
                   label = "Lower Specification limits:",
                   value = character(0)
      )
    }
  })
  
  output[["lev1dow"]] <- renderUI({
   downloadButton("lev1down","Download..!")
    
  })
  
  output$lev1down <- downloadHandler(
    filename = function() {
      paste("controluni-level1 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(lev1downcont(), file, row.names = FALSE)
    })
  
  lev1downcont <- reactive({
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    ee <- input$myiny
    
    data2 <- data.frame(y=data0[ee])
    names(data2) <-"y"
    
    s=qcc(na.omit(data2$y),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    datacontrol=data.frame(out[,1],data2,out[,2])
    colnames(datacontrol) =c("s.no",paste(input$myiny),"outofcontrol")
    datacontrol
    
  })
  
  output[["contc"]]<- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = 'Please Wait...!')
    withProgress(message='Loading Plot..!',value=30,style = "notification",
                 {
                   
                   n<-1
                   
                   for(i in 1:n){
                     incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                     file1 <- input$file20
                     data0 <- data.frame(read.csv(file1$datapath))
                     ee <- input$myiny
                     
                     data2 <- data.frame(y=data0[ee])
                     names(data2) <-"y"
                     
                     p=qcc(na.omit(data2$y),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
                   }
                 })
    return(p)
  })
  
  
  output[["lev2con"]] <- renderUI({
    tags$div(bsButton("lev2control",strong("Go for Level-2 Analysis..!"),icon = icon("refresh"),style = "primary",size="medium"),
             style="color:white;font-weight:100%;",align="right")
    
  })
  
  observeEvent(input$lev2control, {
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1',
      title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Level-2 Analysis<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      size = "l",
      
      tabsetPanel(type = "tabs", 
                  tabPanel(tags$div(h4(strong("Control-Chart"),style="color:black;font-size:120%"),align="center"),br(),
                           tags$div(downloadButton("lev2downcon","Download..!"),align="center"),br(),
                                    plotOutput('lev2conchart',height = "600px"))
                  
      ),
      footer = modalButton("OK"),
      easyClose = FALSE
    )))
  })
  
  lev2downcont <- reactive({
    file1 <- input$file20
    data0 <- data.frame(filter(lev1downcont(),outofcontrol=="No"))[,c(1,2)]
    ee <- input$myiny
    
    data2 <- data.frame(y=data0[ee])
    names(data2) <-"y"
    
    s=qcc(na.omit(data2$y),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    datacontrol=data.frame(out[,1],data2,out[,2])
    colnames(datacontrol) =c("s.no",paste(input$myiny),"outofcontrol")
    datacontrol
    
  })

  output$lev2downcon <- downloadHandler(
    filename = function() {
      paste("controluni-level2 output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(lev2downcont(), file, row.names = FALSE)
    })
  
  observeEvent(input$file20, {
    output[["lev2conchart"]]<- renderPlot({
      withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
        
        qcc <- function(data, type = c("xbar", "R", "S", "xbar.one", "p", "np", "c", "u", "g"), sizes, center, std.dev, limits, data.name, labels, newdata, newsizes, newdata.name, newlabels, nsigmas = 3, confidence.level, rules = shewhart.rules, plot = TRUE, ...)
        {
          call <- match.call()
          
          if (missing(data))
            stop("'data' argument is not specified")
          
          if(identical(type, eval(formals(qcc)$type)))
          { type <- as.character(type)[1]
          warning("chart 'type' not specified, assuming \"", type, "\"",
                  immediate. = TRUE) }
          if(!exists(paste("stats.", type, sep = ""), mode="function") |
             !exists(paste("sd.", type, sep = ""), mode="function") |
             !exists(paste("limits.", type, sep = ""), mode="function"))
            stop(paste("invalid", type, "control chart. See help(qcc) "))
          
          if (missing(data.name)) 
            data.name <- deparse(substitute(data))
          data <- data.matrix(data)
          if (missing(sizes)) 
          { if (any(type==c("p", "np", "u")))
            stop(paste("sample 'sizes' must be given for a", type, "Chart"))
            else
              sizes <- apply(data, 1, function(x) sum(!is.na(x)))  }
          else
          { if (length(sizes)==1)
            sizes <- rep(sizes, nrow(data))
          else if (length(sizes) != nrow(data))
            stop("sizes length doesn't match with data") }
          
          if (missing(labels))
          { if (is.null(rownames(data))) labels <- 1:nrow(data)
          else                         labels <- rownames(data) }
          
          stats <- paste("stats.", type, sep = "")
          if (!exists(stats, mode="function"))
            stop(paste("function", stats, "is not defined"))
          stats <- do.call(stats, list(data, sizes))
          statistics <- stats$statistics
          if (missing(center)) center <- stats$center
          
          sd <- paste("sd.", type, sep = "")
          if (!exists(sd, mode="function"))
            stop(paste("function", sd, "is not defined!"))
          missing.std.dev <- missing(std.dev)
          if (missing.std.dev)
          { std.dev <- NULL
          std.dev <- switch(type, 
                            "xbar" = { if(any(sizes > 25)) "RMSDF"
                              else                "UWAVE-R" },
                            "xbar.one" = "MR",
                            "R" = "UWAVE-R",
                            "S" = "UWAVE-SD",
                            NULL)
          std.dev <- do.call(sd, list(data, sizes, std.dev)) }
          else 
          { if (is.character(std.dev))
          { std.dev <- do.call(sd, list(data, sizes, std.dev)) }
            else
            { if (!is.numeric(std.dev))
              stop("if provided the argument 'std.dev' must be a method available or a numerical value. See help(qcc).")  }
          }
          
          names(statistics) <-  rownames(data) <-  labels
          names(dimnames(data)) <- list("Group", "Samples")
          
          object <- list(call = call, type = type, 
                         data.name = data.name, data = data, 
                         statistics = statistics, sizes = sizes, 
                         center = center, std.dev = std.dev)
          # check for new data provided and update object
          if (!missing(newdata))
          {   if (missing(newdata.name))
          {newdata.name <- deparse(substitute(newdata))}
            newdata <- data.matrix(newdata)
            if (missing(newsizes))
            { if (any(type==c("p", "np", "u")))
              stop(paste("sample sizes must be given for a", type, "Chart"))
              else
                newsizes <- apply(newdata, 1, function(x) sum(!is.na(x))) }
            else
            { if (length(newsizes)==1)
              newsizes <- rep(newsizes, nrow(newdata))
            else if (length(newsizes) != nrow(newdata))
              stop("newsizes length doesn't match with newdata") }
            stats <- paste("stats.", type, sep = "")
            if (!exists(stats, mode="function"))
              stop(paste("function", stats, "is not defined"))
            newstats <- do.call(stats, list(newdata, newsizes))$statistics
            if (missing(newlabels))
            { if (is.null(rownames(newdata)))
            { start <- length(statistics)
            newlabels <- seq(start+1, start+length(newstats)) }
              else
              { newlabels <- rownames(newdata) }
            }
            names(newstats) <- newlabels
            object$newstats <- newstats
            object$newdata  <- newdata
            object$newsizes <- newsizes
            object$newdata.name <- newdata.name
            statistics <- c(statistics, newstats)
            sizes <- c(sizes, newsizes)
          }
          
          conf <- nsigmas
          if (!missing(confidence.level))
            conf <- confidence.level
          if (conf >= 1)
          { object$nsigmas <- conf }
          else
            if (conf > 0 & conf < 1)
            { object$confidence.level <- conf } 
          
          # get control limits
          if (missing(limits))
          { limits <- paste("limits.", type, sep = "")
          if (!exists(limits, mode="function"))
            stop(paste("function", limits, "is not defined"))
          limits <- do.call(limits, list(center = center, std.dev = std.dev,
                                         sizes = sizes, conf = conf)) 
          }
          else 
          { if (!missing.std.dev)
            warning("'std.dev' is not used when limits is given")
            if (!is.numeric(limits))
              stop("'limits' must be a vector of length 2 or a 2-columns matrix")
            limits <- matrix(limits, ncol = 2)
            dimnames(limits) <- list(rep("",nrow(limits)), c("LCL ", "UCL"))
          }
          
          lcl <- limits[,1]
          ucl <- limits[,2]
          object$limits <- limits
          if (is.function(rules)) violations <- rules(object)
          else                    violations <- NULL
          object$violations <- violations
          
          class(object) <- "qcc"
          if(plot) plot(object, ...) 
          return(object)
        }
        
        
        blues.colors <- function (n) 
        {
          palette <- grDevices::colorRampPalette(c("#03396c", "#005b96", "#6497b1", "#b3cde0"), 
                                                 space = "Lab")
          palette(n)
        }
        
        
        #----------------------------------------------------------------------------#
        # print a short version of a matrix by allowing to select the number of 
        # head/tail rows and columns to display
        
        .printShortMatrix <- function(x, head = 2, tail = 1, chead = 5, ctail = 1, ...)
        { 
          x <- as.matrix(x)
          nr <- nrow(x)
          nc <- ncol(x)
          if(is.na(head <- as.numeric(head))) head <- 2
          if(is.na(tail <- as.numeric(tail))) tail <- 1
          if(is.na(chead <- as.numeric(chead))) chead <- 5
          if(is.na(ctail <- as.numeric(ctail))) ctail <- 1
          
          if(nr > (head + tail + 1))
          { rnames <- rownames(x)
          if(is.null(rnames)) 
            rnames <- paste("[", 1:nr, ",]", sep ="")
          x <- rbind(x[1:head,,drop=FALSE], 
                     rep(NA, nc), 
                     x[(nr-tail+1):nr,,drop=FALSE])
          rownames(x) <- c(rnames[1:head], "...", rnames[(nr-tail+1):nr])
          }
          if(nc > (chead + ctail + 1))
          { cnames <- colnames(x)
          if(is.null(cnames)) 
            cnames <- paste("[,", 1:nc, "]", sep ="")
          x <- cbind(x[,1:chead,drop=FALSE], 
                     rep(NA, nrow(x)), 
                     x[,(nc-ctail+1):nc,drop=FALSE])
          colnames(x) <- c(cnames[1:chead], "...", cnames[(nc-ctail+1):nc])
          }
          
          print(x, na.print = "", ...)
        }
        
        # old version
        # .printShortMatrix <- function(x, head = 2, tail = 1, ...)
        # { 
        #   x <- as.matrix(x)
        #   nr <- nrow(x)
        #   nc <- ncol(x)
        #   if(nr > 4)
        #     { rnames <- rownames(x)
        #       if(is.null(rnames)) 
        #         rnames <- paste("[", 1:nr, ",]", sep ="")
        #       x <- rbind(x[1:head,], rep(NA, nc), x[(nr-tail+1):nr,])
        #       rownames(x) <- c(rnames[1:head], "...", rnames[(nr-tail+1):nr])
        #       print(x, na.print = "", ...)
        #     }
        #   else
        #     { print(x, ...)}  
        # }
        
        #-------------------------------------------------------------------#
        #                                                                   #
        #
        # Options retrieval and setting
        #
        
        qcc.options <- function (...)
        {
          current <- .qcc.options
          if(nargs() == 0) return(current)
          #  if(is.character(...))
          #       temp <- eval(parse(text = paste(c("list(", ..., ")"))))
          #  else temp <- list(...)
          temp <- list(...)
          if(length(temp) == 1 && is.null(names(temp))) 
          { arg <- temp[[1]]
          switch(mode(arg),
                 list = temp <- arg,
                 character = return(.qcc.options[[arg]]),
                 stop(paste("invalid argument:", sQuote(arg)))) }
          if(length(temp) == 0) return(current)
          name <- names(temp)
          if(is.null(name)) stop("options must be given by name")
          changed <- current[name]
          current[name] <- temp
          env <- if(sys.parent() == 0) asNamespace("qcc") 
          else                  parent.frame()
          assign(".qcc.options", current, envir = env)
          invisible(current)
        }
        
        ".qcc.options" <- list(exp.R.unscaled = c(NA, 1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931),
                               se.R.unscaled = c(NA, 0.8525033, 0.8883697, 0.8798108, 0.8640855, 0.8480442, 0.8332108, 0.8198378, 0.8078413, 0.7970584, 0.7873230, 0.7784873, 0.7704257, 0.7630330, 0.7562217, 0.7499188, 0.7440627, 0.7386021, 0.7334929, 0.7286980, 0.7241851, 0.7199267, 0.7158987, 0.7120802, 0.7084528, 0.7050004, 0.7017086, 0.6985648, 0.6955576, 0.6926770, 0.6899137, 0.6872596, 0.6847074, 0.6822502, 0.6798821, 0.6775973, 0.6753910, 0.6732584, 0.6711952, 0.6691976, 0.6672619, 0.6653848, 0.6635632, 0.6617943, 0.6600754, 0.6584041, 0.6567780, 0.6551950, 0.6536532, 0.6521506),
                               beyond.limits = list(pch=19, col="red"),
                               violating.runs = list(pch=19, col="black"),
                               run.length = 7,
                               # bg.margin = "lightgrey",
                               bg.margin = "#E5E5E5",
                               bg.figure = "white",
                               cex = 1,
                               font.stats = 1,
                               cex.stats = 0.9)
        
        .onAttach <- function(library, pkg)
        {
          ## we can't do this in .onLoad
          unlockBinding(".qcc.options", asNamespace("qcc"))
          description <- readLines(system.file("DESCRIPTION", package = "qcc"))
          version <- grep("Version:", description, ignore.case = TRUE, value = TRUE)
          version <- gsub(pattern = "Version:", replacement = "", version, ignore.case = TRUE)
          version <- gsub(pattern = " ", replacement = "", version)
          packageStartupMessage("Package 'qcc', version ", version)
          packageStartupMessage("Type 'citation(\"qcc\")' for citing this R package in publications.")
          invisible()
        }
        
        print.qcc <- function(x, ...) str(x,1)
        
        summary.qcc <- function(object, digits =  getOption("digits"), ...)
        {
          #object <- x   # Argh.  Really want to use 'object' anyway
          cat("\nCall:\n",deparse(object$call),"\n\n",sep="")
          data.name <- object$data.name
          type <- object$type
          cat(paste(type, "chart for", data.name, "\n"))
          statistics <- object$statistics
          cat("\nSummary of group statistics:\n")
          print(summary(statistics), digits = digits, ...)
          sizes <- object$sizes
          if(length(unique(sizes))==1)
            sizes <- sizes[1]
          if(length(sizes) == 1)
            cat("\nGroup sample size: ", format(sizes))
          else {
            cat("\nSummary of group sample sizes: ")
            tab <- table(sizes)
            print(matrix(c(as.numeric(names(tab)), tab), 
                         ncol = length(tab), byrow = TRUE, 
                         dimnames = list(c("  sizes", "  counts"),
                                         character(length(tab)))), 
                  digits = digits, ...)
          }
          cat("\nNumber of groups: ", length(statistics))
          
          center <- object$center
          if(length(center) == 1)
          { cat("\nCenter of group statistics: ", format(center, digits = digits)) }
          else
          { out <- paste(format(center, digits = digits))
          out <- out[which(cumsum(nchar(out)+1) < getOption("width")-40)]      
          out <- paste0(paste(out, collapse = " "), " ...")
          cat("\nCenter of group statistics: ", out, sep = "")
          }
          
          sd <- object$std.dev
          if(length(sd) == 1)
          { cat("\nStandard deviation: ", format(sd, digits = digits), "\n") }
          else
          { out <- paste(format(sd, digits = digits))
          out <- out[which(cumsum(nchar(out)+1) < getOption("width")-40)]
          out <- paste0(paste(out, collapse = " "), " ...")
          cat("\nStandard deviation: ", out, "\n", sep = "")
          }
          
          newdata.name <- object$newdata.name
          newstats <- object$newstats
          if (!is.null(newstats)) 
          { cat(paste("\nSummary of group statistics in ", 
                      newdata.name, ":\n", sep = ""))
            print(summary(newstats), digits = digits, ...)
            newsizes <- object$newsizes
            if (length(unique(newsizes)) == 1)
              newsizes <- newsizes[1]
            if (length(newsizes) == 1)
              cat("\nGroup sample size: ", format(newsizes))
            else 
            { cat("\nSummary of group sample sizes:\n")
              new.tab <- table(newsizes)
              print(matrix(c(as.numeric(names(new.tab)), new.tab),
                           ncol = length(new.tab), byrow = TRUE, 
                           dimnames = list(c("  sizes", "  counts"),
                                           character(length(new.tab)))), 
                    digits = digits, ...)
            }
            cat("\nNumber of groups: ", length(newstats), "\n")
          }
          
          limits <- object$limits
          if (!is.null(limits)) 
          { cat("\nControl limits:\n")
            .printShortMatrix(limits, digits = digits, ...) }
          
          invisible()
        }
        
        
        plot.qcc <- function(x, add.stats = TRUE, chart.all = TRUE, 
                             label.limits = c("LCL ", "UCL"),
                             title, xlab, ylab, ylim, axes.las = 0,
                             digits =  getOption("digits"),
                             restore.par = TRUE, ...) 
        {
          object <- x  # Argh.  Really want to use 'object' anyway
          if ((missing(object)) | (!inherits(object, "qcc")))
            stop("an object of class `qcc' is required")
          
          # collect info from object
          type <- object$type
          std.dev <- object$std.dev
          data.name <- object$data.name
          center <- object$center
          stats <- object$statistics
          limits <- object$limits 
          lcl <- limits[,1]
          ucl <- limits[,2]
          newstats <- object$newstats
          newdata.name <- object$newdata.name
          violations <- object$violations
          if(chart.all) 
          { statistics <- c(stats, newstats)
          indices <- 1:length(statistics) }
          else
          { if(is.null(newstats))
          { statistics <- stats
          indices <- 1:length(statistics) }
            else
            { statistics <- newstats 
            indices <- seq(length(stats)+1, length(stats)+length(newstats)) }
          }
          
          if (missing(title))
          { if (is.null(newstats))
            main.title <- paste(type, "Chart\nfor", data.name)
          else if (chart.all)
            main.title <- paste(type, "Chart\nfor", data.name, 
                                "and", newdata.name)
          else main.title <- paste(type, "Chart\nfor", newdata.name) 
          }
          else main.title <- paste(title)
          
          oldpar <- par(no.readonly = TRUE)
          if(restore.par) on.exit(par(oldpar))
          mar <- pmax(oldpar$mar, c(4.1,4.1,3.1,2.1))
          par(bg  = qcc.options("bg.margin"), 
              cex = oldpar$cex * qcc.options("cex"),
              mar = if(add.stats) pmax(mar, c(7.6,0,0,0)) else mar)
          
          # plot Shewhart chart
          plot(indices, statistics, type="n",
               ylim = if(!missing(ylim)) ylim 
               else range(statistics, limits, center),
               ylab = if(missing(ylab)) "Group summary statistics" else ylab,
               xlab = if(missing(xlab)) "Group" else xlab, 
               axes = FALSE)
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
               col = qcc.options("bg.figure"))
          axis(1, at = indices, las = axes.las,
               labels = if(is.null(names(statistics))) 
                 as.character(indices) else names(statistics))
          axis(2, las = axes.las)
          box()
          top.line <- par("mar")[3]-length(capture.output(cat(main.title)))
          top.line <- top.line - if(chart.all & (!is.null(newstats))) 0.1 else 0.5
          mtext(main.title, side = 3, line = top.line,
                font = par("font.main"), 
                cex  = qcc.options("cex"), 
                col  = par("col.main"))
          
          lines(indices, statistics, type = "b", pch=20) 
          
          if(length(center) == 1)
            abline(h = center)
          else lines(indices, center[indices], type="s")
          
          if(length(lcl) == 1) 
          { abline(h = lcl, lty = 2)
            abline(h = ucl, lty = 2)
          }
          else 
          { lines(indices, lcl[indices], type="s", lty = 2)
            lines(indices, ucl[indices], type="s", lty = 2) }
          mtext(label.limits, side = 4, at = c(rev(lcl)[1], rev(ucl)[1]), 
                las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
          mtext("CL", side = 4, at = rev(center)[1], 
                las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
          
          if(is.null(qcc.options("violating.runs")))
            stop(".qcc.options$violating.runs undefined. See help(qcc.options).")
          if(length(violations$violating.runs))
          { v <- violations$violating.runs
          if(!chart.all & !is.null(newstats))
          { v <- v - length(stats) 
          v <- v[v>0] }
          points(indices[v], statistics[v], 
                 col = qcc.options("violating.runs")$col, 
                 pch = qcc.options("violating.runs")$pch) 
          }
          
          if(is.null(qcc.options("beyond.limits")))
            stop(".qcc.options$beyond.limits undefined. See help(qcc.options).")
          if(length(violations$beyond.limits))
          { v <- violations$beyond.limits
          if(!chart.all & !is.null(newstats))
          { v <- v - length(stats) 
          v <- v[v>0] }
          points(indices[v], statistics[v], 
                 col = qcc.options("beyond.limits")$col, 
                 pch = qcc.options("beyond.limits")$pch) 
          }
          
          if(chart.all & (!is.null(newstats)))
          { len.obj.stats <- length(object$statistics)
          len.new.stats <- length(statistics) - len.obj.stats
          abline(v = len.obj.stats + 0.5, lty = 3)
          mtext(# paste("Calibration data in", data.name),
            "Past Data", cex = par("cex")*0.8,
            at = len.obj.stats/2, line = 0, adj = 0.5)
          mtext(# paste("New data in", object$newdata.name),  
            "Forecast Data", cex = par("cex")*0.8, 
            at = len.obj.stats + len.new.stats/2, line = 0, adj = 0.5)
          }
          
          if(add.stats) 
          { 
            # computes the x margins of the figure region
            plt <- par()$plt; usr <- par()$usr
            px <- diff(usr[1:2])/diff(plt[1:2])
            xfig <- c(usr[1]-px*plt[1], usr[2]+px*(1-plt[2]))
            at.col <- xfig[1] + diff(xfig[1:2])*c(0.10, 0.40, 0.65)
            top.line <- 4.5
            # write info at bottom
            mtext(paste("Number of Observations = ", length(statistics), sep = ""), 
                  side = 1, line = top.line, adj = 0, at = at.col[1],
                  font = qcc.options("font.stats"),
                  cex = par("cex")*qcc.options("cex.stats"))
            center <- object$center
            if(length(center) == 1)
            { mtext(paste("Center = ", signif(center[1], digits), sep = ""),
                    side = 1, line = top.line+1, adj = 0, at = at.col[1],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            else 
            { mtext("Center is variable",
                    side = 1, line = top.line+1, adj = 0, at = at.col[1],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            
            if(length(std.dev) == 1)
            { mtext(paste("StdDev = ", signif(std.dev, digits), sep = ""),
                    side = 1, line = top.line+2, adj = 0, at = at.col[1],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }      
            else
            { mtext("StdDev is variable",
                    side = 1, line = top.line+2, adj = 0, at = at.col[1],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            
            if(length(unique(lcl)) == 1)
            { mtext(paste("LCL = ", signif(lcl[1], digits), sep = ""), 
                    side = 1, line = top.line+1, adj = 0, at = at.col[2],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            else 
            { mtext("LCL is variable", 
                    side = 1, line = top.line+1, adj = 0, at = at.col[2],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            
            if(length(unique(ucl)) == 1)
            { mtext(paste("UCL = ", signif(ucl[1], digits), sep = ""),
                    side = 1, line = top.line+2, adj = 0, at = at.col[2],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats")) 
            }
            else 
            { mtext("UCL is variable", 
                    side = 1, line = top.line+2, adj = 0, at = at.col[2],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
            }
            
            if(!is.null(violations))
            { mtext(paste("Number beyond limits =",
                          length(unique(violations$beyond.limits))), 
                    side = 1, line = top.line+1, adj = 0, at = at.col[3],
                    font = qcc.options("font.stats"),
                    cex = par("cex")*qcc.options("cex.stats"))
              # mtext(paste("Number in warning Zone =",
              #             length(unique(violations$violating.runs))), 
              #       side = 1, line = top.line+2, adj = 0, at = at.col[3],
              #       font = qcc.options("font.stats"),
              #       cex = par("cex")*qcc.options("cex.stats"))
            }
          }
          
          invisible()
        }
        
        #
        #  Functions used to compute Shewhart charts statistics
        #
        
        .qcc.c4 <- function(n)
        { sqrt(2/(n - 1)) * exp(lgamma(n/2) - lgamma((n - 1)/2)) }
        
        # xbar
        
        stats.xbar <- function(data, sizes)
        {
          if (missing(sizes))
            sizes <- apply(data, 1, function(x) sum(!is.na(x)))
          statistics <- apply(data, 1, mean, na.rm=TRUE)
          center <- sum(sizes * statistics)/sum(sizes)
          list(statistics = statistics, center = center)
        }
        
        sd.xbar <- function(data, sizes, std.dev = c("UWAVE-R", "UWAVE-SD", "MVLUE-R", "MVLUE-SD", "RMSDF"))
        {
          if (!is.numeric(std.dev))
            std.dev <- match.arg(std.dev)
          if (missing(sizes))
            sizes <- apply(data, 1, function(x) sum(!is.na(x)))
          if (any(sizes == 1))
            stop("group sizes must be larger than one")
          c4 <- .qcc.c4
          if (is.numeric(std.dev))
          { sd <- std.dev }
          else
          { switch(std.dev, 
                   "UWAVE-R" = {  R <- apply(data, 1, function(x) 
                     diff(range(x, na.rm = TRUE)))
                   d2 <- qcc.options("exp.R.unscaled")[sizes]
                   sd <- sum(R/d2)/length(sizes) }, 
                   "UWAVE-SD" = { S <- apply(data, 1, sd, na.rm = TRUE)
                   sd <- sum(S/c4(sizes))/length(sizes) },
                   "MVLUE-R"  = { R <- apply(data, 1, function(x) 
                     diff(range(x, na.rm = TRUE)))
                   d2 <- qcc.options("exp.R.unscaled")[sizes]
                   d3 <- qcc.options("se.R.unscaled")[sizes]
                   w  <- (d2/d3)^2
                   sd <- sum(R/d2*w)/sum(w) }, 
                   "MVLUE-SD" = { S <- apply(data, 1, sd, na.rm = TRUE)
                   w  <- c4(sizes)^2/(1-c4(sizes)^2)
                   sd <- sum(S/c4(sizes)*w)/sum(w) },
                   "RMSDF" =    { S <- apply(data, 1, sd, na.rm = TRUE)
                   w  <- sizes-1
                   sd <- sqrt(sum(S^2*w)/sum(w))/c4(sum(w)+1) }
          )
          }
          #  if (missing(std.dev))
          #     var.within <- apply(data, 1, var, na.rm=TRUE)
          #  else 
          #     var.within <- std.dev^2
          #  var.df <- sum(sizes - 1)
          #  if (equal.sd) 
          #     { std.dev <- sqrt(sum((sizes - 1) * var.within)/var.df) / c4(var.df + 1) }
          #  else 
          #     { c <- c4(sizes)/(1 - c4(sizes)^2)
          #       std.dev <- sum(c * sqrt(var.within))/sum(c * c4(sizes)) }
          return(sd)
        }
        
        limits.xbar <- function(center, std.dev, sizes, conf)
        {
          if (length(unique(sizes))==1)
            sizes <- sizes[1]
          se.stats <- std.dev/sqrt(sizes)
          if (conf >= 1) 
          { lcl <- center - conf * se.stats
          ucl <- center + conf * se.stats
          }
          else 
          { if (conf > 0 & conf < 1) 
          { nsigmas <- qnorm(1 - (1 - conf)/2)
          lcl <- center - nsigmas * se.stats
          ucl <- center + nsigmas * se.stats
          }
            else stop("invalid 'conf' argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          return(limits)
        }
        
        # S chart
        
        stats.S <- function(data, sizes)
        {
          if (missing(sizes))
            sizes <- apply(data, 1, function(x) sum(!is.na(x)))
          if(ncol(data)==1) 
          { statistics <- as.vector(data) }
          else 
          { statistics <- sqrt(apply(data, 1, var, na.rm=TRUE)) }
          if (length(sizes == 1))
            sizes <- rep(sizes, length(statistics))
          center <- sum(sizes * statistics)/sum(sizes)
          list(statistics = statistics, center = center)
        }
        
        sd.S <- function(data, sizes, std.dev = c("UWAVE-SD", "MVLUE-SD", "RMSDF"))
        {
          if (!is.numeric(std.dev))
            std.dev <- match.arg(std.dev)
          sd.xbar(data, sizes, std.dev)
        }
        
        limits.S <- function(center, std.dev, sizes, conf)
        {
          if (length(unique(sizes))==1)
            sizes <- sizes[1]
          c4 <- .qcc.c4
          se.stats <- std.dev * sqrt(1 - c4(sizes)^2)
          if (conf >= 1) 
          { lcl <- pmax(0, center - conf * se.stats)
          ucl <- center + conf * se.stats
          }
          else 
          { if (conf > 0 & conf < 1) 
          { ucl <- std.dev * sqrt(qchisq(1 - (1 - conf)/2, sizes - 1)/
                                    (sizes - 1))
          lcl <- std.dev * sqrt(qchisq((1 - conf)/2, sizes - 1)/
                                  (sizes - 1))
          }
            else stop("invalid conf argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          limits
        }
        
        # R Chart 
        
        stats.R <- function(data, sizes)
        {
          if (missing(sizes))
            sizes <- apply(data, 1, function(x) sum(!is.na(x)))
          if(ncol(data)==1) 
          { statistics <- as.vector(data) }
          else 
          { statistics <- apply(data, 1, function(x) diff(range(x, na.rm=TRUE))) }
          if (length(sizes == 1))
            sizes <- rep(sizes, length(statistics))
          center <- sum(sizes * statistics)/sum(sizes)
          list(statistics = statistics, center = center)
        }
        
        sd.R <- function(data, sizes, std.dev = c("UWAVE-R", "MVLUE-R"))
        {
          if (!is.numeric(std.dev))
            std.dev <- match.arg(std.dev)
          sd.xbar(data, sizes, std.dev)
        }
        
        limits.R <- function(center, std.dev, sizes, conf)
        {
          if (length(unique(sizes))==1)
            sizes <- sizes[1]
          se.R.unscaled <- qcc.options("se.R.unscaled")
          Rtab <- length(se.R.unscaled)
          if (conf >= 1) 
          { if (any(sizes > Rtab))
            stop(paste("group size must be less than", 
                       Rtab + 1, "when giving nsigmas"))
            se.R <- se.R.unscaled[sizes] * std.dev
            lcl <- pmax(0, center - conf * se.R)
            ucl <- center + conf * se.R
          }
          else 
          { if (conf > 0 && conf < 1) 
          { ucl <- qtukey(1 - (1 - conf)/2, sizes, 1e100) * std.dev
          lcl <- qtukey((1 - conf)/2, sizes, 1e100) * std.dev
          }
            else stop("invalid conf argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          return(limits)
        }
        
        # xbar Chart for one-at-time data
        
        stats.xbar.one <- function(data, sizes)
        {
          statistics <- as.vector(data)
          center <- mean(statistics)
          list(statistics = statistics, center = center)
        }
        
        sd.xbar.one <- function(data, sizes, std.dev = c("MR", "SD"), k = 2)
        {
          data <- as.vector(data)
          n <- length(data)
          if(!is.numeric(std.dev)) 
            std.dev <- match.arg(std.dev)
          c4 <- .qcc.c4
          if(is.numeric(std.dev)) 
          { sd <- std.dev }
          else
          { switch(std.dev, 
                   "MR" = { d2 <- qcc.options("exp.R.unscaled")
                   if(is.null(d2))
                     stop(".qcc.options$exp.R.unscaled is null")
                   d <- 0
                   for(j in k:n)
                     d <- d+abs(diff(range(data[c(j:(j-k+1))])))
                   sd <- (d/(n-k+1))/d2[k] },
                   "SD" = { sd <- sd(data)/c4(n) },
                   sd <- NULL)
          }
          return(sd)
        }
        
        
        limits.xbar.one <- function(center, std.dev, sizes, conf)
        {
          se.stats <- std.dev
          if (conf >= 1) 
          { lcl <- center - conf * se.stats
          ucl <- center + conf * se.stats
          }
          else 
          { if (conf > 0 & conf < 1) 
          { nsigmas <- qnorm(1 - (1 - conf)/2)
          lcl <- center - nsigmas * se.stats
          ucl <- center + nsigmas * se.stats
          }
            else stop("invalid conf argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          return(limits)
        }
        
        
        # p Chart
        
        stats.p <- function(data, sizes)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          pbar <- sum(data)/sum(sizes)
          list(statistics = data/sizes, center = pbar)
        }
        
        sd.p <- function(data, sizes, ...)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          pbar <- sum(data)/sum(sizes)
          std.dev <- sqrt(pbar * (1 - pbar))
          return(std.dev)
        }
        
        limits.p <- function(center, std.dev, sizes, conf)
        { 
          limits.np(center * sizes, std.dev, sizes, conf) / sizes
        }
        
        # np Chart
        
        stats.np <- function(data, sizes)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          pbar <- sum(data)/sum(sizes)
          center <- sizes * pbar
          if (length(unique(center)) == 1)
            center <- center[1]
          list(statistics = data, center = center)
        }
        
        sd.np <- function(data, sizes, ...)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          pbar <- sum(data)/sum(sizes)
          std.dev <- sqrt(sizes * pbar * (1 - pbar))
          if (length(unique(std.dev)) == 1)
            std.dev <- std.dev[1]
          return(std.dev)
        }
        
        limits.np <- function(center, std.dev, sizes, conf)
        { 
          sizes <- as.vector(sizes)
          if (length(unique(sizes)) == 1)
            sizes <- sizes[1]
          pbar <- mean(center / sizes)
          if (conf >= 1)
          { tol <- conf * sqrt(pbar * (1 - pbar) * sizes)
          lcl <- pmax(center - tol, 0)
          ucl <- pmin(center + tol, sizes)
          }
          else
          { if (conf > 0 & conf < 1)
          { lcl <- qbinom((1 - conf)/2, sizes, pbar)
          ucl <- qbinom((1 - conf)/2, sizes, pbar, lower.tail = FALSE)
          }
            else stop("invalid conf argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          return(limits)
        }
        
        # c Chart
        
        stats.c <- function(data, sizes)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          if (length(unique(sizes)) != 1)
            stop("all sizes must be be equal for a c chart")
          statistics <- data
          center <- mean(statistics)
          list(statistics = statistics, center = center)
        }
        
        sd.c <- function(data, sizes, ...)
        {
          data <- as.vector(data)
          std.dev <- sqrt(mean(data))
          return(std.dev)
        }
        
        limits.c <- function(center, std.dev, sizes, conf)
        {
          if (conf >= 1) 
          { lcl <- center - conf * sqrt(center)
          lcl[lcl < 0] <- 0
          ucl <- center + conf * sqrt(center)
          }
          else 
          { if (conf > 0 & conf < 1) 
          { ucl <- qpois(1 - (1 - conf)/2, center)
          lcl <- qpois((1 - conf)/2, center)
          }
            else stop("invalid conf argument. See help.")
          }
          limits <- matrix(c(lcl, ucl), ncol = 2)
          rownames(limits) <- rep("", length = nrow(limits))
          colnames(limits) <- c("LCL", "UCL")
          return(limits)
        }
        
        # u Chart
        
        stats.u <- function(data, sizes)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          statistics <- data/sizes
          center <- sum(sizes * statistics)/sum(sizes)
          list(statistics = statistics, center = center)
        }
        
        sd.u <- function(data, sizes, ...)
        {
          data <- as.vector(data)
          sizes <- as.vector(sizes)
          std.dev <- sqrt(sum(data)/sum(sizes))
          return(std.dev)
        }
        
        limits.u <- function(center, std.dev, sizes, conf)
        {
          sizes <- as.vector(sizes)
          if (length(unique(sizes))==1)
            sizes <- sizes[1]
          limits.c(center * sizes, std.dev, sizes, conf) / sizes
        }
        
        #
        # Functions used to signal points out of control 
        #
        
        shewhart.rules <- function(object, limits = object$limits, run.length = qcc.options("run.length"))
        {
          # Return a list of cases beyond limits and violating runs
          bl <- beyond.limits(object, limits = limits)
          vr <- violating.runs(object, run.length = run.length)
          list(beyond.limits = bl, violating.runs = vr)
        }
        
        beyond.limits <- function(object, limits = object$limits)
        {
          # Return cases beyond limits
          statistics <- c(object$statistics, object$newstats) 
          lcl <- limits[,1]
          ucl <- limits[,2]
          index.above.ucl <- seq(along = statistics)[statistics > ucl]
          index.below.lcl <- seq(along = statistics)[statistics < lcl]
          return(c(index.above.ucl, index.below.lcl))
        }
        
        violating.runs <- function(object, run.length = qcc.options("run.length"))
        {
          # Return indices of points violating runs
          if(run.length == 0)
            return(numeric())
          center <- object$center
          statistics <- c(object$statistics, object$newstats)
          cl <- object$limits
          diffs <- statistics - center
          diffs[diffs > 0] <- 1
          diffs[diffs < 0] <- -1
          runs <- rle(diffs)
          vruns <- rep(runs$lengths >= run.length, runs$lengths)
          vruns.above <- (vruns & (diffs > 0))
          vruns.below <- (vruns & (diffs < 0))
          rvruns.above <- rle(vruns.above)
          rvruns.below <- rle(vruns.below)
          vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
            (rvruns.above$lengths - run.length)[rvruns.above$values]
          vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
          vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
            (rvruns.below$lengths - run.length)[rvruns.below$values]
          vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
          violators <- numeric()
          if (length(vbeg.above)) 
          { for (i in 1:length(vbeg.above))
            violators <- c(violators, vbeg.above[i]:vend.above[i]) }
          if (length(vbeg.below)) 
          { for (i in 1:length(vbeg.below))
            violators <- c(violators, vbeg.below[i]:vend.below[i]) }
          return(violators)
        }
        
        #-------------------------------------------------------------------#
        #                                                                   #
        #          Operating Characteristic Function                        #
        #                                                                   #
        #-------------------------------------------------------------------#
        
        oc.curves <- function(object, ...)
        {
          # Draws the operating characteristic curves for the qcc object 
          
          if ((missing(object)) | (!inherits(object, "qcc")))
            stop("an object of class 'qcc' is required")
          
          size <- unique(object$sizes)
          if (length(size)>1)
            stop("Operating characteristic curves available only for equal sample sizes!")
          
          beta <- switch(object$type,
                         xbar = oc.curves.xbar(object, ...),
                         R    = oc.curves.R(object, ...),
                         S    = oc.curves.S(object, ...),
                         np   =,
                         p    = oc.curves.p(object, ...),
                         u    =,
                         c    = oc.curves.c(object, ...))
          if (is.null(beta))
            stop("Operating characteristic curves not available for this type of chart.")
          
          invisible(beta)
        }
        
        oc.curves.xbar <- function(object, n, c = seq(0, 5, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
        {
          # Draw the operating-characteristic curves for the xbar-chart with nsigmas
          # limits. The values on the vertical axis give the probability of not detecting
          # a shift of c*sigma in the mean on the first sample following the shift.
          
          if (!(object$type=="xbar"))
            stop("not a `qcc' object of type \"xbar\".")
          
          size <- unique(object$sizes)
          if (length(size) > 1)
            stop("Operating characteristic curves available only for equal sample sizes!")
          if (missing(n))
            n <- unique(c(size, c(1,5,10,15,20)))
          if (is.null(nsigmas))
            nsigmas <- qnorm(1 - (1 - object$confidence.level) / 2)
          
          beta <- matrix(as.double(NA), length(n), length(c))
          for (i in 1:length(n))
            beta[i,] <- pnorm(nsigmas-c*sqrt(n[i])) - pnorm(-nsigmas-c*sqrt(n[i]))
          rownames(beta) <- paste("n=",n,sep="")
          colnames(beta) <- c
          
          oldpar <- par(no.readonly = TRUE)
          if(restore.par) on.exit(par(oldpar))
          par(bg  = qcc.options("bg.margin"), 
              cex = oldpar$cex * qcc.options("cex"),
              mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
          
          plot(c, beta[1,], type="n",
               ylim = c(0,1), xlim = c(0,max(c)),
               xlab = "Process shift (std.dev)",
               ylab = "Prob. type II error ")
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
               col = qcc.options("bg.figure"))
          box()
          mtext(paste("OC curves for", object$type, "Chart"), 
                side = 3, line = par("mar")[3]/3,
                font = par("font.main"), 
                cex  = qcc.options("cex"), 
                col  = par("col.main"))
          for(i in 1:length(n))
            lines(c, beta[i,], type = "l", lty=i)
          beta <- t(beta)
          names(dimnames(beta)) <- c("shift (std.dev)", "sample size")
          
          if (identify)
          { cs <- rep(c,length(n))
          betas <- as.vector(beta)
          labels <- paste("c=", formatC(cs, 2, flag="-"), 
                          ": beta=", formatC(betas, 4, flag="-"), 
                          ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
          i <- identify(cs, betas, labels, pos=4, offset=0.2)
          apply(as.matrix(labels[i$ind]), 1, cat, "\n")
          }
          else
          { legend(max(c), 1, legend = paste("n =", n), 
                   bg = qcc.options("bg.figure"),
                   lty = 1:length(n), xjust = 1, yjust = 1)
          }
          invisible(beta)
        }
        
        oc.curves.p <- function(object, nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
        {
          if (!(object$type=="p" | object$type=="np"))
            stop("not a `qcc' object of type \"p\" or \"np\".")
          
          size <- unique(object$sizes)
          if (length(size) > 1)
            stop("Operating characteristic curves available only for equal sample sizes!")
          
          if (is.null(object$limits))
            stop("the `qcc' object does not have control limits!")
          limits <- object$limits
          p <- seq(0, 1, length=101)
          
          if (object$type=="p") 
          { UCL <- min(floor(size*limits[,2]), size)
          LCL <- max(floor(size*limits[,1]), 0) }
          else
          { UCL <- min(floor(limits[,2]), size)
          LCL <- max(floor(limits[,1]), 0) }
          beta <- pbinom(UCL, size, p) - pbinom(LCL-1, size, p)
          names(beta) <- p
          
          oldpar <- par(no.readonly = TRUE)
          if(restore.par) on.exit(par(oldpar))
          par(bg  = qcc.options("bg.margin"), 
              cex = oldpar$cex * qcc.options("cex"),
              mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
          
          plot(p, beta, type = "n", 
               ylim = c(0,1), xlim = c(0,1),
               xlab = expression(p), 
               ylab = "Prob. type II error ")
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
               col = qcc.options("bg.figure"))
          box()
          mtext(paste("OC curves for", object$type, "Chart"), 
                side = 3, line = par("mar")[3]/3,
                font = par("font.main"), 
                cex  = qcc.options("cex"), 
                col  = par("col.main"))
          
          lines(p, beta)
          lines(rep(p[which.max(beta)], 2), c(0, max(beta)), lty = 2)
          
          warning("Some computed values for the type II error have been rounded due to the discreteness of the binomial distribution. Thus, some ARL values might be meaningless.")
          
          if (identify)
          { labels <- paste("p=", formatC(p, 2, flag="-"), 
                            ": beta=", formatC(beta, 4, flag="-"), 
                            ", ARL=", formatC(1/(1-beta), 2, flag="-"), sep="")
          i <- identify(p, beta, labels, pos=4, offset=0.2)
          apply(as.matrix(labels[i$ind]), 1, cat, "\n")
          }
          invisible(beta)  
        }
        
        oc.curves.c <- function(object, nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
        {
          type <- object$type
          if (!(object$type=="c" | object$type=="u"))
            stop("not a `qcc' object of type \"c\" or \"u\".")
          
          size <- unique(object$sizes)
          if (length(size) > 1)
            stop("Operating characteristic curves available only for equal sample size!")
          
          if (is.null(object$limits))
            stop("the `qcc' object does not have control limits!")
          limits <- object$limits
          CL  <- object$center
          std.dev <- object$std.dev
          if (object$type=="c") 
          { max.lambda <- ceiling(CL+10*std.dev)
          UCL <- floor(limits[1,2])
          LCL <- floor(limits[1,1])
          }
          else
          { max.lambda <- ceiling(CL*size+10*std.dev)[1]
          UCL <- floor(size*limits[1,2])
          LCL <- floor(size*limits[1,1])
          }
          lambda <- seq(0, max.lambda)
          beta <- ppois(UCL, lambda) - ppois(LCL-1, lambda)
          names(beta) <- lambda
          
          oldpar <- par(no.readonly = TRUE)
          if(restore.par) on.exit(par(oldpar))
          par(bg  = qcc.options("bg.margin"), 
              cex = oldpar$cex * qcc.options("cex"),
              mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
          
          plot(lambda, beta, type = "n", 
               ylim = c(0,1), xlim = range(lambda),
               xlab = "Mean", 
               ylab = "Prob. type II error ")
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
               col = qcc.options("bg.figure"))
          box()
          mtext(paste("OC curves for", object$type, "Chart"), 
                side = 3, line = par("mar")[3]/3,
                font = par("font.main"), 
                cex  = qcc.options("cex"), 
                col  = par("col.main"))
          
          lines(lambda, beta)     
          lines(rep(lambda[which.max(beta)], 2), c(0, max(beta)), lty = 2)
          
          warning("Some computed values for the type II error have been rounded due to the discreteness of the Poisson distribution. Thus, some ARL values might be meaningless.")
          
          if (identify)
          { labels <- paste("lambda=", formatC(lambda, 0, flag="-"), 
                            ": beta=", formatC(beta, 4, flag="-"), 
                            ", ARL=", formatC(1/(1-beta), 2, flag="-"), sep="")
          i <- identify(lambda, beta, labels, pos=4, offset=0.2)
          apply(as.matrix(labels[i$ind]), 1, cat, "\n")
          }
          invisible(beta)
        }
        
        oc.curves.R <-
          function(object, n, c = seq(1, 6, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
          {
            # Draw the operating-characteristic curves for the R-chart with nsigmas
            # limits. The values on the vertical axis give the probability of not detecting
            # a change from sigma to c*sigma on the first sample following the change.
            
            if (!(object$type=="R"))
              stop("not a `qcc' object of type \"R\".")
            
            size <- unique(object$sizes)
            if (length(size) > 1)
              stop("Operating characteristic curves available only for equal sample sizes!")
            if (missing(n))
              n <- unique(c(size, c(2,5,10,15,20)))
            if (is.null(nsigmas))
            { tail.prob <- (1 - object$confidence.level) / 2
            beta.fun1 <- function(c, n, p)
            {
              lcl <- qtukey(p, n, Inf)
              ucl <- qtukey(p, n, Inf, lower.tail = FALSE)
              ptukey(ucl / c, n, Inf) - ptukey(lcl / c, n, Inf)
            }
            beta <- outer(c, n, beta.fun1, tail.prob)
            }
            else
            { exp.R.unscaled <- qcc.options("exp.R.unscaled")
            se.R.unscaled <- qcc.options("se.R.unscaled")
            Rtab <- min(length(exp.R.unscaled), length(se.R.unscaled))
            if (any(n > Rtab))
              stop(paste("group size must be less than",
                         Rtab + 1, "when giving nsigmas"))
            beta.fun2 <- function(c, n, conf)
            {
              d2 <- exp.R.unscaled[n]
              d3 <- se.R.unscaled[n]
              lcl <- pmax(0, d2 - conf * d3)
              ucl <- d2 + conf * d3
              ptukey(ucl / c, n, Inf) - ptukey(lcl / c, n, Inf)
            }
            beta <- outer(c, n, beta.fun2, nsigmas)
            }
            
            colnames(beta) <- paste("n=",n,sep="")
            rownames(beta) <- c
            
            oldpar <- par(no.readonly = TRUE)
            if(restore.par) on.exit(par(oldpar))
            par(bg  = qcc.options("bg.margin"), 
                cex = oldpar$cex * qcc.options("cex"),
                mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
            
            plot(c, beta[,1], type="n",
                 ylim = c(0,1), xlim = c(1,max(c)),
                 xlab = "Process scale multiplier",
                 ylab = "Prob. type II error ")
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
                 col = qcc.options("bg.figure"))
            box()
            mtext(paste("OC curves for", object$type, "Chart"), 
                  side = 3, line = par("mar")[3]/3,
                  font = par("font.main"), 
                  cex  = qcc.options("cex"), 
                  col  = par("col.main"))
            
            matlines(c, beta, lty = 1:length(n), col = 1)
            
            names(dimnames(beta)) <- c("scale multiplier", "sample size")
            
            if (identify)
            { cs <- rep(c,length(n))
            betas <- as.vector(beta)
            labels <- paste("c=", formatC(cs, 2, flag="-"),
                            ": beta=", formatC(betas, 4, flag="-"),
                            ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
            i <- identify(cs, betas, labels, pos=4, offset=0.2)
            apply(as.matrix(labels[i$ind]), 1, cat, "\n")
            }
            else
            { legend(max(c), 1, legend = paste("n =", n),
                     bg = qcc.options("bg.figure"),
                     lty = 1:length(n), xjust = 1, yjust = 1)
            }
            invisible(beta)
          }
        
        oc.curves.S <- function(object, n, c = seq(1, 6, length=101), nsigmas = object$nsigmas, identify=FALSE, restore.par=TRUE)
        {
          # Draw the operating-characteristic curves for the S-chart with nsigmas
          # limits. The values on the vertical axis give the probability of not detecting
          # a change from sigma to c*sigma on the first sample following the change.
          
          if (!(object$type=="S"))
            stop("not a `qcc' object of type \"S\".")
          
          size <- unique(object$sizes)
          if (length(size) > 1)
            stop("Operating characteristic curves available only for equal sample sizes!")
          if (missing(n))
            n <- unique(c(size, c(2,5,10,15,20)))
          if (is.null(nsigmas))
          { tail.prob <- (1 - object$confidence.level) / 2
          beta.fun1 <- function(c, n, p)
          {
            ucl <- sqrt(qchisq(1 - p, n - 1) / (n - 1))
            lcl <- sqrt(qchisq(p, n - 1) / (n - 1))
            pchisq((n - 1) * (ucl / c)^2, n - 1) - pchisq((n - 1)* (lcl / c)^2, n - 1)
          }
          beta <- outer(c, n, beta.fun1, tail.prob)
          }
          else
          { c4 <- .qcc.c4
          beta.fun2 <- function(c, n)
          {
            center <- c4(n)
            tol <- sqrt(1 - c4(n)^2)
            lcl <- pmax(0, center - nsigmas * tol)
            ucl <- center + nsigmas * tol
            pchisq((n - 1) * (ucl / c)^2, n - 1) - pchisq((n - 1) * (lcl / c)^2, n - 1)
          }
          beta <- outer(c, n, beta.fun2)
          }
          
          colnames(beta) <- paste("n=",n,sep="")
          rownames(beta) <- c
          
          oldpar <- par(no.readonly = TRUE)
          if(restore.par) on.exit(par(oldpar))
          par(bg  = qcc.options("bg.margin"), 
              cex = oldpar$cex * qcc.options("cex"),
              mar = pmax(oldpar$mar, c(4.1,4.1,2.1,2.1)))
          
          plot(c, beta[,1], type="n",
               ylim = c(0,1), xlim = c(1,max(c)),
               xlab = "Process scale multiplier",
               ylab = "Prob. type II error ")
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
               col = qcc.options("bg.figure"))
          box()
          mtext(paste("OC curves for", object$type, "Chart"), 
                side = 3, line = par("mar")[3]/3,
                font = par("font.main"), 
                cex  = qcc.options("cex"), 
                col  = par("col.main"))
          matlines(c, beta, lty = 1:length(n), col = 1)
          
          names(dimnames(beta)) <- c("scale multiplier", "sample size")
          
          if (identify)
          { cs <- rep(c,length(n))
          betas <- as.vector(beta)
          labels <- paste("c=", formatC(cs, 2, flag="-"),
                          ": beta=", formatC(betas, 4, flag="-"),
                          ", ARL=", formatC(1/(1-betas), 2, flag="-"), sep="")
          i <- identify(cs, betas, labels, pos=4, offset=0.2)
          apply(as.matrix(labels[i$ind]), 1, cat, "\n")
          }
          else
          { legend(max(c), 1, legend = paste("n =", n),
                   bg = qcc.options("bg.figure"),
                   lty = 1:length(n), xjust = 1, yjust = 1)
          }
          invisible(beta)
        }
        
        #-------------------------------------------------------------------#
        #                                                                   #
        # Miscellaneous functions                                           #
        #                                                                   #
        #-------------------------------------------------------------------#
        
        qcc.groups <- function(data, sample)
        {
          if(length(data)!=length(sample))
            stop("data and sample must be vectors of equal length")
          x <- lapply(split(data, sample), as.vector)
          lx <- sapply(x, length)
          for(i in which(lx != max(lx)))
            x[[i]] <- c(x[[i]], rep(NA, max(lx)-lx[i]))
          x <- t(sapply(x, as.vector))
          return(x)
        }
        
        qcc.overdispersion.test <- function(x, size, 
                                            type=ifelse(missing(size), "poisson", "binomial"))
        {
          type <- match.arg(type, c("poisson", "binomial"))
          if (type=="binomial" & missing(size))
            stop("binomial data require argument \"size\"")
          if (!missing(size))
            if (length(x) != length(size))   
              stop("arguments \"x\" and \"size\" must be vector of same length")
          
          n <- length(x)
          obs.var <- var(x)
          if (type=="binomial")
          { p <- sum(x)/sum(size)
          theor.var <- mean(size)*p*(1-p) }
          else if (type=="poisson")
          { theor.var <- mean(x) }
          else
            stop("invalid \"type\" argument. See help.")
          
          D <- (obs.var * (n-1)) / theor.var
          p.value <- 1-pchisq(D, n-1)
          
          out <- matrix(c(obs.var/theor.var, D, signif(p.value,5)), 1, 3)
          rownames(out) <- paste(type, "data")
          colnames(out) <- c("Obs.Var/Theor.Var", "Statistic", "p-value") 
          names(dimnames(out)) <- c(paste("Overdispersion test"), "")
          return(out)
        }
        
        qcc(na.omit(allfinalcontrol()[,2]),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
      }) })
  })
  
  
  
  allfinalcontrol <- function()
  {
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    ee <- input$myiny
    
    data <- data.frame(y=data0[ee])
    #names(data2) <-"y"
    #data=read.csv("eee.csv")
    s=qcc(na.omit(data),type = "xbar.one",nsigmas = 3,std.dev = "SD",title="") 
    
    #s$violations$beyond.limits
    #s$statisticsm
    
    r=data.frame(s.no=s$violations$beyond.limits,outofcontrol=rep("Yes",dim(data.frame(s$violations$beyond.limits))[1]))
    #r2=data.frame(s.no=s$violations$violating.runs,outofcontrol=rep("No",dim(data.frame(s$violations$violating.runs))[1]))
    qccdata=data.frame(s.no=1:dim(data.frame(s$statistics))[1],outofcontrol=rep("No",dim(data.frame(s$statistics))[1]))
    qe=data.frame(rbind(r,qccdata))
    df=qe[order(qe$s.no),]
    out=df[!duplicated(df[c("s.no")]),]
    final=data.frame(s.no=1:dim(data.frame(data))[1],data,outofcontrol=out$outofcontrol)
    con=data.frame(filter(final, outofcontrol == "No"))
    data.frame(con[,c(1,2)])
    
  }
##########################################################CROSS TABULATE##########################################################################################################################

    output[["crossui"]]<- renderUI({ 
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
 column(12, 
  column(4,tags$div(checkboxGroupInput("cross1",label = "Select first Variable",choices = names(data0)),align="center")),
  column(4,tags$div(tableOutput("crosstab"),align="center")),
  column(4,tags$div(checkboxGroupInput("cross2",label = "Select second Variable",choices = names(data0)),align="center")))
  
  })
  
  tableform <- reactive({
    file1 <- input$file20
    data0 <- data.frame(read.csv(file1$datapath))
    validate(need(input$cross1,''),
             need(input$cross2,''))
    xtabs(as.formula(paste0("~",input$cross1,"+",input$cross2)), data0)
  })

  output[["crosstab"]]<- renderTable({
    tableform()
    })
  
  
  output[["crossdown"]]<- renderUI({ 
    if(!is.null(input$cross1) & !is.null(input$cross2)) {
    tags$div(downloadButton("crossdownload","Download Data..!"),align="center")
    }
  })
  
  output$crossdownload <- downloadHandler(
   
    filename = function() {
      paste("crosstabu output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(tableform()), file, row.names = FALSE)
    })
  
  
####################################################forecast next set of values#########################################################################################
  
  output[["forecas"]]<- renderUI({ 
    fluidPage(
      
      fluidRow(
        column(4,wellPanel(
          uiOutput("chos000"),
          uiOutput("chos300"),
          uiOutput("bbbb"),
          uiOutput("button205")
          # br(), 
          # 
          # uiOutput("dpo")
          
          
        ) # Close wellPanel
        ),
        column(8,wellPanel(
          fluidRow(
            p(tags$b("Forecasting Method:")),
            uiOutput("downfore205"),br(),
            dygraphOutput('fore205')
            #closes column-8
          )),#closes fluidRow
          HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
          
        )
      ))
    
    
  })
 
  
  output$chos000 <- renderUI({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    selectInput("forevardata", label = h5("Date (format* dd-mm-YYYY):"),
                choices =names(df),
                selected = character(0))
    
  })
  
  output$chos300 <- renderUI({
    file1 <- input$file20
    df <- data.frame(read.csv(file1$datapath))
    selectInput("forevardata2", label = h5("Value:"),
                choices =names(df),
                selected = character(0))
    
  })
  
  # 
  # 
  # 
  # output[["fileupload205"]] <- renderUI({
  #   input$reset205
  #   tags$div(fileInput("file205",label = tags$h4(strong(em("Upload data..")),style="color:#034e91;font-size:160%"),
  #                      accept=c('csv','comma-seperated-values','.csv')),align="center")#
  #   
  # })
  
  # output[["checkbox205"]] <- renderUI({
  #   input$reset205
  #   
  # })
  # 
  # output[["bspop205"]] <- renderUI({
  #   bsPopover(id="fileupload205",title = "",content = "To get results, click the Lets go! button...",placement = "top")
  # })
  # 
  output[["bbbb"]] <- renderUI({
    if (is.null(input$file20)) return()
    tags$div(numericInput("foreperiod","Forecasting count ?",value = 10),align="center")
    
  })
  
  
  data205 <-reactive({
    file1 <- input$file205
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    # data=data.frame(abs(data))
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data
    
  })
  
  output[["button205"]] <- renderUI({
    if (is.null(input$foreperiod)) return()
    tags$div(bsButton("analyse205",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
             style="color:white;font-weight:100%;",align="center")
    
  }) 
  
  observeEvent(input$analyse205, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation205",
      type = "success",
      title = "Make sure that the data was uploaded..!",
      btn_labels = c("Nope", "Yes"),
      danger_mode = TRUE
    )
  })
  
  
  # observeEvent(input$confirmation205, {
  #   if(input$confirmation205==TRUE){
  #     showModal(tags$div(id="modal1", modalDialog(
  #       inputId = 'Dialog1', 
  #       title = HTML('<span style="color:black; font-size: 20px; font-weight:bold; font-family:sans-serif ">Output<span>
  #                      <button type = "button" class="close" data-dismiss="modal" ">
  #                      <span style="color:white; ">x <span>
  #                      </button> '),
  #       footer = modalButton("Close"),
  #       size = "l",br(),
  #       uiOutput("downfore205"),br(),
  #       dygraphOutput('fore205'),
  #       easyClose = T
  #     )))
  #   }
  # })
  
  
  outfore <- reactive({
    withProgress(message = 'Generating plot Please Wait..!', style = 'notification', value = 0.75, {
      Sys.sleep(1)
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data0 <- data.frame(fread(file1$datapath))
    one=data0[input$forevardata]
    two=data0[input$forevardata2]
    data=data.frame(one,two)
    #data=read.csv("converted_temp - Copy.csv")
    colnames(data) <- c("ds","y")
    data$ds <- as.Date(data$ds,format="%d-%m-%Y")
    m <- prophet(data)
    future <- make_future_dataframe(m, periods =input$foreperiod)
    forecast <- predict(m, future)
    dyplot.prophet(m, forecast)
    })
  })
  
  
observeEvent(input$confirmation205, {
  if(input$confirmation205==TRUE){ 
  output[["fore205"]] <- renderDygraph({
    outfore()
  })  }
})

  
  outforedata <- reactive({
    file1 <- input$file20
    if(is.null(file1)) {return(NULL)}
    data0 <- data.frame(fread(file1$datapath))
    one=data0[input$forevardata]
    two=data0[input$forevardata2]
    data=data.frame(one,two)
    #data=read.csv("converted_temp - Copy.csv")
    colnames(data) <- c("ds","y")
    data$ds <- as.Date(data$ds,format="%d-%m-%Y")
    m <- prophet(data)
    future <- make_future_dataframe(m, periods =input$foreperiod)
    forecast <- predict(m, future)
    x=data.frame(forecast[,1],forecast[,19])
    colnames(x)<- c(colnames(data0[,1]),colnames(data0[,2]))
    x
  })
  
  output$down205 <- downloadHandler(
    
    filename = function() {
      paste("forecast output-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      x=data.frame(outfore())
      file1 <- input$file20
      if(is.null(file1)) {return(NULL)}
      data0 <- data.frame(fread(file1$datapath))
      colnames(x)<- c(colnames(data0[,1]),colnames(data0[,2]))
                                                            write.csv(x, file, row.names = FALSE)
    }
  )

  observeEvent(input$confirmation205, {
    if(input$confirmation205==TRUE){   
  output[["downfore205"]] <- renderUI({
    tags$div(downloadButton(outputId = "down205",label = "Download Data..!",color = "success",size = "md"),align="center")
    
  }) 
    } })
  
######################################################################### Summary (control-box-univariate outliers)#############################################################################################
  
  output$comdown <- renderUI({
    downloadBttn(outputId = "comdownload",label = "Download Data..!",color = "success",size = "md")
  })
  
  output$comdownload <- downloadHandler(
    filename = function() {
      paste("overall_summary.csv", sep = "")
    },
    content = function(file) {
      
      write.csv(overall_summaryjoint(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$file20, {
    output[["xxx1"]]<- renderPlotly({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      progress$set(message = 'Please Wait...!')
      withProgress(message='Loading Plot..!',value=30,style = "notification",
                   {
                     
                     n<-10
                     
                     for(i in 1:n){
                       incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                       data=lev1downcont()
                       tab2 <- table(lev1downcont()$outofcontrol) 
                       outofcontrol_pi <- data.frame(tab2,label=paste0("out of control - ",rownames(tab2)))[,c(3,2)]  
                       plot_ly(outofcontrol_pi(), labels = ~label, values = ~Freq, type = 'pie',textinfo = 'label+percent') %>%
                         layout(title = 'Process Control Percentage Plot',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                       
                       
                     }
                   })
      
      return(p)
    })
  })
  
################################################################################################################################################################################################
  
  
  
}

shinyApp(ui, server)