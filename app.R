#Author: Matthew Crittenden
#File Name: app.R      (CMS Dashboard v3.0)
#Purpose: Created for ANSA-EAP's CheckMySchool Program to host, process, and display its CMS App reports data
#Date Created: 12/27/2019
#Date Edited: 1/7/2019


#-------Loading packages-----------------------------------------

#shiny
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
#tidyverse
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
#mapping
library(leaflet)
library(rgdal)
#others
library(slickR)
library(DT)
library(questionr)
library(googledrive)
library(googlesheets4)
library(rlist)


#-------Loading data----------------------------------------------
#----------reports data------------------------------------------
reports1 <- readxl::read_excel('./cleaned_cms_data_dec3-2019.xlsx')

#date is in yyyy-mm-dd format
reports1$`Reported On` <- format(strptime(reports1$`Reported On`, format = "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d %H:%M:%S")
reports1$`Reported On` <- as.Date(reports1$`Reported On`)
colnames(reports1)[colnames(reports1)=="Reported On"] <- "Date"
reports1$Date <- gsub("/","-",reports1$Date)

#add a unique school column
reports1$`Unique School` <- paste0(reports1$`School Name`, " (ID: ", reports1$`School ID`,")") #may add division name here later...


#----------live reports data (not working)-------------


#enable use of googlesheets API in non-interactive settings
source("global.R") #this pulls in the hidden variable information from the global.R file
drive_auth_configure(api_key = cms_dashboard_key)
sheets_auth_configure(api_key = drive_api_key())
drive_deauth()
sheets_deauth()


#read in the live data from googlesheets and subset it

# csv_data <- read_sheet(ss = cms_csv_data)
# 
# #date is in yyyy-mm-dd format
# csv_data$`Reported On` <- format(strptime(csv_data$`Reported On`, format = "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d %H:%M:%S")
# csv_data$`Reported On` <- as.Date(csv_data$`Reported On`)
# colnames(csv_data)[colnames(csv_data)=="Reported On"] <- "Date"
# 
# #remove the observations in reports1
# csv_data <- csv_data[which(csv_data$Date >= as.Date.character("2019/12/03")),]
# names(csv_data) <- names(reports1)
# 
# #merge the two dataframes
# reports1 <- rbind(csv_data,reports1)
# reports1$Date <- gsub("/","-",reports1$Date)


#----------sni and coordinate data--------------------------------
coord_data <- read.csv('./coord_data.csv')

#remove duplicates which were from different calculations of sni
coord_data <- coord_data[!duplicated(coord_data[,1]),]

#keep only schools which have reports (make sure this uses the correct regions and divisions too)
colnames(coord_data)[colnames(coord_data)=="School_Name_y"] <- "School Name"
colnames(coord_data)[colnames(coord_data)=="School_ID"] <- "School ID"
colnames(coord_data)[colnames(coord_data)=="Region_Name"] <- "Region"


coord_data <- merge(coord_data, reports1, by = c("School ID"), all = FALSE)

#find out which School IDs are missing from coord_data

ok_list <- c()
uhoh_list <- c()

for (id in unique(reports1$`School ID`)) {
    if (id %in% coord_data$`School ID`) {
        ok_list <- list.append(ok_list,id)
    }
    else {
        uhoh_list <- list.append(uhoh_list,id)
    }
}

ok_list <- as.list(ok_list)
uhoh_list <- as.list(uhoh_list)


#FUTURE PROBLEM: ONLY 297 OF 308 SCHOOLS MATCHED. 11 SCHOOL IDS ARE NOT IN THE COORDINATE DATASET
#FOR NOW, JUST REMOVE THESE FROM THE SCHOOL_LEVEL MAPPING

#----------shapefile data-----------------------------------------
region_shape <- readOGR('./region_data_shp', layer = 'region_data')
division_shape <- readOGR('./divisions_data_shp', layer = 'division_polygons')

#-------UI--------------------------------------------------------


ui <- dashboardPagePlus(
    collapse_sidebar = TRUE,
    
    header  = dashboardHeaderPlus(title = "CMS App Analytics",
                                  enable_rightsidebar = TRUE,
                                  rightSidebarIcon = "info",
                                  dropdownMenu(type = "message",
                                               messageItem(from = "New Look",
                                                           message = "Our dashboard has a new look!",
                                                           icon = icon("laugh-beam"))
                                  )
    ), #close dashboardHeaderPlus
    
    
    #-------left sidebar----------------------
    
    
    
    sidebar = dashboardSidebar(collapsed = TRUE,
                               sidebarMenu(id = "left_sidebar",
                                           menuItem("Welcome", tabName = "welcome", icon = icon("smile",lib='font-awesome')),
                                           menuItem("User Guide", tabName = "user_guide", icon = icon("toolbox",lib='font-awesome')),
                                           menuItem("Quick Statistics", tabName = "quick_statistics", icon = icon("th",lib='font-awesome')),
                                           menuItem("Data Explorer", tabName = "data_explorer", icon = icon("search",lib='font-awesome')),
                                           menuItem("Submissions Map", tabName = "submissions_map", icon = icon("globe-asia",lib='font-awesome')),
                                           menuItem("Raw Data", tabName = "raw_data", icon = icon("download",lib='font-awesome')),
                                           menuItem("Get Involved", icon = icon("hands-helping",lib = 'font-awesome'),
                                                    menuSubItem("Download iOS app", icon = icon("app-store",lib='font-awesome'), 
                                                                href = "https://apps.apple.com/ph/app/checkmyschool/id1458068394"),
                                                    menuSubItem("Download Android app", icon = icon("google-play",lib='font-awesome'), 
                                                                href = "https://play.google.com/store/apps/details?id=com.checkmyschool.app&hl=en"),
                                                    menuSubItem("Visit us on Facebook", icon = icon("facebook-square",lib='font-awesome'), 
                                                                href = "https://www.facebook.com/CheckMySchool/"),
                                                    menuSubItem("Visit us on Twitter", icon = icon("twitter-square", lib='font-awesome'),
                                                                href = "https://twitter.com/onlinecms?lang=en"),
                                                    menuSubItem("Watch us on Youtube", icon = icon("youtube-square", lib='font-awesome'),
                                                                href = "https://www.youtube.com/user/onlinecms"))
                               )
    ), #close dashboardSidebar
    
    
    
    #-------right sidebar---------------------
    
    
    rightsidebar = rightSidebar(
        background = "light",
        
        #----------data query tools-------------------
        
        
        rightSidebarTabContent(
            id = 1,
            title = "Data Query Tools",
            icon = "cog",
            active = TRUE,
            fluidPage(
                fluidRow(column(width = 12,
                                div(style = "font-size: 11px; font-weight: normal;",
                                    
                                    selectInput(inputId = "selected_BP",
                                                label = "Include best practices?",
                                                choices = c("yes", "no"),
                                                selected = "yes"),
                                    
                                    selectInput(inputId = "selected_adopt",
                                                label = "Include Adopt-a-School?",
                                                choices = c("yes", "no"),
                                                selected = "yes"),
                                    
                                    dateRangeInput(inputId = "selected_dates",
                                                   label = "Timeframe:",
                                                   start = as.Date.character("2019-04-06"),
                                                   end = Sys.Date(),
                                                   min = as.Date.character("2019-04-06"),
                                                   max = Sys.Date()),
                                    
                                    selectInput(inputId = "selected_region",
                                                label = "Region:",
                                                choices = c("default (all)",sort(unique(as.character(reports1$Region)))),
                                                selected = "default (all)"),
                                    
                                    selectInput(inputId = "selected_division",
                                                label = "Division:",
                                                choices = "default (all)",
                                                selected = "default (all)"),
                                    
                                    selectInput(inputId = "selected_school",
                                                label = "School:",
                                                choices = "default (all)",
                                                selected = "default (all)"),
                                    
                                    selectInput(inputId = "selected_category",
                                                label = "Service Classification/Category:",
                                                choices = c("default (all)",sort(unique(reports1$`Service Classification/Category`))),
                                                selected = "default (all)"),
                                    
                                    selectInput(inputId = "selected_item",
                                                label = "Service Item:",
                                                choices = "default (all)",
                                                selected = "default (all)"),
                                    
                                    actionButton(inputId = "reset1", label = "Reset", style='padding:4px; font-size:90%; margin-top:0.5em; margin-bottom:1em;')
                                    
                                )
                )
                )
            )
        ),
        
        #----------collaborators--------------------
        
        rightSidebarTabContent(
            id = 2,
            title = "Collaborators",
            icon = "users",
            h6("Matthew Crittenden developed this dashboard from May to December 2019 
                             as CMS's data specialist. He came to the Philippines as a
                             Summer Fellow of both William & Mary's Global Research Institute and
                             AMES-APIA Freeman Program in Asia."),
            img(src='cms_logo.png', width = 200),
            img(src='ansa_logo.png', width = 200),
            img(src='goalkeepers_logo.png', width = 200),
            img(src='freeman_logo1.png', width = 200),
            fluidRow(column(width = 12,
                            img(src='gri_logo.png', width = 200), align = "center"))
        )
        
    ), #close rightSidebar
    
    
    
    #-------dashboard body-------------------
    
    body = dashboardBody(
        
        shinyjs::useShinyjs(),
        
        #-------HTML/CSS tags------------------------------
        
        tags$head(
            tags$style(HTML('div#control-sidebar-2-tab.tab-pane.active>img {margin-bottom: 1.5em;}')), #this adds space in between logo pics
            tags$style(HTML('.shiny-output-error-validation {color: red;}')), #change color of validation error
            tags$style(HTML('.shiny-output-error {color: grey;}')), #change color of regular error
            tags$style(HTML('div#DataTables_Table_0_info.dataTables_info {font-size: 12px;}')), #change font size of datatable info blurb
            #below changes the navbar and left sidebar
            tags$style(HTML('
                      /* logo */
                      .skin-blue .main-header .logo {
                      background-color: #172869;
                      font-size: 15px;
                      }
                      
                      /* logo when hovered */
                      .skin-blue .main-header .logo:hover {
                      background-color: #172869;
                      }
                      
                      /* navbar (rest of the header) */
                      .skin-blue .main-header .navbar {
                      background-color: #172869;
                      } 
                      
                      /* main sidebar */
                      .skin-blue .main-sidebar {
                      color: #FFFFFF;
                      }
                      
                      /* active selected tab in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                      background-color: #FFCC00;
                      }
                      
                      /* other links in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                      color: #FFFFFF;
                      }
                      
                      /* other links in the sidebarmenu when hovered */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                      background-color: #FFCC00;
                      }
                      
                      /* toggle button when hovered */                    
                      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                      background-color: #223DA4;
                      }
                      
                      /* right info toggle button when hovered */
                      .skin-blue .main-header .navbar .navbar-custom-menu li:hover{
                      background-color:#223DA4;
                      }
                      
                      /* right info toggle button when hovered */
                      .skin-blue .main-header .navbar .navbar-custom-menu .dropdown-toggle:hover{
                      background-color:#223DA4;
                      }
                      
                      /* body */
                      .content-wrapper, .right-side {
                      background-color: #FFFFFE;
                      }
                      '))),
        #below changes boxes
        tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#172869
                    }
                    
                    .box.box-solid.box-primary{
                    border-bottom-color:#FFCC00;
                    border-left-color:#FFCC00;
                    border-right-color:#FFCC00;
                    border-top-color:#FFCC00;
                    border-bottom-width:2px;
                    border-left-width:2px;
                    border-right-width:2px;
                    border-top-width:2px;
                    }
                    
                    ")),
        
        
        #-------tabItems---------------------------------
        
        tabItems(
            
            
            #-------welcome tab------------------------
       
            tabItem(tabName = "welcome",
                    
                    fluidPage(
                        
                        mainPanel(width = 12,
                                  h1("CheckMySchool App Analytics Dashboard", align = "center"),
                                  br(),
                                  h4("This dashboard presents the data analytics for the CheckMySchool mobile app
                             since its release in April 2019. The data is automatically updated each evening at 10:00 PM PST."),
                                  h4("The goal of the CMS mobile app is to provide
                             a constructive and collaborative platform for users to provide necessary
                             feedback to the Department of Education of the Philippines. Principals,
                             teachers, parents, students, DepEd officials, and other local stakeholders
                             are able to send feedback in the form of 'submissions' to the proper authorities.
                             This hopefully will improve the ability of stakeholders to provide valuable
                             feedback to DepEd in a reasonable timeframe. The app also establishes a platform
                             for constructive and collaborative discourse/advertisement by allowing users to
                             see the success stories of school improvement around the Philippines.", align = "justify"),
                                  br(),
                                  fluidRow(width = 12, align = "center",
                                           checkboxInput("condition_agreement", paste0("To ensure the ethical use of the information
                                                                               presented in this dashboard, CheckMySchool respectfully requires all users
                                                                               to accept the terms of use outlined in our Code of Conduct."), FALSE, width = "60%"),
                                           a("Click here to view our Code of Conduct",target="_blank",href="CMSapp_code_of_conduct.pdf")
                                  )
                        ) #close mainPanel
                        
                    ) #close fluidPage
            ), #close tabItem "welcome"
            
            
            
            #-------user guide tab-----------------------
        
            tabItem(tabName = "user_guide",# class = "my_style_2",
                    
                    fluidPage(
                        
                        mainPanel(width = 12,
                                  
                                  fluidRow(width = 12,
                                           column(width = 10, offset = 1,
                                                  slickROutput("slickr", height = "60vh")
                                           )
                                  ) #close fluidRow
                        ) #close mainPanel
                    ) #close fluidPage
                    
            ), #close tabItem "user_guide"
            
            
            
            #-------quick statistics tab--------------------
            
            tabItem(tabName = "quick_statistics",
                    
                    fluidPage(
                        mainPanel(width = 8,
                                  
                                  box(width = 12, title = "Statistics at a Glance", solidHeader = TRUE, status = "primary",
                                      tags$head(tags$style(HTML(".small-box {height: 110px}"))),
                                      
                                      fluidRow(
                                          column(width = 12,
                                                 valueBoxOutput("users", width = 6), #number of app users
                                                 valueBoxOutput("uniquereporters", width = 6))), #number of total reporters
                                      
                                      fluidRow(
                                          column(width = 12,
                                                 valueBoxOutput("numreports") #number of total reports
                                                 ,valueBoxOutput("numreports_BP") #number of only Best Practice reports
                                                 ,valueBoxOutput("numreports_nonBP"))), #number of only non-BP reports
                                      
                                      fluidRow(
                                          column(width = 12,
                                                 valueBoxOutput("numregions") #number of regions present in all reports
                                                 ,valueBoxOutput("numdivisions") #number of divisions present in all reports
                                                 ,valueBoxOutput("numschools"))), #number of schools present in all reports
                                      
                                      fluidRow(
                                          column(width = 12,
                                                 valueBoxOutput("topregion", width = 6) #region with the most reports
                                                 ,valueBoxOutput("topdivision", width = 6))), #division with the most reports
                                      
                                      fluidRow(
                                          column(width = 12,
                                                 valueBoxOutput("topschool", width = 6) #school with the most reports
                                                 ,valueBoxOutput("topcategory", width = 6))) #category with the most reports
                                  )        
                        ),
                        
                        mainPanel(width = 4,
                                  
                                  fluidRow(#style = "margin-top:-1.35em;",
                                      box(offset = 0,
                                          title = "Daily Number of Submissions"
                                          ,status = "primary"
                                          ,solidHeader = TRUE
                                          ,width = "300px"
                                          ,plotOutput("dailyplot", height = "226px"))),
                                  
                                  fluidRow(
                                      box(offset = 0,
                                          title = "Submissions by Region"
                                          ,status = "primary"
                                          ,solidHeader = TRUE 
                                          ,width = "300px"
                                          ,plotOutput("regionplot", height = "340px")))
                        ),
                        
                        mainPanel(width = 12,
                                  
                                  fluidRow(
                                      column(width = 12,
                                             div(style = "color: grey;",
                                                 h5(HTML("The features on this page are only affected by the best practices, Adopt-a-School, and date selectors in the rightsidebar.
                                               The other selectors will not change the statistics shown."))))
                                  ))
                    )
                    
            ), #close tabItem "quick_statistics"
            
            
            
            #-------data explorer tab-------------------
        
            tabItem(tabName = "data_explorer",
                    
                    fluidPage(
                        
                        mainPanel(width = 12,
                                  box(title = "Pie chart of the submissions for this selection", solidHeader = TRUE, status = "primary",
                                      plotOutput("categoryplot", height = "410px"),
                                      downloadButton("download_pie", "Download Pie Chart", style='padding:4px; font-size:85%;')
                                  )),
                        mainPanel(width = 12,
                                  box(title = "Line graph of daily submissions for this selection", width = 10, solidHeader = TRUE, status = "primary",
                                      plotOutput("selectdailyplot", height = "145px"),
                                      downloadButton("download_line", "Download Line Graph", style='padding:4px; font-size:85%; margin-left:1em;')
                                  ))
                    ) #close fluidPage
                    
            ), #close tabItem "data_explorer"
            
            
            #-------submissions map tab--------------------

            tabItem(tabName = "submissions_map",
                    
                    fluidPage(
                        
                        sidebarPanel(width = 3,
                                     div(style = "color: red;",
                                         h5(HTML("Coordinate data for some schools are not yet available. Thank you for your patience."))),
                                     div(style="display:inline-block;vertical-align:top;",
                                         fluidRow(
                                             column(4,h5(strong("Region"))),
                                             column(8,actionButton("region_level", "Map!", style='padding:4px; font-size:80%; margin-top:0.5em'))
                                         ),
                                         fluidRow(
                                             column(4,h5(strong("Division"))),
                                             column(8,actionButton("division_level", "Map!", style='padding:4px; font-size:80%; margin-top:0.5em'))
                                         ),
                                         fluidRow(
                                             column(4,h5(strong("School"))),
                                             column(8,actionButton("school_level", "Map!", style='padding:4px; font-size:80%; margin-top:0.5em'))
                                         ),
                                         hr(),
                                         h5(strong(HTML("You can also display a map of School Neediness Index scores:"))),
                                         fluidRow(
                                             column(4,h5(strong("SNI"))),
                                             column(8,actionButton("sni_level", "Map!", style='padding:4px; font-size:80%; margin-top:0.5em'))
                                         )),
                                     hr(),
                                     selectInput(inputId = "selected_specific",
                                                 label = div(style = "font-size:13px;", "Find a Specific Location:"),
                                                 choices = "default (all)",
                                                 selected = "default (all)")
                                     
                                     
                                     
                        ),#close sidebarPanel
                        
                        mainPanel(width = 7,
                                  box(title = "Map of the user's selection", width = "12", solidHeader = TRUE, status = "primary",
                                      leafletOutput("submap", height = "720px"))
                                  
                        )#close mainPanel
                        
                    )#close fluidPage
                    
            ), #close tabItem "submissions_map"
            
            
            
            #-------raw data tab-----------------------------
            
            tabItem(tabName = "raw_data",
                    
                    fluidPage(
                        
                        sidebarPanel(width = 2,
                                     textInput(inputId = "name_authentication", label = div(style = "font-size:13px;",
                                                                                                'To access and download the raw data, type valid legal name and email.'),
                                               placeholder = "<type name here>", width = 300),
                                     div(style = "margin-top: -0.5em;",
                                         textInput(inputId = "email_authentication", label = NULL, 
                                                       placeholder = "<type email here>", width = 300)),
                                     downloadButton("download_data", "Download", style='padding:4px; font-size:85%; margin-top: -1em;')
                        ),
                        
                        mainPanel(width = 8,
                                  box(title = "Preview of the raw data for this selection", width = "12", solidHeader = TRUE, status = "primary",
                                      div(dataTableOutput("categoryreports"),
                                          style='height: 660px;
                                                width: 100%;
                                                table-layout: fixed;
                                                word-wrap: break-word;'
                                      )
                                  )
                        )
                        
                    )#close fluidPage
            )#close tabItem "raw_data"
            
        ) #close tabItems
    ) #close dashboardBody
) #close dashboardPagePlus



#-------server------------------------

server <- function(input, output, session) {
    
    #-------show/hide selectInputs on rightSidebar for different tabs----------------
    
    observe({
        if (input$left_sidebar == "quick_statistics") {
            shinyjs::hide("selected_region")
            shinyjs::hide("selected_division")
            shinyjs::hide("selected_school")
            shinyjs::hide("selected_category")
            shinyjs::hide("selected_item")
            
        } else if (input$left_sidebar == "submissions_map") {
            shinyjs::hide("selected_region")
            shinyjs::hide("selected_division")
            shinyjs::hide("selected_school")
            shinyjs::show("selected_category")
            shinyjs::show("selected_item")
            
        } else {
            shinyjs::show("selected_region")
            shinyjs::show("selected_division")
            shinyjs::show("selected_school")
            shinyjs::show("selected_category")
            shinyjs::show("selected_item")
        }
    })
    
    
    #-------open rightSidebar when data explorer, submissions map, or raw data are opened-------------------
    
    observe({
        if (input$left_sidebar == "data_explorer" | input$left_sidebar == "submissions_map" | input$left_sidebar == "raw_data") {
            shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
        } else {
            shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
        }
    })
    
    
    #-------TAB 2: USER GUIDE-------------------
    #----------2A. making the slideshow------------------
    
    slides=c("dashboard_tutorial1","dashboard_tutorial2","dashboard_tutorial3",
             "dashboard_tutorial4","dashboard_tutorial5","dashboard_tutorial6")
    
    slides_pics=sprintf("./www/%s.png",slides)
    
    output$slickr <- renderSlickR({
        presentation <- slickR(obj=slides_pics)
        
    })
    
    
    #-------setting up blank theme for future pie charts-------------------
    
    blank_theme <- theme_minimal()+
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank()
        )
    
    #-------make the data reactive-----------------
    
    reports <- reactive({
        reports_reactive <- reports1[which(reports1$Date >= as.Date.character(input$selected_dates[1]) & reports1$Date <= as.Date.character(input$selected_dates[2])),]
        
        #subset by best practices
        
        if (input$selected_BP == "no") {
            reports_reactive <- reports_reactive[which(reports_reactive$`Service Classification/Category` != "School Events"),]
            
        } else {
            reports_reactive <- reports_reactive
        }
        
        #subset by Adopt-a-School
        
        if (input$selected_adopt == "no") {
            reports_reactive <- reports_reactive[which(reports_reactive$`Service Classification/Category` != "Adopt-a-School Program"),]
            
        } else {
            reports_reactive <- reports_reactive
        }
        
        #validate to the code of conduct
        
        validate(
            need(input$condition_agreement == TRUE, "To access this information, please agree with our terms of use on the Welcome tab."))
        
        #and the inputted dates
        
        validate(
            need(nrow(reports_reactive) != 0, "There are 0 reports with the user's selected combination of region, division, school, best practice status, and dates. Please select a different combination."))
        
        return(reports_reactive)
    })
    
    
    #-------updating selectors------------------------------
    
    #best practice, adopt-a-school, date
    
    observeEvent(input$reset1,{
        
        updateSelectInput(session = session,
                          inputId = "selected_BP",
                          label = "Include best practices?",
                          choices = c("yes", "no"),
                          selected = "yes")
        
        updateSelectInput(session = session,
                          inputId = "selected_adopt",
                          label = "Include Adopt-a-School?",
                          choices = c("yes", "no"),
                          selected = "yes")
        
        updateDateRangeInput(session = session,
                             inputId = "selected_dates",
                             label = "Timeframe:",
                             start = as.Date.character("2019-04-06"),
                             end = Sys.Date(),
                             min = as.Date.character("2019-04-06"),
                             max = Sys.Date())
    })
    
    
    #region
    
    observeEvent({
        length(input$selected_BP) != 0
        length(input$selected_adopt) != 0
        length(input$selected_dates) != 0
        input$reset1
    }, {
        updateSelectInput(session = session,
                          inputId = "selected_region",
                          label = "Region:",
                          choices = c("default (all)",sort(unique(as.character(reports()$Region)))),
                          selected = "default (all)")
    }, ignoreNULL = FALSE)
    
    
    #division
    
    observeEvent(length(input$selected_region) != 0, {
        
        if (input$selected_region == "default (all)") {
            updateSelectInput(session = session,
                              inputId = "selected_division",
                              label = "Division:",
                              choices = "default (all)",
                              selected = "default (all)")
        } else {
            data_available <- reports()[which(reports()$Region == input$selected_region),]
            
            updateSelectInput(session = session,
                              inputId = "selected_division",
                              label = "Division:",
                              choices = c("default (all)",sort(unique(data_available$Division))),
                              selected = "default (all)")
        }
    })
    
    
    #school
    
    observeEvent(length(input$selected_division) != 0, {
        
        if (input$selected_division == "default (all)") {
            updateSelectInput(session = session,
                              inputId = "selected_school",
                              label = "School:",
                              choices = "default (all)",
                              selected = "default (all)")
        } else {
            data_available <- reports()[which(reports()$Region == input$selected_region),]
            data_available <- data_available[which(data_available$Division == input$selected_division),]
            
            updateSelectInput(session = session,
                              inputId = "selected_school",
                              label = "School:",
                              choices = c("default (all)",sort(unique(data_available$`Unique School`))),
                              selected = "default (all)")
        }
    })
    
    
    #category
    
    observeEvent({
        length(input$selected_BP) != 0
        length(input$selected_adopt) != 0
        length(input$selected_dates) != 0
        length(input$selected_region) != 0
        length(input$selected_division) != 0
        length(input$selected_school) != 0
        input$reset1
    }, {
        
        data_available <- reports()
        
        if (input$selected_region != "default (all)") {
            
            data_available <- data_available[which(data_available$Region == input$selected_region),]
        }
        
        if (input$selected_division != "default (all)") {
            
            data_available <- data_available[which(data_available$Division == input$selected_division),]
        }
        
        if (input$selected_school != "default (all)") {
            
            data_available <- data_available[which(data_available$`Unique School` == input$selected_school),]
        }
        
        updateSelectInput(session = session,
                          inputId = "selected_category",
                          label = "Service Classification/Category:",
                          choices = c("default (all)",sort(unique(data_available$`Service Classification/Category`))),
                          selected = "default (all)")
        
    }, ignoreNULL = FALSE)
    
    
    #item
    
    observeEvent(length(input$selected_category) != 0,{
        
        if (input$selected_category == "default (all)") {
            
            updateSelectInput(session = session,
                              inputId = "selected_item",
                              label = "Service Item:",
                              choices = "default (all)",
                              selected = "default (all)")
            
        } else {
            
            data_available <- reports()
            
            if (input$selected_region != "default (all)") {
                
                data_available <- data_available[which(data_available$Region == input$selected_region),]
            }
            
            if (input$selected_division != "default (all)") {
                
                data_available <- data_available[which(data_available$Division == input$selected_division),]
            }
            
            if (input$selected_school != "default (all)") {
                
                data_available <- data_available[which(data_available$`Unique School` == input$selected_school),]
            }
            
            data_available <- data_available[which(data_available$`Service Classification/Category` == input$selected_category),]
            
            updateSelectInput(session = session,
                              inputId = "selected_item",
                              label = "Service Item:",
                              choices = c("default (all)",sort(unique(data_available$`Service Item`))),
                              selected = "default (all)")            
        }
    })
    
    
    #-------TAB 3: QUICK STATISTICS---------------------
    #----------3A. making frequency tables--------------------------
    #-------------i. region table (for statistic boxes and pie chart of reports by region)--------------------
    
    #make a table of total reports by region
    region_summary <- reactive({
        
        region_summary <- table(reports()$Region)
        region_summary <- as.data.frame(region_summary)
        colnames(region_summary) <- c("Region", "Reports")
        region_summary <- region_summary %>% arrange(desc(Reports))
        
        #make percentage column (it'll improve understandability of the pie chart)
        region_summary <- region_summary %>% mutate(Percentage = plyr::round_any((Reports / sum(Reports))*100, accuracy=.01, f=floor))
        region_summary$`Region Percentage` <- paste0(region_summary$Region, " - ", region_summary$Percentage, '%') #treated as text data (not numeric) after this
        
        return(region_summary)
    })
    
    
    #-------------ii. division table (for statistic boxes)--------------------------
    
    #make a table of total reports by division
    division_summary <- reactive({
        
        division_summary <- table(reports()$Division)
        division_summary <- as.data.frame(division_summary)
        colnames(division_summary) <- c("Division", "Reports")
        division_summary <- division_summary %>% arrange(desc(Reports))
        
        return(division_summary)
    })
    
    
    #-------------iii. school table (for statistic boxes)--------------------------
    
    #make a table of total reports by school
    school_summary <- reactive({
        
        school_summary <- table(reports()$`Unique School`)
        school_summary <- as.data.frame(school_summary)
        colnames(school_summary) <- c("School", "Reports")
        school_summary <- school_summary %>% arrange(desc(Reports))
        
        return(school_summary)
    })
    
    
    #-------------iv. category table (for statistic boxes)-------------------
    
    #make a table of total reports by category
    category_summary <- reactive({
        
        category_summary <- table(reports()$`Service Classification/Category`)
        category_summary <- as.data.frame(category_summary)
        colnames(category_summary) <- c("Category", "Reports")
        category_summary <- category_summary %>% arrange(desc(Reports))
        
        return(category_summary)
    })
    
    
    #----------3B. statistic boxes-------------------
    
    ##total number of users
    
    user_data <- reactive({
        validate(
            need(input$condition_agreement == TRUE, "To access this information, please agree with our terms of use on the Welcome tab."))
        
        user_data <- 1808 #added manually lol. this needs to be scraped in the future
        user_data
    })
    
    output$users <- renderValueBox({
        valueBox(
            formatC(user_data(), format="d", big.mark=',')
            ,paste0('Recorded Users')
            ,icon = icon("user",lib='glyphicon')
            ,color = 'red'
        )
    })
    
    
    
    ##total number of unique reporters
    
    output$uniquereporters <- renderValueBox({
        valueBox(
            formatC(length(unique(reports()$`Reported By`)), format="d", big.mark=',')
            ,paste0('Recorded Submitters')
            ,icon = icon("user",lib='glyphicon')
            ,color = 'red'
        )
    })
    
    
    
    ##total number of reports
    
    output$numreports <- renderValueBox({
        valueBox(
            formatC(nrow(reports()), format="d", big.mark=',')
            ,HTML('Total <br/> Submissions')
            ,icon = icon("stats",lib='glyphicon')
        )
    })
    
    
    
    ##total number of BP reports
    
    output$numreports_BP <- renderValueBox({
        valueBox(
            formatC(nrow(reports()[which(reports()$`Service Classification/Category` == "School Events"),]), format="d", big.mark=',')
            ,HTML('Best <br/> Practices')
            ,icon = icon("stats",lib='glyphicon')
        )
    })
    
    
    
    ##total number of non-BP reports
    
    output$numreports_nonBP <- renderValueBox({
        valueBox(
            formatC(nrow(reports()) - (nrow(reports()[which(reports()$`Service Classification/Category` == "School Events"),])), format="d", big.mark=',') #may need to change these to as.factor
            ,HTML('School <br/> Issues')
            ,icon = icon("stats",lib='glyphicon')
        )
    })
    
    
    
    ##total number of regions with reports
    
    output$numregions <- renderValueBox({
        valueBox(
            formatC(nrow(region_summary()), format="d", big.mark=',')
            ,HTML('Regions <br/> with submissions')
            ,icon = icon("stats",lib='glyphicon')
            ,color = 'yellow'
        )
    })
    
    
    
    ##total number of divisions with reports
    
    output$numdivisions <- renderValueBox({
        valueBox(
            formatC(nrow(division_summary()), format="d", big.mark=',')
            ,HTML('Divisions <br/> with submissions')
            ,icon = icon("stats",lib='glyphicon')
            ,color = 'yellow'
        )
    })
    
    
    
    ##total number of schools with reports
    
    output$numschools <- renderValueBox({
        valueBox(
            formatC(nrow(school_summary()), format="d", big.mark=',')
            ,HTML('Schools <br/> with submissions')
            ,icon = icon("stats",lib='glyphicon')
            ,color = 'yellow'
        )
    })
    
    
    
    ##top reporting region
    
    output$topregion <- renderValueBox({
        valueBox(
            formatC('Top Region', format="d", big.mark=',')
            ,paste0('"',region_summary()[1,1],'"',' with ',region_summary()[1,2],' submissions')
            ,color = 'green'
        )
    })
    
    
    
    ##top reporting division
    
    output$topdivision <- renderValueBox({
        valueBox(
            formatC('Top Division', format="d", big.mark=',')
            ,paste0('"',division_summary()[1,1],'"',' with ',division_summary()[1,2],' submissions')
            ,color = 'green'
        )
    })
    
    
    
    ##top reporting school
    
    output$topschool <- renderValueBox({
        valueBox(
            formatC('Top School', format="d", big.mark=',')
            ,paste0('"',school_summary()[1,1],'" with ',school_summary()[1,2],' submissions')
            ,color = 'green'
        )
    })
    
    
    
    #top reporting category
    
    output$topcategory <- renderValueBox({
        valueBox(
            formatC('Top Category', format="d", big.mark=',')
            ,paste0('"',category_summary()[1,1],'"',' with ',category_summary()[1,2],' submissions')
            ,color = 'green'
        )
    })
    
    
    
    #----------3C. line graph of reports by day--------------------
    
    output$dailyplot <- renderPlot({
        #make the complete daily frequency table now
        #group the data by date (making the incomplete frequency table)
        reports_freq <- freq(reports()$Date)  
        reports_freq <- as.data.frame((reports_freq))
        reports_freq <- data.table::setDT(reports_freq, keep.rownames = TRUE)[]
        
        #rename columns
        colnames(reports_freq)[colnames(reports_freq)=="rn"] <- "Date"
        colnames(reports_freq)[colnames(reports_freq)=="n"] <- "Frequency"
        reports_freq <- reports_freq[,-(3:4)]
        
        #We want to add in 0's for dates when there were no reports
        
        #first, create a dataframe of dates in the correct YYYY-MM-DD format
        ts <- seq.POSIXt(as.POSIXlt("2019-04-06"), as.POSIXlt(Sys.Date()), by="day") 
        ts <- format.POSIXct(ts, format = '%y/%m/%d')
        df <- data.frame(timestamp=ts)
        df$timestamp <- paste(20, df$timestamp, sep="")
        df$timestamp <- gsub('/', '-', df$timestamp)
        colnames(df)[colnames(df)=="timestamp"] <- "Date"
        
        #join the date dateframe to the data dateframe
        reports_freq_complete <- full_join(df,reports_freq, by = "Date")
        
        #change NAs to 0
        reports_freq_complete[is.na(reports_freq_complete)] <- 0
        
        #make sure data is type date
        reports_freq_complete$Date <- as.factor(reports_freq_complete$Date)
        reports_freq_complete$Date <- strptime(reports_freq_complete$Date, format="%Y-%m-%d")
        reports_freq_complete$Date <- as.Date(reports_freq_complete$Date, format="%Y-%m-%d")
        
        #weekly bins
        reports_freq_complete$Week <- as.Date(cut(reports_freq_complete$Date,
                                                  breaks = "week",
                                                  start.on.monday = FALSE))
        
        #subset by user input
        reports_freq_complete <- reports_freq_complete[which(reports_freq_complete$Date >= as.Date.character(input$selected_dates[1]) & reports_freq_complete$Date <= as.Date.character(input$selected_dates[2])),]
        
        ggplot(data=reports_freq_complete, aes(x=Date, y=Frequency, group=1)) +
            stat_summary(fun.y = sum,
                         geom = "line") +
            xlab(reports_freq_complete$Week) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
    })
    
    
    
    #----------3D. pie chart of reports by region--------------------------
    
    output$regionplot <- renderPlot({
        ggplot(data=region_summary(), aes(x = "", y = Reports, fill = fct_inorder(`Region Percentage`))) +
            blank_theme + theme(axis.text.x=element_blank()) +
            geom_bar(width = 1, stat = "identity", color = "black") +
            coord_polar("y", start = 0) +
            guides(fill = guide_legend(title = "Regional Percentage"))
    })
    
    
    #-------TAB 4: DATA EXPLORER---------------------------------------
    #----------4A. making the pie chart---------------------------------
    #-------------i. making reports_count------------------------------
    
    reports_count <- reactive({
        
        reports_count <- reports()
        
        if (input$selected_region != "default (all)") {
            reports_count <- reports_count[which(reports_count$Region %in% input$selected_region),]
        }
        
        if (input$selected_division != "default (all)") {
            reports_count <- reports_count[which(reports_count$Division %in% input$selected_division),]
        }
        
        if (input$selected_school != "default (all)") {
            reports_count <- reports_count[which(reports_count$`Unique School` %in% input$selected_school),]
        }
        
        if (input$selected_category != "default (all)") {
            reports_count <- reports_count[which(reports_count$`Service Classification/Category` %in% input$selected_category),]
        }
        
        if (input$selected_item != "default (all)") {
            reports_count <- reports_count[which(reports_count$`Service Item` %in% input$selected_item),]
        }
        
        validate(
            need(nrow(reports_count) != 0, "There are 0 reports with the user's selected combination of region, division, school, best practice status, and dates. Please select a different combination."))
        
        reports_count
    })
    
    #-------------ii. making DE_category_summary----------------------------
    
    DE_category_summary <- reactive({
        
        #make a table of reports by category for the subsetted data
        category_summary <- table(reports_count()$`Service Classification/Category`)
        category_summary <- as.data.frame(category_summary)
        colnames(category_summary) <- c("Category", "Reports")
        category_summary <- category_summary %>% arrange(desc(Reports))
        
        #make percentage column (it'll improve understandability of the pie chart)
        category_summary <- category_summary %>% mutate(Percentage = plyr::round_any((Reports / sum(Reports))*100, accuracy=.01, f=floor))
        category_summary$`Category Percentage` <- paste0(category_summary$Category, " - ", category_summary$Reports, " (", category_summary$Percentage, '%)') #treated as text data (not numeric) after this

        return(category_summary)
    })
    
    
    #-------------iii. setting the pie chart colors----------------------------
    
    #light blue, red, orange, yellow, light green, dark green, blue, dark blue, purple, violet
    plot_colors <- c("#C8E2F2","#C20000","#FF7800","#FFCC00","#53A913","#246D16","#30A7EF","#172689","#7A25EE","#BE28D0")
    names(plot_colors) = c("Adopt-a-School Program","Equipment","Funds","Instructional Materials","Others","School Based Management","School Building Program", "School Events", "Student Welfare","Teachers and Personnel")
    
    item_color <- reactive({
        item_color2 <- plot_colors[[input$selected_category]]
        
        colfunc <- colorRampPalette(c(item_color2,"#FFFFFF"))
        item_color3 <- colfunc(nrow(DE_category_summary()))
        
        return(item_color3)
    })
    
    #-------------iv. making the pie chart-----------------------------
    
    category_plot_graphs <- function() {
        
        #the basic pie chart
        p <- ggplot(DE_category_summary(), aes(x = "", y = Reports, fill = fct_inorder(Category))) +
            blank_theme + theme(axis.text.x=element_blank()) +
            geom_bar(width = 1, stat = "identity", color = "grey") +
            coord_polar("y", start = 0) +
            labs(subtitle = paste0(nrow(reports_count())," submissions from ", input$selected_dates[1], " to ", input$selected_dates[2])) +
            theme(legend.title=element_text(size=15),legend.text=element_text(size=12),
                  title=element_text(size=13), legend.key.height=unit(0.8, "cm"))
        
        #add a legend based on the subset of the data (category, item, issue)
        if (input$selected_category == "default (all)") {
            p <- p +
                scale_fill_manual(values=plot_colors, 
                                  name="Service Category Percentage",
                                  breaks=DE_category_summary()$Category,
                                  labels=str_wrap(DE_category_summary()$`Category Percentage`, 40))
            
        } else if ((input$selected_category != "default (all)") & (input$selected_item == "default (all)")) {
            p <- p +
                scale_fill_manual(values=item_color(),
                                  name="Service Item Percentage",
                                  breaks=DE_category_summary()$Category,
                                  labels=str_wrap(DE_category_summary()$`Category Percentage`, 40))
            
        } else if ((input$selected_category != "default (all)") & (input$selected_item != "default (all)")) {
            p <- p +
                scale_fill_manual(values=item_color(),
                                  name="Service Issue Percentage",
                                  breaks=DE_category_summary()$Category,
                                  labels=str_wrap(DE_category_summary()$`Category Percentage`, 40))
        }
        
        #take out the lines between the pie sections if there is only one section (more aesthetic)
        if (nrow(DE_category_summary()) > 1){
            p <- p + geom_bar(width = 1, stat = "identity", color = "grey")
        } else if (nrow(DE_category_summary()) == 1) {
            p <- p + geom_bar(width = 1, stat = "identity")
        }
        
        #add a plot title based on the subset of data (region, division, school)
        if(input$selected_region == "default (all)") {
            p <- p + ggtitle("All Regions, Divisions, and Schools")
        } else if ((input$selected_region != "default (all)") & (input$selected_division == "default (all)")) {
            p <- p + ggtitle(input$selected_region)
        } else if ((input$selected_region != "default (all)") & (input$selected_division != "default (all)") & (input$selected_school == "default (all)")) {
            p <- p + ggtitle(input$selected_division)
        } else if ((input$selected_region != "default (all)") & (input$selected_division != "default (all)") & (input$selected_school != "default (all)")) {
            p <- p + ggtitle(input$selected_school)
        }
        
        return(p)
    }
    
    
    #-------------v. downloading the pie chart (add googlesheets update function here too)-----------------------------------
    
    
    output$categoryplot <- renderPlot(category_plot_graphs())
     ()
    
    ##Pie chart downloader
    
    output$download_pie <- downloadHandler(
        filename = function(){paste("CMSapp_pie",'.png',sep='')},
        content = function(file){
            
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            
            ggsave(file, plot=category_plot_graphs(), device = device,
                   width = 7,
                   height = 5)
        }
    )
    
    
    
    #----------4B. line graph by day and region/division/school (and unique date range)-----------------
    #-------------i. making reports_freq_complete--------------------
    
    reports_freq_complete <- reactive({
        
        #group the data by date (making the incomplete frequency table)
        reports_freq <- freq(reports_count()$Date)  
        reports_freq <- as.data.frame((reports_freq))
        reports_freq <- data.table::setDT(reports_freq, keep.rownames = TRUE)[]
        
        #rename columns
        colnames(reports_freq)[colnames(reports_freq)=="rn"] <- "Date"
        colnames(reports_freq)[colnames(reports_freq)=="n"] <- "Frequency"
        reports_freq <- reports_freq[,-(3:4)]
        
        #We want to add in 0's for dates when there were no reports
        
        #first, create a dataframe of dates in the correct YYYY-MM-DD format
        ts <- seq.POSIXt(as.POSIXlt("2019-04-06"), as.POSIXlt(Sys.Date()), by="day") 
        ts <- format.POSIXct(ts,'%y/%m/%d')
        df <- data.frame(timestamp=ts)
        df$timestamp <- paste(20, df$timestamp, sep="")
        df$timestamp <- gsub('/', '-', df$timestamp)
        colnames(df)[colnames(df)=="timestamp"] <- "Date"
        
        #join the date dateframe to the data dateframe
        reports_freq_complete <- full_join(df,reports_freq, by = "Date")
        
        #change NAs to 0
        reports_freq_complete[is.na(reports_freq_complete)] <- 0
        
        #make sure data is type date
        reports_freq_complete$Date <- as.factor(reports_freq_complete$Date)
        reports_freq_complete$Date <- strptime(reports_freq_complete$Date, format="%Y-%m-%d")
        reports_freq_complete$Date <- as.Date(reports_freq_complete$Date, format="%Y-%m-%d")
        
        #weekly bins
        reports_freq_complete$Week <- as.Date(cut(reports_freq_complete$Date,
                                                  breaks = "week",
                                                  start.on.monday = FALSE))
        
        #subset by user input
        reports_freq_complete <- reports_freq_complete[which(reports_freq_complete$Date >= as.Date.character(input$selected_dates[1]) & reports_freq_complete$Date <= as.Date.character(input$selected_dates[2])),]
        
        return(reports_freq_complete)
    })
    
    
    #-------------ii. making the line graph---------------------
    
    select_daily_plot <- function () {
        q <- ggplot(data=reports_freq_complete(), aes(x=Date, y=Frequency, group=1)) +
            stat_summary(fun.y = sum,
                         geom = "line") +
            xlab(reports_freq_complete()$Week) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
        
        return(q)
    }
    
    output$selectdailyplot <- renderPlot(select_daily_plot())
    
    #-------------iii. downloading the line graph (add googlesheets update function here too)----------------------
    
    output$download_line <- downloadHandler(
        filename = function(){paste("CMSapp_line",'.png',sep='')},
        content = function(file){
            
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            
            ggsave(file, plot=select_daily_plot(), device = device,
                   width = 7,
                   height = 3)}
    )
    
    
    
    
    #-------TAB 5: SUBMISSIONS MAP-----------------------------
    #----------5A. subset the data-----------------------------
    
    reports_map <- reactive({
        
        reports_map <- reports()
        
        if (input$selected_category != "default (all)") {
            reports_map <- reports_map[which(reports_map$`Service Classification/Category` %in% input$selected_category),]
        }
        
        if (input$selected_item != "default (all)") {
            reports_map <- reports_map[which(reports_map$`Service Item` %in% input$selected_item),]
        }
        
        reports_map
    })
    
    
    #region level
    reports_only <- reactive({
        reports_only <- reports_map()[which(reports_map()$Region %in% region_shape$Region_Nam),]
        return(reports_only)
    })
    
    
    #division level
    reports_only2 <- reactive({
        reports_only <- reports_map()[which(reports_map()$Region %in% region_shape$Region_Nam),]
        reports_only <- reports_only[which(reports_only$Division %in% division_shape$Division_N),]
        return(reports_only)
    })
    
    
    #school level
    reports_only3a <- reactive({
        #subsets by date and location by only keeping the report IDs which are in reports
        reports_only <- coord_data[which(coord_data$`Report ID` %in% reports_map()$`Report ID`),]
        return(reports_only)
    })
    
    #school and sni level
    reports_only3 <- reactive({
        #subsets by date and location by only keeping the report IDs which are in reports
        reports_only <- coord_data[which(coord_data$`Report ID` %in% reports_map()$`Report ID`),]
        reports_only <- reports_only[!duplicated(reports_only$`School ID`),]
        reports_only <- reports_only[order(reports_only$`School Name.x`),]
        reports_only$`Extra Unique School` <- paste0(reports_only$`School Name.x`, ' (ID: ', reports_only$`School ID`, '; Division: ', reports_only$Division.x ,')')
        return(reports_only)
    })
    
    
    #----------5B. specific selector----------------------------
    
    observeEvent(input$region_level, {
        
        updateSelectInput(session = session,
                          inputId = "selected_specific",
                          choices = c("default (all)", sort(unique(region_level_data()$Region))),
                          selected = "default (all)")        
    }, priority = 1)
    
    
    observeEvent(input$division_level, {
        
        updateSelectInput(session = session,
                          inputId = "selected_specific",
                          choices = c("default (all)", sort(unique(division_level_data()$Division))),
                          selected = "default (all)")
    }, priority = 1)
    
    
    observeEvent({
        input$school_level
        input$sni_level
        }, {
        
        updateSelectInput(session = session,
                          inputId = "selected_specific",
                          choices = c("default (all)", sort(unique(as.character(reports_only3()$`Extra Unique School`)))),
                          selected = "default (all)")
    }, ignoreNULL = FALSE, priority = 1)
    
    
    #a second updateSelectInput because school and sni-level mapping is affected by subsetting, whereas region and division always has the same polygons
    observeEvent({
        input$selected_category
        input$selected_item
        }, {

        '%ni%' <- Negate('%in%')
        
        if (input$selected_specific %ni% reports_only3()$`Extra Unique School`) {
            
            updateSelectInput(session = session,
                              inputId = "selected_specific",
                              choices = c("default (all)", sort(unique(as.character(reports_only3()$`Extra Unique School`)))),
                              selected = "default (all)")
        }
    }, ignoreNULL = FALSE)
    
    
    #----------5C. making the maps------------------
    #-------------i. basic empty map------------------------------
    
    output$submap <- renderLeaflet({
        
        r <- leaflet() %>%
            
            setView(lat=12.8797, lng=121.7740, zoom=6) %>% #set the view
            addProviderTiles(providers$Esri) #add basemap
        
        return(r)
        
    })
    
    #-------------ii. region map-------------------------------
    
    region_level_data <- eventReactive(input$region_level, {
        
        region_summary <- table(reports_only()$Region)
        region_summary <- as.data.frame(region_summary)
        colnames(region_summary) <- c("Region", "Reports")
        
        #add regions with no reports
        if (nrow(region_summary) != nrow(as.data.frame(region_shape))) {
            region_shape2 <- as.data.frame(region_shape)
            '%ni%' <- Negate('%in%')
            new_df <- region_shape2[which(region_shape2$Region_Nam %ni% region_summary$Region),]
            new_df <- select(new_df, Region_Nam)
            colnames(new_df) <- "Region"
            new_df$Reports <- 0
            
            region_summary <- rbind(region_summary, new_df)
        }
        
        region_summary$Region <- as.character(region_summary$Region)
        region_summary <- region_summary[order(region_summary$Region),]
        
        return(region_summary)
    })
    
    observeEvent(input$region_level, {
        
        region_pal <- colorNumeric(
            palette = "Greens",
            domain = region_level_data()$Reports
        )
        
        region_popup <- paste0(region_level_data()$Region, "<br>",
                               "No. of Submissions: ", region_level_data()$Reports)
        
        output$submap <- renderLeaflet({
            
            if (input$selected_specific != "default (all)") {
                subsetted <- subset(region_shape, Region_Nam %in% input$selected_specific)
                bounding_box <- bbox(subsetted)
                
                r <- leaflet(region_shape) %>%
                    
                    fitBounds(bounding_box[1], bounding_box[2], bounding_box[3], bounding_box[4], options = list()) %>%
                    addProviderTiles(providers$Esri) %>% #add basemap
                    addPolygons(fillColor = ~region_pal(region_level_data()$Reports), weight = 2,
                                opacity = 1, color = "grey", fillOpacity = 0.9,
                                popup = region_popup) %>% #add the actual regions polygons
                    addLegend("topright",
                              pal = region_pal,
                              values = region_level_data()$Reports,
                              title = "Regional Submissions")
                return(r)
                
            } else {
                r <- leaflet(region_shape) %>%
                    
                    setView(lat=12.8797, lng=121.7740, zoom=6) %>% #set the view
                    addProviderTiles(providers$Esri) %>% #add basemap
                    addPolygons(fillColor = ~region_pal(region_level_data()$Reports), weight = 2,
                                opacity = 1, color = "grey", fillOpacity = 0.9,
                                popup = region_popup) %>% #add the actual regions polygons
                    addLegend("topright",
                              pal = region_pal,
                              values = region_level_data()$Reports,
                              title = "Regional Submissions")
                
                return(r)
            }
            
        })
    }, priority = -1)
    
    #-------------iii. division map-----------------------------
    
    division_level_data <- eventReactive(input$division_level, {
        
        division_summary <- table(reports_only2()$Division)
        division_summary <- as.data.frame(division_summary)
        colnames(division_summary) <- c("Division", "Reports")
        
        #add divisions with no reports
        if (nrow(division_summary) != nrow(as.data.frame(division_shape))) {
            division_shape2 <- as.data.frame(division_shape)
            '%ni%' <- Negate('%in%')
            new_df <- division_shape2[which(division_shape2$Division_N %ni% division_summary$Division),]
            new_df <- select(new_df, Division_N)
            colnames(new_df) <- "Division"
            new_df$Reports <- 0
            
            division_summary <- rbind(division_summary, new_df)
        }
        
        division_summary$Division <- as.character(division_summary$Division)
        division_summary <- division_summary[order(division_summary$Division),]  
        
        return(division_summary)
    })
    
    observeEvent(input$division_level, {
        
        division_pal <- colorNumeric(
            palette = "Greens",
            domain = division_level_data()$Reports
        )
        
        division_popup <- paste0(division_level_data()$Division, "<br>",
                                 "No. of Submissions: ", division_level_data()$Reports)
        
        output$submap <- renderLeaflet({
            
            if (input$selected_specific != "default (all)") {
                subsetted <- subset(division_shape, Division_N %in% input$selected_specific)
                bounding_box <- bbox(subsetted)
                
                d <- leaflet(division_shape) %>%
                    
                    fitBounds(bounding_box[1], bounding_box[2], bounding_box[3], bounding_box[4], options = list()) %>%          addProviderTiles(providers$Esri) %>% #add basemap
                    addPolygons(fillColor = ~division_pal(division_level_data()$Reports), weight = 2,
                                opacity = 1, color = "grey", fillOpacity = 0.9,
                                popup = division_popup) %>% #add the actual regions polygons
                    addLegend("topright",
                              pal = division_pal,
                              values = division_level_data()$Reports,
                              title = "Divisional Submissions")
                
                return(d)
                
            } else {
                d <- leaflet(division_shape) %>%
                    
                    setView(lat=12.8797, lng=121.7740, zoom=6) %>% #set the view
                    addProviderTiles(providers$Esri) %>% #add basemap
                    addPolygons(fillColor = ~division_pal(division_level_data()$Reports), weight = 2,
                                opacity = 1, color = "grey", fillOpacity = 0.9,
                                popup = division_popup) %>% #add the actual regions polygons
                    addLegend("topright",
                              pal = division_pal,
                              values = division_level_data()$Reports,
                              title = "Divisional Submissions")
                
                return(d)
            }
        })
    }, priority = -1)
    
    #-------------iv. school map-------------------------------
    
    school_level_data <- eventReactive(input$school_level, {
        
        #make a table of reports by school ID
        school_summary <- table(reports_only3a()$`School ID`)
        school_summary <- as.data.frame(school_summary)
        colnames(school_summary) <- c("School ID", "Reports")
        school_summary$`School ID` <- as.character(school_summary$`School ID`)
        school_summary <- school_summary[order(school_summary$`School ID`),]
        
        #get the subsetted data for coordinates and other characteristics
        reports_only4 <- reports_only3()
        reports_only4$`School ID` <- as.character(reports_only4$`School ID`)
        
        #merge the two
        school_summary <- merge(school_summary, reports_only4, by = c("School ID"), all = FALSE)
        
        return(school_summary)
    })
    
    
    observeEvent(input$school_level, {
        
        school_pal <- colorNumeric(
            palette = "Greens",
            domain = school_level_data()$Reports
        )
        
        school_popup <- paste0(school_level_data()$`Extra Unique School`, "<br>",
                               "No. of Submissions: ", school_level_data()$Reports)
        
        output$submap <- renderLeaflet({
            if (input$selected_specific != "default (all)") {
                subsetted <- subset(school_level_data(), as.character(`Extra Unique School`) %in% input$selected_specific)
                
                s <- leaflet(school_level_data()) %>%
                    
                    setView(lat=subsetted$Latitude, lng=subsetted$Longitude, zoom=15) %>% 
                    addProviderTiles(providers$Esri) %>% #add basemap
                    addCircleMarkers(lat = school_level_data()$Latitude, lng = school_level_data()$Longitude,
                                     color = "grey",
                                     stroke = TRUE,
                                     weight = 2,
                                     fill = TRUE,
                                     fillColor = ~school_pal(school_level_data()$Reports),
                                     fillOpacity = 0.9,
                                     radius = 4 + ((school_level_data()$Reports)/7),
                                     popup = school_popup,
                                     popupOptions = popupOptions(maxWidth = 220, closeOnClick = TRUE)) %>%
                    addLegend("topright",
                              pal = school_pal,
                              values = school_level_data()$Reports,
                              title = "School Submissions")
                
                return(s)
                
            } else {
                s <- leaflet(school_level_data()) %>%
                    
                    setView(lat=12.8797, lng=121.7740, zoom=6) %>% #set the view
                    addProviderTiles(providers$Esri) %>% #add basemap
                    addCircleMarkers(lat = school_level_data()$Latitude, lng = school_level_data()$Longitude,
                                     color = "grey",
                                     stroke = TRUE,
                                     weight = 2,
                                     fill = TRUE,
                                     fillColor = ~school_pal(school_level_data()$Reports),
                                     fillOpacity = 0.9,
                                     radius = 4 + ((school_level_data()$Reports)/7),
                                     popup = school_popup,
                                     popupOptions = popupOptions(maxWidth = 220, closeOnClick = TRUE)) %>%
                    addLegend("topright",
                              pal = school_pal,
                              values = school_level_data()$Reports,
                              title = "School Submissions")
                
                return(s)
            }
        })
    }, priority = -1)
    
    #-------------v. sni map----------------------------------
    
    reports_only4 <- eventReactive(input$sni_level, {
        
        reports_only4 <- reports_only3()
    })
    
    
    observeEvent(input$sni_level, {
        output$submap <- renderLeaflet({
            
            sni_pal <- colorNumeric(
                palette = "Blues",
                domain = reports_only4()$sni_score
            )
            
            if (input$selected_specific != "default (all)") {
                subsetted <- subset(reports_only4(), `Extra Unique School` %in% input$selected_specific)
                
                sni <- leaflet(reports_only4()) %>%
                    
                    setView(lat=subsetted$Latitude, lng=subsetted$Longitude, zoom=15) %>% 
                    addProviderTiles(providers$Esri) %>% #add Esri basemap
                    addCircleMarkers(data = reports_only4(), lat = reports_only4()$Latitude, lng = reports_only4()$Longitude,
                                     color = "grey",
                                     stroke = TRUE,
                                     weight = 2,
                                     fill = TRUE,
                                     fillColor = ~sni_pal(reports_only4()$sni_score),
                                     fillOpacity = 0.9,
                                     radius = 3 + (reports_only4()$sni_score*3),
                                     popup = paste0(strong("School: "), reports_only4()$`Extra Unique School`, "<br>",
                                                    strong("SNI Score: "), plyr::round_any(reports_only4()$sni_score, accuracy = .01, f=floor), "<br>",
                                                    strong("Student-Teacher Ratio: "), reports_only4()$Total_Enrollment_y, ":", reports_only4()$Total_Teaching, "<br>",
                                                    strong("Student-Classroom Ratio: "), reports_only4()$Total_Enrollment_y, ":", reports_only4()$Instructional_Rooms, "<br>",
                                                    strong("Access to Water: "), reports_only4()$Water_Access, "<br>",
                                                    strong("Access to Electricity: "), reports_only4()$Electricity_Access, "<br>",
                                                    strong("Access to Internet: "), reports_only4()$Internet_Access, "<br>",
                                                    strong("Remoteness Index: "), plyr::round_any(reports_only4()$remoteness_index, accuracy = .01, f=floor), "<br>",
                                                    strong("Students Receiving CCT's: "), plyr::round_any(reports_only4()$cct_percentage, accuracy = .01, f=floor), "%", "<br>"),
                                     popupOptions = popupOptions(maxWidth = 220, closeOnClick = TRUE)) %>%
                    addLegend("topright",
                              pal = sni_pal,
                              values = reports_only4()$sni_score,
                              title = "SNI Score")
                
                return(sni)
                
            } else {
                sni <- leaflet(reports_only4()) %>%
                    
                    setView(lat=12.8797, lng=121.7740, zoom=6) %>% #set the view
                    addProviderTiles(providers$Esri) %>% #add Esri basemap
                    addCircleMarkers(lat = reports_only4()$Latitude, lng = reports_only4()$Longitude,
                                     color = "grey",
                                     stroke = TRUE,
                                     weight = 2,
                                     fill = TRUE,
                                     fillColor = ~sni_pal(reports_only4()$sni_score),
                                     fillOpacity = 0.9,
                                     radius = 3 + (reports_only4()$sni_score*3),
                                     popup = paste0(strong("School: "), reports_only4()$`Extra Unique School`, "<br>",
                                                    strong("SNI Score: "), plyr::round_any(reports_only4()$sni_score, accuracy = .01, f=floor), "<br>",
                                                    strong("Student-Teacher Ratio: "), reports_only4()$Total_Enrollment_y, ":", reports_only4()$Total_Teaching, "<br>",
                                                    strong("Student-Classroom Ratio: "), reports_only4()$Total_Enrollment_y, ":", reports_only4()$Instructional_Rooms, "<br>",
                                                    strong("Access to Water: "), reports_only4()$Water_Access, "<br>",
                                                    strong("Access to Electricity: "), reports_only4()$Electricity_Access, "<br>",
                                                    strong("Access to Internet: "), reports_only4()$Internet_Access, "<br>",
                                                    strong("Remoteness Index: "), plyr::round_any(reports_only4()$remoteness_index, accuracy = .01, f=floor), "<br>",
                                                    strong("Students Receiving CCT's: "), plyr::round_any(reports_only4()$cct_percentage, accuracy = .01, f=floor), "%", "<br>"),
                                     popupOptions = popupOptions(maxWidth = 220, closeOnClick = TRUE)) %>%
                    addLegend("topright",
                              pal = sni_pal,
                              values = reports_only4()$sni_score,
                              title = "SNI Score")
                
                return(sni)
            }
        })
    }, priority = -1)
    
    #-------TAB 6: RAW DATA-------------------------------
    #----------6A. categoryreportsdata---------------------
    categoryreportsdata <- function () {
        
        reports_count <- data.table::as.data.table(reports_count())
        reports_count <- reports_count[order(-`Report ID`),]
        
        #error message if there are no reports
        validate(
            need(nrow(reports_count) != 0, "There are 0 reports with the user's selected combination of region, division, school, best practice status, and dates. Please select a different combination."))
        
        #only show the necessary columns
        reports_count <- select(reports_count, -c(`Reported By`, `Unique School`))
        reports_count$`Report ID` <- as.character(reports_count$`Report ID`)
        reports_count$`School ID` <- as.character(reports_count$`School ID`)
        
        return(reports_count)
    }
    
    #----------6B. categoryreports table-------------------
    
    output$categoryreports <- renderDataTable({datatable(categoryreportsdata(),
                                                         options = list(paging = FALSE, scrollY = "660px", scrollX = T,
                                                                        bFilter = FALSE, ordering = FALSE))
    })
    
    
    #----------6C. data download and credentials export (not working)------
    
    #flag to use in observeEvent
    rv <- reactiveValues(download_flag = "red")
    
    observeEvent(rv$download_flag == "green", {
        
        if ((nchar(input$name_authentication) > 5) & (grepl(" ", input$name_authentication)) & (nchar(input$email_authentication) > 6) & (grepl("@", input$email_authentication))) {
            
            download_df <- data.frame(input$name_authentication,input$email_authentication, "raw data")
            colnames(download_df) <- c("Name", "Email", "File")
            
            #retrieve old data from googlesheet using googlesheets4 package
            old_df <- read_sheet(ss="1dCjCUmMR6GrjTQNTuTdmOHy9SgvCDMmU0HfQ_fSCqq8")
            
            new_df <- rbind(as.data.frame(old_df), download_df)
            
            #save temporary file (googledrive's drive_update function only works with a file, not object)
            f <- (file.path(tempdir(), "cms_data_downloads.csv", fsep = "\\"))
            
            write.csv(new_df, file = f)
            
            
            
            
            
            
            
            
            #upload new data using googledrive package (THIS STILL ISN'T WOrkING AGGGHHHHHH)
            cms_data_downloads <- drive_upload(media = f, name = "CMS App Dashboard Data Downloads",type = "spreadsheet", overwrite = TRUE)
            
            
            
            
            
        }
    })
    
    
    #download button, if name and email are given (should find a way to verify these later...)
    output$download_data <- downloadHandler(
        filename = function() {
            paste0('CMSapp-',input$selected_dates[1],'-',input$selected_dates[2], '.csv')
        },
        
        content = function(file) {
            
            if ((nchar(input$name_authentication) > 5) & (grepl(" ", input$name_authentication)) & (nchar(input$email_authentication) > 6) & (grepl("@", input$email_authentication))) {
                
                rv$download_flag <- "green"
                
                write.csv(categoryreportsdata(), file, row.names = FALSE)
                
            } else {
                
                rv$download_flag <- "red"
                
                
                write.csv(NULL, file)
            }
        }
    )

    
}


#-------run the app-----------------------

shinyApp(ui = ui, server = server)