#Author: Matthew Crittenden
#File Name: auto_data.R
#File Purpose: Download the Data from the CMS App backend and Upload it to Google Sheets
#Date Created: 12/3/2019
#Date Edited: 1/7/2019
#Related scripts/tasks: (1) app.R (CMS Dashboard v3.0); global.R (CMS Dashboard v3.0); (3) task to run this file daily at 10AM EST


library(stringr)
library(RSelenium)
library(googledrive)
library(googlesheets4)


#step 1: start selenium and set up a temporary directory
td = tempdir()

eCaps <- list(
  chromeOptions = 
    list(#args = c('--headless', '--disable-gpu'), #make RSelenium run quietly (might not work w downloads)
      prefs = list(
        "profile.default_content_settings.popups" = 0L,
        "download.prompt_for_download" = FALSE,
        "download.default_directory" = "~/William_and_Mary/WM_Year2Summer2019/Philippines_GRI_Freeman_CMS/CMS/cms_app_usage_dashboard/CMS_Dashboard_v3.0/auto_data"
      )
    )
)

rD <- rsDriver(browser = "chrome", chromever = "79.0.3945.36", extraCapabilities = eCaps) #the chromever may need to be changed every couple months to match chrome version updates
remDr <- rD$client



#step 2: navigate to page
remDr$navigate("https://app.checkmyschool.org/admin")



#step 3: pass log-in credentials to access the dashboard

#send username
username <- remDr$findElement(using = "id", value = "email")
username$sendKeysToElement(list("test"))

#send password and Enter
passwd <- remDr$findElement(using = "id", value = "pswd")
passwd$sendKeysToElement(list("123456", "\uE007"))



#step 4: click the "show-filter" button (arrow shaped)
#but first i need to tell the system to wait because the page takes a while to load
Sys.sleep(10)
webElem <-NULL
while(is.null(webElem)){
  webElem <- tryCatch({remDr$findElement(using = "id", value = "show-filter")},
                      error = function(e){NULL})
}
webElem$clickElement()



#step 5: click the "within-month" button (circle shaped)
# webElem2 <-NULL
# while(is.null(webElem2)){
#   webElem2 <- tryCatch({remDr$findElement(using = "id", value = "within-month")},
#                       error = function(e){NULL})
# }
Sys.sleep(3)
webElem2 <- remDr$findElement(using = "id", value = "within-month")
webElem2$clickElement()



#step 6: change the "start-date" to April 5, 2019 and we'll just cut the reports before 6/14
Sys.sleep(3)
webElem3 <- remDr$findElement(using = "id", value = "start-date")
webElem3$sendKeysToElement(list("04052019"))



#step 7: click the "extract-btn"
webElem4 <- remDr$findElement(using = "id", value = "extract-btn")
webElem4$clickElement()

Sys.sleep(10)

#step 8: upload the data to Google Sheets
#enable use of googlesheets API in non-interactive settings
source("global.R") #this pulls in the hidden variable information from the global.R file
drive_auth_configure(api_key = cms_dashboard_key)
drive_deauth()




#this line still isn't working UGGGGGGGGHHHHHHHHHHHHH
drive_upload(media = paste0("C:/Users/Matt/Downloads","/Report_", Sys.Date(),".csv"), path = as_id(drive_get('cms_data/')), name = "cms_csv_data",type = "spreadsheet", overwrite = TRUE)





#let's try to free up the port for the next time the code runs
remDr$close()
rD$server$stop()
rm(rD)
