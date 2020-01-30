#-------load reports data-----------------

reports1 <- readxl::read_excel('./cleaned_cms_data_dec3-2019.xlsx')

#date is in yyyy-mm-dd format
reports1$`Reported On` <- format(strptime(reports1$`Reported On`, format = "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d %H:%M:%S")
reports1$`Reported On` <- as.Date(reports1$`Reported On`)
colnames(reports1)[colnames(reports1)=="Reported On"] <- "Date"
reports1$Date <- gsub("/","-",reports1$Date)

#change Report ID to ID
colnames(reports1)[colnames(reports1)=="Report ID"] <- "ID"


#-------load coordinate and sni data------------------


coord_data <- read.csv('./coord_data.csv')


coord_data <- coord_data[!duplicated(coord_data[,1]),]

#keep only schools which have reports (make sure this uses the correct regions and divisions too)
colnames(coord_data)[colnames(coord_data)=="School_Name_y"] <- "School Name"
colnames(coord_data)[colnames(coord_data)=="School_ID"] <- "School ID"
colnames(coord_data)[colnames(coord_data)=="Region_Name"] <- "Region"

#coord_data <- merge(coord_data, reports1, by = c("School Name", "Region", "Division"), all = FALSE)


#-------figure out what's happening----------------------------


coord_data <- merge(coord_data, reports1, by = c("School ID"), all = FALSE)


id_list <- c()

id_list <- list.append(id_list, unique(reports1$`School ID`))
id_list <- as.list(id_list)

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
