#set working directory
setwd("~/William_and_Mary/WM_Year2Summer2019/Philippines_GRI_Freeman_CMS/CMS/cms_app_usage_dashboard/Updated_App_Dashboard/fix_old_data")


#load packages
library(rlist)
library(xlsx)


#load data
reports1 <- readxl::read_excel('./cmsappreports_december.xlsx')
reports2 <- readxl::read_excel('./cmsappreports_september.xlsx')

#remove potential duplicates
reports2 <- reports2[!duplicated(reports2[,2:10]), ]

#create an empty list to store report IDs
id_list <- c()

#create a loop to find and append to the above list all report IDs which have BE as their service item
i <- 1
while (i <= nrow(reports2)) {
  y <- reports2[i,]
  if (y$`Service Item` == "Brigada Eskwela") {
    id_list <- list.append(id_list,y$ID)
  }
  i = i + 1
}

#create another two empty lists to check for equivalence between IDs
checking_list <- c()
uhoh_list <- c()

for (id in id_list) {
  if (id %in% reports1$`Report ID`) {
    checking_list <- list.append(checking_list,id)
  }
  else {
    uhoh_list <- list.append(uhoh_list,id)
  }
}

#okay, so we see here that there are only 406/435 reports with the same id. That's because how Bridge360 removed duplicates
#differs from how I removed duplicates.

#After lots of data examining, it seems that after consulting with CMS more, they decided to not differ by picture url, so they
#deleted more reports. In other words, we don't need to worry about the uhoh_list after all :)


#now, we can finally change the categories
for (id in id_list) {
  reports1[which(reports1$`Report ID` == id),6] <- reports2[which(reports2$ID == id),5]
  reports1[which(reports1$`Report ID` == id),7] <- reports2[which(reports2$ID == id),6]
  reports1[which(reports1$`Report ID` == id),8] <- reports2[which(reports2$ID == id),7]
}


#write.xlsx(reports1, "cleaned_cms_data_dec3-2019.xlsx")
