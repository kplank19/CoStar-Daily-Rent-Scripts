
rm(list = ls())
pacman::p_load(RDCOMClient, htmlTable, praise)

# Daily Encouragement
print(paste("email time", praise()))

# Retrieve date for file selection and appropriate body
time <- as.character(Sys.time())
time <- substr(time, 1, 10) # Date for email
curr <- gsub("-", "", time) # Date to retrieve proper files
print(time)

curr_1 <- substr(curr, 5, 8)
curr_2 <- substr(curr, 1, 2)
curr <- paste0(curr_1, curr_2) # Rearrange date format for report name



curr_1 <- substr(curr, 1, 2)
curr_2 <- substr(curr, 3, 4)
curr_3 <- substr(curr, 5, 6)
curr_comp <- paste0(curr_1,"-", curr_2, "-", curr_3) # Rearrange date format for comp table



# Select attachments
#data <- paste0("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/costar_df_", curr, ".csv")
#report <- paste0("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Reports/costar_rent_report_markdown_", curr, ".html")
report <- paste0("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Reports/daily_asking_rents_", curr, ".html")

table <- paste0("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/comp_tables/Comp Table ", curr_comp, ".html")

# add email/ adjust text here

#recipients <- "MSchall@essex.com; AKleiman@essex.com; BPak@essex.com; jburkart@essex.com; JeAnderson@essex.com; aberry@essex.com; MGerard@essex.com; JFoley@essex.com; ngodsk@essex.com; lwu@essex.com; RBurns@essex.com; SReinert@essex.com; COdonoghue@essex.com; dmorley@essex.com; NGibson@essex.com; BEgge@essex.com; CNeta@essex.com; SKerr@essex.com; tjacops@essex.com; RSischo@essex.com; bsusim@essex.com; RSischo@essex.com; tjacops@essex.com; pmorgan@essex.com; citran@essex.com;  phummelt@essex.com; kplank@essexpropertytrust.com"
recipients <- "kplank@essexpropertytrust.com"

subject <- paste("CoStar Daily Rent Report:", time)
body <- paste("Hello All,\nAttached is the Costar Rent Report & Subject vs. Comp Table for", time, ". Please let us know of any feedback!\nThanks!\n-Kristian")


# take screen shot and attach image
img_archive <- paste0("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Screenshots/Report Image ", time, ".png")
img_curr <- "C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Screenshots/Report.png"

webshot::webshot(url = report, 
                file = img_archive)

webshot::webshot(url = report, 
                file = img_curr)

#----
# 
# webshot::webshot(url = "E:/Projects_network/HeadRoomAnalysis/costar_rent_report_markdown.html", 
#                  file = "C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Screenshots/tester.png")


# <- paste0( "<img src='cid:",
#                             basename(SimplePlot.file),
#                             "' width = '400' height = '400'>")


#png(file = "agrf.png")

#Email[["attachments"]]$Add("C:/Users/kplank/Pictures/agrf.png")
#----


#MyHTML <- paste0("<html><p>Hello All,\nAttached is the Costar Rent Report for ", time, ". Please let us know of any feedback!\nThanks!\n-Kristian</p> 
#<img src='C:/Users/kplank/Pictures/garf.jpg' >")


MyHTML <- paste0("<html><p>Hello All,\nAttached is the Costar Rent Report & Subject vs. Comp Table for ", time, ". Please let us know of any feedback!\nThanks!\n-Kristian</p> 
<img src='C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/Screenshots/Report.png' >")


# Send the Email
OutApp <- COMCreate("Outlook.Application")

# # ## create an email 
outMail = OutApp$CreateItem(0)
# # ## configure  email parameter 
outMail[["To"]] = recipients
outMail[["subject"]] = subject
outMail[["HTMLbody"]] = MyHTML
#outMail[["attachments"]]$Add(data)
outMail[["attachments"]]$Add(report)
outMail[["attachments"]]$Add(img_curr)
outMail[["attachments"]]$Add(table)
# # ## send it  
outMail$Send()


