#
# Script to manage the updating and distribution of the legacy swap report
#

source("\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R")
# set parameters -----------------------------------------------------

# Use this method to avoid drive mappings in dynamic loading of content
FX_PATH <- function(subpath) {
  file.path("\\\\markets-nwsrv/DATA/Offdata/FX/FX Dealers", subpath)
}

# boe checkpoint
checkpoint_date <- "2022-01-01"
message(paste0("checkpoint_date: ", checkpoint_date))
boeCheckpoint(checkpoint_date, scanForPackages = FALSE)

ROOT <- FX_PATH(subpath = "_FX_jobs/FXScheduledJobs")  
DASHBOARD_ROOT <- readLines("N:/Offdata/RM/DASHBOARD_ROOT")

library(dplyr)
library(ggplot2)
library(DT)
library(futile.logger)
library(janitor)
library(readr)
library(tableHTML)
library(taskscheduleR)

RDCOM_str <- "library(RDCOMClient, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
eval(parse(text=RDCOM_str))

clean_tmpfiles_mod <- function() {
  message("Calling clean_tmpfiles_mod()")
}

assignInNamespace("clean_tmpfiles", clean_tmpfiles_mod, ns = "rmarkdown")


flog.info("--------------- Started ---------------")

if(as.numeric(R.Version()$major) < 4) flog.error("R version needs to be 4.0 or greater to BRE report!")

#
# Set the pandoc path for users without pandoc in their path
#

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

#
# Set the working directory
#

file_name <- "BREdailyreport.html"

if(!file.exists("~/temprMarkdownRender")){
  dir.create("~/temprMarkdownRender")
}

#Determine output file + path
intermediate_path <- "~/temprMarkdownRender"
output_path <- file.path(intermediate_path, "doc")
output_doc <- file.path(output_path, file_name)

rmarkdown::render(input = file.path(ROOT, "tasks", "BREreport", "report", "BREdailyreport.Rmd"),
                  intermediates_dir = intermediate_path,
                  output_dir = output_path,
                  output_file = file_name
)

# Copy the files to the archive and to the dashboard

flog.info("Copying the report to the root directory ... ")
file.copy(
  overwrite = T, 
  output_doc,
  file.path(ROOT, "tasks", "BREreport")
)

flog.info("Copying to the _Report archive ... ")
file.copy(
  overwrite = T, 
  output_doc,
  file.path(dirname(dirname(ROOT)), "_Reports/BREdailyreport"), 
)

flog.info("Copying to the RM dashboard ... ")
file.copy(
  output_doc,
  file.path(DASHBOARD_ROOT, "/RMDashboard/www/reports/BREdailyreport.html"),
  overwrite = TRUE
)

# generate email report ---------------------------------------------------

static <- readr::read_csv("./config/bre_users.csv", show_col_types = F) 

flog.info("Sending email summary")

run_date <- Sys.time()

nostro <- c(nostro_errors, nostro_warnings)

if(!is.null(nostro)){
  
  nostro_result <- c(nostro_errors, nostro_warnings) %>%
    tidyr::as_tibble() %>%
    magrittr::set_names(c("")) %>% 
    tableHTML::tableHTML(rownames = FALSE) 
  
} else{
  
  nostro_result <- data.frame()
  
}



repo_shorts_results <- shorts %>%
  tidyr::as_tibble() %>%
  tableHTML::tableHTML(rownames = FALSE) 

repo_special_results <- specials %>%
  tidyr::as_tibble() %>%
  tableHTML::tableHTML(rownames = FALSE) 


# HTML formatting
body <- stringr::str_c(
  paste0("Please find the latest BRE report attached, the report was generated at  ", format(run_date, "%d-%b %Y, %H:%M"), 
         " \n \n ", 
         "<br>", 
         "<br>", 
         " Nostro report results:",
         "<br>", 
         paste0(
           "<html><head>
               <style>
               body{font-family:Arial, \"sans-serif\";}
               table{border-left:1px solid #000000;border-top:1px solid #000000;}
               table td{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:12px; font-weight:normal;}
               table th{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:14px;}
               </style>
               </head><body>",
           nostro_result,
           "</body></html>"
         ), 
         "<br>", 
         "<br>", 
         "\n  ", 
         " Repo short covers (up to t + 10):",
         " <br> ", 
         " <br> ", 
         
         paste0(
           "<html><head>
               <style>
               body{font-family:Arial, \"sans-serif\";}
               table{border-left:1px solid #000000;border-top:1px solid #000000;}
               table td{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:12px; font-weight:normal;}
               table th{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:14px;}
               </style>
               </head><body>",
           repo_shorts_results,
           "</body></html>"
         ), 
         "<br>", 
         "<br>", 
         "\n  ", 
         " Repo specials rolling off (up to t + 10):",
         " <br> ", 
         " <br> ", 
         
         paste0(
           "<html><head>
               <style>
               body{font-family:Arial, \"sans-serif\";}
               table{border-left:1px solid #000000;border-top:1px solid #000000;}
               table td{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:12px; font-weight:normal;}
               table th{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:14px;}
               </style>
               </head><body>",
           repo_special_results,
           "</body></html>"
         ), 
         "<br>", 
         "<br>", 
         "if you have any questions please contact: \n \n ", 
         "<br>",
         "<br>",
         "Nicole.Webster@bankofengland.co.uk   or \n ", 
         "<br>",
         "Nirvar.Singh@bankofengland.co.uk \n \n \n ", 
         "<br>",
         "<br>",
         "Note: this is an automated email."
         
  ))


Outlook <- RDCOMClient::COMCreate("Outlook.Application")
Email = Outlook$CreateItem(0)
Email[["to"]] = paste(static$email, collapse = "; ")
Email[["subject"]] = paste0("BRE daily report ", format(run_date, "%d-%b %Y"))
Email[["HTMLbody"]] = body

Email[["attachments"]]$Add(file.path(ROOT, "tasks", "BREreport", file_name))

Email$Send()

flog.info("Email sent")

# Delete the temporary file created earlier
if(file.exists("~/temprMarkdownRender")){
  unlink("~/temprMarkdownRender", recursive = TRUE)
}


flog.info(Sys.time())
flog.info("Finished")
