


source("01_ConfigInputs.R")

#create a new directory to save the figures too based on the date. 

figure_location <- "Output/"

# Specify the path where you want to create the new folder
output_path <- paste0(figure_location, "Model_results/", as.character(Sys.Date()))



library(aws.s3)


b <- 'rfc-conditions/gw_forecasting/outputs'
r <- ""

file_loc <- normalizePath(output_path)

output_files <- list.files(file_loc)

for (file_name in output_files) {
  # file_name <- output_files[1]
  put_object(file = paste0(file_loc,"\\", file_name),
             object = file_name,
             bucket = b, region = r, 
             acl = "public-read")
}




# b <- 'rfc-conditions/gw_forecasting/models'
# r <- ""
# 
# file_loc <- "models/"
# 
# output_files <- list.files(file_loc)
# 
# for (file_name in output_files) {
#   put_object(paste0(file_loc, file_name),
#              object = file_name,
#              bucket=b, region=r,acl = "public-read")
# }
# 
# load(url("https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/models/ANN_Model_OW002_14.Rdata"))
# 
