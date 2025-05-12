


source("01_ConfigInputs.R")

#create a new directory to save the figures too based on the date. 


library(aws.s3)

figure_location <- "Output/"
output_path <- file.path(figure_location, "Model_results", as.character(Sys.Date()))
file_loc <- normalizePath(output_path)

bucket <- "rfc-conditions/gw_forecasting/outputs"
region <- ""

# Recursively list all files in the directory and subdirectories
all_files <- list.files(file_loc, recursive = TRUE, full.names = TRUE)

for (file_path in all_files) {
  # Determine S3 object key (relative path from output_path)
  relative_key <- sub(paste0(normalizePath(output_path), .Platform$file.sep), "", file_path, fixed = TRUE)
  
  message("Uploading: ", relative_key)
  
  put_object(file = file_path,
             object = relative_key,
             bucket = bucket,
             region = region,
             acl = "public-read")
}




# figure_location <- "Output/"
# 
# # Specify the path where you want to create the new folder
# output_path <- paste0(figure_location, "Model_results/", as.character(Sys.Date()))
# 
# 
# 
# library(aws.s3)
# 
# 
# b <- 'rfc-conditions/gw_forecasting/outputs'
# r <- ""
# 
# file_loc <- normalizePath(output_path)
# 
# output_files <- list.files(file_loc)
# 
# all_files <- list.files(file_loc, recursive = TRUE, full.names = TRUE)
# 
# for (file_name in all_files) {
#   
#   relative_key <- sub(paste0(normalizePath(output_path), .Platform$file.sep), "", file_name, fixed = TRUE)
#   
#   
#   # file_name <- output_files[1]
#   put_object(file = file.path(file_loc, file_name),
#              object = file_name,
#              bucket = b, region = r, 
#              acl = "public-read")
# }







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
