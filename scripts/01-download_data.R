library(opendatatoronto)
library(dplyr)

# get package and resources for the dataset 
packages <- show_package("police-annual-statistical-report-victims-of-crime")
resources <- list_package_resources("police-annual-statistical-report-victims-of-crime")

# filter for only CSV datastore
datastore_resources <- filter(resources, tolower(format) %in% c('csv'))

# load the datastore
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

# set saving file path and write to it
file_path <- "data/raw_data/toronto_crime_victims_raw.csv"
write.csv(data, file_path, row.names = FALSE)