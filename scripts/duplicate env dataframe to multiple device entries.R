## This script is to create a dataframe with duplicate rows of the environmental data so that each row will match with a device. Current dataframe has only one row/entry per site and this script will multiply that entry 25 times to have it for each device. 

#load data --> FinalMetrics_Summary.xlsx 
#name df: Y1Environment

davies.env<- Y1Environment|> filter(Reef=="Davies") |> select(-Reef)

# Create an empty dataframe to store the duplicated observations
duplicated_df <- data.frame()

# Loop through each site in your original dataframe
for (Site in unique(davies.env$Site)) {
  # Subset the original dataframe for the current site
  site_data <- davies.env[davies.env$Site == Site, ]
  
  # Duplicate the observations 25 times for the current site
  duplicated_site_data <- site_data[rep(row.names(site_data), each = 25), ]
  
  # Append the duplicated observations to the new dataframe
  duplicated_df <- rbind(duplicated_df, duplicated_site_data)
}

# Reset row names of the duplicated dataframe
rownames(duplicated_df) <- NULL

# Now 'duplicated_df' contains the dataframe with observations duplicated 25 times for each site

############################
moore.env<- Y1Environment|> filter(Reef=="Moore") |> select(-Reef)

# Create an empty dataframe to store the duplicated observations
duplicated_df2 <- data.frame()

# Loop through each site in your original dataframe
for (Site in unique(moore.env$Site)) {
  # Subset the original dataframe for the current site
  site_data <- moore.env[moore.env$Site == Site, ]
  
  # Duplicate the observations 25 times for the current site
  duplicated_site_data <- site_data[rep(row.names(site_data), each = 25), ]
  
  # Append the duplicated observations to the new dataframe
  duplicated_df2 <- rbind(duplicated_df2, duplicated_site_data)
}

# Reset row names of the duplicated dataframe
rownames(duplicated_df2) <- NULL

# Now 'duplicated_df' contains the dataframe with observations duplicated 25 times for each site

############################
heron.env<- Y1Environment|> filter(Reef=="Heron") |> select(-Reef)

# Create an empty dataframe to store the duplicated observations
duplicated_df3 <- data.frame()

# Loop through each site in your original dataframe
for (Site in unique(heron.env$Site)) {
  # Subset the original dataframe for the current site
  site_data <- heron.env[heron.env$Site == Site, ]
  
  # Duplicate the observations 25 times for the current site
  duplicated_site_data <- site_data[rep(row.names(site_data), each = 25), ]
  
  # Append the duplicated observations to the new dataframe
  duplicated_df3 <- rbind(duplicated_df3, duplicated_site_data)
}

# Reset row names of the duplicated dataframe
rownames(duplicated_df3) <- NULL

# Now 'duplicated_df' contains the dataframe with observations duplicated 25 times for each site

############
YR1Env_device <-rbind(duplicated_df,duplicated_df2,duplicated_df3)


library(openxlsx)
write.xlsx(YR1Env_device, "YEAR1 Environmental Data Devices.xlsx", rowName = FALSE)
