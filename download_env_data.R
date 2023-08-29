#This code is to be run in R

#install all necessary packages

library(tidyverse)
library(data.table)
library(ggplot2)

#for opening shape file
library(sf)

#Websites to download data from:
#loc_plus_label_28feb17_numeric.csv   -from liz
#https://github.com/kelvins/US-Cities-Database/blob/main/csv/us_cities.csv
#https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-cities-and-towns.html
#https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2010&layergroup=Roads
#https://maps.princeton.edu/catalog/harvard-glb-oilgas
#for next link: data from each individual place says 1km in name i think
#https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/ModeledStreamTemperatureScenarioMaps.shtml

#Read the Excel file containing the coordinates and HUC codes
data <- read.csv("loc_plus_label_28feb17_numeric.csv")
data_just_hyb<-data%>%
  filter(HUC2==14)   #only select rows where HUC2 is 14 (hybrids)
#38 - should have 38 site numbers for downloading environmental data



#Create a list of sites with latitude and longitude
sites <- data.frame(site_no = data_just_hyb$name,
                    lat = data_just_hyb$north,
                    long = data_just_hyb$east)


#Find duplicate site_no values
duplicate_sites <- duplicated(sites$site_no) | duplicated(sites$site_no, fromLast = TRUE)

#Get the indices of the duplicate values
duplicate_indices <- which(duplicate_sites)

#Add "_2" to the site_no of only one duplicate
sites$site_no[duplicate_indices[1]] <- paste0(sites$site_no[duplicate_indices[1]], "_2")



#######Beginning of population/distance/city variables


#load a data set that contains city coordinates. (from https://github.com/kelvins/US-Cities-Database/blob/main/csv/us_cities.csv)
us.cities <- read.csv("us_cities.csv")

#set lat and lon
us_city_coordinates <-us.cities[, c("LATITUDE", "LONGITUDE")]


#Define the Haversine distance function - shortest distance of two points on a sphere
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  #Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  #Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  r <- 6371  # Radius of the Earth in kilometers
  
  #Calculate distance
  distance <- r * c
  
  return(distance)
}

#Calculate distances between each stream coordinate and all city coordinates
distances <- matrix(NA, nrow = nrow(sites), ncol = nrow(us_city_coordinates))
for (i in 1:nrow(sites)) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}



#Find the index of the closest city for each stream coordinate
closest_city_indices <- apply(distances, 1, which.min)

#Get the names and distances of the closest cities
closest_cities <- us.cities[closest_city_indices, "CITY"]
closest_distances <- distances[cbind(1:nrow(distances), closest_city_indices)]
closest_states <- us.cities[closest_city_indices, "STATE_NAME"]

#Create a data frame with city names, distances, and states
result <- data.frame(city = closest_cities, distance = closest_distances, state = closest_states)


#this distance is in km, 


#Retrieve population data for each city 

#from https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-cities-and-towns.html
city_pop_big <- read.csv("sub-est00int.csv")

#create a vector of the desired state names
desired_states <- c("Utah", "Colorado", "Wyoming", "New Mexico")

#subset the dataset to keep only the desired states
city_pop <- subset(city_pop_big, STNAME %in% desired_states)



#Create a new column for population in the result data frame
result$population <- NA

#Convert city_pop to a data.table for efficient partial string matching
city_pop_dt <- as.data.table(city_pop)

#Loop through each row in the result data frame
for (i in 1:nrow(result)) {
  city <- result$city[i]
  state <- result$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the result data frame
  if (nrow(filtered_rows) > 0) {
    result$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}



#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}


#Find the index of the second closest city for each unmatched stream coordinate
second_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  second_closest_index <- sorted_indices[2]  
  return(second_closest_index)
})

#Get the names and distances of the second closest cities for unmatched entries
second_closest_cities_unmatched <- us.cities[second_closest_city_indices_unmatched, "CITY"]
second_closest_distances_unmatched <- distances[cbind(unmatched_indices, second_closest_city_indices_unmatched)]
second_closest_states_unmatched <- us.cities[second_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
second_closest_result_unmatched <- data.frame(city = second_closest_cities_unmatched, distance = second_closest_distances_unmatched, state = second_closest_states_unmatched)

#Update the population data for unmatched entries
second_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(second_closest_result_unmatched)) {
  city <- second_closest_result_unmatched$city[i]
  state <- second_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    second_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1] 
  }
}

#Combine the matched and unmatched result data frames
result[unmatched_indices, ] <- second_closest_result_unmatched



#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the third closest city for each unmatched stream coordinate
third_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  third_closest_index <- sorted_indices[3]  
  return(third_closest_index)
})

#Get the names and distances of the third closest cities for unmatched entries
third_closest_cities_unmatched <- us.cities[third_closest_city_indices_unmatched, "CITY"]
third_closest_distances_unmatched <- distances[cbind(unmatched_indices, third_closest_city_indices_unmatched)]
third_closest_states_unmatched <- us.cities[third_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
third_closest_result_unmatched <- data.frame(city = third_closest_cities_unmatched, distance = third_closest_distances_unmatched, state = third_closest_states_unmatched)

#Update the population data for unmatched entries
third_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(third_closest_result_unmatched)) {
  city <- third_closest_result_unmatched$city[i]
  state <- third_closest_result_unmatched$state[i]
  
  # Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  # Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  # If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    third_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, and newly unmatched result data frames
result[unmatched_indices, ] <- third_closest_result_unmatched



#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the fourth closest city for each unmatched stream coordinate
fourth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  fourth_closest_index <- sorted_indices[4]  
  return(fourth_closest_index)
})

#Get the names and distances of the fourth closest cities for unmatched entries
fourth_closest_cities_unmatched <- us.cities[fourth_closest_city_indices_unmatched, "CITY"]
fourth_closest_distances_unmatched <- distances[cbind(unmatched_indices, fourth_closest_city_indices_unmatched)]
fourth_closest_states_unmatched <- us.cities[fourth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
fourth_closest_result_unmatched <- data.frame(city = fourth_closest_cities_unmatched, distance = fourth_closest_distances_unmatched, state = fourth_closest_states_unmatched)

#Update the population data for unmatched entries
fourth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(fourth_closest_result_unmatched)) {
  city <- fourth_closest_result_unmatched$city[i]
  state <- fourth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    fourth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched, and newly unmatched (fourth closest) result data frames
result[unmatched_indices, ] <- fourth_closest_result_unmatched



#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the fifth closest city for each unmatched stream coordinate
fifth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  fifth_closest_index <- sorted_indices[5]  
  return(fifth_closest_index)
})

#Get the names and distances of the fifth closest cities for unmatched entries
fifth_closest_cities_unmatched <- us.cities[fifth_closest_city_indices_unmatched, "CITY"]
fifth_closest_distances_unmatched <- distances[cbind(unmatched_indices, fifth_closest_city_indices_unmatched)]
fifth_closest_states_unmatched <- us.cities[fifth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
fifth_closest_result_unmatched <- data.frame(city = fifth_closest_cities_unmatched, distance = fifth_closest_distances_unmatched, state = fifth_closest_states_unmatched)

#Update the population data for unmatched entries
fifth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(fifth_closest_result_unmatched)) {
  city <- fifth_closest_result_unmatched$city[i]
  state <- fifth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    fifth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, and fifth closest) result data frames
result[unmatched_indices, ] <- fifth_closest_result_unmatched


#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the sixth closest city for each unmatched stream coordinate
sixth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  sixth_closest_index <- sorted_indices[6]  
  return(sixth_closest_index)
})

#Get the names and distances of the sixth closest cities for unmatched entries
sixth_closest_cities_unmatched <- us.cities[sixth_closest_city_indices_unmatched, "CITY"]
sixth_closest_distances_unmatched <- distances[cbind(unmatched_indices, sixth_closest_city_indices_unmatched)]
sixth_closest_states_unmatched <- us.cities[sixth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
sixth_closest_result_unmatched <- data.frame(city = sixth_closest_cities_unmatched, distance = sixth_closest_distances_unmatched, state = sixth_closest_states_unmatched)

#Update the population data for unmatched entries
sixth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(sixth_closest_result_unmatched)) {
  city <- sixth_closest_result_unmatched$city[i]
  state <- sixth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    sixth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, fifth, and sixth closest) result data frames
result[unmatched_indices, ] <- sixth_closest_result_unmatched

#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the seventh closest city for each unmatched stream coordinate
seventh_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  seventh_closest_index <- sorted_indices[7]  
  return(seventh_closest_index)
})

#Get the names and distances of the seventh closest cities for unmatched entries
seventh_closest_cities_unmatched <- us.cities[seventh_closest_city_indices_unmatched, "CITY"]
seventh_closest_distances_unmatched <- distances[cbind(unmatched_indices, seventh_closest_city_indices_unmatched)]
seventh_closest_states_unmatched <- us.cities[seventh_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
seventh_closest_result_unmatched <- data.frame(city = seventh_closest_cities_unmatched, distance = seventh_closest_distances_unmatched, state = seventh_closest_states_unmatched)

#Update the population data for unmatched entries
seventh_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(seventh_closest_result_unmatched)) {
  city <- seventh_closest_result_unmatched$city[i]
  state <- seventh_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    seventh_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, fifth, sixth, and seventh closest) result data frames
result[unmatched_indices, ] <- seventh_closest_result_unmatched

#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the eighth closest city for each unmatched stream coordinate
eighth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  eighth_closest_index <- sorted_indices[8]  
  return(eighth_closest_index)
})

#Get the names and distances of the eighth closest cities for unmatched entries
eighth_closest_cities_unmatched <- us.cities[eighth_closest_city_indices_unmatched, "CITY"]
eighth_closest_distances_unmatched <- distances[cbind(unmatched_indices, eighth_closest_city_indices_unmatched)]
eighth_closest_states_unmatched <- us.cities[eighth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
eighth_closest_result_unmatched <- data.frame(city = eighth_closest_cities_unmatched, distance = eighth_closest_distances_unmatched, state = eighth_closest_states_unmatched)

#Update the population data for unmatched entries
eighth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(eighth_closest_result_unmatched)) {
  city <- eighth_closest_result_unmatched$city[i]
  state <- eighth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    eighth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, fifth, sixth, seventh, and eighth closest) result data frames
result[unmatched_indices, ] <- eighth_closest_result_unmatched

#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the ninth closest city for each unmatched stream coordinate
ninth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  ninth_closest_index <- sorted_indices[9]  
  return(ninth_closest_index)
})

#Get the names and distances of the ninth closest cities for unmatched entries
ninth_closest_cities_unmatched <- us.cities[ninth_closest_city_indices_unmatched, "CITY"]
ninth_closest_distances_unmatched <- distances[cbind(unmatched_indices, ninth_closest_city_indices_unmatched)]
ninth_closest_states_unmatched <- us.cities[ninth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
ninth_closest_result_unmatched <- data.frame(city = ninth_closest_cities_unmatched, distance = ninth_closest_distances_unmatched, state = ninth_closest_states_unmatched)

#Update the population data for unmatched entries
ninth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(ninth_closest_result_unmatched)) {
  city <- ninth_closest_result_unmatched$city[i]
  state <- ninth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    ninth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, fifth, sixth, seventh, eighth, and ninth closest) result data frames
result[unmatched_indices, ] <- ninth_closest_result_unmatched

#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

#Find the index of the tenth closest city for each unmatched stream coordinate
tenth_closest_city_indices_unmatched <- apply(distances[unmatched_indices, ], 1, function(x) {
  sorted_indices <- order(x)
  tenth_closest_index <- sorted_indices[10]  
  return(tenth_closest_index)
})

#Get the names and distances of the tenth closest cities for unmatched entries
tenth_closest_cities_unmatched <- us.cities[tenth_closest_city_indices_unmatched, "CITY"]
tenth_closest_distances_unmatched <- distances[cbind(unmatched_indices, tenth_closest_city_indices_unmatched)]
tenth_closest_states_unmatched <- us.cities[tenth_closest_city_indices_unmatched, "STATE_NAME"]

#Create a data frame for unmatched entries with city names, distances, and states
tenth_closest_result_unmatched <- data.frame(city = tenth_closest_cities_unmatched, distance = tenth_closest_distances_unmatched, state = tenth_closest_states_unmatched)

#Update the population data for unmatched entries
tenth_closest_result_unmatched$population <- NA

#Loop through each row in the unmatched result data frame
for (i in 1:nrow(tenth_closest_result_unmatched)) {
  city <- tenth_closest_result_unmatched$city[i]
  state <- tenth_closest_result_unmatched$state[i]
  
  #Find the matching rows in city_pop_dt
  match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
  
  #Filter for rows with population over 1000
  filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
  
  #If any matches with population over 1000 are found, assign the population to the unmatched result data frame
  if (nrow(filtered_rows) > 0) {
    tenth_closest_result_unmatched$population[i] <- filtered_rows$POPESTIMATE2003[1]  
  }
}

#Combine the matched, previously unmatched, newly unmatched (second, third, fourth, fifth, sixth, seventh, eighth, ninth, and tenth closest) result data frames
result[unmatched_indices, ] <- tenth_closest_result_unmatched


#Create a vector to store the indices of the unmatched entries
unmatched_indices <- which(is.na(result$population))

#Calculate distances between unmatched stream coordinates and all city coordinates
for (i in unmatched_indices) {
  for (j in 1:nrow(us_city_coordinates)) {
    distances[i, j] <- haversine_distance(sites$lat[i], sites$long[i], 
                                          us_city_coordinates[j, "LATITUDE"], us_city_coordinates[j, "LONGITUDE"])
  }
}

# Find the index of the twelfth closest city for the single unmatched stream coordinate (index 9)
twelfth_closest_city_index_unmatched <- order(distances[9, ])[12]

# Make sure the twelfth_closest_city_index_unmatched is a valid index
if (twelfth_closest_city_index_unmatched > ncol(distances) || twelfth_closest_city_index_unmatched < 1) {
  stop("Invalid index for twelfth_closest_city_index_unmatched.")
}

# Get the name and distance of the twelfth closest city for the single unmatched stream coordinate
twelfth_closest_city_unmatched <- us.cities[twelfth_closest_city_index_unmatched, "CITY"]
twelfth_closest_distance_unmatched <- distances[9, twelfth_closest_city_index_unmatched]
twelfth_closest_state_unmatched <- us.cities[twelfth_closest_city_index_unmatched, "STATE_NAME"]

# Create a data frame for the single unmatched entry with city name, distance, and state
twelfth_closest_result_unmatched <- data.frame(
  city = twelfth_closest_city_unmatched,
  distance = twelfth_closest_distance_unmatched,
  state = twelfth_closest_state_unmatched
)

# Update the population data for the single unmatched entry
twelfth_closest_result_unmatched$population <- NA

# Find and assign the population to the unmatched result data frame
city <- twelfth_closest_result_unmatched$city
state <- twelfth_closest_result_unmatched$state
match_rows <- city_pop_dt[NAME %like% city & STNAME %like% state]
filtered_rows <- match_rows[POPESTIMATE2003 > 1000]
if (nrow(filtered_rows) > 0) {
  twelfth_closest_result_unmatched$population <- filtered_rows$POPESTIMATE2003[1]  
}

# Combine the matched and unmatched result data frames (assuming you already have "result" data frame)
result[unmatched_indices, ] <- twelfth_closest_result_unmatched


####END OF CITY/POPULATION/DISTANCE CODE


##Road Data variable:


#load shapefile of all primary and secondary roads from (https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2010&layergroup=Roads) - 2010 - first year available with road data from TIGER/Line
roads_shape <- st_read("tl_2010_08_prisecroads/tl_2010_08_prisecroads.shp")


#Convert sites data frame to an sf object
sites_sf_roads <- st_as_sf(sites, coords = c("long", "lat"), crs = st_crs(roads_shape))


#Buffer the sites by ten miles (the coordinates are in decimal degrees)
buffer_distance <- 10 * 1609.34  # Convert miles to meters
sites_buffered <- st_buffer(sites_sf_roads, buffer_distance)

#Spatially join the roads_shape with the buffered sites
sites_with_roads <- st_join(sites_buffered, roads_shape)

#Count the roads within each buffer for each set of coordinates
site_roads_count <- sites_with_roads %>%
  group_by(site_no) %>%
  summarise(
    road_count = n_distinct(LINEARID)
  )


#from https://maps.princeton.edu/catalog/harvard-glb-oilgas

#oil/gas data
oil_shapefile <- st_read("harvard-glb-oilgas-shapefile/GLB_OILGAS.shp")


#Convert sites data frame to an sf object
sites_sf <- st_as_sf(sites, coords = c("long", "lat"), crs = st_crs(oil_shapefile))

#Buffer the sites by ten miles (the coordinates are in decimal degrees)
buffer_distance <- 10 * 1609.34  # Convert miles to meters
sites_buffered <- st_buffer(sites_sf, buffer_distance)

#Spatially join the oil_shapefile with the buffered sites
sites_with_oil_gas <- st_join(sites_buffered, oil_shapefile)

#Count the oil/gas sites within each category for each set of coordinates (**there was a condensate section but there were no condensate sites so I removed that variable - same with oil OR gas)
site_counts <- sites_with_oil_gas %>%
  group_by(site_no) %>%
  summarise(
    oil_count = sum(COMMODITY %in% c("Oil")),
    gas_count = sum(COMMODITY %in% c("Gas")),
    oil_and_gas_count = sum(COMMODITY %in% c("Oil and Gas","Oil and gas","Gas and oil")),
    total_oil_gas_count = sum(!is.na(COMMODITY))
  )

#Merge the counts with the sites data frame
sites_with_counts <- merge(sites, site_counts, by = "site_no", all.x = TRUE)




#Download the NorWeST environmental data - lines and points - shapefile format


#https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/ModeledStreamTemperatureScenarioMaps.shtml
#variable descriptors: https://www.fs.usda.gov/rm/boise/AWAE/projects/NorWeST/downloads/NorWeST_StreamTemperatureModelDescription.pdf

points_col <- st_read("NorWeST_PredictedStreamTempPoints_Colorado/NorWeST_PredictedStreamTempPoints_Colorado.shp")

points_utah <- st_read("NorWeST_PredictedStreamTempPoints_Utah_Aug/NorWeST_PredictedStreamTempPoints_Utah_Aug.shp")

points_upp_gr <- st_read("NorWeST_PredictedStreamTempPoints_UpperGreenNorthPlatte_Aug/NorWeST_PredictedStreamTempPoints_UpperGreenNorthPlatte_Aug.shp")


#Combine all points datasets into a single dataset
all_points <- bind_rows(points_col, points_utah, points_upp_gr)

#use "sites" from above for lat and lon of sampling data

#Convert sites dataframe to a spatial dataframe with the given CRS (decimal degrees)
sites_sf <- st_as_sf(sites, coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

#Transform the coordinates of sites_sf to the projected CRS of all_points
sites_sf_transformed <- st_transform(sites_sf, crs = st_crs(all_points))

#Create an empty matrix to store the nearest point indices
nearest_indices <- matrix(data = NA, ncol = 1, nrow = nrow(sites_sf_transformed))

#Find the nearest point in all_points for each site
for (i in 1:nrow(sites_sf_transformed)) {
  # Calculate the distances between each point in all_points and the current site
  distances <- st_distance(all_points, sites_sf_transformed[i, ])
  
  #Find the index of the closest point
  nearest_indices[i, ] <- which.min(distances)
}

#Extract the nearest points based on the indices
nearest_points <- all_points[unlist(nearest_indices), ]

#Combine the attributes with the sites_sf_transformed dataframe
results <- cbind(sites_sf_transformed, nearest_points)

#Subset the desired columns
temps.subset <- results[, c("site_no", "GNIS_NAME", "ELEV", "CANOPY", "SLOPE", "PRECIP", "CUMDRAINAG", "S1_93_11")]

#Get the coordinates of the nearest points
temps.coords <- st_coordinates(nearest_points)

#Add a column for the geometry of points_col
temps.subset$points_col_geometry <- st_geometry(nearest_points)



#Put all of the data together into one data frame
environmental_data <- data.frame(
  river = data_just_hyb$locations.loc,
  site_no = sites$site_no,
  lat = sites$lat,
  lon = sites$lon,
  closest_city = result$city,
  distance_to_city = result$distance,
  population = result$population,
  road_count = site_roads_count$road_count,
  oil_count = sites_with_counts$oil_count,
  gas_count = sites_with_counts$gas_count,
  oil_and_gas_count = sites_with_counts$oil_and_gas_count,
  total_oil_gas_count = sites_with_counts$total_oil_gas_count,
  GNIS_NAME = temps.subset$GNIS_NAME,
  ELEV = temps.subset$ELEV,
  CANOPY = temps.subset$CANOPY,
  SLOPE = temps.subset$SLOPE,
  PRECIP = temps.subset$PRECIP,
  CUMDRAINAG = temps.subset$CUMDRAINAG,
  S1_93_11 = temps.subset$S1_93_11
)



#Save the data frame as a CSV file
write.csv(environmental_data, "C:\\Users\\saman\\Downloads\\env_data.csv", row.names = FALSE)

