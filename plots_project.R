#To be run in R, a script to visualize the data and create some initial plots

#load in genetics data and use environmental data from "download_env_data.R" script
genetics <- read.csv("genetics_out_21July.csv")

#filter out "fw"
filtered_genetics <- subset(genetics, genID == "fw")

#filter the genetic_data DataFrame for rows satisfying the given conditions
filtered_data <- filtered_genetics[filtered_genetics$fms_ci_UB < 1 & filtered_genetics$fms_ci_LB > 0, ]

#select only the "river" and "fms" columns from the filtered data
filtered_data <- subset(filtered_data, select = c("river", "fms"))


#merge the subset_filtered_genetics and environmental_data using an inner join on the "river" column
merged_data <- merge(filtered_data, environmental_data, by = "river", all.x = FALSE, all.y = FALSE)

par(mar = c(5, 5, 4, 4))
plot(merged_data$fms,xlab = "Individuals", ylab = "Flannelmouth sucker distribution")

#remove the categorical columns from the merged_data data frame
merged_data <- merged_data[, !(names(merged_data) %in% c("river", "site_no", "closest_city", "GNIS_NAME"))]

#create a scatterplot matrix
par(mfrow = c(3, 5))  # 3 rows and 5 columns
par(mar = c(4, 4, 1, 1))  # Adjust margins

#iterate through each variable and create scatterplots against fms
for (col in names(merged_data)) {
  if (col != "fms") {
    plot(merged_data[[col]], merged_data$fms,  # Swap the x and y variables
         xlab = col,
         ylab = "fms")
    par(mar = c(4, 4, 1, 1))  # Reset the margin after plotting
    par(cex.lab = 1.5)
  }
}

#reset the layout to default
par(mfrow = c(1, 1))

name<-lm(height~age*sex)

correlation_matrix1 <- cor(merged_data)


#reset the default margins
par(oma=c(2,2,2,2))# Increase the right margin to create space for the legend
par(mar = c(5, 4, 4, 2) + 0.1)

#calculate the range of correlation coefficients
min_corr <- min(correlation_matrix1, na.rm = TRUE)
max_corr <- max(correlation_matrix1, na.rm = TRUE)

#define your own numeric thresholds for low, medium, and high
low_threshold <- 0.2  # For example, consider correlations below 0.2 as low
high_threshold <- 0.6  # For example, consider correlations above 0.6 as high

#create legend labels with numeric values
legend_labels <- c(paste("Low (", min_corr, " -", low_threshold, ")"),
                   paste("Medium (", low_threshold, " -", high_threshold, ")"),
                   paste("High (", high_threshold, " -", max_corr, ")"))

#plot the heatmap 
heatmap(correlation_matrix1,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        col = colorRampPalette(c("blue", "white", "red"))(100),
        margins = c(10,10),
        cexCol = 1.3,
        cexRow = 1.3,
        main = "Correlation Heatmap",
        cex.main = 3.0
        
)



#add the color legend manually
legend("bottomright",
       legend = legend_labels,
       fill = colorRampPalette(c("blue", "white", "red"))(100)[c(1, 50, 100)],
       bty = "n",
       title = "Correlation",
       cex = 1.2,
       xpd = TRUE,
       inset = c(-0.7, -0.4)
)



#violin plot

#define custom colors for the violin plot
custom_colors <- c("blue", "green", "red", "orange", "purple", "yellow", "cyan", "magenta", "gray", "brown")

#create the violin plot with custom colors
ggplot(merged_data, aes(x = as.factor(cut(ELEV, breaks = 10)), y = fms, fill = as.factor(cut(ELEV, breaks = 10)))) +
  geom_violin() +
  labs(x = "Elevation Groups", y = "fms", title = "fms across Elevation Groups") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors)



#get the unique river names in the merged_data DataFrame
unique_rivers <- unique(merged_data$river)

#set up a multi-panel layout for the plots (6 columns per row)
num_rows <- ceiling(length(unique_rivers) / 6)
par(mfrow = c(num_rows, 6))

#loop through each unique river and create a histogram for its "fms" values
for (i in 1:length(unique_rivers)) {
  river_name <- unique_rivers[i]
  
  #filter the merged_data DataFrame to include only the data for the current river
  specific_river_data <- merged_data[merged_data$river == river_name, ]
  
  #create a histogram of the "fms" values for the current river
  hist(specific_river_data$fms, main = paste("Histogram of fms for", river_name), xlab = "fms")
}

#reset the plotting layout to default
par(mfrow = c(1, 1))



#correlation plot

#calculate another correlation matrix
correlation_matrix <- cor(environmental_data[, c("lat", "lon", "distance_to_city", "population", "road_count", "oil_count", "gas_count", "oil_and_gas_count", "total_oil_gas_count", "ELEV", "CANOPY", "SLOPE", "PRECIP", "CUMDRAINAG", "S1_93_11")])


#define the panel.cor function
panel.cor <- function(x, y, digits = 2, color = "black", ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor.test(x, y)
  txt <- paste("corr:", round(r$estimate, digits = digits), "\n", "p-value:", round(r$p.value, digits = 4))
  if (r$p.value < 0.05) color <- "red"
  text(0.5, 0.5, txt, col = color)
}

# Increase the outer margin and margin parameters
par(oma = c(2, 2, 2, 2))  # Adjust outer margin
par(mar = c(1, 1, 1, 1))  # Adjust margin

#create scatterplot matrix and save as PDF
pdf("pairs_environmental_data.pdf", width = 17, height = 17)
pairs(environmental_data[, c("lat", "lon", "distance_to_city", "population", "road_count", "oil_count", "gas_count", "oil_and_gas_count", "total_oil_gas_count", "ELEV", "CANOPY", "SLOPE", "PRECIP", "CUMDRAINAG", "S1_93_11")],
      labels = c("Latitude", "Longitude", "Distance to City", "Population", "Road Count", "Oil Count", "Gas Count", "Oil and Gas Count", "Total Oil and Gas Count", "Elevation", "Canopy", "Slope", "Precipitation", "Cumulative Drainage", "S1_93_11"),
      upper.panel = panel.cor)
dev.off()


