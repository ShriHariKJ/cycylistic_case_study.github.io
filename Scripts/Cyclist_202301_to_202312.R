# Installing the packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("reshape2")


# Loading the packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(data.table)
library(reshape2)
library(scales)

# Loading the dataset in dataframes and Combining the indivdual dataframes into single dataframe
data_files <- list.files(pattern = "*.csv")
cycle_trips <- lapply(data_files, read_csv) %>% bind_rows()

# To view data and colnames
View(cycle_trips)
str(cycle_trips)
head(cycle_trips)
summary(cycle_trips)
colnames(cycle_trips)

# Columns renaming and data preparation
cycle_trips <- cycle_trips %>%
  rename(start_time = started_at, end_time = ended_at) %>%
  mutate(
    date = as.Date(start_time),
    month = format(date, '%m'),
    day = format(date, '%d'),
    year = format(date, '%Y'),
    day_of_week = format(date, '%A'),
    ride_length = as.numeric(difftime(end_time, start_time, units = "mins"))
  )

# Calculating No of rows
count_rows <- nrow(cycle_trips)

# Print the no of rows
print(count_rows)

# Calculating No of rows with NA values
na_count_rows <- sum(rowSums(is.na(cycle_trips)) > 0)

# Print the no of rows with NA values
print(na_count_rows)

# No of rows with NA values: 1388170

# Calculating No of columns with NA values
na_count_cols <- sum(colSums(is.na(cycle_trips)) > 0)

# Print the no of columns with NA values
print(na_count_cols)

# No of columns with NA values: 6

# To print the first data with NA values 
first_row_with_na <- cycle_trips[which(rowSums(is.na(cycle_trips)) > 0)[1], ]

# Print the first row with NA values
print(first_row_with_na)

# Calculating percent of rows with missing values
total_rows <- nrow(cycle_trips)
rows_with_na <- sum(rowSums(is.na(cycle_trips)) > 0)
percentage_missing <- (rows_with_na / total_rows) * 100

# Percentage of rows with missing values: 24.27 %

# Print the result
cat("Percentage of rows with missing values:", round(percentage_missing, 2), "%\n")

# Dropping rows with NA values
cleaned_df <- cycle_trips[complete.cases(cycle_trips), ]

# Calculating No of rows with NA values after cleaning
na_count_rows <- sum(rowSums(is.na(cleaned_df)) > 0)

# Print the no of rows with NA values after cleaning
print(na_count_rows)


# Calculating duplicate rows
dt <- as.data.table(cleaned_df)
duplicate_rows <- cleaned_df[duplicated(dt), ]

# Get the number of duplicate rows
duplicate_row_count <- nrow(duplicate_rows)

# Print the total number of duplicate rows
cat("Total number of duplicate rows:", duplicate_row_count, "\n")

# The rideable_type "docked_bike" designates bikes that were taken out of circulation by Cyclistic to assess for quality control.
# In cleaned_df Filter rideable_type not in docked_bike is added to remove those records.
cleaned_df_new <- cleaned_df %>%
  filter(rideable_type != "docked_bike")

# Count rows in original dataframe
n_rows_cleaned_df <- nrow(cleaned_df)

# Count rows in new dataframe
n_rows_cleaned_df_new <- nrow(cleaned_df_new)

# Calculate percentage difference
percentage_diff <- ((n_rows_cleaned_df - n_rows_cleaned_df_new) / n_rows_cleaned_df) * 100

# Print results
print(paste("Number of rows in cleaned_df: ", n_rows_cleaned_df))
print(paste("Number of rows in cleaned_df_new: ", n_rows_cleaned_df_new))
print(paste("Percentage difference in row count: ", round(percentage_diff, 2), "%"))

# Percentage difference in row count:  1.76 %

# Assigning the filter to cleaned_df 
cleaned_df <- cleaned_df %>%
  filter(rideable_type != "docked_bike")

# To view data and colnames for cleaned_df
View(cleaned_df)
str(cleaned_df)
head(cleaned_df)
summary(cleaned_df)
colnames(cleaned_df)

# Negative ride length values 
negative_ride_length_df <- cleaned_df[cleaned_df$ride_length < 0, ]

# Negative row count
negative_row_count <- nrow(negative_ride_length_df)

# Print the total number of rows with negative ride lengths
cat("Total number of rows with negative ride lengths:", negative_row_count, "\n")

# Total number of rows with negative ride lengths: 66 

# Data with only positive ride length is extracted
cleaned_df <- cleaned_df[cleaned_df$ride_length > 0, ]

# Average, min and max of ride_length
summary(cleaned_df)

# Average, min and max of ride_length by member_casual 
aggregate(cleaned_df$ride_length ~ cleaned_df$member_casual, FUN=mean)
aggregate(cleaned_df$ride_length ~ cleaned_df$member_casual, FUN=median)
aggregate(cleaned_df$ride_length ~ cleaned_df$member_casual, FUN=max)
aggregate(cleaned_df$ride_length ~ cleaned_df$member_casual, FUN=min)
aggregate(cleaned_df$ride_length ~ cleaned_df$member_casual + cleaned_df$day_of_week, FUN=mean)

# Total number of riders per category
riders_per_category <- cleaned_df %>%
  group_by(member_casual) %>%
  summarise(total_riders = n())

# Print the riders_per_category
head(riders_per_category)


# Pie chart to show riders per Membership Type
riders_per_membership_plot <- ggplot(data = riders_per_category, aes(x = "", y = total_riders, fill = member_casual)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Total Cycle Hires per Membership Type (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    caption = "Note: Total riders are calculated in number of hires", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  geom_text(aes(label = paste0(str_to_title(member_casual), "\n", round(total_riders / sum(total_riders) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#1E1B8B", "#19B12D"), name = str_to_title("Membership Type")) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

# Total number of riders by month
riders_by_month <- cleaned_df %>%
  group_by(month) %>%
  summarise(total_riders = n())

# Casting the month from month.number to month.name format 
riders_by_month$month_name <- factor(month.abb[as.integer(riders_by_month$month)], levels = month.abb)

# Print the riders_by_month
head(riders_by_month)
print(riders_by_month, max=nrow(riders_by_month))

# Manual assigning of color for each month
month_colors <- c("Jan" = "red", "Feb" = "blue", "Mar" = "green",
                  "Apr" = "orange", "May" = "purple", "Jun" = "brown",
                  "Jul" = "pink", "Aug" = "gray", "Sep" = "cyan",
                  "Oct" = "yellow", "Nov" = "magenta", "Dec" = "gold")

# Bar chart to show riders per month 
riders_by_month_plot <- ggplot(riders_by_month, aes(x = month_name, y = total_riders, fill = month_name)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = month_colors) +
  labs(
    title = "Total Riders per Month (2023)",
    subtitle = "Data collected from Divvy source",
    x = "Month",
    y = "Total Riders",
    caption = "Note: Total riders are calculated in number of hires",
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )



# Total number of riders per each category and month
riders_per_category_month <- cleaned_df %>%
  group_by(member_casual, month) %>%
  summarise(total_riders = n(), .groups = "drop")

# Casting the month from month.number to month.name format 
riders_per_category_month$month_name <- factor(month.abb[as.integer(riders_per_category_month$month)], levels = month.abb)

# Print the riders_per_category_month
head(riders_per_category_month)
print(riders_per_category_month,n=nrow(riders_per_category_month))

# Bar chart to show riders per month by Membership Type
riders_by_month_per_category_plot <- ggplot(riders_per_category_month, aes(x = month, y = total_riders, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Total Riders per Month by Membership Type (2023)",
    subtitle = "Data collected from Divvy source",
    x = "Month",
    y = "Total Riders",
    caption = "Note: Total riders are calculated in number of hires",
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#a83232", "#6cf70f"), name = "Membership Type") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )


# Total number of riders by day_of_week
riders_per_day <- cleaned_df %>%
  group_by(day_of_week) %>%
  summarise(total_riders = n())

# Print the riders_per_category_day
head(riders_per_day)
print(riders_per_day,n=nrow(riders_per_day))

# Manual assigning of color for each day_of_week
day_colors <- c("Sunday" = "red", "Monday" = "blue", "Tuesday" = "green",
                "Wednesday" = "orange", "Thursday" = "purple", "Friday" = "brown", "Saturday" = "pink")

# Reorder the levels of day_of_week to match the order in your data
riders_per_day$day_of_week <- factor(riders_per_day$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Bar chart to show riders per day_of_week
riders_per_day_plot <- ggplot(riders_per_day, aes(x = day_of_week, y = total_riders, fill = day_of_week)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = day_colors) +
  labs(
    title = "Total Riders per Day of Week (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Day of Week",
    y = "Total Riders",
    caption = "Note: Total riders are calculated in number of hires", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12), # Increase the size of axis labels for better readability
    panel.grid.major = element_line(linetype = "dashed", color = "grey") # Add grid lines for better understanding of the plot
  )+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )


# Total number of riders per each category and day_of_week
riders_per_category_day <- cleaned_df %>%
  group_by(member_casual,day_of_week) %>%
  summarise(total_riders = n(), .groups = "drop")

# Print the riders_per_category_day
head(riders_per_category_day)

# Bar chart to show riders per day_of_week by Membership Type 
riders_per_category_day_plot <- ggplot(riders_per_category_day, aes(x = day_of_week, y = total_riders, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Total Riders per Day of Week by Membership Type (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Day of Week",
    y = "Total Riders",
    caption = "Note: Total riders are calculated in number of hires", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#a83232", "#6cf70f"), name = "Membership Type") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )


# Average ride_length per Membership type
average_ride_length <- aggregate(ride_length ~ member_casual, data = cleaned_df, FUN = mean)

# Total_percentage of average_ride_length
total_percentage <- sum(average_ride_length$ride_length)

# Pie chart to show average_ride_length per Membership type 
pie_chart_avg  <- ggplot(average_ride_length, aes(x = "", y = ride_length, fill = member_casual)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Average Ride Length per Membership Type (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    caption = "Note: Average ride length is calculated in minutes", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#1E1B8B", "#19B12D"), name = str_to_title("Membership Type")) +
  geom_text(aes(label = paste0(str_to_title(member_casual), "\n", round(ride_length / total_percentage * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

# Pie chart to show riders per Membership type 
pie_chart_total  <- ggplot(data = riders_per_category, aes(x = "", y = total_riders, fill = member_casual)) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Total Cycle Hires per Membership Type (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    caption = "Note: Total riders are calculated in number of hires", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  geom_text(aes(label = paste0(str_to_title(member_casual), "\n", round(total_riders / sum(total_riders) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#1E1B8B", "#19B12D"), name = str_to_title("Membership Type")) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )


# Displaying Average Ride Length per Membership Type vs Total Cycle Hires per Membership Type
gridExtra::grid.arrange(pie_chart_total, pie_chart_avg, ncol = 2)

# Calculate average ride length per membership type by day of the week
average_ride_length_by_day <- aggregate(ride_length ~ member_casual + day_of_week, data = cleaned_df, FUN = mean)

# Total percentage of average ride length
total_percentage_day <- sum(average_ride_length_by_day$ride_length)

# Bar chart to show average_ride_length by day per membership type
average_ride_length_by_day_per_membership_plot <- ggplot(average_ride_length_by_day, aes(x = day_of_week, y = ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Average Ride Length per Membership Type by Day of the Week (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Day of Week",
    y = "Ride length in minutes",
    caption = "Note: Ride length is calculated in minutes", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "top", 
    legend.title = element_blank(),
    axis.text = element_text(size = 12), # Increase the size of axis labels for better readability
    panel.grid.major = element_line(linetype = "dashed", color = "grey") # Add grid lines for better understanding of the plot
  ) +
  scale_fill_manual(values = c("#702810", "#0e2ee6")) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

# Calculate average ride length per membership type by Month
average_ride_length_by_month <- aggregate(ride_length ~ member_casual + month, data = cleaned_df, FUN = mean)

# Total percentage of average ride length
total_percentage_month <- sum(average_ride_length_by_month$ride_length)

# Bar chart to show average_ride_length per Membership type by month
average_ride_length_by_month_per_membership_plot <- ggplot(average_ride_length_by_month, aes(x = month, y = ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Average Ride Length per Membership Type by Month (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Month",
    y = "Ride length in minutes",
    caption = "Note: Ride length is calculated in minutes", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "top", 
    legend.title = element_blank(),
    axis.text = element_text(size = 12), # Increase the size of axis labels for better readability
    panel.grid.major = element_line(linetype = "dashed", color = "grey") # Add grid lines for better understanding of the plot
  ) +
  scale_fill_manual(values = c("#702810", "#0e2ee6")) +
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

# Total number of riders per Rideable Type and Membership Type
riders_per_category_ridetype <- cleaned_df %>%
  group_by(member_casual,rideable_type) %>%
  summarise(total_riders = n(), .groups = "drop")

# Print the riders_per_category_ridetype
head(riders_per_category_ridetype)

# For analysis of Top 10 Routes per Membership type
# Add a new column station_name
cleaned_df_station <- cleaned_df %>%
  mutate(station_name = paste(start_station_name, end_station_name, sep = " - "))

# Filter out rides where ride_length is less than 5 minutes if start and end stations are the same
cleaned_df_station <- cleaned_df_station %>%
  filter(!(start_station_name == end_station_name & ride_length < 5))

station_riders <- cleaned_df_station %>%
  group_by(member_casual, station_name) %>%
  summarise(total_riders = n(), .groups = "drop")

# Get top 10 stations for each member_casual
top_10_stations_member <- station_riders %>%
  filter(member_casual == "member") %>%
  arrange(desc(total_riders)) %>%
  head(10)

top_10_stations_casual <- station_riders %>%
  filter(member_casual == "casual") %>%
  arrange(desc(total_riders)) %>%
  head(10)

# Bind rows of top_10_stations_member and top_10_stations_casual
top_10_stations <- bind_rows(top_10_stations_member, top_10_stations_casual)

# Order by member_casual and total_riders
top_10_stations_ordered <- top_10_stations %>%
  arrange(member_casual, desc(total_riders))

# Create dataframe for riders across days and month
df_heatmap <- cleaned_df %>%
  group_by(day_of_week, month) %>%
  summarise(total_riders = n(), .groups = "drop")
head(df_heatmap)

df_heatmap$month <- factor(df_heatmap$month, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Display the heatmap for Total Number of Riders Across Days of the Week and Months of the Year
totalriders_across_days_months <- ggplot(df_heatmap, aes(x = month, y = day_of_week, fill = total_riders)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", labels = comma) +
  theme_minimal() +
  labs(title = "Total Number of Riders Across Days of the Week and Months of the Year",
       subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
       x = "Month",
       y = "Day of the Week",
       fill = "Total Riders",    
       caption = "Note: Total riders is calculated in number of rides", # Add a caption for more context or explanation
       fontface = "bold") + 
    theme(
         legend.position = "right", 
         legend.title = element_blank(),
         axis.text = element_text(size = 12), # Increase the size of axis labels for better readability
         panel.grid.major = element_line(linetype = "dashed", color = "grey") # Add grid lines for better understanding of the plot
       ) +
    theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )


# Create a bar plot ordered by Membership Type and total_riders
Top10_routes_for_membership_plot <- ggplot(top_10_stations_ordered, aes(x = reorder(station_name, -total_riders), y = total_riders, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 Routes for Members and Casual Riders (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Station",
    y = "Total Riders",
    caption = "Note: Total riders is calculated in number of rides", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#702810", "#0e2ee6")) +
  theme(
    legend.position = "top", 
    legend.title = element_blank(),
    axis.text = element_text(size = 12), # Increase the size of axis labels for better readability
    panel.grid.major = element_line(linetype = "dashed", color = "grey") # Add grid lines for better understanding of the plot
  ) +
  facet_grid(. ~ member_casual, scales = "free", space = "free")+
  facet_wrap(~ factor(member_casual, levels = c("casual", "member"), labels = c("Casual", "Member")))+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )


# Popular start stations by membership type
popular_start_stations_member <- cleaned_df %>%
  group_by(member_casual, start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(member_casual, desc(total_rides)) %>%
  group_by(member_casual) %>%
  slice_head(n = 10)

# Popular end stations by membership type
popular_end_stations_member <- cleaned_df %>%
  group_by(member_casual, end_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(member_casual, desc(total_rides)) %>%
  group_by(member_casual) %>%
  slice_head(n = 10)

# Plot for Top 10 start stations
print_popular_start_stations_member <- ggplot(popular_start_stations_member, aes(x = reorder(start_station_name, -total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Top 10 Popular Start Stations (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Station",
    y = "Total Rides",
    caption = "Note: Total riders is calculated in number of rides", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major = element_line(linetype = "dashed", color = "grey")) +
  facet_wrap(~ factor(member_casual, levels = c("casual", "member"), labels = c("Casual", "Member")), scales = "free")+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

# Plot for Top 10 end stations
print_end_stations_member <- ggplot(popular_end_stations_member, aes(x = reorder(end_station_name, -total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(
    title = "Top 10 Popular End Stations (2023)",
    subtitle = "Data collected from Divvy source", # Add a subtitle for additional information
    x = "Station",
    y = "Total Rides",
    caption = "Note: Total riders is calculated in number of rides", # Add a caption for more context or explanation
    fontface = "bold"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major = element_line(linetype = "dashed", color = "grey")) +
  facet_wrap(~ factor(member_casual, levels = c("casual", "member"), labels = c("Casual", "Member")), scales = "free")+
  theme(
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 12) # Increase the size of legend labels for better readability
  )

gridExtra::grid.arrange(print_popular_start_stations_member, print_end_stations_member, ncol = 2)
ggsave(filename="Top 10 Start Stations vs Top 10 End Stations for each membership type.jpg",plot =gridExtra::grid.arrange(print_popular_start_stations_member, print_end_stations_member, ncol = 2)
,width = 10, height = 6)


# Save the pie chart for total riders per membership type
ggsave(filename = "Total_Cycle_Hires_per_Membership_Type.jpg", plot = pie_chart_total, width = 12, height = 6)

# Save the pie chart for average ride length per membership type
ggsave(filename = "Average_Ride_Length_per_Membership_Type.jpg", plot = pie_chart_avg, width = 12, height = 6)

# Save the bar chart for total riders per month
ggsave(filename = "Total_Riders_per_Month.jpg", plot = riders_by_month_plot, width = 12, height = 6)

# Save the bar chart for total riders per day of week
ggsave(filename = "Total_Riders_per_Day_of_Week.jpg", plot = riders_per_day_plot, width = 12, height = 6)

# Save the bar chart for total riders by month per membership type
ggsave(filename = "Total_Riders_by_Month_per_Membership_Type.jpg", plot = riders_by_month_per_category_plot, width = 12, height = 6)

# Save the bar chart for total riders by day of week per membership type
ggsave(filename = "Total_Riders_by_Day_of_Week_per_Membership_Type.jpg", plot = riders_per_category_day_plot, width = 12, height = 6)

# Save the bar chart for average ride length per membership type by day of week
ggsave(filename = "Average_Ride_Length_per_Membership_Type_by_Day_of_Week.jpg", plot = average_ride_length_by_day_per_membership_plot, width = 12, height = 6)

# Save the bar chart for average ride length per membership type by month
ggsave(filename = "Average_Ride_Length_per_Membership_Type_by_Month.jpg", plot = average_ride_length_by_month_per_membership_plot, width = 12, height = 6)

# Save the bar chart for total riders per rideable type and membership type
ggsave(filename = "Total_Riders_per_Rideable_Type_and_Membership_Type.jpg", plot = Top10_routes_for_membership_plot, width = 12, height = 6)

# Save the bar chart for top 10 routes for members and casual riders
ggsave(filename = "Top_10_Routes_for_Members_and_Casual_Riders.jpg", plot = Top10_routes_for_membership_plot, width = 12, height = 6)

# Save the bar chart for top 10 start stations by membership type
ggsave(filename = "Top_10_Popular_Start_Stations_by_Membership_Type.jpg", plot = print_popular_start_stations_member, width = 12, height = 6)

# Save the bar chart for top 10 end stations by membership type
ggsave(filename = "Top_10_Popular_End_Stations_by_Membership_Type.jpg", plot = print_end_stations_member, width = 12, height = 6)

# Save the combined plot for average ride length per membership type vs total cycle hires per membership type
ggsave(filename = "Average_Ride_Length_vs_Total_Cycle_Hires_per_Membership_Type.jpg", plot = gridExtra::arrangeGrob(pie_chart_total, pie_chart_avg, ncol = 2), width = 14, height = 8)

# Save the bar chart for top 10 end stations by membership type
ggsave(filename = "Total_Number_of_Riders_Across_Days_of_the_Week_and_Months_of_the_Year.jpg", plot = totalriders_across_days_months, width = 12, height = 6)

