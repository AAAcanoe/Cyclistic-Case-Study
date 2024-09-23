
# Prepare Data for Analysis
# Import data about Cyclistic for the past 12 months (Aug 2023 ~ Jul 2024)
jul2024 <- read.csv("Desktop/Case Study Data/202407-divvy-tripdata.csv")
june2024 <- read.csv("Desktop/Case Study Data/202406-divvy-tripdata.csv")
may2024 <- read.csv("Desktop/Case Study Data/202405-divvy-tripdata.csv")
apr2024 <- read.csv("Desktop/Case Study Data/202404-divvy-tripdata.csv")
mar2024 <- read.csv("Desktop/Case Study Data/202403-divvy-tripdata.csv")
feb2024 <- read.csv("Desktop/Case Study Data/202402-divvy-tripdata.csv")
jan2024 <- read.csv("Desktop/Case Study Data/202401-divvy-tripdata.csv")
dec2023 <- read.csv("Desktop/Case Study Data/202312-divvy-tripdata.csv")
nov2023 <- read.csv("Desktop/Case Study Data/202311-divvy-tripdata.csv")
oct2023 <- read.csv("Desktop/Case Study Data/202310-divvy-tripdata.csv")
sep2023 <- read.csv("Desktop/Case Study Data/202309-divvy-tripdata.csv")
aug2023 <- read.csv("Desktop/Case Study Data/202308-divvy-tripdata.csv")

# Check the dimensions of each data set: all contains the same amount of columns
dim(jul2024)
dim(june2024)
dim(may2024)
dim(apr2024)
dim(mar2024)
dim(feb2024)
dim(jan2024)
dim(dec2023)
dim(nov2023)
dim(oct2023)
dim(sep2023)
dim(aug2023)

# Convert the 12 datasets into 1 huge dataframe
bikes_raw <- rbind(jul2024, june2024, may2024, apr2024, mar2024, feb2024, jan2024,
               dec2023, nov2023, oct2023, sep2023, aug2023)

################################################################################################ 

# Data Prepared!

# Now onto process data

# Remove rows contain NA values
bikes_no_row_NA <- drop_na(bikes_raw)

# Remove Duplicate Rows
bikes_distinct = distinct(bikes_no_row_NA)

# Keep Important Columns
# Start and end latitudes and longitudes are not important in this analysis
# Start and end stations are also not important in this analysis
bikes_update <- select(bikes_distinct, -c(start_station_name, start_station_id,
                                          end_station_name, end_station_id,
                                          start_lat, start_lng, end_lat, end_lng,
                                          ride_id))

################################################################################################ 

# Separate Casual and Annual Member Riders
casual <- bikes_update %>% filter(member_casual=="casual")
member <- bikes_update %>% filter(member_casual=="member")

people <- data.frame(name=c("casual", "member"), amount=c(nrow(casual), nrow(member)))
ggplot(people, aes(x=name, y=amount, fill=name)) + geom_bar(stat = "identity")


# Check Ride Types by Casual Riders
types <- casual$rideable_type
print(unique(types))

electric_bike_c <- casual %>% filter(rideable_type=="electric_bike")
classic_bike_c <- casual %>% filter(rideable_type=="classic_bike")
docked_bike_c <- casual %>% filter(rideable_type=="classic_bike")
bike_type <- data.frame(name=c("electric_bike", "classic_bike", "docked_bike"), amount=c(nrow(electric_bike_c), nrow(classic_bike_c), nrow(docked_bike_c)))
ggplot(bike_type, mapping=aes(x=name, y=amount, fill=name)) + geom_bar(stat="identity")

# Check Annual Member Riders
member_types <- member$rideable_type
print(unique(member_types))

electric_bike_m <- member %>% filter(rideable_type=="electric_bike")
classic_bike_m <- member %>% filter(rideable_type=="classic_bike")
member_type_bikes <- data.frame(bike_type=c("electric_bike", "classic_bike"), amount=c(nrow(electric_bike_m), nrow(classic_bike_m)))
ggplot(member_type_bikes, mapping=aes(x=bike_type, y=amount, fill=bike_type)) + geom_bar(stat="identity")

################################################################################################ 

################################################################################################ 

# Splitting Starting Date and Time in Casual Riders
casual_start <- data.frame(Start_Date_Time=casual$started_at)
casual_start[c("Date", "Time")] <- str_split_fixed(casual_start$Start_Date_Time, ' ', 2)
casual_start <- casual_start[c("Date", "Time")]
casual[c("Date", "Time")] <- casual_start[c("Date", "Time")]
casual <- select(casual, -c(started_at))
casual <- casual %>% rename(Start_Date = Date, Start_Time = Time)

# Splitting Ending Date and Time in Casual Riders
casual_end <- data.frame(End_Date_Time=casual$ended_at)
casual_end[c("End_Date", "End_Time")] <- str_split_fixed(casual_end$End_Date_Time, ' ', 2)
casual_end <- casual_end[c("End_Date", "End_Time")]
casual[c("End_Date", "End_Time")] <- casual_end[c("End_Date", "End_Time")]
casual <- select(casual, -c(ended_at))

# Extracting hour from time, then find frequency for casual riders
start_hour <- substring(casual$Start_Time, 0, 2)
casual$start_hour <- start_hour
hour_data_frame <- as.data.frame(table(casual$start_hour))
colnames(hour_data_frame) <- c("Hour", "Frequency")

# Plotting Hour versus Frequency of casual riders
plot1 <- ggplot(hour_data_frame, mapping=aes(x=Hour, y=Frequency)) +
  geom_col()

################################################################################################ 

# Clean up some memory space
remove(jan2024, feb2024, mar2024, apr2024, may2024, june2024, jul2024,
       aug2023, sep2023, oct2023, nov2023, dec2023, bikes_raw, bikes_no_row_NA)
remove(bike_type, people, member_type_bikes, classic_bike_c, classic_bike_m,
       electric_bike_c, electric_bike_m, docked_bike_c)
remove(casual_start, casual_end, types)

################################################################################################
# Splitting Starting Date and Time in Member Riders
member_start <- data.frame(Start_Date_Time=member$started_at)
member_start[c("Start_Date", "Start_Time")] <- str_split_fixed(member_start$Start_Date_Time, ' ', 2)
member_start <- member_start[c("Start_Date", "Start_Time")]
member[c("Start_Date", "Start_Time")] <- member_start[c("Start_Date", "Start_Time")]
member <- select(member, -c(started_at))

# Splitting Ending Date and Time in Member Riders
member_end <- data.frame(End_Date_Time=member$ended_at)
member_end[c("End_Date", "End_Time")] <- str_split_fixed(member_end$End_Date_Time, ' ', 2)
member_end <- member_end[c("End_Date", "End_Time")]
member[c("End_Date", "End_Time")] <- member_end[c("End_Date", "End_Time")]
member <- select(member, -c(ended_at))

# Extracting hour from time, then find frequency for member riders
start_hour <- substring(member$Start_Time, 0, 2)
member$start_hour <- start_hour
hour_data_frame <- as.data.frame(table(member$start_hour))
colnames(hour_data_frame) <- c("Hour", "Frequency")

# Plotting Hour versus Frequency of member riders
plot2 <- ggplot(hour_data_frame, mapping=aes(x=Hour, y=Frequency)) +
  geom_col()

grid.arrange(plot1, plot2, ncol=2)

################################################################################################

# Check users by month

jan <- bikes_update %>% filter(grepl("2024-01", started_at))
feb <- bikes_update %>% filter(grepl("2024-02", started_at))
mar <- bikes_update %>% filter(grepl("2024-03", started_at))
apr <- bikes_update %>% filter(grepl("2024-04", started_at))
may <- bikes_update %>% filter(grepl("2024-05", started_at))
jun <- bikes_update %>% filter(grepl("2024-06", started_at))
jul <- bikes_update %>% filter(grepl("2024-07", started_at))
aug <- bikes_update %>% filter(grepl("2023-08", started_at))
sep <- bikes_update %>% filter(grepl("2023-09", started_at))
oct <- bikes_update %>% filter(grepl("2023-10", started_at))
nov <- bikes_update %>% filter(grepl("2023-11", started_at))
dec <- bikes_update %>% filter(grepl("2023-12", started_at))

user_by_month <- data.frame(Month=c("January", "February", "March", "April", "May", "June",
                                    "July", "August", "September", "October", "November", "December"),
                            Frequency=c(nrow(jan), nrow(feb), nrow(mar),
                                        nrow(apr), nrow(may), nrow(jun),
                                        nrow(jul), nrow(aug), nrow(sep),
                                        nrow(oct), nrow(nov), nrow(dec)))
user_by_month$Month <- factor(user_by_month$Month, levels=month.name)
ggplot(user_by_month, mapping=aes(x=Month, y=Frequency)) + geom_col()

################################################################################################

# Casual and Member Distribution Per Month

# January
jan_casual <- jan %>% filter(member_casual=="casual")
jan_member <- jan %>% filter(member_casual=="member")

month_vs_rider <- data.frame(Month = c("Jan"), Type = c("Casual"), Amount=c(nrow(jan_casual)))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Jan", Type = "Member", Amount=nrow(jan_member))

jan_plot <- ggplot(month_vs_rider[1:2,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Jan 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# February
feb_casual <- feb %>% filter(member_casual=="casual")
feb_member <- feb %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Feb", Type = "Casual", Amount=nrow(feb_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Feb", Type = "Member", Amount=nrow(feb_member))

feb_plot <- ggplot(month_vs_rider[3:4,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Feb 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# March
mar_casual <- mar %>% filter(member_casual=="casual")
mar_member <- mar %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Mar", Type = "Casual", Amount=nrow(mar_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Mar", Type = "Member", Amount=nrow(mar_member))

mar_plot <- ggplot(month_vs_rider[5:6,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Mar 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# April
apr_casual <- apr %>% filter(member_casual=="casual")
apr_member <- apr %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Apr", Type = "Casual", Amount=nrow(apr_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Apr", Type = "Member", Amount=nrow(apr_member))

apr_plot <- ggplot(month_vs_rider[7:8,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Apr 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# May
may_casual <- may %>% filter(member_casual=="casual")
may_member <- may %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "May", Type = "Casual", Amount=nrow(may_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "May", Type = "Member", Amount=nrow(may_member))

may_plot <- ggplot(month_vs_rider[9:10,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("May 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# June
jun_casual <- jun %>% filter(member_casual=="casual")
jun_member <- jun %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "June", Type = "Casual", Amount=nrow(jun_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "June", Type = "Member", Amount=nrow(jun_member))

june_plot <- ggplot(month_vs_rider[11:12,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Jun 2024") + theme(text=element_text(family="Times New Roman", color="black"))

grid.arrange(jan_plot, feb_plot, mar_plot, apr_plot, may_plot, june_plot, ncol=3)

# July
jul_casual <- jul %>% filter(member_casual=="casual")
jul_member <- jul %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "July", Type = "Casual", Amount=nrow(jul_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "July", Type = "Member", Amount=nrow(jul_member))

july_plot <- ggplot(month_vs_rider[13:14,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Jul 2024") + theme(text=element_text(family="Times New Roman", color="black"))


# August
aug_casual <- aug %>% filter(member_casual=="casual")
aug_member <- aug %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Aug", Type = "Casual", Amount=nrow(aug_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Aug", Type = "Member", Amount=nrow(aug_member))

aug_plot <- ggplot(month_vs_rider[15:16,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Aug 2023") + theme(text=element_text(family="Times New Roman", color="black"))


# September
sep_casual <- sep %>% filter(member_casual=="casual")
sep_member <- sep %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Sep", Type = "Casual", Amount=nrow(sep_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Sep", Type = "Member", Amount=nrow(sep_member))

sep_plot <- ggplot(month_vs_rider[17:18,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Sep 2023") + theme(text=element_text(family="Times New Roman", color="black"))


# October
oct_casual <- oct %>% filter(member_casual=="casual")
oct_member <- oct %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Oct", Type = "Casual", Amount=nrow(oct_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Oct", Type = "Member", Amount=nrow(oct_member))

oct_plot <- ggplot(month_vs_rider[19:20,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Oct 2023") + theme(text=element_text(family="Times New Roman", color="black"))


# November
nov_casual <- nov %>% filter(member_casual=="casual")
nov_member <- nov %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Nov", Type = "Casual", Amount=nrow(nov_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Nov", Type = "Member", Amount=nrow(nov_member))

nov_plot <- ggplot(month_vs_rider[21:22,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Nov 2023") + theme(text=element_text(family="Times New Roman", color="black"))


# December
dec_casual <- dec %>% filter(member_casual=="casual")
dec_member <- dec %>% filter(member_casual=="member")

month_vs_rider <- month_vs_rider %>% add_row(Month = "Dec", Type = "Casual", Amount=nrow(dec_casual))
month_vs_rider <- month_vs_rider %>% add_row(Month = "Dec", Type = "Member", Amount=nrow(dec_member))

dec_plot <- ggplot(month_vs_rider[23:24,], mapping=aes(x=Type, y=Amount, fill=Type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type") + ggtitle("Dec 2023") + theme(text=element_text(family="Times New Roman", color="black"))


grid.arrange(jan_plot, feb_plot, mar_plot, apr_plot, may_plot, june_plot,
             july_plot, aug_plot, sep_plot, oct_plot, nov_plot, dec_plot, ncol=4)

################################################################################################

# Find Duration Per Ride for Riders
bikes_distinct$td_s <- strptime(bikes_distinct$started_at, "%Y-%m-%d %H:%M:%S")
bikes_distinct$td_e <- strptime(bikes_distinct$ended_at, "%Y-%m-%d %H:%M:%S")
td <- difftime(bikes_distinct$td_e, bikes_distinct$td_s, units="mins")
bikes_distinct$td <- td

casual_td <- bikes_distinct %>% filter(member_casual=="casual")
member_td <- bikes_distinct %>% filter(member_casual=="member")
mean(casual_td$td)
mean(member_td$td)

ride_duration <- data.frame(type=c("Casual", "Member"), amount=c(sprintf(mean(casual_td$td), fmt='%#.2f'), sprintf(mean(member_td$td), fmt='%#.2f')))
ggplot(ride_duration, mapping=aes(x=type, y=amount, fill=type)) + geom_bar(stat="identity") +
  labs(x = "Rider Type", y = "Duration (in mins)") + ggtitle("Average Duration of Ride") + 
  theme(text=element_text(family="Times New Roman", color="black")) + geom_text(aes(label=amount))

################################################################################################

# Find days in a week
days_in_week <- weekdays(bikes_distinct$td_s, abbreviate=FALSE)
bikes_distinct$days_in_week <- days_in_week

casual_w <- bikes_distinct %>% filter(member_casual=="casual")
member_w <- bikes_distinct %>% filter(member_casual=="member")

casual_w <- as.data.frame(table(casual_w$days_in_week))
member_w <- as.data.frame(table(member_w$days_in_week))

casual_w$Var1 <- factor(casual_w$Var1, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot() + geom_line(data=casual_w, mapping=aes(x=Var1, y=Freq, group=1), color="blue") +
  geom_line(data=member_w, mapping=aes(x=Var1, y=Freq, group=2), color="red") +
  labs(x="Weekdays", y="Users")

################################################################################################

# Clean up RAM Space
remove(jan, jan_casual, jan_member, jan_plot, feb, feb_casual, feb_member, feb_plot,
       mar, mar_casual, mar_member, mar_plot, apr, apr_casual, apr_member, apr_plot)
remove(may, may_casual, may_member, may_plot, jun, jun_casual, jun_member, june_plot,
       jul, jul_casual, jul_member, july_plot, aug, aug_casual, aug_member, aug_plot)
remove(sep, sep_casual, sep_member, sep_plot, oct, oct_casual, oct_member, oct_plot,
       nov, nov_casual, nov_member, nov_plot, dec, dec_casual, dec_member, dec_plot)

remove(casual_td, casual_w, hour_data_frame, member_end, member_start, member_td, member_w,
       month_vs_rider, plot1, plot2, ride_duration, td_e, td_s, user_by_month)

remove(colors, days_in_week, member_types, start_hour, td)



