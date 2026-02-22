###################################
#Draw some figures to look at the data#
####################################
#Imputation
m3$SO2[m3$SO2 < 0] <- 0
m3$CO[m3$CO < 0] <- 0
m3$NO2[m3$NO2 < 0] <- 0

#Figure 1: 13 states that are under RSV-net surveillance

library(ggplot2)
library(maps)

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure 1_13states.jpg", width = 800, height = 600)

# States to label
states_to_label <- c("CA","CO","CT","GA","MD","MI","MN","NM","NY","NC","OR","TN", "UT")

# Map data
us <- map_data("state")

# State centers + abbreviations
centers <- data.frame(
  name = tolower(state.name),
  abb  = state.abb,
  x    = state.center$x,
  y    = state.center$y,
  stringsAsFactors = FALSE
)

centers_to_plot <- centers[ centers$abb %in% states_to_label, ]

# Plot
ggplot() + 
  geom_polygon(data = us, aes(x = long, y = lat, group = group),
               fill = "light blue", color = "black") +
  geom_text(data = centers_to_plot, aes(x = x, y = y, label = abb),
            size = 5, fontface = "bold", color = "red") +
  coord_fixed(1.3) +
  theme_minimal() 

ggsave("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure 1_13states.pdf", width = 7, height = 5)
#dev.off()

# #Figure 2 RSV rate plot time trend
# set1<-m3[(m3$State=="RSV-NET")&(m3$Age.Category=="All")&(m3$Sex=="All")&(m3$Race=="All"),]
# set1$newdate <- as.Date(set1$Week.ending.date, format = "%m/%d/%Y")
# tmp <- set1[order(set1$newdate), c("Rate", "newdate")]
# tmp$year <- format(tmp$newdate, "%Y")
# 
# jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure2_RSV by year.jpg", width = 800, height = 600)
# par(mfrow = c(1, 1))
# plot(tmp$newdate, tmp$Rate, type="l", xlab = "Year", ylab = "Number of RSV Hospitalizations among 100,000 people", xaxt="n")
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 1)
# title(" Overall RSV Rate over time")
# dev.off()

#Figure 3 RSV rate plot time trend by Sex

set2<-m3[(m3$State=="RSV-NET")&(m3$Age.Category=="All")&(m3$Race=="All")&(m3$Sex %in% c("Male","Female")),]
set2$newdate <- as.Date(set2$Week.ending.date, format = "%m/%d/%Y")
tmp <- set2[order(set2$newdate), c("Rate", "newdate","Sex")]
tmp$year <- format(tmp$newdate, "%Y")

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure3_RSV by sex.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure3_RSV by sex.pdf", width = 7, height = 5)
# plot(tmp$newdate[tmp$Sex=="Male"], tmp$Rate[tmp$Sex=="Male"], type="l", xlab = "Time", ylab = "Weekly Rate (per 100,000 persons)", col="red", xaxt="n")
# lines(tmp$newdate[tmp$Sex=="Female"], tmp$Rate[tmp$Sex=="Female"], col="blue")
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 0.7)
# #title(" Overall RSV Rate over time by Sex")
# legend("topright", legend = c("Male", "Female"), col = c("red", "blue"), lty = 1, bty = "n", cex = 1.5)
# Subset data if needed
par(mar=c(6, 5, 2, 2))  
# Define min and max dates
min_date <- min(tmp$newdate)
max_date <- max(tmp$newdate)

# Create Nov and May dates for x-axis
years <- seq(as.numeric(format(min_date, "%Y")), as.numeric(format(max_date, "%Y")), by = 1)
nov_dates <- as.Date(paste0(years, "-11-01"))
may_dates <- as.Date(paste0(years, "-05-01"))

# Combine and keep only dates within range
x_ticks <- sort(c(nov_dates, may_dates))
x_ticks <- x_ticks[x_ticks >= min_date & x_ticks <= max_date]

# Plot
plot(tmp_sub$newdate[tmp_sub$Sex=="Male"], tmp_sub$Rate[tmp_sub$Sex=="Male"],
     type="l", xlab="", ylab="Weekly Rate (per 100,000 persons)",
     col="red", xaxt="n", cex.lab=1.5,  lwd=2)

lines(tmp_sub$newdate[tmp_sub$Sex=="Female"], tmp_sub$Rate[tmp_sub$Sex=="Female"], col="blue",  lwd=2)

# Add x-axis with labels
#axis(1, at=x_ticks, labels=format(x_ticks, "%b %Y"), las=1, cex.axis=0.5)  # las=1 makes labels horizontal

# axis(1, at=x_ticks, labels=format(x_ticks, "%b %Y"), las=2, cex.axis=0.8)
# 
# 


# Example: slant (45 degrees) the x-axis labels
axis(1, at = x_ticks, labels = FALSE)  # draw ticks only

# # add slanted labels
# text(x = x_ticks, 
#      y = par("usr")[3]-0.25,  # move labels down a bit
#      labels = format(x_ticks, "%b %Y"), 
#      srt = 45,                 # rotation angle
#      adj = 1,                  # text alignment
#      xpd = TRUE,               # allow plotting outside margin
#      cex = 1)                # text size

# Add slanted labels in mm/YYYY format
text(x = x_ticks, 
     y = par("usr")[3]-0.2,   # adjust spacing
     labels = format(x_ticks, "%m/%Y"),  # <-- change here
     srt = 45,                  
     adj = 1,                   
     xpd = TRUE, 
     cex = 1)
# Add "Time" label further down
mtext("Time", side=1, line=4, cex=1.5)  # line=4 moves it further from the axis
# Add legend
legend("topleft", legend=c("Male", "Female"), col=c("red","blue"), lty=1, lwd=2,bty="n", cex=1.5)
dev.off()

#Figure 4 RSV rate plot time trend by age group
set4<-m3[(m3$State=="RSV-NET")&(m3$Age.Category!="All")&(m3$Race=="All")&(m3$Sex=="All"),]
set4$newdate <- as.Date(set4$Week.ending.date, format = "%m/%d/%Y")
tmp <- set4[order(set4$newdate), c("Rate", "newdate","Age.Category")]
tmp$year <- format(tmp$newdate, "%Y")

pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure4_RSV by age.pdf", width = 7, height = 5)
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure4_RSV by age.jpg", width = 800, height = 600)
# plot(tmp$newdate[tmp$Age.Category=="0-4 years"], tmp$Rate[tmp$Age.Category=="0-4 years"], type="l", xlab = "Time", ylab = "Weekly Rate (per 100,000 persons)", col="red", xaxt="n")
# lines(tmp$newdate[tmp$Age.Category=="5-17 years"], tmp$Rate[tmp$Age.Category=="5-17 years"], col="blue")
# lines(tmp$newdate[tmp$Age.Category=="18-49 years"], tmp$Rate[tmp$Age.Category=="18-49 years"], col="yellow")
# lines(tmp$newdate[tmp$Age.Category=="50-64 years"], tmp$Rate[tmp$Age.Category=="50-64 years"], col="green")
# lines(tmp$newdate[tmp$Age.Category==">=65 years"], tmp$Rate[tmp$Age.Category==">=65 years"], col="pink")
# 
# lines(tmp$newdate[tmp$Age.Category=="0-17 years (Children)"], tmp$Rate[tmp$Age.Category=="0-17 years (Children)"], col="purple")
# lines(tmp$newdate[tmp$Age.Category==">=18 years (Adults)"], tmp$Rate[tmp$Age.Category==">=18 years (Adults)"], col="orange")
# 
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 0.7)
# #title(" Overall RSV Rate over time by Age groups")
# legend("topright", legend = c("0-4 years", "5-17 years","18-49 years","50-64 years",">=65 years","0-17 years (Children)",">=18 years (Adults)"), 
#        col = c("red", "blue", "yellow", "green", "pink", "purple", "orange"), lty = 1, bty = "n", cex=1.3)

tmp <- tmp[tmp$newdate >= as.Date("2018-10-01"), ]

# Define min and max dates
min_date <- min(tmp$newdate)
max_date <- max(tmp$newdate)

# Create Nov and May dates for x-axis
years <- seq(as.numeric(format(min_date, "%Y")), as.numeric(format(max_date, "%Y")), by = 1)
nov_dates <- as.Date(paste0(years, "-11-01"))
may_dates <- as.Date(paste0(years, "-05-01"))

# Combine and keep only dates within range
x_ticks <- sort(c(nov_dates, may_dates))
x_ticks <- x_ticks[x_ticks >= min_date & x_ticks <= max_date]

# Expand bottom margin for slanted labels
par(mar=c(6, 5, 2, 2))  

# Plot first age group
plot(tmp$newdate[tmp$Age.Category=="0-4 years"], tmp$Rate[tmp$Age.Category=="0-4 years"],
     type="l", xlab="", 
     ylab="Weekly Rate (per 100,000 persons)",
     col="red", xaxt="n", cex.lab=0.8, lwd=2)

# Add other age groups
lines(tmp$newdate[tmp$Age.Category=="5-17 years"], tmp$Rate[tmp$Age.Category=="5-17 years"], col="blue",lwd=2)
lines(tmp$newdate[tmp$Age.Category=="18-49 years"], tmp$Rate[tmp$Age.Category=="18-49 years"], col="yellow",lwd=2)
lines(tmp$newdate[tmp$Age.Category=="50-64 years"], tmp$Rate[tmp$Age.Category=="50-64 years"], col="green",lwd=2)
lines(tmp$newdate[tmp$Age.Category==">=65 years"], tmp$Rate[tmp$Age.Category==">=65 years"], col="pink",lwd=2)
lines(tmp$newdate[tmp$Age.Category=="0-17 years (Children)"], tmp$Rate[tmp$Age.Category=="0-17 years (Children)"], col="purple",lwd=2)
lines(tmp$newdate[tmp$Age.Category==">=18 years (Adults)"], tmp$Rate[tmp$Age.Category==">=18 years (Adults)"], col="orange",lwd=2)

# Draw ticks only
axis(1, at = x_ticks, labels = FALSE)

# Add slanted labels
text(x = x_ticks, 
     y = par("usr")[3]-3,      # adjust spacing further down
     labels = format(x_ticks, "%m/%Y"), 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 0.8)                # make labels a bit larger

# Add bottom x-axis label
mtext("Time", side=1, line=5, cex=0.8)  # font=2 = bold

# Add legend
legend("topleft", legend=c("0-4 years", "5-17 years","18-49 years","50-64 years",">=65 years",
                           "0-17 years (Children)",">=18 years (Adults)"), 
       col=c("red", "blue", "yellow", "green", "pink", "purple", "orange"), 
       lty=1, lwd=2, bty="n", cex=1)

dev.off()



#Figure 5 RSV rate plot time trend by Race
set3<-m3[(m3$State=="RSV-NET")&(m3$Age.Category=="All")&(m3$Race!="All")&(m3$Sex=="All"),]
set3$newdate <- as.Date(set3$Week.ending.date, format = "%m/%d/%Y")
tmp <- set3[order(set3$newdate), c("Rate", "newdate","Race", "State")]
tmp$year <- format(tmp$newdate, "%Y")

pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure5_RSV by race.pdf", width = 7, height = 5)
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure5_RSV by race.jpg", width = 800, height = 600)
# plot(tmp$newdate[tmp$Race=="White, non-Hispanic"], tmp$Rate[tmp$Race=="White, non-Hispanic"], type="l", xlab = "Time", ylab = "Weekly Rate (per 100,000 persons)", col="red",xaxt="n",ylim = c(0,17))
# lines(tmp$newdate[tmp$Race=="AI/AN, non-Hispanic"], tmp$Rate[tmp$Race=="AI/AN, non-Hispanic"], col="blue")
# lines(tmp$newdate[tmp$Race=="Black, non-Hispanic"], tmp$Rate[tmp$Race=="Black, non-Hispanic"], col="yellow")
# lines(tmp$newdate[tmp$Race=="A/PI, non-Hispanic"], tmp$Rate[tmp$Race=="A/PI, non-Hispanic"], col="green")
# lines(tmp$newdate[tmp$Race=="Hispanic"], tmp$Rate[tmp$Race=="Hispanic"], col="pink")
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 0.7)
# #title(" Overall RSV Rate over time by Race")
# legend("topleft", legend = c("White, non-Hispanic", "AI/AN, non-Hispanic","Black, non-Hispanic","A/PI, non-Hispanic","Hispanic"), col = c("red", "blue", "yellow", "green", "pink"), lty = 1, bty = "n", cex=1.3)
# Define min and max dates
tmp<- tmp[tmp$newdate >= as.Date("2018-10-01"), ]
min_date <- min(tmp$newdate)
max_date <- max(tmp$newdate)
par(mar=c(6, 5, 2, 2))  
# Create Nov and May dates for x-axis
years <- seq(as.numeric(format(min_date, "%Y")), as.numeric(format(max_date, "%Y")), by = 1)
nov_dates <- as.Date(paste0(years, "-11-01"))
may_dates <- as.Date(paste0(years, "-05-01"))

# Combine and keep only dates within range
x_ticks <- sort(c(nov_dates, may_dates))
x_ticks <- x_ticks[x_ticks >= min_date & x_ticks <= max_date]

# Plot first race group
plot(tmp$newdate[tmp$Race=="White, non-Hispanic"], tmp$Rate[tmp$Race=="White, non-Hispanic"],
     type="l", xlab="", ylab="Weekly Rate (per 100,000 persons)",
     col="red", xaxt="n", cex.lab=0.8,ylim=c(0,17),lwd=2)

# Add other race groups
lines(tmp$newdate[tmp$Race=="AI/AN, non-Hispanic"], tmp$Rate[tmp$Race=="AI/AN, non-Hispanic"], col="blue",lwd=2)
lines(tmp$newdate[tmp$Race=="Black, non-Hispanic"], tmp$Rate[tmp$Race=="Black, non-Hispanic"], col="yellow",lwd=2)
lines(tmp$newdate[tmp$Race=="A/PI, non-Hispanic"], tmp$Rate[tmp$Race=="A/PI, non-Hispanic"], col="green",lwd=2)
lines(tmp$newdate[tmp$Race=="Hispanic"], tmp$Rate[tmp$Race=="Hispanic"], col="pink",lwd=2)

# Draw ticks only
axis(1, at = x_ticks, labels = FALSE)

# Add slanted labels
text(x = x_ticks, 
     y = par("usr")[3]-0.6,      # adjust spacing further down
     labels = format(x_ticks, "%m/%Y"), 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 0.8)                # make labels a bit larger

# Add bottom x-axis label
mtext("Time", side=1, line=5, cex=0.8)  # font=2 = bold

# Add legend
legend("topleft", legend=c("White, non-Hispanic", "AI/AN, non-Hispanic", "Black, non-Hispanic", 
                           "A/PI, non-Hispanic", "Hispanic"), 
       col=c("red", "blue", "yellow", "green", "pink"), lty=1, lwd=2,bty="n", cex=0.8)


dev.off()

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#########AI/AN rate by age group and by states
# set10<-m3[(m3$Age.Category!="All")&(m3$Race=="AI/AN, non-Hispanic")&(m3$Sex=="All")&(m3$State=="RSV-NET"),]
# set10$newdate <- as.Date(set10$Week.ending.date, format = "%m/%d/%Y")
# tmp <- set10[order(set10$State,set10$newdate), c("Rate", "newdate","Age.Category")]
# tmp$year <- format(tmp$newdate, "%Y")
# 
# library(ggplot2)
# library(lubridate)
# 
# 
# ggplot(tmp, aes(x = newdate, y = Rate, color = Age.Category, group = Age.Category)) +
#   geom_line(size = 1, alpha = 0.7) +
#   scale_x_date(
#     date_breaks = "6 months",
#     date_labels = "%b %Y",
#     breaks = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months")
#   ) +
#   labs(title = "Rate Over Time for AI/AN",
#        x = "Date",
#        y = "Rate") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
# 
# set11<-m3[(m3$Age.Category=="0-17 years (Children)")&(m3$Race=="AI/AN, non-Hispanic")&(m3$Sex=="All")&(m3$State!="RSV-NET"),]
# set11$newdate <- as.Date(set11$Week.ending.date, format = "%m/%d/%Y")
# tmp <- set11[order(set11$State,set11$newdate), c("Rate", "newdate","State")]
# tmp$year <- format(tmp$newdate, "%Y")
# 
# library(ggplot2)
# library(lubridate)
# 
# 
# ggplot(tmp, aes(x = newdate, y = Rate, color = State, group = State)) +
#   geom_line(size = 1, alpha = 0.7) +
#   scale_x_date(
#     date_breaks = "6 months",
#     date_labels = "%b %Y",
#     breaks = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months")
#   ) +
#   labs(title = "Rate Over Time for AI/AN by state for 0-17 years old",
#        x = "Date",
#        y = "Rate") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
# 
# 
# library(ggplot2)
# library(lubridate)
# 
# set12<-m3[(m3$Age.Category=="All")&(m3$Race=="All")&(m3$Sex=="All")&(m3$State!="RSV-NET"),]
# set12$newdate <- as.Date(set12$Week.ending.date, format = "%m/%d/%Y")
# tmp <- set12[order(set12$State,set12$newdate), c("Rate", "newdate","State")]
# tmp$year <- format(tmp$newdate, "%Y")
# ggplot(tmp, aes(x = newdate, y = Rate, color = State, group = State)) +
#   geom_line(size = 1, alpha = 0.7) +
#   scale_x_date(
#     date_breaks = "6 months",
#     date_labels = "%b %Y",
#     breaks = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months")
#   ) +
#   labs(title = "Rate Over Time for AI/AN",
#        x = "Date",
#        y = "Rate") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

######################################
#seems that 0-4 years group has the highest rate, maybe focus onto toddlers and infants, who are most impacted
######################################

##############Waste water activity data
set5<-m3[(m3$Age.Category=="0-4 years")&(m3$Race=="All")&(m3$Sex=="All"),]
set5 <- na.omit(set5)
set5$newdate <- as.Date(set5$Week.ending.date, format = "%m/%d/%Y")
tmp <- set5[order(set5$newdate), c("Rate", "newdate","activity_level", "WVAL")]
tmp$year <- format(tmp$newdate, "%Y")


# plot(tmp$newdate, tmp$activity_level, type="l", xlab = "Year", ylab = "RSV Level in Waste Water", col="red", xaxt="n")
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 0.7)
# title("Wastewater RSV Activity Over Time")


####Figure 6 WVAL data
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure6_wasterwater_RSVlevel.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure6_wasterwater_RSVlevel.pdf", width = 7, height = 5)
# plot(tmp$newdate, tmp$WVAL, type="l", xlab = "Time", ylab = "WasteWater RSV Level (WVAL)", col="red", xaxt="n")
# axis(1, 
#      at = seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), 
#      labels = format(seq(min(tmp$newdate), max(tmp$newdate), by = "6 months"), "%b %Y"),
#      cex.axis = 0.7)
# #title("Wastewater RSV Activity Over Time")
# Ensure dates are in Date format
par(mar=c(6, 5, 2, 2))  
tmp$newdate <- as.Date(tmp$newdate)

min_date <- min(tmp$newdate)
max_date <- max(tmp$newdate)
par(mar=c(6, 5, 2, 2))  
# Create Nov and May dates for x-axis
years <- seq(as.numeric(format(min_date, "%Y")), as.numeric(format(max_date, "%Y")), by = 1)
nov_dates <- as.Date(paste0(years, "-11-01"))
may_dates <- as.Date(paste0(years, "-05-01"))

# Combine and keep only dates within range
x_ticks <- sort(c(nov_dates, may_dates))
x_ticks <- x_ticks[x_ticks >= min_date & x_ticks <= max_date]

# Plot
plot(tmp$newdate, tmp$WVAL, type="l", xlab="", ylab="Wastewater RSV Level (WVAL)",
     col="red", xaxt="n", lwd=2, cex.lab=1.5)

# Draw ticks only
axis(1, at = x_ticks, labels = FALSE)

# Add slanted labels
text(x = x_ticks, 
     y = par("usr")[3]-1,      # adjust spacing further down
     labels = format(x_ticks, "%m/%Y"), 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 1.5)                # make labels a bit larger

# Add bottom x-axis label
mtext("Time", side=1, line=5, cex=1.5)  # font=2 = bold

dev.off()


################Air data
set6<-m3[(m3$Age.Category=="0-4 years")&(m3$Race=="All")&(m3$Sex=="All"),]
set6$newdate <- as.Date(set6$Week.ending.date, format = "%m/%d/%Y")
tmp <- set6[order(set6$newdate), c("Rate", "newdate", "CO", "NO2", "Ozone", "PM10", "PM2.5", "SO2")]
tmp$SO2[tmp$SO2 < 0] <- 0
tmp$year <- format(tmp$newdate, "%Y")

tmp$SO2[tmp$SO2 < 0] <- 0
tmp$CO[tmp$CO < 0] <- 0
tmp$NO2[tmp$NO2 < 0] <- 0
# Ensure these columns are numeric

pollutants <- tmp[, 3:8]
pollutants[] <- lapply(pollutants, function(x) as.numeric(as.character(x)))

# Remove rows with any missing values
pollutants <- na.omit(pollutants)

# Plot

#boxplot.matrix(as.matrix(pollutants), use.cols = TRUE)
# jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure7_air_boxplot.jpg", width = 800, height = 600)
# par(mfrow = c(2, 3))
# 
# boxplot(tmp$CO, main="CO")
# boxplot(tmp$NO2, main="NO2")
# boxplot(tmp$Ozone, main="O3")
# boxplot(tmp$PM10, main="PM10")
# boxplot(tmp$PM2.5, main="PM2")
# boxplot(tmp$SO2, main="SO2")
# dev.off()

#####Violin plots of air data
#install.packages("vioplot")
library(vioplot)
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure7_air_violin.jpg", width = 800, height = 600)

pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure7_air_violin.pdf", width = 7, height = 5)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))  # set layout and margins
vioplot(tmp$CO, main = "CO", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$NO2, main = "NO2", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$Ozone, main = "Ozone", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$PM10, main = "PM10", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$`PM2.5`, main = "PM2.5", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$SO2, main = "SO2", col = "lightblue", drawRect = TRUE, cex.main=1.5)
dev.off()

####Violin plots of weather data
set7<-m3[(m3$Age.Category=="0-4 years")&(m3$Race=="All")&(m3$Sex=="All"),]
set7$newdate <- as.Date(set7$Week.ending.date, format = "%m/%d/%Y")
tmp <- set7[order(set7$newdate), c("Rate", "newdate", "WVAL", "T2M","T2MDEW",	"T2MWET",	"TS",	"QV2M",	"RH2M",	"PRECTOTCORR","PS",	"WS2M","WS10M",	"WD10MN")]
tmp$year <- format(tmp$newdate, "%Y")

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure8_weather_violin.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure8_weather_violin.pdf", width = 7, height = 5)
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))  # set layout and margins
vioplot(tmp$PRECTOTCORR, main = "PRECTOTCORR", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$PS, main = "PS", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$QV2M, main = "QV2M", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$RH2M, main = "RH2M", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$T2M, main = "T2M", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$T2MDEW, main = "T2MDEW", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$T2MWET, main = "T2MWET", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$TS, main = "TS", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$WD10MN, main = "WD10MN", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$WS10M, main = "WS10M", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$WS2M, main = "WS2M", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(tmp$WVAL, main = "WVAL", col = "lightblue", drawRect = TRUE, cex.main=1.5)
dev.off()

#Figure 9: look at response variable transformation
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure9_reponse_violin.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure9_reponse_violin.pdf", width = 7, height = 5)
par(mfrow=c(1,3))
vioplot(tmp$Rate, main = "Weekly Rate", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(log(tmp$Rate+0.5), main = "Log(Weekly Rate)", col = "lightblue", drawRect = TRUE, cex.main=1.5)
vioplot(sqrt(tmp$Rate), main = "Square Root (Weekly Rate)", col = "lightblue", drawRect = TRUE, cex.main=1.5)
dev.off()


sub1<-m3[m3$Age.Category %in% c("0-4 years"), ]
sub1<-sub1[!is.na(sub1$Rate), ]
sub1<-sub1[, !(names(sub1) %in% c("State", "Week.ending.date","Age.Category", "Sex", "Race","year", "month", "day", "Sstae.Territory.WVAL"))]
sub1 <- na.omit(sub1)


# #Investiate the relation between rate and each preditor variable
# plot(sub1$WS2M_1, log(sub1$Rate+1))
# lines(lowess(sub1$WS2M_1, log(sub1$Rate+1)), col = "orange", lwd = 2)
# 
# plot(sub1$T2M_1, sub1$logate)
# lines(lowess(sub1$T2M_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$T2MDEW_1, sub1$lograte)
# lines(lowess(sub1$T2MDEW_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$T2MWET_1, sub1$lograte)
# lines(lowess(sub1$T2MWET_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$TS_1, sub1$lograte)
# lines(lowess(sub1$TS_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$QV2M_1, sub1$lograte)
# lines(lowess(sub1$QV2M_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$RH2M_1, sub1$lograte)
# lines(lowess(sub1$RH2M_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$PRECTOTCORR_1, sub1$lograte)
# lines(lowess(sub1$PRECTOTCORR_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$PS_1, sub1$lograte)
# lines(lowess(sub1$PS_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$WS10M_1, sub1$lograte)
# lines(lowess(sub1$WS10M_1, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$activity_level, sub1$lograte)
# lines(lowess(sub1$activity_level, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$WVAL, sub1$lograte)
# lines(lowess(sub1$WVAL, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$CO, sub1$lograte)
# lines(lowess(sub1$CO, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$NO2, sub1$lograte)
# lines(lowess(sub1$NO2, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$Ozone, sub1$Rate)
# lines(lowess(sub1$Ozone, sub1$Rate), col = "orange", lwd = 2)
# 
# plot(sub1$PM10, sub1$lograte)
# lines(lowess(sub1$PM10, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$PM2.5, sub1$lograte)
# lines(lowess(sub1$PM2.5, sub1$lograte), col = "orange", lwd = 2)
# 
# plot(sub1$SO2, sub1$lograte)
# lines(lowess(sub1$SO2, sub1$lograte), col = "orange", lwd = 2)


# #Correlation heatmap
 tmp <- sub1[, !(names(sub1) %in% c("State", "Week.ending.date","Age.Category", 
                                  "Sex", "Race","MO", "YEAR", "State.Territory_WVAL", "activity_level"))]
# # cor_mat <- cor(tmp[, sapply(tmp, is.numeric)])
# # heatmap(cor_mat)
# 
# #Correlation coefficent matrix map install.packages("corrplot")  # if not installed\
# jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure10_correlation.jpg", width = 800, height = 600)
# library(corrplot)
# par(mfrow=c(1,1))
# num_cols <- tmp[, sapply(tmp, is.numeric)]
# cor_mat <- cor(num_cols, use = "pairwise.complete.obs")
# custom_col <- colorRampPalette(c("red", "white", "blue"))(200)
# 
# # Show coefficients
# corrplot(cor_mat, method = "color", col=custom_col,type = "upper", 
#          addCoef.col = "black", number.cex = 0.6, # smaller numbers
#          tl.cex = 0.6, # smaller axis labels
#          tl.col = "black", tl.srt = 45)
# 
# dev.off()

par(mfrow=c(1,1))
#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure10_correlation.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Figure10_correlation.pdf", width = 8, height = 8)
# Extract numeric columns
num_cols <- tmp[, sapply(tmp, is.numeric)]

# All variable names
vars <- colnames(num_cols)

# --- Define variable groups ---
# (adjust these lists based on your dataset column names)
weather_vars <- c("PRECTOTCORR", "PS", "QV2M", "RH2M", "T2M", "T2MDEW", "T2MWET", "TS",
                  "WD10MN", "WS10M","WS2M", "WVAL")
air_vars <- c("CO","NO2", "Ozone", "PM10","PM2.5","SO2")  

# Keep only those that actually exist in the data
weather_vars <- sort(intersect(weather_vars, vars))
air_vars <- sort(intersect(air_vars, vars))

# Define the final order
order_vars <- c("Rate", weather_vars, air_vars)

# Reorder dataframe
num_cols <- num_cols[, order_vars, drop = FALSE]

# Correlation matrix
cor_mat <- cor(num_cols, use = "pairwise.complete.obs")

# Plot
library(corrplot)
custom_col <- colorRampPalette(c("red", "white", "blue"))(200)

corrplot(cor_mat, method = "color", col = custom_col, type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.cex = 0.8, tl.col = "black", tl.srt = 45)
dev.off()

#Colorado state vs others:
set15<-m3[(m3$Age.Category=="0-4 years")&(m3$Race=="All")&(m3$Sex=="All")&(m3$State != "RSV-NET"),]
set15$newdate <- as.Date(set15$Week.ending.date, format = "%m/%d/%Y")
tmp <- set15[order(set15$newdate), c("Rate", "newdate", "State", "PS")]
tmp$year <- format(tmp$newdate, "%Y")

# Create Group variable with 4 categories
tmp$Group <- ifelse(tmp$State == "Colorado", "Colorado",
                    ifelse(tmp$State == "New Mexico", "New Mexico",
                           ifelse(tmp$State == "Utah", "Utah", "Others")))

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Rate_high_latitudes.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/Rate_high_latitudes.pdf", width = 7, height = 5)
# Ensure dates are Date type
tmp$newdate <- as.Date(tmp$newdate)

min_date <- min(tmp$newdate)
max_date <- max(tmp$newdate)
par(mar=c(6, 5, 2, 2))  
# Create Nov and May dates for x-axis
years <- seq(as.numeric(format(min_date, "%Y")), as.numeric(format(max_date, "%Y")), by = 1)
nov_dates <- as.Date(paste0(years, "-11-01"))
may_dates <- as.Date(paste0(years, "-05-01"))

# Combine and keep only dates within range
x_ticks <- sort(c(nov_dates, may_dates))
x_ticks <- x_ticks[x_ticks >= min_date & x_ticks <= max_date]


# Plot lines for each group
plot(tmp$newdate[tmp$Group=="Colorado"], tmp$Rate[tmp$Group=="Colorado"], type="l", 
     xlab = "", ylab = "Weekly Rate (per 100,000 persons)", 
     ylim=c(0,300), col="red", xaxt="n",cex.lab=1.5)

lines(tmp$newdate[tmp$Group=="New Mexico"], tmp$Rate[tmp$Group=="New Mexico"], col="blue", lwd=2)
lines(tmp$newdate[tmp$Group=="Utah"], tmp$Rate[tmp$Group=="Utah"], col="green", lwd=2)
lines(tmp$newdate[tmp$Group=="Others"], tmp$Rate[tmp$Group=="Others"], col="yellow", lwd=2)

# Draw ticks only
axis(1, at = x_ticks, labels = FALSE)

# Add slanted labels
text(x = x_ticks, 
     y = par("usr")[3]-10,      # adjust spacing further down
     labels = format(x_ticks, "%m/%Y"), 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 1.2)                # make labels a bit larger

# Add bottom x-axis label
mtext("Time", side=1, line=5, cex=1.5)  # font=2 = bold
# Add legend
legend("topleft", legend = c("Colorado", "New Mexico", "Utah", "Others"), 
       col = c("red", "blue", "green", "yellow"), lty = 1, bty = "n", cex = 1.2, lwd=2)

dev.off()

library(vioplot)

# Define the high states
high_states <- c("Colorado", "New Mexico", "Utah")

# Extract data for each group
colorado <- tmp$PS[tmp$State == "Colorado"]
new_mexico <- tmp$PS[tmp$State == "New Mexico"]
utah <- tmp$PS[tmp$State == "Utah"]
other <- tmp$PS[!(tmp$State %in% high_states)]

#jpeg("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/PS_high_latitudes.jpg", width = 800, height = 600)
pdf("C:/Users/ericj/OneDrive/Documents/Eric/Eric Summer 2025/Pioneer/Final project/RSV/Result/PS_high_latitudes.pdf", width = 7, height = 5)
# Plot with vioplot
par(cex.lab = 1.5,   # makes xlab and ylab larger
    cex.axis = 1.2)  # makes tick mark labels larger
vioplot(
  colorado, new_mexico, utah, other,
  names = c("Colorado", "New Mexico", "Utah", "Other"),
  col = "lightblue",
  drawRect = TRUE,
  ylab = "PS (kPa)",
  line = 3  # moves the y-axis label farther from numbers
)
dev.off()
