# Include Libraries
if (!require('readr')) install.packages("readr")
if (!require('dplyr')) install.packages("dplyr")
if (!require('ggplot2')) install.packages("ggplot2")

# Clear environment
rm(list=ls(all=TRUE))

# Change directory
setwd('/Users/kaustubhn/Desktop/projects/zsoc/data/tmp')

az <- read_csv('AmazonIN_facebook_statuses.csv')
az <- cbind2(az, rep('amazon', nrow(az)))
colnames(az)[16] <- "website"

fk <- read_csv('flipkart_facebook_statuses.csv')
fk <- cbind2(fk, rep('flipkart', nrow(fk)))
colnames(fk)[16] <- "website"

sd <- read_csv('Snapdeal_facebook_statuses.csv')
sd <- cbind2(sd, rep('snapdeal', nrow(sd)))
colnames(sd)[16] <- "website"

ib <- read_csv('infibeam_facebook_statuses.csv')
ib <- cbind2(ib, rep('infibeam', nrow(ib)))
colnames(ib)[16] <- "website"

sc <- read_csv('ShopClues_facebook_statuses.csv')
sc <- cbind2(sc, rep('shopclues', nrow(sc)))
colnames(sc)[16] <- "website"

jd <- read_csv('JustDial_facebook_statuses.csv')
jd <- cbind2(jd, rep('justdial', nrow(jd)))
colnames(jd)[16] <- "website"

# Combine the data into Final Data Frame (fdf)
fdf <- rbind(az, fk, sd, ib, sc, jd)
fdf <- as.data.frame(fdf)

# Split date and time into different columns
fdf$status_pub_date <- as.Date(fdf$status_published)
fdf$status_pub_time <- format(fdf$status_published, "%H:%M:%S")

# Split date into year, month, day columns
fdf$status_pub_year <- format(fdf$status_published, "%Y")
fdf$status_pub_month <- format(fdf$status_published, "%m")
fdf$status_pub_day <- format(fdf$status_published, "%d")

# Convert time to hours as integers from 1 to 24
fdf$status_pub_hour <- format(fdf$status_published, "%H")

# Convert month to respective abbribvations
#fdf$status_pub_month <- month.abb[as.integer(fdf$status_pub_month)]

# Save final df
write_csv(fdf, "/Users/kaustubhn/Desktop/projects/zsoc/data/final_df.csv")

# Line chart
pdata <- fdf %>% select(website, status_type, status_pub_year, status_published) %>% group_by(website, status_pub_year, status_type) %>% summarise(count = n())
ggplot(pdata[pdata$website == "amazon",], aes(x=status_published, y=count), group=status_type) +
  geom_line()
pdata <- fdf %>% select(website, status_type) %>% group_by(website, status_type) %>% summarise(count = n())
ggplot(pdata, aes(x=status_type, y=count, fill=website)) + 
  geom_bar(stat="identity",position=position_dodge(), colour="white")
