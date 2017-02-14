#Nathan Lin
#McIntire Investment Institute Point72 Stock Pitch Competition
#Etsy (ETSY - L)

####Libraries Needed####
library(zipcode)
library(ggplot2)
library(sqldf)
library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)

####Import and Preprocessing####
etsy.raw <- read.csv("C:/Users/Nathan/OneDrive/OneDrive Documents/Second Year/Other and Extracurriculars/MII/Spring 2017/P72/Data/DataSample_ETSY.csv",
                     na.strings = c("^    ",""), 
                     colClasses = c("integer", "integer", "character", "character", "Date", "numeric", "character", "character", "character"))
etsy.raw <- etsy.raw[,-c(3,4)]
colnames(etsy.raw) <- c("user", "mailbox", "order_date", "order_total", "description", "bill_zip", "ship_zip")

summary(etsy.raw)
sapply(etsy.raw, class)

####Map Zip Codes (Frequency)####
data(zipcode)
data(zip.regions)
data("df_pop_zip")
etsy.map <- etsy.raw
etsy.map <- merge(etsy.map, zipcode, by.x = "ship_zip", by.y = "zip", all.x = TRUE)

zip.regions2 <- as.data.frame(zip.regions[,1])
colnames(zip.regions2) <- "region"
zips <- sqldf("select ship_zip, count(ship_zip) from 'etsy.map' group by ship_zip")
colnames(zips) <- c("region", "value")
zips <- merge(zips, zip.regions2, by = "region", all.y = TRUE)
zips$value <- ifelse(is.na(zips$value),0,zips$value)
zips <- sqldf("select distinct * from zips")

#Create the map
choro = choroplethrZip::ZipChoropleth$new(zips)
choro$prepare_map()

choro$legend = "Purchase Frequency"
ec_zips = zip.regions[!(zip.regions$state.name %in% c("alaska", "hawaii", NA)), "region"]
ec_df   = choro$choropleth.df[choro$choropleth.df$region %in% ec_zips, ]
ec_plot = choro$render_helper(ec_df, "", choro$theme_clean()) + 
  labs(title = "ETSY Purchases by Zip Code Frequency", subtitle = "Transaction data from January 2013 to October 2016",
       caption = "Source: Point72 Asset Management")
  
ec_plot <- ec_plot + coord_map("polyconic")

ggsave(ec_plot, file="Images/Map_Freq.png",
       width = 22.92, height = 11.46, dpi = 400)


####Map Zip Codes (Average Purchase Price)####
zips2 <- sqldf("select ship_zip, avg(order_total) from 'etsy.map' group by ship_zip")
colnames(zips2) <- c("region", "value")
zips2 <- merge(zips2, zip.regions2, by = "region", all.y = TRUE)
zips2$value <- ifelse(is.na(zips2$value),0,zips2$value)
zips2 <- sqldf("select distinct * from zips2")

#Create the map
choro = choroplethrZip::ZipChoropleth$new(zips2)
choro$prepare_map()

choro$legend = "Average Purchase"
ec_zips = zip.regions[!(zip.regions$state.name %in% c("alaska", "hawaii", NA)), "region"]
ec_df   = choro$choropleth.df[choro$choropleth.df$region %in% ec_zips, ]
ec_plot = choro$render_helper(ec_df, "", choro$theme_clean()) +  scale_fill_brewer(palette = "YlOrRd") + 
  labs(title = "ETSY Average Purchase by Zip Code", subtitle = "Transaction data in U.S. Dollars from January 2013 to October 2016",
       caption = "Source: Point72 Asset Management")

#ggtitle("ETSY Purchase Frequency from January 2013 to October 2016")
ec_plot <- ec_plot + coord_map("polyconic") 

ggsave(ec_plot, file="Images/Map_Purch.png",
       width = 22.92, height = 11.46, dpi = 400)

####US 2012 Population Density####
choro = choroplethrZip::ZipChoropleth$new(df_pop_zip)
choro$prepare_map()

choro$legend = "Population"
ec_zips = zip.regions[!(zip.regions$state.name %in% c("alaska", "hawaii", NA)), "region"]
ec_df   = choro$choropleth.df[choro$choropleth.df$region %in% ec_zips, ]
ec_plot = choro$render_helper(ec_df, "", choro$theme_clean()) +  scale_fill_brewer(palette = "Greens") + 
  labs(title = "US Population Density", subtitle = "Data for Zip Code Tabulated Areas in 2012",
       caption = "United States Census Bureau")

ec_plot <- ec_plot + coord_map("polyconic") 

ggsave(ec_plot, file="Images/Map_PopDensity.png",
       width = 22.92, height = 11.46, dpi = 400)

####US 2012 PC Income####
get_zip_demographics(endyear = 2012, span = 5)

####Recurring Customers####
#Frequency Analysis
user.order <- sqldf("select user, ship_zip, order_date, avg(order_total) from 'etsy.raw' group by user, order_date")
colnames(user.order) <- c("user", "ship_zip", "order_date", "avg_total_per_session")

user.order2 <- sqldf("select user, count(user), ship_zip from 'user.order' group by user")
summary(user.order2) #Number of orders for each user in the time span, average of 2.543 per user
length(which(user.order2$`count(user)` > 1))/length(user.order2$`count(user)`) #36.52692% ordered more than once

user.order3 <- subset(user.order, user.order$avg_total_per_session < 250)
price.hist <- ggplot(data=user.order3, aes(x = user.order3$avg_total_per_session, fill = ..count..)) + 
  geom_histogram(binwidth = 1) +  scale_fill_gradient(name = "Count", low = "orange", high = "red") + xlab("Order Total per Session (USD)") + ylab("Number of Orders") +
  labs(title = "Histogram of Session Cart Totals (USD)", subtitle = "Omits 13,353 sessions with totals greater than $250", caption = "Source: Point72 Asset Management")
price.hist

user.order4 <- subset(user.order2, user.order2$`count(user)` < 20)
user.hist <- ggplot(data=user.order4, aes(x = user.order4$`count(user)`, fill = ..count..)) + 
  geom_histogram(binwidth = 1) +  scale_fill_gradient(name = "Count", low = "light blue", high = "blue") + xlab("Number of Unique Purchases per User") + ylab("Number of Purchases") +
  labs(title = "Histogram of Recurring Purchases", subtitle = "Omits users with more than 20 recurring sessions", caption = "Source: Point72 Asset Management")
user.hist

##Disregard
user.order3 <- user.order2[which(user.order2$`count(user)` > 1),] #128,101 people/350,703 ordered more than once
user.order3 <- user.order3[,c(3,2)]
colnames(user.order3) <- c("region", "value")
user.order3 <- sqldf("select distinct region, sum(value) from 'user.order3' group by region")
colnames(user.order3) <- c("region", "value")
zips3 <- merge(user.order3, zip.regions2, by = "region", all.y = TRUE)
zips3$value <- ifelse(is.na(zips3$value),0,zips3$value)
zips3 <- sqldf("select distinct * from zips3")