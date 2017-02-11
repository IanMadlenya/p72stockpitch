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
