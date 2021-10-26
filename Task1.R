#title: "Quantium Virtual Internship - Retail Strategy and Analytics - Task 1"

library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
# Examine transaction data 

#### Creating local dataset
filePath <- "D:/Parisa/Internship/Quantium/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))
str(customerData)
str(transactionData)
transactionData
head(transactionData)

##We saw that the date format is in _numeric_ format which is wrong so we convert it to the `date` format as shown below**
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
### Examine PROD_NAME

summary(transactionData$PROD_NAME)
#### Examine the words in PROD_NAME to see if there are any incorrect entries 
#### such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), "
")))
setnames(productWords, 'Chips')
productWords
#### Removing digits
productWords$Chips <- str_replace_all(productWords$words,"[0-9]"," ")
productWords$Chips <- str_replace_all(productWords$words,"[gG]"," ")
#### Removing special characters
productWords$Chips <- str_replace_all(productWords$Chips,"[[:punct:]]"," ")
#### Let's look at the most common words by counting the number of times a word appears and  
wordsSep <- strsplit(productWords$Chips," ")
words.freq<-table(unlist(wordsSep))
#### sorting them by this frequency in order of highest to lowest frequenc
words.freq <-  as.data.frame(words.freq)
words.freq <- words.freq[order(words.freq$Freq, decreasing = T),]
words.freq
transactionData[!grepl("&",productWords),] 
tail(names(sort(table(transactionData$PROD_NAME))), 1)
#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]
summary(transactionData)
#### Summarise the data to check for nulls and possible outliers
is.null(transactionData$PROD_NAME)


#### Filter the dataset to find the outlier
library(tidyverse)
library(dplyr)
prod_qty_200 <- transactionData %>% filter(PROD_QTY==200)
#### Let's see if the customer has had other transactions

same_customer <- transactionData %>% filter(LYLTY_CARD_NBR == 226000) 
#### Removing this customer from the list
transactionData <- transactionData[!(transactionData$LYLTY_CARD_NBR == 226000)]
### Re-examine transaction data
summary(transactionData)
#### Count the number of transactions by date
countByDate <- count(transactionData, transactionData$DATE)
countByDate
nrow(countByDate)
summary(countByDate)


#### Create a sequence of dates and join this the count of transactions by date

transaction_by_day <- transactionData[order(DATE),]

sedate <-seq(as.Date("2018-07-01"),by="day",length.out=364)
## Join to the main Table
Jointbydate <- cbind(countByDate,sedate )
#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#### Plot transactions over time
transaction_data <- transactionData
mytransOverTime <- ggplot(countByDate, aes(x = countByDate$`transactionData$DATE`, y = countByDate$n)) +
  geom_line(color = "#00AFBB") +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

 mytransOverTime
#### Filter to December and look at individual days
 filterplot <- countByDate[countByDate$`transactionData$DATE` >= "2018-12-01" & countByDate$`transactionData$DATE` <= "2018-12-31"]
 
 ggplot( filterplot, aes(x =  filterplot$`transactionData$DATE`, y =  filterplot$n)) +
   geom_line(color = "#00AFBB") +
   labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
   scale_x_date(breaks = "1 day") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 #### Pack size
 #### We can work this out by taking the digits that are in PROD_NAME
 transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
 #### Always check your output
 #### Let's check if the pack sizes look sensible 
  Histo<- transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]
 #### Let's plot a histogram of PACK_SIZE since we know that it is a categorical 
 #variable and not a continuous variable even though it is numeric.
  hist(transactionData[, PACK_SIZE])
  ggplot(transactionData, aes(x=PACK_SIZE)) + geom_histogram(color="darkblue", fill="lightblue" ,bins=15)
 #### Brands
 ##Create a column which contains the brand of the product, by 
 #extracting it from the product name.
  transactionData$BRAND <- gsub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)
  transactionData[, .N, by = BRAND][order(N)]
  
 library(stringr)
 transactionData$PROD_Brand <- word(transactionData$PROD_NAME, 1)
 #### Checking brands
 #### Clean brand names
 transactionData[BRAND == "RED", BRAND := "RRD"]
 transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
 transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
 transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
 transactionData[BRAND == "Woolworths", BRAND := "WOOLWORTHS"]
 transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
 transactionData[BRAND == "NCC", BRAND := "NATURAL"]
 transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
 transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]
 
 ###Check again
 transactionData[, .N, by = BRAND][order(BRAND)]
 ## Examining customer data
 summary(customerData)
 #### Examining the values of lifestage and premium_customer
 customerData[, .N, by = LIFESTAGE][order(N)]
 customerData[, .N, by = PREMIUM_CUSTOMER][order(N)]
 #### Merge transaction data to customer data
 data <- merge(transactionData, customerData, all.x = TRUE)
 #### Let's also check if some customers were not matched on by checking for nulls.
 apply(data, 2, function(x) any(is.na(x)))
 #write this dataset into a csv file
 write.csv(data,paste0("D:/Parisa/Internship/Quantium/","QVI_data.csv"))
 #### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
 total_sales <- data %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)
 pf.total_sales <- summarise(total_sales,sales_count=sum(TOT_SALES))
 summary(pf.total_sales)
 #### Create plot
 library (ggmosaic)
  # install.packages("devtools")
 #install.packages("cli")
  ##devtools::install_github("haleyjeppson/ggmosaic")
 p <- ggplot(pf.total_sales) + geom_mosaic(aes(weight = sales_count, x = product(PREMIUM_CUSTOMER, LIFESTAGE),fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
 p +geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))), inherit.aes = F)
 
 ### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER and create a plot
 total_sales <- data %>% group_by(LIFESTAGE,PREMIUM_CUSTOMER)
 no_of_customers <- summarise(total_sales,customer_count = length(unique(LYLTY_CARD_NBR))) 
  summary(no_of_customers)
 p <- ggplot(data = no_of_customers) + geom_mosaic(aes(weight = customer_count, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))
 p
 #### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
 
 units <-  summarise(total_sales, units_count = (sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)))
 summary(units)
 ###create plot
 ggplot(data = units, aes(weight = units_count, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + geom_bar(position = position_dodge()) +
   labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 check <- units[order(units$units_count, decreasing = T),]
 #### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
 pricePerUnit <- summarise(total_sales,price_per_unit = (sum(TOT_SALES)/sum(PROD_QTY))) 
 ###plot
 ggplot(data=pricePerUnit, aes(weight = price_per_unit,x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + geom_bar(position = position_dodge()) + labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 #### Perform an independent t-test between mainstream vs premium and budget midage 
 #### and young singles and couples
 pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
 t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price],data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price], alternative = "greater")
 ### Deep dive into Mainstream, young singles/couples 
 segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
 other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER =="Mainstream"),]
 #### Brand affinity compared to the rest of the population
 quantity_segment1 <- segment1[, sum(PROD_QTY)]
 quantity_other <- other[, sum(PROD_QTY)]
 quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]
 quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]
 brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
 brand_proportions[order(affinityToBrand)]
 ggplot(brand_proportions, aes(brand_proportions$BRAND,brand_proportions$affinityToBrand)) + geom_bar(stat = "identity",fill = "yellow") + labs(x = "Brand", y = "Customers Affinity to Brand", title = "Favorite brands of Customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 #### Preferred pack size compared to the rest of the population
 quantity_segment1_by_pack<- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
 quantity_other_by_pack<- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]
 brand_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityTopack := targetSegment/other]
 brand_proportions[order(affinityTopack)]
 ggplot(brand_proportions, aes(brand_proportions$PACK,brand_proportions$affinityTopack)) + geom_bar(stat = "identity",fill = "yellow") + labs(x = "Brand", y = "Customers Affinity to Brand", title = "Favorite brands of Customers") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 data[PACK_SIZE == 270, unique(PROD_NAME)]
 