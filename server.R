#---Retail Business, Exploratory Data Analisys--------

library(tidyverse)
library(tidyquant)
library(modelr)
library(gridExtra)
library(grid)
library(shiny)

gbretail <- read_csv("./data/gb.csv",
                     col_types = cols(
                       InvoiceNo = col_character(),
                       StockCode = col_character(),
                       Description = col_character(),
                       Quantity = col_integer(),
                       InvoiceDate = col_datetime("%m/%d/%Y %H:%M"),
                       UnitPrice = col_double(),
                       CustomerID = col_integer(),
                       Country = col_character()
                     )) %>%
  mutate(day = parse_date(format(InvoiceDate, "%Y-%m-%d")),
         day_of_week = wday(day, label = TRUE),
         time = parse_time(format(InvoiceDate, "%H:%M")),
         month = format(InvoiceDate, "%m"),
         income = Quantity * UnitPrice,
         income_return = ifelse(Quantity > 0, "income", "return"))
#write_csv(gbretail,"G:/ts-forecasting/gbretail.csv")

mp1 <- gbretail %>%
  filter(Country == "United Kingdom")

gmp1 <- ggplot(mp1,aes(x = Country, fill = income_return)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = palette_light()) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(fill = FALSE) +
  labs(x = "")
gmp1

mp2 <- gbretail %>%
  filter(Country != "United Kingdom")

gmp2 <-  ggplot(mp2,aes(x = Country, fill = income_return)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = palette_light()) +
  theme_tq() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "",
       fill = "")
gmp2

mm1 <- grid.arrange(gmp1, gmp2,widths = c(0.2, 0.8))
plot(mm1)
mp3 <- gbretail %>%
  ggplot(aes(x = day, color = income_return)) +
  facet_grid(income_return ~ ., scales = "free") +
  geom_freqpoly(bins = 100, size = 1, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  guides(color = FALSE) +
  labs(title = "Number of purchases/returns over time",
       x = "")
mp4 <- gbretail %>%
  ggplot(aes(x = day, y = ..density.., color = income_return)) +
  geom_freqpoly(size = 1, alpha = 0.8, bins = 100) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  labs(title = "Density of purchases/returns over time",
       x = "",
       color = "")
#----------------
mp5 <- gbretail %>%
  group_by(day, income_return) %>%
  summarise(sum_income = sum(income))

mmp5 <- mp5 %>%
  ggplot(aes(x = day, y = sum_income, color = income_return)) +
  facet_grid(income_return ~ ., scales = "free") +
  geom_ref_line(h = 0, colour = "grey") +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  guides(color = FALSE) +
  labs(title = "Income/loss from transactions per day",
       x = "",
       y = "sum of income/losses",
       color = "")
mmp5
#-----Income from purchases/returns per time of day-----
mnp6 <- gbretail %>%
  group_by(time, income_return) %>%
  summarise(sum_income = sum(income))

mp6 <- ggplot(mnp6, aes(x = time, y = sum_income, color = income_return)) +
  facet_grid(income_return ~ ., scales = "free") +
  geom_ref_line(h = 0, colour = "grey") +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  guides(color = FALSE) +
  labs(title = "Income from purchases/returns per time of day",
       x = "time of day",
       y = "sum of income/losses",
       color = "")
mp6
#----Finding the extreme outliers-----
gmp6 <- gbretail %>%
  filter(day == "2011-01-18") %>%
  arrange(-Quantity) %>%
  .[1:10, ]
gmp6m <- gbretail %>%
  filter(day == "2011-01-18") %>%
  arrange(Quantity) %>%
  .[1:3, ]
gmp7 <- gbretail %>%
  filter(CustomerID == 12346)

#-----Indicates transaction with colours-----
gcolour_trans <- gbretail %>%
  ggplot(aes(x = time, y = day)) +
  stat_bin2d(alpha = 0.8, bins = 25, color = "white") +
  scale_fill_gradientn(colours = c(palette_light()[[1]], palette_light()[[2]])) +
  theme_tq() +
  theme(legend.position = "right") +
  labs(title = "Purchases/returns per day and time")

#-----Day of the month transaction------
day_mont_transac <- gbretail %>%
  mutate(day2 = format(InvoiceDate, "%d")) %>%
  group_by(month, day2) %>%
  summarise(sum_income = sum(income))
day_mont_transac

gday_mont_transac <-  ggplot(day_mont_transac,aes(x = month, y = day2, fill = sum_income)) +
  geom_tile(alpha = 0.8, color = "white") +
  scale_fill_gradientn(colours = c(palette_light()[[1]], palette_light()[[2]])) +
  theme_tq() +
  theme(legend.position = "right") +
  labs(title = "Net income per month and day",
       y = "day of the month",
       fill = "net sum of income")
gday_mont_transac 
#-----Item being purchased or return-----
gpurch_retur <- gbretail %>%
  group_by(StockCode, Description) %>%
  summarise(sum = sum(Quantity)) %>%
  arrange(-sum)
gpurch_retur
#-----Mayority item purchased------

g_p1 <- gbretail %>%
  group_by(StockCode, Description) %>%
  summarise(sum = sum(Quantity)) %>%
  ggplot(aes(x = sum)) +
  geom_density(fill = palette_light()[[1]], alpha = 0.8) +
  theme_tq()

g_p2 <- gbretail %>%
  group_by(StockCode, Description) %>%
  summarise(sum = sum(Quantity)) %>%
  filter(sum > 1) %>%
  ggplot(aes(x = sum)) +
  geom_density(fill = palette_light()[[1]], alpha = 0.8) +
  theme_tq()

g_p3 <- gbretail %>%
  group_by(StockCode, Description) %>%
  summarise(sum = sum(Quantity)) %>%
  filter(sum > 10000) %>%
  ggplot(aes(x = sum)) +
  geom_density(fill = palette_light()[[1]], alpha = 0.8) +
  theme_tq()

g_majority<- grid.arrange(g_p1, g_p2, g_p3, ncol = 3)
plot(g_majority) 

#-----Most sold----
gmost_sold <- gbretail %>%
  group_by(day, StockCode, Description) %>%
  summarise(sum = sum(Quantity)) %>%
  group_by(StockCode, Description) %>%
  summarise(n = n()) %>%
  arrange(-n)
gmost_sold

g_tran_whitehanging <- gbretail %>%
  filter(StockCode == "85123A") %>%
  group_by(day, income_return) %>%
  summarise(sum = sum(Quantity)) %>%
  ggplot(aes(x = day, y = sum, color = income_return)) +
  facet_wrap(~ income_return, ncol = 1, scales = "free") +
  geom_line(size = 1, alpha = 0.5) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  labs(x = "",
       y = "sum of quantities",
       color = "",
       title = "Transactions of WHITE HANGING HEART T-LIGHT HOLDER")
g_tran_whitehanging

#-----Preparing data for modeling by day-----

#-----Which customers are repeat customers?-----

grep_customer <- gbretail %>%
  group_by(day, CustomerID) %>%
  summarise(sum = sum(Quantity)) %>%
  group_by(CustomerID) %>%
  summarise(n = n()) %>%
  mutate(repeat_customer = ifelse(n > 1, "repeat_cust", "one_time_cust"))

length(which(grep_customer$repeat_customer == "repeat_cust"))
## [1] 1536
grep_customer_day <- left_join(gbretail, grep_customer, by = "CustomerID") %>%
  distinct(day, CustomerID, repeat_customer) %>%
  group_by(day, repeat_customer) %>%
  summarise(n = n()) %>%
  spread(key = repeat_customer, value = n)
grep_customer_day #-table onetime and repeat customer

gg_grep_customer <- grep_customer %>%
  group_by(repeat_customer) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = repeat_customer)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = palette_light()) +
  theme_tq() +
  theme(legend.position = "right") +
  labs(x = "",
       y = "",
       fill = "",
       title = "Proportion of one-time & repeat customers")
gg_grep_customer

#-----Transactions, quantities and items per customer and day-----------
#---I am calculating the following metrics per day and customer.
#•	n: number of different items purchased/returned per customer per day
#•	sum_it: net sum of items (quantity) purchased/returned per customer per day
#•	sum_in: mean net income per customer per day
#---From these, I can further calculate mean numbers per day:
#•	mean_in_cust: mean net income from all customers per day
#•	mean_quant_cust: mean net quantities of items from all customers per day
#•	mean_items_cust: mean number of items from all customers per day

gcustomer_purch <- gbretail %>%
  group_by(day, CustomerID) %>%
  summarise(n = n(),
            sum_it = sum(Quantity),
            sum_in = sum(income)) %>%
  group_by(day) %>%
  summarise(mean_in_cust = mean(sum_in),
            mean_quant_cust = mean(sum_it),
            mean_items_cust = mean(n))
gcustomer_purch
#----------
g_gcustomer_purch <- gcustomer_purch %>%
  gather(x, y, mean_in_cust:mean_items_cust) %>%
  ggplot(aes(x = day, y = y)) +
  facet_wrap(~ x, ncol = 1, scales = "free") +
  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
  geom_smooth(color = palette_light()[[2]], method = 'loess') +
  theme_tq() +
  labs(x = "", 
       y = "")
g_gcustomer_purch

#-----Purchases/returns per day-------
#This calculates the quantities of 
#items that are purchased and returned per day:
gincome_return <- gbretail %>%
  group_by(day, income_return) %>%
  summarise(sum = sum(Quantity)) %>%
  spread(key = income_return, value = sum) %>%
  gather(x, y, income:return) %>%
  ggplot(aes(x = day, y = y, color = x)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = palette_light()) +
  theme_tq() +
  labs(x = "", 
       y = "quantity of items",
       color = "")
gincome_return

#-----How many items are purchased/returned per country?-----

gcountry_purch <- gbretail %>%
  mutate(Country2 = ifelse(Country == "United Kingdom", "uk", "other_country")) %>%
  group_by(day, Country2) %>%
  summarise(sum = sum(Quantity)) %>%
  spread(key = Country2, value = sum) %>%
  mutate(prop_other_country = other_country / sum(other_country + uk),
         prop_uk = uk / sum(other_country + uk))
gcountry_purch

gg_gcountry_purch  <- gcountry_purch %>%
  gather(x, y, prop_other_country:prop_uk) %>%
  ggplot(aes(x = day, y = y)) +
  geom_bar(aes(fill = x), stat = "identity", alpha = 0.6) +
  scale_fill_manual(values = palette_light()) +
  geom_line(data = gcountry_purch, aes(x = day, y = prop_uk), size = 1) +
  theme_tq() +
  labs(x = "", 
       y = "proportion of quantity of items",
       fill = "")
gg_gcountry_purch

#-----How many different items are purchased/returned per day?------
gn_items <- gbretail %>%
  group_by(day, StockCode) %>%
  summarise(n = n()) %>%
  group_by(day) %>%
  summarise(n_items = n())
gn_items

gn_items1 <- gn_items %>%
  ggplot(aes(x = day, y = n_items)) +
  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
  geom_smooth(color = palette_light()[[2]], method = 'loess') +
  theme_tq() +
  labs(x = "", 
       y = "number of different items",
       color = "")
gn_items1
#-----Net income & quantities summaries-----
gincome <- gbretail %>%
  group_by(day) %>%
  summarise(sum_income = sum(income),
            mean_income = mean(income),
            sum_quantity = sum(Quantity),
            mean_quantity = mean(Quantity))
gincome

gg_income <- gincome %>%
  gather(x, y, sum_income:mean_quantity) %>%
  ggplot(aes(x = day, y = y)) +
  facet_wrap(~ x, ncol = 1, scales = "free") +
  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
  geom_smooth(color = palette_light()[[2]], method = 'loess') +
  theme_tq() +
  labs(x = "", 
       y = "")
gg_income

greturns <- gbretail %>%
  filter(income < 0) %>%
  group_by(day) %>%
  summarise(sum_income_return = sum(income),
            mean_income_return = mean(income),
            sum_quantity_return = sum(Quantity),
            mean_quantity_return = mean(Quantity))
g_greturns <- greturns %>%
  gather(x, y, sum_income_return:mean_quantity_return) %>%
  ggplot(aes(x = day, y = y)) +
  facet_wrap(~ x, ncol = 1, scales = "free") +
  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
  theme_tq() +
  labs(x = "", 
       y = "")

#-----Mean price per units sold per day-----

#use temporary data frame
temp <- distinct(select(gbretail, day, StockCode, UnitPrice)) %>%
  mutate(temp = paste(day, StockCode, sep = "_")) %>%
  select(temp, UnitPrice)
#-----Result data temp
mean_unit_price <- gbretail %>%
  filter(income_return == "income") %>%
  group_by(day, StockCode) %>%
  summarise(n = n()) %>%
  mutate(temp = paste(day, StockCode, sep = "_")) %>%
  left_join(temp, by = "temp") %>%
  group_by(day, StockCode) %>%
  summarise(mean = mean(UnitPrice)) %>%
  group_by(day) %>%
  summarise(mean_unit_price = mean(mean))
mean_unit_price

mean_unit_price1 <- mean_unit_price %>%
  ggplot(aes(x = day, y = mean_unit_price)) +
  geom_line(color = palette_light()[[1]], size = 1, alpha = 0.8) +
  theme_tq() +
  labs(x = "", 
       y = "mean unit price of sold items")
mean_unit_price1

#-----Purchases of most sold items-----

#The 10 items that have been sold on the most days throughout 
#the year of recording are also introduced as separate features.
most_sold_day <- gbretail %>%
  filter(StockCode %in% gmost_sold$StockCode[1:10]) %>%
  group_by(day, StockCode) %>%
  summarise(sum = sum(Quantity)) %>%
  spread(key = StockCode, value = sum)
most_sold_day

gmost <- gbretail %>%
  filter(StockCode %in% gmost_sold$StockCode[1:10]) %>%
  group_by(day, StockCode, Description) %>%
  summarise(sum = sum(Quantity))
gmost

pgmost <- ggplot(data = gmost, aes(x = day, y = sum)) +
  facet_wrap(~ StockCode, ncol = 2, scales = "free") +
  geom_line(color = palette_light()[[1]], size = 2, alpha = 0.5) +
  #theme_tq() +
  labs(x = "", 
       y = "net sum of quantites sold")#------sampai disini ok 
pgmost

server <- function(input, output) {
  output$plot_type <- renderPlot({
    
    if (input$plot_type == "Transaction UK") {
      plot(gmp1)
    }else if (input$plot_type == "Transaction all country") {
      plot(gmp2)
    }else if (input$plot_type == "Transaction UK & Transaction all country") {
      plot(mm1)
    }else if (input$plot_type == "Transaction over time") {
      mp3
    }else if (input$plot_type == "Income and Return Transaction") {
      mmp5
    }else if (input$plot_type == "Income from purchases/returns per time of day") {
      mp6
    }else if (input$plot_type == "Purchased return per day per time") {
      gcolour_trans
    }else if (input$plot_type == "Net income comparing month and day") {
      gday_mont_transac
    }else if (input$plot_type == "Majority item purchased") {
      plot(g_majority)
    }else if (input$plot_type == "Transactions of WHITE HANGING HEART T-LIGHT HOLDER") {
      plot(g_tran_whitehanging)
      # }else if (input$plot_type == "Purchased most often per day") {
      #  g_tran_whitehanging
    }else if (input$plot_type == "Proportion one time and repeat customer") {
      gg_grep_customer
    }else if (input$plot_type == "Proportion of quantity of items") {
      gg_gcountry_purch
    }else if (input$plot_type == "Mean unit price of sold items") {
      mean_unit_price1
    }else if (input$plot_type == "Diff items are purchased and return per day") {
      gn_items1
    }else if (input$plot_type == "Net income and quant summaries") {
      gg_income
    }else if (input$plot_type == "Mean price per unit sold per day") {
      g_greturns
    }else if (input$plot_type == "Purchased most sold per items") {
      pgmost
    }
  })
  output$gmost_sold <- renderDataTable({
    gmost_sold}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$day_mont_transac <- renderDataTable({
    day_mont_transac}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gpurch_retur <- renderDataTable({
    gpurch_retur}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5)) 
  output$gcustomer_purch <- renderDataTable({
    gcustomer_purch}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5)) 
  output$gcountry_purch <- renderDataTable({
    gcountry_purch}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gn_items <- renderDataTable({
    gn_items}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$grep_customer_day <- renderDataTable({
    grep_customer_day}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$mnp6 <- renderDataTable({
    mnp6 }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gmp6 <- renderDataTable({
    gmp6  }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gmp6m <- renderDataTable({
    gmp6m }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gmp7 <- renderDataTable({
    gmp7}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gincome<- renderDataTable({
    gincome}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$mean_unit_price<- renderDataTable({
    mean_unit_price}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$gmost<- renderDataTable({
    gmost}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
}

