library(dplyr)
library(plyr)
library(ggplot2)

setwd("C:/Users/Selvy Andy Wijaya/Documents/R")
dataset = read.csv("Skill-Academy.csv")

#jawaban no 1
dataset_1 = select(dataset, c(customer_id, sales)) %>% group_by(customer_id) %>% summarise_all(sum)
jawaban_1 = dataset_1[which.max(dataset_1$sales),]

#jawaban no 2
dataset_2 = select(dataset, c(sub_category, profit))
jawaban_2 = dataset_2 %>% group_by(sub_category) %>% summarise_all(sum)

#jawaban no 3
data3 = data3 = select(dataset, c(sub_category, quantity)) %>% group_by(sub_category) %>% summarise_all(sum)
dataset_3 = inner_join(jawaban_2,data3)
names(dataset_3) [3] = "order"
jawaban_3 = dataset_3 %>% filter(profit < 0) %>% summarise(total_order=sum(order), profit_negatif=sum(profit < 0))

#jawaban no 4
data4 = filter(dataset_1, customer_id %in% c("JE-16165", "KH-16510", "AD-10180"))
jawaban_4 = arrange(data4, desc(sales)) %>% head(1)

#jawaban no 5
data5 = select(dataset, c(order_date, customer_id, sales, profit)) %>% group_by(order_date, customer_id) %>% summarise_all(sum)
data5$year = substr(data5$order_date, 1, 4)
a = ddply(data5, "year", numcolwise(sum))
b = aggregate(customer_id ~ year, data5, function(x) length(unique(x)))
names(b) [2] = "total_customer"
dataset_5 = inner_join(a, b)
jawaban_5 = dataset_5[which.max(dataset_5$profit),]

#jawaban no 6
dataset$year = substr(dataset$order_date, 1, 4)
dataset_6 = select(dataset, c(order_date, year, sales, profit)) %>% group_by(order_date, year) %>% filter(year > 2013 & year < 2016) %>% summarise_all(sum)
ggplot(dataset_6, aes(x = sales, y = profit, group = year, colour = year)) + geom_point() + labs (title = "Sales vs Profit 2014-2015")

#jawaban no 7
data7 = select(dataset, c(year, customer_id, sales, profit)) %>% group_by(year, customer_id) %>% filter(year == 2015) %>% summarise_all(sum) %>% arrange(desc(sales)) %>% head(10)
ggplot(data7, aes(x = customer_id, y = profit)) + geom_bar(stat = "identity", width = 0.3, fill = "blue")
