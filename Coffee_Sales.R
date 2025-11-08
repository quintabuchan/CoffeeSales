# INTRODUCTION

# This analysis focuses on analysing the monthly and quarterly coffee sales from a fictional vending machine company. 
# It looks at the distribution of orders for each beverage, how the preferred method of transaction changes over time, 
# and purchasing patterns of customer behaviour.

# DATA PREPARATION

# Load Packages & Libraries

install.package("pacman")

pacman::p_load("lubridate", "dplyr", "readr", "ggplot2", "purrr", "scales", "magrittr", "ggpubr", "forcats", "see", "stringr", "ggridges", "forecast", "tidyr")

# Load & Transform Data File

Coffee_Sales <- read.csv("Data/index_1.csv") |> 
  select(date,
         datetime,
         cash_type,
         money,
         coffee_name) |> 
  mutate(date = as.Date(date)) |> 
  mutate(datetime = as_datetime(datetime)) |> 
  mutate(year = format(date, format = "%Y")) |> 
  mutate(month = format(date, format = "%B")) |> 
  mutate(day = format(date, format = "%A")) |> 
  mutate(time = format(as.POSIXct(datetime), format = "%H:%M")) |> 
  mutate(hour = format(as.POSIXct(datetime), format = "%I%p")) |> 
  mutate(quarter = quarter(date, with_year = TRUE)) |> 
  mutate(Month = floor_date(date, "month")) |> 
  mutate(Week = floor_date(date, "week")) |> 
  mutate(Day = floor_date(date, "day")) |> 
  mutate(Quarter = floor_date(date, "quarter"))

Coffee_Sales$day <- factor(Coffee_Sales$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

Coffee_Sales$month <- factor(Coffee_Sales$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

Coffee_Sales$hour <- factor(Coffee_Sales$hour, levels = c("06AM", "07AM", "08AM", "09AM", "10AM", "11AM", "12PM", "01PM", "02PM", "03PM", "04PM", "05PM", "06PM", "07PM", "08PM", "09PM", "10PM"))

# Summary

summary(Coffee_Sales)

# Palette and Theme

CoffeePalette <- colorRampPalette(c("#103778",
                                    "#0593A2",
                                    "#FF7A48",
                                    "#E3371E"))

MainTheme <- function(){
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "#C2C2C2"),
    legend.text = element_text(colour = "#151F30", size = 12),
    legend.title = element_text(colour = "#151F30", size = 12, hjust = .5),
    legend.direction = "horizontal",
    legend.box = "vertical",
    guides(colour = guide_legend(nrow = 2, byrow = T)), 
    plot.subtitle = element_text(colour = "#151F30", size = 14, hjust = 0.5, vjust = 0),
    plot.title = element_text(size = 20, colour = "#151F30", hjust = .5, vjust = 0.5), 
    plot.caption = element_text(colour = "#151F30", size = 12, hjust = 0.5, vjust = .5),
    axis.title.x = element_text(colour = "#151F30"),
    axis.title.y = element_text(colour = "#151F30"),
    axis.text = element_text(colour = "#151F30", size = 12),
    axis.text.x = element_text(hjust = 1, colour = "#151F30",),
    axis.ticks = element_line(colour = "#D3D3D3"),
    axis.line = element_line(colour = "#D3D3D3"),
    panel.background = element_rect(fill = "#C2C2C2"),
    panel.grid = element_line(colour = "#D3D3D3"),
    plot.margin = margin(1,2,1,1),
    strip.text = element_text(colour = "#151F30", size = 10),
    strip.background = element_rect(fill = "#C2C2C2", colour = "#E3371E", linewidth = 1, linetype = "solid"),
    plot.background = element_rect(fill = "#C2C2C2"))
}

# DATA VISUALISATION

# Total Sales & Orders

TotalSales <- Coffee_Sales |> group_by(year) |> summarise(TotalUSDSales = sum(money))

(AnnualSales <- ggplot(TotalSales, aes(TotalUSDSales, year, fill = year)) +
    geom_col(alpha = 0.75) +
    geom_text(aes(label = scales::dollar(TotalUSDSales)), colour = "#151F30", size = 4, position = position_stack(vjust = 0.5, reverse = T), fontface = "bold") +
    scale_fill_manual(values = CoffeePalette(2)) +
    MainTheme() +
    theme(legend.position = "none") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Total Orders for each Beverage",
      y = "",
      x = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"
    ))

TotalOrders <- Coffee_Sales |> count(coffee_name, name = "TotalOrders")

(TotalDrinkOrders <- ggplot(TotalOrders, aes(TotalOrders, fct_infreq(coffee_name, TotalOrders), fill = coffee_name)) +
    geom_col(alpha = 0.75) +
    geom_text(aes(label = TotalOrders), colour = "#151F30", size = 4, position = position_stack(vjust = 0.5, reverse = T), fontface = "bold") +
    scale_fill_manual(values = CoffeePalette(8)) +
    MainTheme() +
    theme(legend.position = "none") +
    scale_y_discrete(limits = rev) +
    labs(
      title = "Total Orders for each Beverage",
      y = "",
      x = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"
    ))

# Monthly and Quarterly Profit

MonthSales <- Coffee_Sales |> 
  group_by(Month) |> 
  summarise(MonthSales = sum(money))

(MonthlySales <- ggplot(MonthSales, aes(Month, MonthSales)) +
    geom_smooth(colour = "#E3371E", method = "lm", formula = y ~ x, linewidth = 1.5, linetype = "2262", alpha = 0.4) +
    geom_point(colour = "#0593A2", shape = 15, alpha = 0.75, size = 6) +
    stat_cor(method = "spearman", label.y.npc = "top", aes(label = after_stat(r.label)), colour = "#E3371E") +
    geom_line(colour = "#0593A2") +
    MainTheme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_breaks = "4 week", date_labels = "%b-%y") + 
    labs(
      title = "Monthly Sales",
      colour = "Transaction method:",
      x = "",
      y = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"))


TotalMonthProfit <- Coffee_Sales |> 
  group_by(Quarter, month, coffee_name) |> 
  summarise(MonthlyProfit = sum(money),
            .groups = "keep") 

(monthlyProfit <- ggplot(TotalMonthProfit, aes(MonthlyProfit, month, fill = coffee_name)) +
    geom_col(alpha = 0.75) +
    scale_fill_manual(values = CoffeePalette(8)) +
    MainTheme() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = scales::dollar_format()) +
    facet_wrap(~Quarter, scales = "free_y") +
    labs(
      title = "Total Monthly Sales for each Quarter",
      y = "",
      x = "",
      fill = "Beverage:",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"
    ))

(profitVio <- ggplot(TotalMonthProfit, aes(MonthlyProfit, month)) +
    geom_jitter(aes(colour = coffee_name, size = MonthlyProfit), alpha = 0.75, shape = 15, width = 15) +
    stat_boxplot(geom = "errorbar", colour = "#151F30", linewidth = 2) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75), fill = "#151F30", colour = "#151F30", alpha = 0.6, linewidth = 1) +
    scale_colour_manual(values = CoffeePalette(8)) +
    MainTheme() +
    scale_x_continuous(labels = scales::dollar_format()) +
    facet_wrap(~Quarter, scales = "free_y") +
    scale_y_discrete(limits = rev) + 
    labs(
      title = "Distribution of Total Monthly Profit for each Quarter",
      colour = "Beverage:",
      size = "Monthly Profit:",
      y = "",
      x = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"
    ))

(Quartermoney <- ggplot(Coffee_Sales, aes(money)) +
    geom_histogram(aes(y = after_stat(density), fill = coffee_name), linewidth = 1.5, alpha = 0.75, binwidth = 5) +  
    stat_summary(
      aes(xintercept = after_stat(x), y = 0),
      fun = mean, geom = "vline", orientation = "y", 
      colour = "#FFFFFF", linetype = "dashed", linewidth = 1.5, alpha = 0.5) +
    stat_summary(
      aes(xintercept = after_stat(x), y = 0),
      fun = quantile, geom = "vline", orientation = "y", 
      colour = "#00868B", linetype = "dashed", linewidth = 1.5, alpha = 0.5) +
    geom_density(colour = "#151F30", linewidth = 1.5, alpha = 0.75) +
    scale_fill_manual(values = CoffeePalette(8)) +
    facet_wrap(~Quarter) +
    MainTheme() +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Distribution of Sales for each Quarter",
      x = "",
      fill = "Beverage:",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"))

# Monthly and Quarterly Orders

OrderQuantity <- Coffee_Sales |> 
  count(Quarter, month, coffee_name, name = "Quantity") |>
  group_by(Quarter)

(Orders <- ggplot(OrderQuantity, aes(Quantity, fct_infreq(coffee_name, Quantity), fill = coffee_name)) +
    geom_col(alpha = 0.85, position = position_stack(reverse = T)) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = CoffeePalette(8)) +
    MainTheme() +
    theme(legend.position = "none") +
    facet_wrap(~Quarter) +
    labs(
      title = "Total Number of Orders for each Quarter",
      y = "",
      x = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"
    ))

(OrderViolin <- ggplot(OrderQuantity, aes(Quantity, month,)) +
    geom_jitter(aes(colour = coffee_name, size = Quantity), alpha = 0.75, shape = 15, width = 15) +
    stat_boxplot(geom = "errorbar", colour = "#151F30", linewidth = 2) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75), colour = "#151F30", alpha = 0.6, fill = "#151F30", linewidth = 1) +
    facet_wrap(~Quarter, scales = "free_y") +
    scale_colour_manual(values = CoffeePalette(8)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10), limits = rev) +
    MainTheme() +
    labs(title = "Distribution of Orders for each Month",
         colour = "Beverage:",
         size = "Quantity:",
         x = "",
         y = "",
         caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"))

# Transactions Types Over Time

QuarterSales <- Coffee_Sales |> 
  group_by(Week, quarter, cash_type) |> 
  summarise(TotalSales = sum(money),
            .groups = "keep")

(SalesOverTime <- ggplot(QuarterSales, aes(Week, TotalSales)) +
    geom_smooth(aes(colour = cash_type), method = "lm", formula = y ~ x, linewidth = 1.5, linetype = "2262", alpha = 0.4) +
    geom_point(aes(colour = cash_type), shape = 15, alpha = 0.75, size = 6) +
    geom_line(aes(colour = cash_type, group = cash_type)) +
    facet_wrap(~quarter, scales = "free_x") +
    scale_colour_manual(values = CoffeePalette(2)) +
    MainTheme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Total Quarterly Sales Across for each Transaction Method",
      colour = "Transaction method:",
      x = "",
      y = "",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"))

(TransactionTime <- ggplot(Coffee_Sales, aes(money, cash_type)) +
    geom_jitter(aes(colour = coffee_name), size = 5, alpha = 0.35, shape = 15, width = 15) +
    stat_boxplot(geom = "errorbar", colour = "#151F30", linewidth = 2) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75), colour = "#151F30", alpha = 0.6, fill = "#151F30", linewidth = 1) +
    facet_wrap(~Quarter) +
    scale_colour_manual(values = CoffeePalette(8)) +
    scale_x_continuous(labels = scales::dollar_format()) +
    MainTheme() +
    labs(title = "Distribution of Quarterly Sales for each Transaction Type",
         colour = "Beverage:",
         x = "",
         y = "",
         caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales")
)


# Most Profitable Times of Day

PeriodProfit <- Coffee_Sales |> 
  group_by(Quarter, hour, coffee_name) |> 
  summarise(Profit = sum(money),
            .groups = "keep") |> 
  arrange(desc(Profit), .by_group = T)

(TopSales <- ggplot(PeriodProfit, aes(Profit, hour, fill = coffee_name)) +
    geom_col(alpha = 0.75, position = position_stack(reverse = T)) +
    scale_fill_manual(values = CoffeePalette(8)) +
    facet_wrap(~Quarter) +
    MainTheme() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Total Profits by Hour for Each Quarter",
      x = "",
      y = "",
      fill = "Beverage:",
      caption = "Source: https://www.kaggle.com/datasets/ihelon/coffee-sales"))


# CONCLUSION

# There are no discernible patterns, regarding which beverage will have the highest sales or orders, though there is a moderate 
# correlation between sales increasing over time. 
# The exception to this is how cash became a phased out type of transaction during the second quarter in 2024. 


