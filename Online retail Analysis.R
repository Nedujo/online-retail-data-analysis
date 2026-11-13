# =================================
# ONLINE RETAIL SALES ANALYSIS
# =================================

# 1. Clean the sales dataset

sales_data <- retail[
  !is.na(retail$CustomerID) &
    retail$Quantity > 0 &
    !grepl("^C", retail$InvoiceNo),
]

# 2. Check dataset dimensions

dim(sales_data)

# 3. Create Revenue column

sales_data$Revenue <- sales_data$Quantity *
  sales_data$UnitPrice

# 4. Calculate total revenue

sum(sales_data$Revenue, na.rm = TRUE)

# 5. Calculate revenue by country

country_revenue <- aggregate(
  Revenue ~ Country,
  data = sales_data,
  FUN = sum
)

# 6. Rank countries by revenue

country_revenue <- country_revenue[
  order(-country_revenue$Revenue), ]

# 7. Display top 10 countries

top10 <- head(country_revenue, 10)

print(top10)

# 8. Create country revenue bar chart

barplot(
  top10$Revenue,
  names.arg = top10$Country,
  las = 2,
  main = "Top 10 Countries by Revenue",
  ylab = "Revenue (GBP)",
  col = "skyblue"
)
