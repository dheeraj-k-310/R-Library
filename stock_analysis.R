required_packages <- c(
  "quantmod",
  "ggplot2",
  "dplyr",
  "forecast",
  "TTR",
  "corrplot"
)

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(quantmod)
library(ggplot2)
library(dplyr)
library(forecast)
library(TTR)
library(corrplot)

stock_symbol <- "AAPL"
start_date <- "2022-01-01"
project_dir <- getwd()
output_dir <- file.path(project_dir, "stick_data_csv")
plot_dir <- file.path(project_dir, "stock_plots")

getSymbols(stock_symbol, src = "yahoo", from = start_date)

stock_xts <- get(stock_symbol)
stock <- data.frame(date = index(stock_xts), coredata(stock_xts))

colnames(stock) <- c(
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume",
  "adjusted"
)

stock$Return <- c(NA, as.numeric(dailyReturn(Cl(stock_xts))))
stock$MA50 <- as.numeric(SMA(Cl(stock_xts), 50))
stock$MA200 <- as.numeric(SMA(Cl(stock_xts), 200))
stock$RSI <- as.numeric(RSI(Cl(stock_xts)))
stock$Volatility20 <- as.numeric(runSD(stock$Return, n = 20))
stock$Drawdown <- stock$close / cummax(stock$close) - 1

bb <- BBands(Cl(stock_xts))
stock$Upper <- as.numeric(bb$up)
stock$Lower <- as.numeric(bb$dn)
stock$Middle <- as.numeric(bb$mavg)

macd_values <- MACD(Cl(stock_xts), 12, 26, 9)
stock$MACD <- as.numeric(macd_values$macd)
stock$Signal <- as.numeric(macd_values$signal)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, paste0(tolower(stock_symbol), "_stock_data.csv"))
write.csv(stock, output_file, row.names = FALSE)

cat("\nStock data saved to:", output_file, "\n")

cat("\nFirst few rows of stock data:\n")
print(head(stock))

latest_row <- stock %>%
  filter(!is.na(Return)) %>%
  tail(1)

monthly_returns <- stock %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    monthly_return = (last(close) / first(close)) - 1,
    .groups = "drop"
  )

cat("\nLatest Market Snapshot:\n")
cat("Date:", as.character(latest_row$date), "\n")
cat("Close Price:", round(latest_row$close, 2), "\n")
cat("Daily Return:", round(latest_row$Return * 100, 2), "%\n")
cat("RSI:", round(latest_row$RSI, 2), "\n")
cat("20 Day Volatility:", round(latest_row$Volatility20 * 100, 2), "%\n")
cat("Current Drawdown:", round(latest_row$Drawdown * 100, 2), "%\n")

cat("\nAdditional Performance Metrics:\n")
cat("Average Daily Return:", round(mean(stock$Return, na.rm = TRUE) * 100, 4), "%\n")
cat("Best Daily Return:", round(max(stock$Return, na.rm = TRUE) * 100, 2), "%\n")
cat("Worst Daily Return:", round(min(stock$Return, na.rm = TRUE) * 100, 2), "%\n")
cat("Maximum Drawdown:", round(min(stock$Drawdown, na.rm = TRUE) * 100, 2), "%\n")
cat("Average 20 Day Volatility:", round(mean(stock$Volatility20, na.rm = TRUE) * 100, 2), "%\n")

cat("\nRecent Monthly Returns:\n")
print(tail(monthly_returns, 6))

price_plot <- ggplot(stock, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  ggtitle("Apple Stock Closing Price") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()

print(price_plot)
ggsave(file.path(plot_dir, "price_plot.png"), plot = price_plot, width = 10, height = 6)

returns_plot <- ggplot(stock, aes(x = date, y = Return)) +
  geom_line(color = "red") +
  ggtitle("Daily Stock Returns") +
  xlab("Date") +
  ylab("Return") +
  theme_minimal()

print(returns_plot)
ggsave(file.path(plot_dir, "returns_plot.png"), plot = returns_plot, width = 10, height = 6)

moving_average_plot <- ggplot(stock, aes(x = date)) +
  geom_line(aes(y = close), color = "black") +
  geom_line(aes(y = MA50), color = "blue") +
  geom_line(aes(y = MA200), color = "red") +
  ggtitle("Moving Average Analysis") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()

print(moving_average_plot)
ggsave(file.path(plot_dir, "moving_average_plot.png"), plot = moving_average_plot, width = 10, height = 6)

volume_plot <- ggplot(stock, aes(x = date, y = volume)) +
  geom_col(fill = "darkgreen") +
  ggtitle("Stock Trading Volume") +
  xlab("Date") +
  ylab("Volume") +
  theme_minimal()

print(volume_plot)
ggsave(file.path(plot_dir, "volume_plot.png"), plot = volume_plot, width = 10, height = 6)

rsi_plot <- ggplot(stock, aes(x = date, y = RSI)) +
  geom_line(color = "purple") +
  ggtitle("Relative Strength Index (RSI)") +
  xlab("Date") +
  ylab("RSI") +
  theme_minimal()

print(rsi_plot)
ggsave(file.path(plot_dir, "rsi_plot.png"), plot = rsi_plot, width = 10, height = 6)

bollinger_plot <- ggplot(stock, aes(x = date)) +
  geom_line(aes(y = close), color = "black") +
  geom_line(aes(y = Upper), color = "red") +
  geom_line(aes(y = Lower), color = "blue") +
  geom_line(aes(y = Middle), color = "green") +
  ggtitle("Bollinger Bands Analysis") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()

print(bollinger_plot)
ggsave(file.path(plot_dir, "bollinger_plot.png"), plot = bollinger_plot, width = 10, height = 6)

volatility_plot <- ggplot(stock, aes(x = date, y = Volatility20)) +
  geom_line(color = "orange") +
  ggtitle("20 Day Rolling Volatility") +
  xlab("Date") +
  ylab("Volatility") +
  theme_minimal()

print(volatility_plot)
ggsave(file.path(plot_dir, "volatility_plot.png"), plot = volatility_plot, width = 10, height = 6)

macd_plot <- ggplot(stock, aes(x = date)) +
  geom_line(aes(y = MACD), color = "darkblue") +
  geom_line(aes(y = Signal), color = "darkred") +
  ggtitle("MACD and Signal Line") +
  xlab("Date") +
  ylab("MACD") +
  theme_minimal()

print(macd_plot)
ggsave(file.path(plot_dir, "macd_plot.png"), plot = macd_plot, width = 10, height = 6)

drawdown_plot <- ggplot(stock, aes(x = date, y = Drawdown)) +
  geom_line(color = "brown") +
  ggtitle("Drawdown Analysis") +
  xlab("Date") +
  ylab("Drawdown") +
  theme_minimal()

print(drawdown_plot)
ggsave(file.path(plot_dir, "drawdown_plot.png"), plot = drawdown_plot, width = 10, height = 6)

ts_data <- ts(na.omit(stock$close), frequency = 365)
model <- auto.arima(ts_data)
forecast_data <- forecast(model, h = 30)

plot(forecast_data, main = "30 Day Stock Price Forecast")
dev.copy(png, filename = file.path(plot_dir, "forecast_plot.png"), width = 1000, height = 600)
dev.off()

getSymbols(c("AAPL", "MSFT", "GOOG"), src = "yahoo", from = start_date)

stocks <- na.omit(merge(Cl(AAPL), Cl(MSFT), Cl(GOOG)))
colnames(stocks) <- c("AAPL", "MSFT", "GOOG")

cat("\nCorrelation Matrix:\n")
print(cor(stocks))

corrplot(cor(stocks), method = "color")

cat("\nRisk Analysis:\n")
cat("Standard Deviation of Returns:", sd(stock$Return, na.rm = TRUE), "\n")

risk_plot <- ggplot(stock, aes(x = Return)) +
  geom_histogram(bins = 50, fill = "blue", color = "white") +
  ggtitle("Return Distribution") +
  xlab("Return") +
  ylab("Frequency") +
  theme_minimal()

print(risk_plot)
ggsave(file.path(plot_dir, "risk_plot.png"), plot = risk_plot, width = 10, height = 6)

chartSeries(
  stock_xts,
  theme = chartTheme("white"),
  TA = c(
    addSMA(50, col = "blue"),
    addSMA(200, col = "red"),
    addRSI()
  )
)

dev.copy(png, filename = file.path(plot_dir, "technical_chart.png"), width = 1000, height = 600)
dev.off()

cat("\nPlots saved in:", plot_dir, "\n")

cat("\nProject completed successfully.\n")
