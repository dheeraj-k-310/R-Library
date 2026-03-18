# Smart Stock Market Analysis using R

This is a separate B.Tech CSE mini-project for stock market analysis and visualization using R.

## Project Contents

- `stock_analysis.R`: Main R script

## Features

- Download stock data from Yahoo Finance
- Closing price trend analysis
- Daily return calculation
- Moving average analysis
- Volume analysis
- RSI indicator
- Bollinger Bands
- 30-day ARIMA forecast
- Multi-stock correlation
- Risk analysis
- Dashboard-style chart

## Required R Packages

Install these packages once in R:

```r
install.packages(c(
  "quantmod",
  "ggplot2",
  "dplyr",
  "tidyquant",
  "forecast",
  "TTR",
  "corrplot"
))
```

## Run

Open R or RStudio in this folder and run:

```r
source("stock_analysis.R")
```

## Notes

- Default stock used in the script: `AAPL`
- Default date range starts from `2022-01-01`
- Internet access is required to download Yahoo Finance data
- `Rscript` was not available in this environment, so the script was created but not executed here
