# BDA400 Assignment 2 - Preliminary Technical Analysis
# Student: Hanin Helfawi
# Run from the folder containing portfolio.txt.
# First-time installation (uncomment to run):
# install.packages(c("quantmod", "TTR"), repos = "https://cloud.r-project.org")

required_packages <- c("quantmod", "TTR")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages)) stop("Please install missing packages: ", paste(missing_packages, collapse = ", "))
library(quantmod)
library(TTR)

read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("Portfolio file not found: ", path)
  symbols <- unique(trimws(readLines(path, warn = FALSE)))
  symbols <- symbols[nzchar(symbols) & !grepl("^#", symbols)]
  if (!length(symbols)) stop("No ticker symbols found in portfolio.txt")
  symbols
}

load_stock_data <- function(symbols, from = Sys.Date() - 365, to = Sys.Date()) {
  output <- list()
  for (symbol in symbols) {
    tryCatch({
      xt <- quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to,
                                auto.assign = FALSE, warnings = FALSE)
      if (NROW(xt) == 0) stop("No prices returned")
      output[[symbol]] <- xt
      message("Loaded ", symbol, ": ", NROW(xt), " trading-day records")
    }, error = function(e) warning("Could not load ", symbol, ": ", conditionMessage(e)))
  }
  if (!length(output)) stop("No ticker data loaded. Check connectivity and ticker names.")
  output
}

# Prices are continuous: no mathematical mode normally exists. We report a
# reproducible mode of prices rounded to cents, selecting the smallest mode in a tie.
price_mode <- function(prices) {
  x <- round(as.numeric(prices), 2)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  frequency <- table(x)
  min(as.numeric(names(frequency)[frequency == max(frequency)]))
}

calculate_statistics <- function(stock_data, symbol, window = 20) {
  prices <- as.numeric(quantmod::Cl(stock_data))
  prices <- prices[is.finite(prices)]
  if (!length(prices)) stop("Missing close prices for ", symbol)
  sma <- if (length(prices) >= window) tail(TTR::SMA(prices, n = window), 1) else NA_real_
  data.frame(Symbol = symbol, Observations = length(prices),
             Latest_Close = tail(prices, 1), Mean = mean(prices),
             Median = median(prices), Mode_Rounded_Cents = price_mode(prices),
             Std_Dev = if (length(prices) > 1) sd(prices) else NA_real_,
             SMA20 = as.numeric(sma), row.names = NULL)
}

show_stock_data <- function(stocks, n = 6) {
  for (symbol in names(stocks)) {
    cat("\n===== ", symbol, " : latest OHLCV records =====\n", sep = "")
    print(tail(as.data.frame(stocks[[symbol]]), n))
  }
}

plot_stock_data <- function(stocks, output_dir = "charts", window = 20) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  for (symbol in names(stocks)) {
    xt <- stocks[[symbol]]
    close <- quantmod::Cl(xt)
    sma <- TTR::SMA(close, n = window)
    path <- file.path(output_dir, paste0(symbol, "_close_sma20.png"))
    png(path, width = 1100, height = 650, res = 130)
    tryCatch({
      plot(index(close), as.numeric(close), type = "l", col = "#176b93", lwd = 2,
           xlab = "Date", ylab = "Closing price (USD)",
           main = paste(symbol, "Closing Price and 20-Day Simple Moving Average"))
      lines(index(sma), as.numeric(sma), col = "#d88424", lwd = 2)
      legend("topleft", legend = c("Close", "SMA 20"),
             col = c("#176b93", "#d88424"), lty = 1, lwd = 2, bty = "n")
      grid()
    }, finally = dev.off())
    message("Chart saved: ", path)
  }
}

main <- function() {
  symbols <- read_portfolio()
  stocks <- load_stock_data(symbols)
  show_stock_data(stocks)
  summary <- do.call(rbind, lapply(names(stocks), function(s) calculate_statistics(stocks[[s]], s)))
  print(summary, row.names = FALSE, digits = 4)
  write.csv(summary, "stock_statistics.csv", row.names = FALSE)
  plot_stock_data(stocks)
  message("Finished: stock_statistics.csv and charts/ created")
  invisible(summary)
}

if (sys.nframe() == 0) main()
