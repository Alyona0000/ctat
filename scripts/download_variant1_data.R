# Download daily prices for Variant 1 tickers and save as table_<mnemonic>.csv
# Writes files to data/table_<ticker>.csv with columns: dat,z,opn,mx,mn,clo,vol

required <- c('quantmod','lubridate')
for (pkg in required) {
  if (!requireNamespace(pkg, quietly=TRUE)) {
    install.packages(pkg, repos='https://cloud.r-project.org')
  }
}

library(quantmod)
library(lubridate)

tickers <- c('AAPL','LLY')
out_dir <- file.path(getwd(),'data')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)

for (t in tickers) {
  cat('Fetching', t, '...\n')
  df_xts <- tryCatch(getSymbols(Symbols = t, src = 'yahoo', auto.assign = FALSE),
                     error = function(e) stop('Failed to download ', t, ': ', e$message))

  df <- data.frame(date = index(df_xts), coredata(df_xts))
  # coredata columns: Open, High, Low, Close, Volume, Adjusted (if available)
  # Ensure we have at least 5 columns (Open, High, Low, Close, Volume)
  if (ncol(df) < 6) {
    stop('Unexpected column structure for ', t)
  }
  colnames(df)[2:6] <- c('Opn','Mx','Mn','Clo','Vol')

  df_out <- data.frame(
    dat = format(df$date, '%Y%m%d'),
    z = '',
    opn = df$Opn,
    mx = df$Mx,
    mn = df$Mn,
    clo = df$Clo,
    vol = df$Vol
  )

  out_file <- file.path(out_dir, paste0('table_', tolower(t), '.csv'))
  write.csv(df_out, out_file, row.names = FALSE)
  cat('Saved', out_file, '\n')
}

cat('All done.\n')
