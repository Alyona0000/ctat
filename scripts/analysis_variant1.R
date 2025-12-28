# Analysis for Variant 1: AAPL and LLY
# Produces histograms of closing prices, histograms of rel = high/low,
# boxplots of closing price by year, selects 2-3 unimodal intervals (3-year windows)
# and produces histograms of relative frequencies for these intervals on one plot.

required <- c('ggplot2','dplyr','lubridate','readr')
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

plot_dir <- file.path(getwd(),'plots')
out_dir <- file.path(getwd(),'outputs')
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive=TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)

tickers <- c('aapl','lly')

count_density_peaks <- function(x) {
  d <- density(x, na.rm=TRUE)
  y <- d$y
  # local maxima count
  peaks <- sum((y[-1] - y[-length(y)])[-1] < 0 & (y[-1] - y[-length(y)])[-length(y)] > 0)
  # simpler fallback: count sign changes of derivative
  return(peaks)
}

select_unimodal_intervals <- function(df, min_window = 2, max_window = 10, max_intervals = 3) {
  df$year <- year(df$dat)
  min_y <- min(df$year); max_y <- max(df$year)
  candidates <- list()
  chosen_years <- integer(0)

  for (w in seq(min_window, max_window)) {
    for (start in seq(min_y, max_y - w + 1)) {
      end <- start + w - 1
      # skip if overlaps already chosen interval
      if (any(start <= chosen_years & chosen_years <= end)) next
      sel <- df %>% filter(year >= start & year <= end)
      if (nrow(sel) < 30) next
      peaks <- count_density_peaks(sel$rel)
      if (peaks == 1) {
        candidates[[paste0(start,'_',end)]] <- list(start=start, end=end, n=nrow(sel), w=w)
        # reserve years to avoid overlap
        chosen_years <- c(chosen_years, start:end)
      }
      if (length(candidates) >= max_intervals) break
    }
    if (length(candidates) >= max_intervals) break
  }
  if (length(candidates) == 0) return(NULL)
  res <- do.call(rbind, lapply(names(candidates), function(k) data.frame(interval=k, start=candidates[[k]]$start, end=candidates[[k]]$end, n=candidates[[k]]$n, window=candidates[[k]]$w)))
  return(res)
}

summaries_all <- list()

for (t in tickers) {
  cat('Processing', t, '...\n')
  file <- file.path(getwd(),'data', paste0('table_', t, '.csv'))
  if (!file.exists(file)) stop('Missing data file for ', t)
  df <- read_csv(file, col_types = cols())
  df$dat <- as.Date(as.character(df$dat), format='%Y%m%d')
  df <- df %>% arrange(dat) %>% mutate(rel = mx / mn, year = year(dat))

  # 1. Histogram of closing prices (absolute counts)
  p1 <- ggplot(df, aes(x=clo)) + geom_histogram(bins=40, fill='steelblue', color='black') + ggtitle(paste(toupper(t),'Closing Price (counts)')) + xlab('Closing price')
  ggsave(filename=file.path(plot_dir, paste0(t,'_hist_closing.png')), plot=p1, width=7, height=5)

  # 2. Histogram of rel (all data)
  p2 <- ggplot(df, aes(x=rel)) + geom_histogram(bins=40, fill='darkgreen', color='black') + ggtitle(paste(toupper(t),'rel = High/Low (all data)')) + xlab('rel (high/min)')
  ggsave(filename=file.path(plot_dir, paste0(t,'_hist_rel_all.png')), plot=p2, width=7, height=5)

  # 3. Boxplot of closing price by year (all years on one figure)
  p3 <- ggplot(df, aes(x=factor(year), y=clo)) + geom_boxplot(outlier.size=0.8) + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) + xlab('year') + ylab('closing price') + ggtitle(paste(toupper(t),'Closing Price by Year'))
  ggsave(filename=file.path(plot_dir, paste0(t,'_boxplot_by_year.png')), plot=p3, width=10, height=5)

  # 4. Select 2-3 unimodal intervals (try windows 2..10 years)
  intervals <- select_unimodal_intervals(df, min_window = 1, max_window = 10, max_intervals = 3)
  if (is.null(intervals)) {
    cat('No unimodal intervals found for', t, '\n')
    intervals <- data.frame(interval='none', start=min(df$year), end=max(df$year), n=nrow(df))
  }
  write_csv(intervals, file.path(out_dir, paste0(t,'_intervals.csv')))

  # 5. For selected intervals, produce histograms of relative frequencies on one plot
  if (nrow(intervals) > 0 && intervals$interval[1] != 'none') {
    df_list <- list()
    for (i in 1:nrow(intervals)) {
      s <- intervals$start[i]; e <- intervals$end[i]
      sel <- df %>% filter(year >= s & year <= e) %>% mutate(Period = paste0(s,'-',e))
      df_list[[i]] <- sel
    }
    df_join <- bind_rows(df_list)
    p4 <- ggplot(df_join, aes(x=rel, fill=Period)) + geom_histogram(aes(y=..density..), bins=30, alpha=0.45, position='identity', color='black') + ggtitle(paste(toupper(t),'rel histograms for selected intervals (density overlay)')) + xlab('rel')
    ggsave(filename=file.path(plot_dir, paste0(t,'_rel_intervals_hist.png')), plot=p4, width=8, height=5)

    # 6. Boxplot of rel across selected intervals
    p5 <- ggplot(df_join, aes(x=Period, y=rel)) + geom_boxplot() + ggtitle(paste(toupper(t),'rel by selected intervals')) + xlab('Period') + ylab('rel (high/min)')
    ggsave(filename=file.path(plot_dir, paste0(t,'_rel_intervals_boxplot.png')), plot=p5, width=8, height=5)

    # Save interval summaries
    sum_by_period <- df_join %>% group_by(Period) %>% summarize(n = n(), mean_rel = mean(rel, na.rm=TRUE), sd_rel = sd(rel, na.rm=TRUE), median_rel = median(rel, na.rm=TRUE))
    write_csv(sum_by_period, file.path(out_dir, paste0(t,'_intervals_summary.csv')))
  }

  # Save overall summary
  sum_overall <- df %>% summarize(n=n(), mean_clo = mean(clo, na.rm=TRUE), sd_clo = sd(clo, na.rm=TRUE), mean_rel = mean(rel, na.rm=TRUE), sd_rel = sd(rel, na.rm=TRUE))
  write_csv(sum_overall, file.path(out_dir, paste0(t,'_summary.csv')))

  summaries_all[[t]] <- list(intervals = intervals, overall = sum_overall)
}

# Draft short report
report_file <- file.path(out_dir, 'report_variant1.md')
cat('# Variant 1 Analysis\n\n', file = report_file)
for (t in tickers) {
  cat('##', toupper(t), '\n', file = report_file, append=TRUE)
  cat('- Data file: `data/table_', t, '.csv`\n', file = report_file, append=TRUE)
  intf <- summaries_all[[t]]$intervals
  cat('- Selected intervals (start-end):\n', file = report_file, append=TRUE)
  if (!is.null(intf) && nrow(intf) > 0) {
    for (i in 1:nrow(intf)) cat(sprintf('  - %s (%d observations)\n', paste0(intf$start[i], '-', intf$end[i]), intf$n[i]), file = report_file, append=TRUE)
  } else {
    cat('  - none\n', file = report_file, append=TRUE)
  }
  cat('\n', file = report_file, append=TRUE)
}

cat('Analysis finished. Plots in plots/, tables in outputs/.\n')
