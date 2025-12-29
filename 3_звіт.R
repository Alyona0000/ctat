## Кореляційний аналіз (Робота 3)
## Скрипт читає всі файли виду data/table_*.csv, будує матриці кореляцій для цін закриття,
## малює corrplot, виділяє тісно корельовані групи та зберігає результати у outputs/ і plots/

required <- c('dplyr','readr','tidyr','ggplot2','corrplot','qgraph')
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(qgraph)

plot_dir <- file.path(getwd(),'plots','report3')
out_dir  <- file.path(getwd(),'outputs')
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

files <- list.files(file.path(getwd(),'data'), pattern='^table_.*\\.csv$', full.names=TRUE)
if (length(files) == 0) stop('No table_*.csv files found in data/ — розпакуйте daily_sp500.zip у папку data/')

# Read each file: keep date and closing price (6th column), name column by ticker from filename
datalist <- lapply(files, function(f){
  df <- read_csv(f, col_types = cols())
  df <- df %>% mutate(dat = as.Date(as.character(dat), format='%Y%m%d')) %>% select(dat, clo)
  ticker <- gsub('^table_(.*)\\.csv$','\\1', basename(f))
  colnames(df)[2] <- ticker
  df
})

# Merge all into one wide table y
y <- Reduce(function(a,b) full_join(a,b,by='dat'), datalist)

# Keep only tickers with at least 200 non-NA closing values (if few files available, use at least 2)
obs_count <- sapply(y[-1], function(x) sum(!is.na(x)))
keep <- names(obs_count)[obs_count >= 200]
if (length(keep) < 2) {
  # fallback: accept tickers with >= 50 observations if there are at least two
  keep <- names(obs_count)[obs_count >= 50]
}
if (length(keep) < 2) stop('Not enough tickers with sufficient observations — current files:', paste(names(obs_count), collapse=', '))
Y <- y %>% select(dat, all_of(keep))

# Save merged table
write_csv(Y, file.path(out_dir,'merged_closing_prices.csv'))

# Compute correlation matrices (pairwise complete obs)
M_pearson <- cor(Y[-1], use='pairwise.complete.obs', method='pearson')
M_spearman <- cor(Y[-1], use='pairwise.complete.obs', method='spearman')
M_kendall  <- cor(Y[-1], use='pairwise.complete.obs', method='kendall')

write.csv(M_pearson, file.path(out_dir,'corr_pearson.csv'), row.names=TRUE)
write.csv(M_spearman, file.path(out_dir,'corr_spearman.csv'), row.names=TRUE)
write.csv(M_kendall,  file.path(out_dir,'corr_kendall.csv'), row.names=TRUE)

# Visualize correlation matrix
png(filename=file.path(plot_dir,'corrplot_pearson_mixed.png'), width=1000, height=900)
corrplot.mixed(M_pearson, lower='ellipse', upper='number', tl.cex=0.8)
dev.off()

png(filename=file.path(plot_dir,'corrplot_pearson_hclust.png'), width=1000, height=900)
corrplot(M_pearson, order='hclust', addrect=5, tl.cex=0.8)
dev.off()

# Network layout with qgraph for a visual representation (thresholded)
png(filename=file.path(plot_dir,'qgraph_pearson.png'), width=1000, height=900)
qgraph(M_pearson, layout='spring', threshold=0.6, labels=colnames(M_pearson))
dev.off()

# Find top correlated pairs (absolute Pearson) for each ticker
top_pairs <- lapply(colnames(M_pearson), function(t){
  v <- M_pearson[t,]
  v[t] <- NA
  ord <- order(abs(v), decreasing=TRUE, na.last=NA)
  data.frame(ticker= t, partner = names(v)[ord][1:5], corr = v[ord][1:5], stringsAsFactors = FALSE)
})
top_pairs_df <- do.call(rbind, top_pairs)
write_csv(top_pairs_df, file.path(out_dir,'top_correlated_partners.csv'))

# Select clusters (groups) detected by hierarchical clustering: cut tree into 4 groups
hc <- hclust(as.dist(1 - abs(M_pearson)))
groups <- cutree(hc, k=4)
group_df <- data.frame(ticker = names(groups), group = groups)
write_csv(group_df, file.path(out_dir,'ticker_groups_hclust.csv'))

# For each group, if group size <= 8, make pairs plot
for (g in sort(unique(groups))) {
  members <- names(groups)[groups==g]
  if (length(members) <= 8 && length(members) >= 2) {
    png(filename=file.path(plot_dir, sprintf('pairs_group_%02d.png', g)), width=1000, height=1000)
    pairs(Y %>% select(all_of(members)), main = paste('Pairs plot — group', g))
    dev.off()
  }
}

# For 3-4 selected companies: (a) same cluster, (b) different companies with high corr
# Choose the pair with maximum absolute correlation
Mtmp <- M_pearson
diag(Mtmp) <- 0
idx <- which(abs(Mtmp) == max(abs(Mtmp), na.rm=TRUE), arr.ind=TRUE)[1,]
pair <- colnames(Mtmp)[idx]
pair1 <- pair[1]; pair2 <- pair[2]

# Scatterplot with smoothing
png(filename=file.path(plot_dir,sprintf('scatter_%s_%s.png',pair1,pair2)), width=800, height=600)
dfpair <- Y %>% select(dat, all_of(c(pair1,pair2))) %>% drop_na()
plot(dfpair[[pair1]], dfpair[[pair2]], xlab=pair1, ylab=pair2, main=sprintf('Scatter %s vs %s (r=%.3f)', pair1, pair2, M_pearson[pair1,pair2]))
abline(lm(dfpair[[pair2]] ~ dfpair[[pair1]]), col='red')
dev.off()

# Year-wise correlation for the chosen pair
dfpair$year <- as.integer(format(dfpair$dat, '%Y'))
year_corr <- dfpair %>% group_by(year) %>% summarize(n=n(), r = cor(!!sym(pair1), !!sym(pair2), use='pairwise.complete.obs'))
write_csv(year_corr, file.path(out_dir,sprintf('year_corr_%s_%s.csv',pair1,pair2)))

# Compare Pearson vs Spearman/Kendall for the chosen pair
cmp <- data.frame(method=c('pearson','spearman','kendall'), value=c(M_pearson[pair1,pair2], M_spearman[pair1,pair2], M_kendall[pair1,pair2]))
write_csv(cmp, file.path(out_dir,sprintf('corr_methods_%s_%s.csv',pair1,pair2)))

# Short report
report <- file.path(out_dir,'3_report_correlation.md')
cat('# Робота 3 — Кореляційний аналіз\n\n', file = report)
cat('Оброблено tickers: ', paste(colnames(Y)[-1], collapse=', '), '\n\n', file = report, append = TRUE)
cat('Топ пар з максимальною кореляцією (фрагмент):\n', file = report, append = TRUE)
write.table(head(top_pairs_df,20), file=report, append=TRUE, row.names=FALSE, col.names=TRUE)
cat('\n\nВибрана пара для детального аналізу: ', pair1, ' — ', pair2, '\n', file = report, append = TRUE)
cat(sprintf('\nPearson r = %.3f; Spearman r = %.3f; Kendall tau = %.3f\n', M_pearson[pair1,pair2], M_spearman[pair1,pair2], M_kendall[pair1,pair2]), file=report, append=TRUE)
cat('\nРік за роком кореляція для цієї пари збережена у файлі outputs/', sprintf('year_corr_%s_%s.csv',pair1,pair2), '\n', file=report, append=TRUE)

cat('\nГрафіки (corrplot, pairs, scatter) збережено у plots/report3/.
\n')
cat('\n## Графіки\n', file = report, append = TRUE)
cat('\n![Corrplot (mixed)](../plots/report3/corrplot_pearson_mixed.png)\n', file = report, append = TRUE)
cat('\n![Corrplot (hclust)](../plots/report3/corrplot_pearson_hclust.png)\n', file = report, append = TRUE)
cat('\n![qgraph network](../plots/report3/qgraph_pearson.png)\n', file = report, append = TRUE)
cat('\n![Pairs plot (group 1)](../plots/report3/pairs_group_01.png)\n', file = report, append = TRUE)
cat('\n![Scatter for chosen pair](../plots/report3/scatter_{{T1}}_{{T2}}.png)\n', file = report, append = TRUE)

message('Done. Results in outputs/ and plots/.')
