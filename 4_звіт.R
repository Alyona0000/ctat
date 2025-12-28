## Робота 4 — Аналіз часток (Варіант 1)
## Аналіз для Варіанту 1: Alabama, Hawaii, Massachusetts, New Mexico, Puerto Rico

required <- c('DescTools','plotrix','dplyr','readr')
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')

library(DescTools)
library(plotrix)
library(dplyr)
library(readr)

plot_dir <- file.path(getwd(),'plots')
out_dir  <- file.path(getwd(),'outputs')
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

data_file <- file.path(getwd(),'data','ChGendUS2023.csv')
if (!file.exists(data_file)) stop('Missing data file: data/ChGendUS2023.csv — please add it and rerun')

x <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)
x$Male <- as.numeric(x$Male)
x$Female <- as.numeric(x$Female)
rownames(x) <- x$Location

# Variant 1 states (as given)
Stat <- c('Alabama','Hawaii','Massachusetts','New Mexico','Puerto Rico')
if (!all(Stat %in% x$Location)) {
  missing <- Stat[!Stat %in% x$Location]
  stop('Missing locations in data file: ', paste(missing, collapse=', '))
}

y <- x %>% filter(Location %in% Stat) %>% mutate(n = Male + Female, fraction = Male / n)

# Simultaneous confidence intervals (Wilson) with Bonferroni-like adjustment by using per-interval conf.level = (1-alpha)^(1/k)
alpha <- 0.05
k <- nrow(y)
per_conf <- (1 - alpha)^(1/k)
ci <- BinomCI(y$Male, y$n, conf.level = per_conf, sides = 'two.sided', method = 'wilson')
colnames(ci) <- c('est','lower','upper')

y$est <- ci[,1]
y$lower <- ci[,2]
y$upper <- ci[,3]

# Hypothesis tests vs p0 = 0.51 (both prop.test and exact binom.test)
p0 <- 0.51
prop_pvals <- sapply(seq_len(nrow(y)), function(i) {
  t <- prop.test(x = y$Male[i], n = y$n[i], p = p0)
  t$p.value
})
binom_pvals <- sapply(seq_len(nrow(y)), function(i) {
  bt <- binom.test(y$Male[i], y$n[i], p = p0)
  bt$p.value
})

y$prop_pval <- prop_pvals
y$binom_pval <- binom_pvals

# Adjust prop.test p-values for multiple comparisons (Bonferroni)
y$prop_pval_bonf <- p.adjust(y$prop_pval, method = 'bonferroni')

# Decision rules:
# - Using simultaneous Wilson CIs: if 0.51 not in [lower,upper] then significant at family-wise alpha
y$signif_by_CI <- !(y$lower <= p0 & y$upper >= p0)
# - Using adjusted prop.test p-values
y$signif_by_prop_bonf <- y$prop_pval_bonf < alpha

# Save results
write_csv(y, file.path(out_dir,'4_variant1_results.csv'))

# Plot confidence intervals
png(filename = file.path(plot_dir,'4_variant1_CIs.png'), width = 900, height = 500)
par(mar=c(6,4,2,1))
plotCI(1:nrow(y), y = y$est, ui = y$upper, li = y$lower, xaxt='n', xlab='', ylab='Proportion Male', pch=19, col='blue')
axis(1, at=1:nrow(y), labels = y$Location, las=2)
abline(h = p0, col='red', lty=2)
title('Simultaneous Wilson CIs (Bonferroni-like) for Variant 1')
dev.off()

# Summary report (markdown)
report_file <- file.path(out_dir,'4_variant1_report.md')
cat('# Робота 4 — Варіант 1: результати\n\n', file = report_file)
cat('Штати: ', paste(Stat, collapse=', '), '\n\n', file = report_file, append = TRUE)
cat('Перевірка H0: p = 0.51 (альтернатива двостороння). Перевірки виконані за допомогою prop.test (асимптотичний) і binom.test (точний). Також застосовано множинну поправку Bonferroni для p-значень prop.test і побудовано одночасні Wilson CI з налаштованим рівнем довіри.)\n\n', file = report_file, append = TRUE)

cat('## Результат по штатах (збережено у outputs/4_variant1_results.csv)\n\n', file = report_file, append = TRUE)
write.table(y, file = report_file, append = TRUE, row.names = FALSE, sep = '\t')

cat('\n\n## Висновки\n', file = report_file, append = TRUE)
sig_ci <- y$Location[y$signif_by_CI]
sig_p <- y$Location[y$signif_by_prop_bonf]
cat('- Штати, для яких одночасні Wilson CI не містять 0.51: ', if(length(sig_ci)>0) paste(sig_ci, collapse=', ') else 'none', '\n', file = report_file, append = TRUE)
cat('- Штати, для яких скориговані p-значення (Bonferroni) < 0.05 (prop.test): ', if(length(sig_p)>0) paste(sig_p, collapse=', ') else 'none', '\n', file = report_file, append = TRUE)

cat('\nДокладні результати збережено в outputs/4_variant1_results.csv і графік у plots/4_variant1_CIs.png\n', file = report_file, append = TRUE)

message('Variant 1 analysis completed: results -> outputs/4_variant1_results.csv; plot -> plots/4_variant1_CIs.png; report -> ', report_file)
