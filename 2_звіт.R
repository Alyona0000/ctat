(# Аналіз — Варіант 1: AAPL і LLY
#
# Скрипт реалізує вимоги завдання:
# 1) гістограми абсолютних частот цін закриття (counts)
# 2) гістограми відносних частот для відношення high/min (rel) по всіх даних
# 3) boxplot цін закриття по роках
# 4) пошук 2–3 інтервалів часу, де розподіл цін закриття є одномодальним
# 5) для цих інтервалів: накладені гістограми (щільність) цін закриття на одному рисунку
# 6) для тих самих інтервалів: накладені гістограми (щільність) змінної rel на одному рисунку
# Результати: PNG-файли у папці plots/, таблиці та короткий звіт у outputs/
)

required <- c('ggplot2','dplyr','lubridate','readr')
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

plot_dir <- file.path(getwd(), 'plots')
out_dir  <- file.path(getwd(), 'outputs')
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tickers <- c('aapl','lly')

count_density_peaks <- function(x) {
	x <- x[!is.na(x) & is.finite(x)]
	if (length(x) < 10) return(NA_integer_)
	d <- density(x, na.rm=TRUE)
	y <- d$y
	# local maxima: change of sign of first derivative from + to - => diff(sign(diff(y))) == -2
	peaks_idx <- which(diff(sign(diff(y))) == -2)
	length(peaks_idx)
}

select_unimodal_intervals_clo <- function(df, min_w = 1, max_w = 6, max_intervals = 3, min_obs = 30) {
	yrs <- sort(unique(year(df$dat)))
	yrs_seq <- seq(min(yrs), max(yrs))
	chosen <- list()
	used_years <- integer(0)

	for (w in seq(min_w, max_w)) {
		for (start in yrs_seq) {
			end <- start + w - 1
			sel <- df %>% filter(year(dat) >= start & year(dat) <= end)
			if (nrow(sel) < min_obs) next
			peaks <- count_density_peaks(sel$clo)
			if (!is.na(peaks) && peaks == 1) {
				# skip overlapping
				if (any(start:end %in% used_years)) next
				chosen[[paste0(start, '_', end)]] <- data.frame(start = start, end = end, n = nrow(sel), window = w)
				used_years <- c(used_years, start:end)
			}
			if (length(chosen) >= max_intervals) break
		}
		if (length(chosen) >= max_intervals) break
	}
	if (length(chosen) == 0) return(NULL)
	res <- do.call(rbind, chosen)
	rownames(res) <- NULL
	res
}

write_report_header <- function(path) {
	cat('# Варіант 1 — аналіз AAPL та LLY\n\n', file = path)
	cat('Побудовано графіки: `plots/`, таблиці та резюме: `outputs/`\n\n', file = path, append = TRUE)
}

report_file <- file.path(out_dir, '2_report_variant1.md')
write_report_header(report_file)

for (t in tickers) {
	message('Обробка ', t, ' ...')
	f <- file.path(getwd(), 'data', paste0('table_', t, '.csv'))
	if (!file.exists(f)) stop('Відсутній файл даних для ', t)

	df <- read_csv(f, col_types = cols())
	df <- df %>% mutate(dat = as.Date(as.character(dat), format='%Y%m%d')) %>% arrange(dat)
	df <- df %>% mutate(rel = mx / mn)

	# 1. Гістограма цін закриття (counts)
	p1 <- ggplot(df, aes(x = clo)) +
		geom_histogram(bins = 40, fill = 'steelblue', color = 'black') +
		ggtitle(paste0(toupper(t), ' — ціна закриття (counts)')) +
		xlab('Closing price')
	ggsave(filename = file.path(plot_dir, paste0(t, '_hist_closing_counts.png')), plot = p1, width=7, height=5)

	# 2. Гістограма rel (всі дані)
	p2 <- ggplot(df, aes(x = rel)) +
		geom_histogram(bins = 40, fill = 'darkgreen', color = 'black') +
		ggtitle(paste0(toupper(t), ' — rel = high/min (всі дані)')) +
		xlab('rel (high/min)')
	ggsave(filename = file.path(plot_dir, paste0(t, '_hist_rel_all.png')), plot = p2, width=7, height=5)

	# 3. Boxplot цін закриття по роках
	df <- df %>% mutate(year = year(dat))
	p3 <- ggplot(df, aes(x = factor(year), y = clo)) +
		geom_boxplot(outlier.size = 0.8) +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
		xlab('Year') + ylab('Closing price') + ggtitle(paste0(toupper(t), ' — Closing price by year'))
	ggsave(filename = file.path(plot_dir, paste0(t, '_boxplot_by_year.png')), plot = p3, width=10, height=5)

	# 4. Знайти 2-3 інтервали де розподіл закриття одномодальний
	intervals <- select_unimodal_intervals_clo(df, min_w = 1, max_w = 6, max_intervals = 3, min_obs = 30)
	if (is.null(intervals)) {
		message('Не знайдено одномодальних інтервалів для ', t)
		intervals <- data.frame(interval = 'none', start = min(df$year), end = max(df$year), n = nrow(df))
	} else {
		intervals <- intervals %>% mutate(interval = paste0(start, '-', end)) %>% select(interval, start, end, n)
	}
	write_csv(intervals, file.path(out_dir, paste0(t, '_intervals.csv')))

	# 5. Для вибраних інтервалів: гістограми/щільності цін закриття на одному рисунку
	if (!(nrow(intervals) == 1 && intervals$interval[1] == 'none')) {
		df_list <- list()
		for (i in seq_len(nrow(intervals))) {
			s <- intervals$start[i]; e <- intervals$end[i]
			sel <- df %>% filter(year >= s & year <= e) %>% mutate(Period = paste0(s,'-',e))
			df_list[[i]] <- sel
		}
		df_join <- bind_rows(df_list)

		p4 <- ggplot(df_join, aes(x = clo, color = Period, fill = Period)) +
			geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.35, position = 'identity') +
			ggtitle(paste0(toupper(t), ' — Щільності цін закриття для вибраних інтервалів')) +
			xlab('Closing price')
		ggsave(filename = file.path(plot_dir, paste0(t, '_clo_intervals_density.png')), plot = p4, width=8, height=5)

		# 6. Для тих самих інтервалів: гістограми rel (density) на одному рисунку
		p5 <- ggplot(df_join, aes(x = rel, color = Period, fill = Period)) +
			geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.35, position = 'identity') +
			ggtitle(paste0(toupper(t), ' — rel (high/min) для вибраних інтервалів')) +
			xlab('rel')
		ggsave(filename = file.path(plot_dir, paste0(t, '_rel_intervals_density.png')), plot = p5, width=8, height=5)

		# зберегти статистики по інтервалах
		sum_by_period <- df_join %>% group_by(Period) %>% summarize(n = n(), mean_clo = mean(clo, na.rm=TRUE), sd_clo = sd(clo, na.rm=TRUE), mean_rel = mean(rel, na.rm=TRUE), sd_rel = sd(rel, na.rm=TRUE))
		write_csv(sum_by_period, file.path(out_dir, paste0(t, '_intervals_summary.csv')))
	}

	# зберегти підсумки по фірмі
	sum_overall <- df %>% summarize(n = n(), mean_clo = mean(clo, na.rm=TRUE), sd_clo = sd(clo, na.rm=TRUE), mean_rel = mean(rel, na.rm=TRUE), sd_rel = sd(rel, na.rm=TRUE))
	write_csv(sum_overall, file.path(out_dir, paste0(t, '_summary.csv')))

	# додати короткий опис у звіт
	cat('##', toupper(t), '\n', file = report_file, append = TRUE)
	cat('- Data file: `data/table_', t, '.csv`\n', file = report_file, append = TRUE)
	cat('- Selected intervals (start-end):\n', file = report_file, append = TRUE)
	if (!is.null(intervals) && nrow(intervals) > 0 && !(nrow(intervals) == 1 && intervals$interval[1] == 'none')) {
		for (i in 1:nrow(intervals)) cat(sprintf('  - %s (%d observations)\n', paste0(intervals$start[i], '-', intervals$end[i]), intervals$n[i]), file = report_file, append = TRUE)
	} else {
		cat('  - none\n', file = report_file, append = TRUE)
	}
	cat('\n', file = report_file, append = TRUE)
}

cat('Готово. Графіки збережено в plots/, таблиці у outputs/.\n')

