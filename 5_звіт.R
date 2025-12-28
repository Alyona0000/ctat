## Робота 5 — Розрахунок вартості страхової суми (аналітично та методом імітацій)
## Скрипт реалізує приклади з опису: аналітична оцінка V, резерви за формулою та
## імітаційна перевірка (монте-карло) для заданих варіантів.

required <- c('readr','dplyr','ggplot2')
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')

library(readr)
library(dplyr)
library(ggplot2)

plot_dir <- file.path(getwd(),'plots')
out_dir  <- file.path(getwd(),'outputs')
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- User parameters ---
# life_table_file must contain columns lx and dx (with ages starting at 0 indexing)
life_file <- file.path(getwd(),'data','Canada0.txt')
if (!file.exists(life_file)) stop('Life table file not found: ', life_file, '. Please add it to data/ or set life_file variable.')

# Read life table (expect header with columns lx and dx)
tbl <- read.table(life_file, header = TRUE, stringsAsFactors = FALSE)
if (!all(c('lx','dx') %in% colnames(tbl))) stop('Life table must contain columns lx and dx')

# Variants table (x, n, S, i, N, beta)
variants <- data.frame(
  var = 1:10,
  x = c(35,40,45,32,47,51,30,37,35,35),
  n = c(7,6,7,6,8,5,8,5,7,7),
  S = c(10000,15000,20000,25000,18000,24000,14000,30000,10000,10000),
  i = c(0.05,0.04,0.03,0.05,0.04,0.03,0.05,0.04,0.05,0.05),
  N = c(1000,2000,1000,2000,1000,2000,1000,2000,1000,1000),
  beta = c(0.05,0.04,0.03,0.05,0.04,0.03,0.05,0.04,0.05,0.05),
  stringsAsFactors = FALSE
)

# Simulation parameters
B <- 200000    # number of Monte Carlo replicates (reduce if slow)
set.seed(123456)

results <- list()
sim_summaries <- list()

for (r in seq_len(nrow(variants))) {
  v <- variants[r,]
  x0 <- v$x; n <- v$n; S <- v$S; irate <- v$i; N <- v$N; beta <- v$beta
  nu <- 1/(1 + irate)

  # Use lx and dx; note that in the example indexing used lx[x+1] and dx[(x+1):(x+n)]
  if ((x0 + n) > (nrow(tbl)-1)) stop('Life table too short for variant ', v$var)
  lx_vec <- tbl$lx
  dx_vec <- tbl$dx

  V <- (S / lx_vec[x0 + 1]) * sum(nu^(1:n) * dx_vec[(x0 + 1):(x0 + n)])

  # Variance term V2 and sigma
  V2 <- (S^2 / lx_vec[x0 + 1]) * sum(nu^(2 * (1:n)) * dx_vec[(x0 + 1):(x0 + n)]) - V^2
  sigma <- sqrt(abs(V2))

  lambda <- qnorm(1 - beta)
  Rezerve <- V * N + lambda * sigma * sqrt(N)
  Vrisk <- Rezerve / N

  # Simulation: probabilities of loss amounts per life (discrete distribution)
  prob <- dx_vec[(x0 + 1):(x0 + n)] / lx_vec[x0 + 1]
  prob_last <- 1 - sum(prob)
  prob_all <- c(prob, prob_last)
  discont <- c(nu^(1:n), 0)

  # Monte Carlo: simulate aggregate losses for N lives, B replicates
  message('Simulating variant ', v$var, ' with B=', B, ' replicates (this may take a while)')
  ca <- replicate(B, {
    draws <- sample(discont * S, size = N, replace = TRUE, prob = prob_all)
    sum(draws)
  })

  Vsim <- mean(ca) / N
  RezerveSim <- quantile(ca, probs = 1 - beta)
  VriskSim <- as.numeric(RezerveSim / N)

  # Save histogram plot for aggregate loss
  png(filename = file.path(plot_dir, paste0('5_variant', v$var, '_agg_loss_hist.png')), width = 800, height = 500)
  hist(ca / N, breaks = 100, main = paste('Variant', v$var, '- distribution of loss per life (simulated)'), xlab = 'loss per life', xlim = range(ca / N))
  abline(v = VriskSim, col = 'red', lwd = 2)
  legend('topright', legend = sprintf('Simulated VaR (1-beta): %.2f', VriskSim), col = 'red', lwd = 2)
  dev.off()

  # Summaries and store
  res <- data.frame(var = v$var, x = x0, n = n, S = S, i = irate, N = N, beta = beta,
                    V = V, sigma = sigma, lambda = lambda, Rezerve = Rezerve, Vrisk = Vrisk,
                    Vsim = Vsim, VriskSim = VriskSim, RezerveSim = as.numeric(RezerveSim), stringsAsFactors = FALSE)
  results[[r]] <- res

  sim_summaries[[r]] <- data.frame(var = v$var, mean_agg = mean(ca), sd_agg = sd(ca), VaR_agg = as.numeric(RezerveSim), VaR_per_life = VriskSim)
  # Save simulated quantiles
  qs <- quantile(ca / N, probs = c(0.01,0.05,0.5,0.95,0.99))
  write.csv(as.data.frame(t(qs)), file = file.path(out_dir, paste0('5_variant', v$var, '_sim_quantiles.csv')), row.names = FALSE)
}

res_df <- do.call(rbind, results)
write.csv(res_df, file = file.path(out_dir,'5_variants_results.csv'), row.names = FALSE)
sim_df <- do.call(rbind, sim_summaries)
write.csv(sim_df, file = file.path(out_dir,'5_simulation_summary.csv'), row.names = FALSE)

# Short report
report_file <- file.path(out_dir,'5_report_reserves.md')
cat('# Робота 5 — Результати розрахунків резервів\n\n', file = report_file)
cat('Аналітичні оцінки V, σ, резерви за формулою, та імітаційні VaR для кожного варіанту збережено у outputs/5_variants_results.csv та outputs/5_simulation_summary.csv\n\n', file = report_file, append = TRUE)
cat('Таблиця результатів (аналітично та імітаційно):\n', file = report_file, append = TRUE)
write.table(res_df, file = report_file, append = TRUE, row.names = FALSE, sep='\t')

message('Completed. Results -> outputs/5_variants_results.csv; simulation summaries -> outputs/5_simulation_summary.csv; plots in plots/')
