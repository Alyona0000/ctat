# Analysis: Mobile vs Fitness prices (Variant data)
# Saves plots to ./plots and summary to ./outputs

# --- Setup ---
dirs <- c("data","plots","outputs")
for(d in dirs) if(!dir.exists(d)) dir.create(d, recursive=TRUE, showWarnings=FALSE)

# Data
City <- c("Paris","Orleans","London","Edinburgh","Rome","Milan","Berlin","Munich","Madrid","Barcelona")
Mobile <- c(13.77,16.88,13.89,13.29,10.12,9.94,15.09,15.16,16.76,17.79)
Fitness <- c(39.24,27.67,52.50,38.36,59.94,71.56,32.19,42.65,45.02,48.62)
data <- data.frame(City, Mobile, Fitness)

# Save raw data (optional)
write.table(data, file="data/mobile_fitness.txt", row.names=FALSE)

# --- Scatter plot with labels ---
png(filename="plots/scatter_mobile_fitness.png", width=800, height=600)
plot(data$Mobile, data$Fitness, pch=19, col="darkred",
     xlab="Mobile (monthly plan)", ylab="Fitness (monthly fee)",
     xlim=c(min(data$Mobile)*0.95, max(data$Mobile)*1.05))
text(data$Mobile, data$Fitness, labels=data$City, pos=2, col="blue")
dev.off()

# --- Analytic OLS ---
mx <- mean(data$Mobile); my <- mean(data$Fitness)
b1_analytic <- sum((data$Mobile - mx)*(data$Fitness - my))/sum((data$Mobile - mx)^2)
b0_analytic <- my - b1_analytic*mx

# --- lm() ---
fit <- lm(Fitness ~ Mobile, data=data)

# Add regression line to scatter (save)
png(filename="plots/scatter_with_lm.png", width=800, height=600)
plot(data$Mobile, data$Fitness, pch=19, col="darkred",
     xlab="Mobile (monthly plan)", ylab="Fitness (monthly fee)")
text(data$Mobile, data$Fitness, labels=data$City, pos=2, col="blue")
abline(coef(fit), col="red", lwd=2)
dev.off()

# --- Diagnostics ---
cooks <- cooks.distance(fit)
hatv <- hatvalues(fit)
rstd <- rstandard(fit)

# Identify most influential observation
most_inf_idx <- which.max(cooks)
most_inf_city <- data$City[most_inf_idx]

# Fit without most influential point
data_minus <- data[-most_inf_idx, ]
fit_minus <- lm(Fitness ~ Mobile, data=data_minus)

# Save diagnostic plots
png(filename="plots/diagnostic_cooks.png", width=900, height=600)
plot(cooks, type="h", ylab="Cook's distance", xaxt="n", col="darkgreen", lwd=2)
axis(1, at=1:nrow(data), labels=data$City, las=2)
abline(h = 4 / (nrow(data) - length(coef(fit))), col="red", lty=2)
text(x=most_inf_idx, y=cooks[most_inf_idx], labels=data$City[most_inf_idx], pos=3, col="blue")
dev.off()

png(filename="plots/diag_leverage_resid.png", width=900, height=600)
plot(hatv, rstd, xlab="Leverage (hat)", ylab="Standardized residuals", pch=19, col="purple")
text(hatv, rstd, labels=data$City, pos=2, cex=0.8)
abline(h=c(-2,2), col="red", lty=2)
dev.off()

# --- Robust regression (optional quick check) ---
robust_available <- requireNamespace("MASS", quietly=TRUE)
if(robust_available){
  rfit <- MASS::rlm(Fitness ~ Mobile, data=data)
  rcoef <- coef(rfit)
} else {
  rcoef <- c(NA,NA)
  message("Package 'MASS' not available — install.packages('MASS') to run robust regression (optional).")
}

# --- Summary output ---
summary_lines <- c(
  sprintf("Analytic OLS: intercept = %.4f, slope = %.4f", b0_analytic, b1_analytic),
  sprintf("lm() coefficients: intercept = %.4f, slope = %.4f", coef(fit)[1], coef(fit)[2]),
  sprintf("Most influential observation by Cook's D: %s (index %d, CookD = %.4f)", most_inf_city, most_inf_idx, cooks[most_inf_idx]),
  sprintf("lm() without %s: intercept = %.4f, slope = %.4f", most_inf_city, coef(fit_minus)[1], coef(fit_minus)[2]),
  sprintf("Robust regression coefficients (MASS::rlm): intercept = %s, slope = %s", ifelse(is.na(rcoef[1]),"NA","present"), ifelse(is.na(rcoef[2]),"NA","present"))
)

writeLines(summary_lines, con="outputs/summary_mobile_fitness.txt")

# Save full lm() summary and diagnostics for further inspection
capture.output(summary(fit), file = "outputs/lm_summary.txt")
write.csv(data.frame(City = data$City, cooks = cooks, hat = hatv, rstd = rstd), "outputs/diagnostics.csv", row.names = FALSE)


# Print key info to console
cat(paste(summary_lines, collapse="\n"), "\n")

# Also print a short interpretation
cat("\nInterpretation:\n")
cat(sprintf("- Fitness is modeled as dependent on Mobile (Fitness ~ Mobile).\n- Identified influential city: %s.\n- Coefficients change when excluding it; check robustness or investigate data point.\n", most_inf_city))

# End of script
