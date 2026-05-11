
library(ggplot2)

# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/prelim/")
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)



# Part (g): Price-Wage relationship for different beta values
# Parameters
a     <- 100
b     <- 0.5
alpha <- 10
betas <- c(0.3, 0.5, 1)

# Market-clearing condition rearranged: F(w; p, beta) = 0
# (pa - w)/(pb) = alpha * w^beta
# => 100p - w - 5p * w^beta = 0
clearing_eq <- function(w, p, beta) {
  100 * p - w - 5 * p * w^beta
}

# Solver: find w* for given (p, beta) using bisection via uniroot
solve_wage <- function(p, beta) {
  # w must be positive and less than the choke wage pa = 100p
  uniroot(clearing_eq, 
          interval = c(1e-6, 100 * p - 1e-6), 
          p = p, 
          beta = beta,
          tol = 1e-8)$root
}

# Build a grid of prices
prices <- seq(5, 10, length.out = 100)

# Compute w* for each (p, beta) combination
results <- expand.grid(p = prices, beta = betas)
results$w_star <- mapply(solve_wage, results$p, results$beta)

ggplot(results, aes(x = p, y = w_star, color = factor(beta))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("0.3" = "red", "0.5" = "blue", "1" = "darkgreen"),
    labels = paste("beta =", betas)
  ) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  labs(
    x = "Price (p)",
    y = "Equilibrium Wage (w*)",
    color = NULL,
    title = "Price-Wage Relationship for Different Labor Supply Elasticities"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.18, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 0.85,
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 14)   )

ggsave(paste0(OUT_PLOT_DIR, "price_wage.png"), width = 10, height = 8, dpi = 300)

################################################################################


# Employment
results$E_star <- alpha * results$w_star^results$beta


ggplot(results, aes(x = p, y = E_star, color = factor(beta))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("0.3" = "red", "0.5" = "blue", "1" = "darkgreen"),
    labels = paste("beta =", betas)
  ) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  labs(
    x = "Price (p)",
    y = "Equilibrium Employment (E*)",
    color = NULL,
    title = "Price-Employment Relationship for Different Labor Supply Elasticities"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.18, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 0.85,
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 14)   )

ggsave(paste0(OUT_PLOT_DIR, "price_employment.png"), width = 10, height = 8, dpi = 300)

################################################################################
# Output
results$y_star <- a * results$E_star - 0.5 * b * results$E_star^2

################################################################################
# Profit
results$profit <- results$p * results$y_star - results$w_star * results$E_star

ggplot(results, aes(x = p, y = profit, color = factor(beta))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("0.3" = "red", "0.5" = "blue", "1" = "darkgreen"),
    labels = paste("beta =", betas)
  ) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  labs(
    x = "Price (p)",
    y = "Profit",
    color = NULL,
    title = "Price-Profit Relationship for Different Labor Supply Elasticities"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.18, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 0.85,
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 14)   )

ggsave(paste0(OUT_PLOT_DIR, "price_profit.png"), width = 10, height = 8, dpi = 300)


################################################################################
# Wage bill
results$wage_bill <- results$w_star * results$E_star



ggplot(results, aes(x = p, y = wage_bill, color = factor(beta))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("0.3" = "red", "0.5" = "blue", "1" = "darkgreen"),
    labels = paste("beta =", betas)
  ) +
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  labs(
    x = "Price (p)",
    y = "Wage Bill (w*E*)",
    color = NULL,
    title = "Price-Wage Bill Relationship for Different Labor Supply Elasticities"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.18, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 0.85,
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 14)   )

ggsave(paste0(OUT_PLOT_DIR, "price_wage_bill.png"), width = 10, height = 8, dpi = 300)
