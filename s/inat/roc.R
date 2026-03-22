rate_linear_per_year <- function(fit) {
  # works when the model includes a linear time term:
  #   ... + year_cont_num + offset(log(effort_offset))
  beta <- coef(fit)[["year_cont_num"]]
  V    <- vcov(fit)[["year_cont_num","year_cont_num"]]
  se   <- sqrt(V)
  
  # multiplicative change per year on the mean (mu)
  change_per_year   <- exp(beta) - 1
  lo                <- exp(beta - 1.96*se) - 1
  hi                <- exp(beta + 1.96*se) - 1
  
  # % per year
  list(
    pct_per_year = 100*change_per_year,
    ci95         = 100*c(lo, hi)
  )
}

lin_rate <- rate_linear_per_year(m1$fit_curve)
lin_rate
