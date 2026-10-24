# =============================================================================
# Stage 04. Numerical example of the article (Section 3, four households A to D).
# Two resources (income, bathrooms), one demand (residents), two categorical
# dimensions. Groups use delta = 0.5 and a minimum of two reference households;
# the share floor is alpha = 0.10 (with two resources alpha must be <= 1/2).
# The script also contrasts a local frontier with the fixed reference when the
# income of household D (a frontier household) falls by half.
# Requires only lpSolve. Every number printed in the article is checked by stopifnot.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
out_dir <- file.path(PATHS$output, "numerical_example"); dir.create(out_dir, showWarnings = FALSE)

d <- data.table(household = LETTERS[1:4], income = c(8, 4, 2, 1), bathrooms = c(2, 1, 1, 1), residents = 2,
                category_1 = c(3, 2, 1, 1), category_2 = c(3, 3, 3, 2))
d[, `:=`(g1 = (3 - category_1) / 2, g2 = (3 - category_2) / 2)]
d[, S := g1 + g2]
delta <- 0.5; min_ref <- 2L; alpha <- 0.10

radial <- function(X, Y, i, group) {
  A <- rbind(cbind(-X[i, ], t(X[group, , drop = FALSE])), cbind(0, t(Y[group, , drop = FALSE])))
  r <- lpSolve::lp("min", c(1, rep(0, length(group))), A, c(rep("<=", ncol(X)), rep(">=", ncol(Y))),
                   c(rep(0, ncol(X)), Y[i, ]))
  stopifnot(r$status == 0); r$objval
}
frontiers <- function(X, Y, S) {
  levels <- seq(0, max(S), by = delta)
  groups <- lapply(levels, function(t) which(S >= t - 1e-9))
  list(levels = levels, groups = groups,
       frontier = lapply(groups, function(g) g[vapply(g, function(i) radial(X, Y, i, g), 0) >= 1 - 1e-8]))
}
# Model (1) for household o: max u'y_o s.t. v'x_o = 1, u'y_f - v'x_f <= 0 (f in F), v_r x_ro >= alpha.
model1 <- function(F, X, Y, x_o, y_o, S_o, alpha) {
  k <- max(which(F$levels <= S_o + 1e-9 & lengths(F$groups) >= min_ref)); f <- F$frontier[[k]]
  A <- rbind(c(rep(0, ncol(Y)), x_o), cbind(Y[f, , drop = FALSE], -X[f, , drop = FALSE]),
             cbind(matrix(0, ncol(X), ncol(Y)), diag(x_o, ncol(X))))
  r <- lpSolve::lp("max", c(y_o, rep(0, ncol(X))), A, c("=", rep("<=", length(f)), rep(">=", ncol(X))),
                   c(1, rep(0, length(f)), rep(alpha, ncol(X))))
  stopifnot(r$status == 0)
  list(theta = r$objval, level = F$levels[k], group = paste(d$household[F$groups[[k]]], collapse = ", "),
       frontier = paste(d$household[f], collapse = ", "))
}
score_all <- function(F, X, Y, alpha, Xeval = X) vapply(1:4, function(i) model1(F, X, Y, Xeval[i, ], Y[i, ], d$S[i], alpha)$theta, 0)

X <- as.matrix(d[, .(income, bathrooms)]); Y <- as.matrix(d[, .(residents)])
F <- frontiers(X, Y, d$S)
info <- lapply(1:4, function(i) model1(F, X, Y, X[i, ], Y[i, ], d$S[i], alpha))
res <- data.table(household = d$household, S = d$S,
                  level = vapply(info, `[[`, 0, "level"), group = vapply(info, `[[`, "", "group"),
                  frontier = vapply(info, `[[`, "", "frontier"),
                  theta_no_floor = score_all(F, X, Y, 0), theta_floor = score_all(F, X, Y, alpha))
res[, h := pmin(theta_floor, 1)]
M <- mean(res$h); m0 <- sort(res$h)[2]          # median of four values: second value (cumulative weight reaches 1/2)

# Shock 1: income of A falls from 8 to 4. Fixed reference: only A's score changes.
XA <- X; XA[1, 1] <- 4
theta_A <- model1(F, X, Y, XA[1, ], Y[1, ], d$S[1], alpha)$theta

# Shock 2: income of D falls from 1 to 0.5. D belongs to the frontier.
XD <- X; XD[4, 1] <- 0.5
fixed_D <- res$theta_floor; fixed_D[4] <- model1(F, X, Y, XD[4, ], Y[4, ], d$S[4], alpha)$theta
FD <- frontiers(XD, Y, d$S)                    # local frontier: re-estimated with the new data
local_D <- score_all(FD, XD, Y, alpha)
shockD <- data.table(household = d$household, before = res$theta_floor,
                     fixed_reference = fixed_D, local_frontier = local_D)

summary_tab <- data.table(M = M, m0 = m0, T = M / m0, H1 = mean(res$h >= m0), H2 = mean(res$h >= 2 * m0),
                          theta_A_after = theta_A, M_after_A = mean(pmin(c(theta_A, res$theta_floor[-1]), 1)),
                          theta_D_after_fixed = fixed_D[4], M_after_D_fixed = mean(pmin(fixed_D, 1)),
                          M_after_D_local = mean(pmin(local_D, 1)))

stopifnot(max(abs(res$theta_no_floor - c(.5, 1, 1, 1))) < 1e-9,
          max(abs(res$theta_floor - c(.4625, .925, .95, 1))) < 1e-9,
          abs(M - .834375) < 1e-9, abs(theta_A - .475) < 1e-9, abs(fixed_D[4] - 1.9) < 1e-9,
          # local frontier: D stays at 1 and the other scores fall, so M falls after D becomes poorer
          max(abs(local_D - c(.45625, .9125, .925, 1))) < 1e-9, abs(mean(pmin(local_D, 1)) - .8234375) < 1e-9)

fwrite(d, file.path(out_dir, "example_data.csv"))
fwrite(res, file.path(out_dir, "example_scores.csv"))
fwrite(shockD, file.path(out_dir, "example_shock_D.csv"))
fwrite(summary_tab, file.path(out_dir, "example_summary.csv"))
print(res); print(shockD); print(summary_tab)
log_msg("stage 04 complete")
