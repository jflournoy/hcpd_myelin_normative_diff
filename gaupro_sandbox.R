#make some data with an elbow
age <- sort(c(sample(8:21, size = 50*4, replace = TRUE), sample(30:40, size = 25*4, replace = TRUE)))
y <- ifelse(age < 15, 0 + .4*age, .4*15 + .1*(age - 15))
plot(age, y)
y1 <- y + rnorm(length(age), 0, 1)
y2 <- y + rnorm(length(age), 0, 2)

library(brms)
dat1 <- data.frame(y = y1, age = age)
## We could play with the priors a bit, or use defaults.
fit1 <- brm(y ~ gp(age), data = dat1, 
            iter = 4000,
            warmup = 1000,
            chains = 4,
            cores = 4, 
            control = list(adapt_delta = .999, max_treedepth = 20),
            file = 'gp_fit1')
summary(fit1)
dat2 <- data.frame(y = y2, age = age)
## We could play with the priors a bit, or use defaults.
fit2 <- brm(y ~ gp(age), data = dat2, 
            iter = 4000,
            warmup = 1000,
            chains = 4,
            cores = 4, 
            control = list(adapt_delta = .999, max_treedepth = 20),
            file = 'gp_fit2')
summary(fit2)

#Notice with both of these plots that the prediction is worse when the data are
#absent or more diffuse.
library(ggplot2)
library(patchwork)

me1p <- plot(me1 <- conditional_effects(fit1, nug = 1e-7), points = TRUE)
me2p <- plot(me2 <- conditional_effects(fit2, nug = 1e-7), points = TRUE)

me1p$age + coord_cartesian(y = c(-1, 15)) + 
  me2p$age + coord_cartesian(y = c(-1, 15))

## This is the function to use for prediction error (both deviation of the
## prediction from observed, and the error in that). That is, the output of this
## function gives us samples from y_obs - y_hat. The standard deviation of these
## samples (governed entirely by the sampling distribution of the expected mean,
## y_hat) is the standard error of the prediction (i.e, variance in the expected
## response), which, when squared, is what Marquand et al (2016) report as
## \sigma^2_{ij}. The normative differerence method also takes into account the
## error variance from the model's residuals, which is is what Marquand et al
## (2016) refer to as "the variance learned from the normative distribution" and
## \sigma^2_{nj}. We can get the mean of the posterior distribution of
## \sigma^2_{nj} using the posterior_summary function.
## 
pe1 <- brms::predictive_error(fit1)
pe2 <- brms::predictive_error(fit2)

sigma1 <- brms::posterior_summary(fit1, pars = 'sigma')
sigma2 <- brms::posterior_summary(fit2, pars = 'sigma')

pe1_summary <- apply(pe1, 2, function(col) {
  qstats <- quantile(col, probs = c(.025, .5, .975))
  m <- mean(col) #y_obs - y_hat
  se <- sd(col) #\sigma_ij
  Z <- m / sqrt( se^2 + sigma1[, 'Estimate']^2 )
  stats <- c(qstats, mean = m, se = se, Z = Z)
  return(stats)
})
pe2_summary <- apply(pe2, 2, function(col) {
  qstats <- quantile(col, probs = c(.025, .5, .975))
  m <- mean(col) #y_obs - y_hat
  se <- sd(col) #\sigma_ij
  Z <- m / sqrt( se^2 + sigma2[, 'Estimate'] ^2 )
  stats <- c(qstats, mean = m, se = se, Z = Z)
  return(stats)
})

pos <- position_identity()
ggplot(cbind(as.data.frame(t(pe1_summary)), dat1), aes(x = age, y = Z)) + 
  geom_point(position = pos, aes(color = age)) + 
  coord_cartesian(y = c(-3, 3)) + 
ggplot(cbind(as.data.frame(t(pe2_summary)), dat1), aes(x = age, y = Z)) + 
  geom_point(position = pos, aes(color = age)) + 
  coord_cartesian(y = c(-3, 3))

## We can also use 
#?brms::kfold
#?brms::kfold_predict()
#?brms::predict.brmsfit
#?brms::posterior_predict.brmsfit()
#?brms::prepare_predictions
library(future)
#CHANGE THE NUMBER OF WORKERS TO MATCH YOUR MACHINE!
future::plan(future::multiprocess, workers = 15)
kfold1 <- brms::kfold(fit1, K = 10, save_fits = TRUE, chains = 1, nug = 1e-07)

oos_norm_Z <- function(pe_sample, sigma_nj) {
  qstats <- quantile(pe_sample, probs = c(.025, .5, .975))
  m <- mean(pe_sample) #y_obs - y_hat
  se <- sd(pe_sample) #\sigma_ij
  Z <- m / sqrt( se^2 + sigma_nj^2 )
  stats <- c(qstats, mean = m, se = se, Z = Z)
  return(stats)
}

norm_diffs <- apply(kfold1$fits[, 1:2], 1, function(arow, data = kfold1$data){
  afit <- arow[['fit']]
  omitted <- arow[['omitted']]
  nd <- data[omitted, ]
  nd$id <- omitted
  pe <- brms::predictive_error(afit, newdata = nd, nug = 1e-07)
  sigma <- brms::posterior_summary(afit, pars = 'sigma')[,'Estimate']
  stats <- apply(pe, 2, function(pe_col) {
    s <- oos_norm_Z(pe_sample = pe_col, sigma_nj = sigma)
    return(s)
  })
  return(cbind(as.data.frame(t(stats)), nd))
})

norm_diffs_df <- do.call(rbind, norm_diffs)

layout <- "
AABB
AABB
AABB
CCCC
"

ggplot(cbind(as.data.frame(t(pe1_summary)), dat1), aes(x = age, y = Z)) + 
  geom_point(position = pos, alpha = .5) + 
  coord_cartesian(y = c(-3, 3)) +
  labs(title = 'Z computed without CV') +
ggplot(norm_diffs_df[order(norm_diffs_df$id),], aes(x = age, y = Z)) + 
  geom_point(position = pos, alpha = .5) + 
  coord_cartesian(y = c(-3, 3)) + 
  labs(title = 'Z computed with 10-fold CV') +
qplot(as.data.frame(t(pe1_summary))$Z, norm_diffs_df[order(norm_diffs_df$id), 'Z'], 
      ylab = '10-fold CV', xlab = 'Without-CV') +
plot_layout(design = layout, guides = "collect")
