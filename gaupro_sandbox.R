# install.packages('GauPro')
library(GauPro)
library(MASS)

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

## This is the function to use
pe1 <- brms::predictive_error(fit1)
pe2 <- brms::predictive_error(fit2)
## We can also make use of brms::kfold_predict()
####
y_pred <- predict(fit1)
pe1_summary <- apply(pe1, 2, function(col) {
  qstats <- quantile(col, probs = c(.025, .5, .975))
  m <- mean(col)
  se <- sd(col)
  stats <- c(qstats, mean = m, se = se, Z = m/se)
  return(stats)
})
pe2_summary <- apply(pe2, 2, function(col) {
  qstats <- quantile(col, probs = c(.025, .5, .975))
  m <- mean(col)
  se <- sd(col)
  stats <- c(qstats, mean = m, se = se, Z = m/se)
  return(stats)
})

pos <- position_identity()
ggplot(cbind(as.data.frame(t(pe1_summary)), dat1), aes(x = age, y = Z)) + 
  geom_point(position = pos, aes(color = age)) + 
  coord_cartesian(y = c(-4, 4)) + 
ggplot(cbind(as.data.frame(t(pe2_summary)), dat1), aes(x = age, y = Z)) + 
  geom_point(position = pos, aes(color = age)) + 
  coord_cartesian(y = c(-4, 4))
s1 <- summary(fit1)$spec_pars[,'Estimate']
s2 <- summary(fit2)$spec_pars[,'Estimate']
plot(sqrt(pe1_summary['se',]^2 - s1^2), sqrt(pe2_summary['se',]^2 - s2^2), xlim = c(0,.6), ylim = c(0,.6))
abline(a = 0, b = 1)
