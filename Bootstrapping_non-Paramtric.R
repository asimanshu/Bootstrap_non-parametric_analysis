# Regression with bootstrap
boot_reg_hbsag <- function(data, idx) {
  fit_hbsag <- lm(DNA ~ HBsAg, data = df.log[idx, ])
  coef(fit_hbsag)
}

b1 <- boot::boot(df.log, boot_reg_hbsag, 3000)

boot::boot.ci(b1, index = 2, type = 'bca')

### Sampling distribution for the slope
quantile(b1$t[,2], c(0.1, 0.5, 0.9))
ggplot(data.frame(x=b1$t[,2]), aes(x)) +
  geom_density()


### Lets try to plot linear model of bootstrap
plot(fit_hbsag)




###Try this one (Method2)

# Containers for the coefficients
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:1000) {
  #Creating a resampled dataset from the sample data
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]
  
  #Running the regression on these data
  model_bootstrap <- lm(y ~ x, data = sample_d)
  
  #Saving the coefficients
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}


dev.new(width=10, height=5, unit="in")
plot(model_bootstrap)
abline(model_bootstrap, lwd=3, col="red")