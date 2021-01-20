library("tidyverse")
library("foreign")
library("ggplot2")
library("lmtest")

fil <- 
    "C:\\Users\\Lui Yu Sen\\Google Drive\\NTU_study materials\\Economics\\HE3021 Intermediate Econometrics\\Week 1\\HE3021-Week-1-Tutorial-1\\rawdata\\bwght2.dta"
dat <- read.dta(file = fil)
dat <- mutate(dat, npvis_squared = npvis**2)

model <- lm(bwght ~ npvis + npvis_squared + cigs + male, dat)

mean_med <- summary(dat[, "bwght"])
# part a
mean_bwght <- mean_med[4]
# part b
med_bwght <- mean_med[3]
# part c
plot_bwght <- ggplot(dat, aes(x = bwght)) + 
    geom_histogram(binwidth = 100, color = "black")
plot_bwght
# part d
beta_3 <- model$coefficients["cigs"]
# part e
t_test_2tail_variable_hypo <- function(hypothesis, significance_lvl, df, sample_value, standard_error){
    confidence_interval <- hypothesis +c(-1, 1)*qt(p = 1-significance_lvl/2,
                                            df = df)*standard_error
    if (between(sample_value, confidence_interval[1], confidence_interval[2])){
        return(FALSE)
    } else {return(TRUE)}
}
test_beta_3 <- t_test_2tail_variable_hypo(-10, 0.05, 1651, beta_3, 3.3330) # insignificant
confidence_interval <- -10 + c(-1, 1)*qt(p = 0.975, df = 1651)*3.3330
# part f
male_bwght <- filter(dat, male == 1)[, "bwght"]
female_bwght <- filter(dat, male == 0)[, "bwght"]
diff_means_test <- t.test(male_bwght, female_bwght, var.equal = FALSE)
conf_int <- c(diff_means_test$conf.int[1], diff_means_test$conf.int[2])
diff_means_result <- between(100, conf_int[1], conf_int[2])
# part g
beta_1 <- model$coefficients["npvis"]
beta_2 <- model$coefficients["npvis_squared"]
# part h
turning_pt <- beta_1/(2*beta_2)
# part i
hetero_test <- model$model[, 2:5]
hetero_test <- mutate(hetero_test, residuals = model$residuals)
hetero_test <- mutate(hetero_test, residuals = residuals**2)
hetero_test_model <- lm(residuals ~ npvis + npvis_squared + cigs + male, hetero_test)
hetero_test_p_value <- 4.476*(10**(-5)) # heteroscedastic
