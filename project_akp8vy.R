NBA_salary_stats <- read.csv(file = 'NBA_salary_stats.csv', row.names = 1, header = TRUE)
library(corrplot)
library(GGally)
library(PerformanceAnalytics)
library(plotly)

data <- read.csv(file = 'NBA_salary_stats.csv', row.names = 1, header = TRUE)

# drop Guaranteed salary in exchange for using 'X2019.20' (actual pay) when predictingsalary (index = 3rd column)
# also drop one of the age variables (irrelevant/multicollinearity) (index = 4th column)
corrplot(cor(data[, -c(1, 3, 4, 5, 31, 39)], 
             use = 'complete.obs'), 
         method = 'circle', 
         type = 'upper')

# correlation with actual salary
cor(data[, -c(1, 3, 4, 5, 31, 39)])[, 1]




correlations<-cor(data[, -c(1,3,4, 5, 31, 39)])[ ,1]
# save the highly correlated variables (above .5 is my threshold)
high_cor <- Filter(function(x) x>.5,correlations)
# remove the y variable
high_cor<-high_cor[2:length(high_cor)]
# Loop through all highly correlated vars to get regressions and plots
for (c in names(high_cor)) {
  # get the model for each highly correlated predictor
  l<-lm(data$X2019.20~data[[c]])
  
  # plot the model with the regression line
  plot(data$X2019.20~data[[c]], main=c)
  abline(l,col='red')
}

#
factor(data$Pos, )
regfull <- lm(X2019.20~.-Player-Guaranteed-Age-url-team, data=data)
summary(regfull)

#
regnull <- lm(data$X2019.20~1, data=data)
##model with all predictors
regfull <- lm(X2019.20~.-Player-Guaranteed-Age-url-team, data=data)
step_reg <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

full_model <- lm(X2019.20 ~ num_accolades + PTS + yrs_exp + num_teams_played + G + num_teams_1920 + GS + injury_yrs + PF + DRB + AST + TOV, data = data)
summary(full_model)

reduced <- lm(X2019.20 ~ num_accolades + PTS + yrs_exp + num_teams_played + G + num_teams_1920 + GS + injury_yrs + DRB + AST, data = data)
summary(reduced)
anova(reduced,full_model)

summary(reduced)


model_1 <- lm(X2019.20 ~ num_accolades + PTS + yrs_exp + num_teams_played + G + num_teams_1920 + GS + injury_yrs, data = data)

#model_1 above has R-squared of .6337. The R-squared of the full_model above is .6447
#Therefore. Choosing model_1 is appropriate since it has 4 less predictors and only ~1% reduction in fit

library(faraway)
library(MASS)
vif(model_1)

plot(model_1$fitted.values,model_1$residuals, main = "residual plot of model")
abline(h=0,col="red")

acf(model_1$residuals)


boxcox(model_1)

qqnorm(model_1$residuals)
qqline(model_1$residuals, col = "red")
