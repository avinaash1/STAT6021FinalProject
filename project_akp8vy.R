NBA_salary_stats <- read.csv(file = 'NBA_salary_stats.csv', row.names = 1, header = TRUE)
library(corrplot)
library(GGally)
library(PerformanceAnalytics)
library(plotly)

data <- read.csv(file = 'NBA_salary_stats.csv', row.names = 1, header = TRUE)

#Step wise regression to have a baseline model
factor(data$Pos, )
regfull <- lm(data$Actual~.-Player-Guaranteed-url, data=data)
summary(regfull)
regnull <- lm(data$Actual~1, data=data)

step_reg <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

full_model <- lm(data$Actual ~ num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB +GS +eFG. + all_NBA, data = data)
summary(full_model)

reduced <- lm(data$Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB +GS, data = data) #model with eFG. and all_nba predictors dropped
summary(reduced)
anova(reduced,full_model)

summary(reduced)

#rename reduced as model_1 as this is the initial model that will be pursued and linearity assumptions analyzed

model_1 <- lm( Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB +GS, data = data)

#model_1 above has R-squared of .6512. The R-squared of the full_model above is .6543
#Therefore. Choosing model_1 is appropriate since it has 2 less predictors and only ~1% reduction in fit

#check lineartiy assumptions of model with boxcox

library(faraway)
library(MASS)
vif(model_1) #GS has vif over 10, so removing that from model and comparing. GS has multicollinearity with GS.
plot(model_1$fitted.values,model_1$residuals, main = "residual plot of model_1")
abline(h=0,col="red")
acf(model_1$residuals) #acf looks fine, but slight lag around 14. #residuals not so great around lower
qqnorm(model_1$residuals)
qqline(model_1$residuals, col = "red") #normality issue
boxcox(model_1,lambda = seq(.1, .4, by = 0.05)) #variance issue


#new model without GS as a predictor
model_2 <- lm( Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB, data = data)
summary(model_2) #R^2 of .6485
anova(model_2,model_1) #F-test suggests keeping GS as predictor
#although F-test suggest keeping GS, we will remove it due to multicollinearity with GS. 
vif(model_2) #no VIF issues on model 2. Therefore use this model over model 1 since there is less than 1% difference in R^2. Remove GS from model, but keep GS.


#checking model 2 linearity assumptions
plot(model_2$fitted.values,model_2$residuals, main = "residual plot of model") #slight issue around lower fitted values
abline(h=0,col="red")
acf(model_2$residuals) #acf looks fine, but slight lag around 14. #residuals not so great around lower
qqnorm(model_2$residuals)
qqline(model_2$residuals, col = "red") #normality issue
boxcox(model_2,lambda = seq(.1, .4, by = 0.05)) #variance issue
plot(residuals(model_2))


#transforming model_2 using (Salary^.25) response variable transformation based on box cox plot above
trans_model <- lm((Actual^.25) ~ num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB, data = data)

boxcox(trans_model) #new boxcox shows that variance issue has been fixed.
acf(trans_model$residuals) #shows slight lag issue around 24
qqnorm(trans_model$residuals)
qqline(trans_model$residuals, col = "red")

plot(trans_model$fitted.values,trans_model$residuals, main = "residual plot of model") #residual plot looks good after transformation. 
abline(h=0, col = 'red')
summary(trans_model) #R^2 of .6295. There is reduction in R-squared, but now all linearity assumptions are met. 

vif(trans_model) #no multicollinearity issues

#new model has many issues with predictors, will try filtering data to remove players that did not play many minutes to improve model
#filter data by removing players that played less than 1000 minutes. This removes players that either got injured early in season or played very insignificant role on team. 
#1000 minutes played for last season means that the player averaged 13.8 MPG. The average role for a bench player in the NBA is usually around 20MPG. Therefore, it is acceptable to remove these players below 1000 Minutes played (MP)
filtered_data <- data %>% filter(MP > 1000)


#run new model on filtered data
regfull <- lm(Actual~.-Player-Guaranteed-url, data=filtered_data)
summary(regfull)
regnull <- lm(Actual~1, data=filtered_data)

step_reg <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

new_model <- lm(Actual~ num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920 + eFG. + Pos + AST, data = filtered_data)
summary(new_model)

#remove position from new model since most of these factors are not significant. Player salary is also more likely to be based on actual numerical statistics rather than position
new_model1 <-  lm(Actual~ num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920 + eFG. + AST, data = filtered_data)
summary(new_model1) #R^2 of .6492. Also remove assists since it is showing to be highly insignificant

new_model2 <- lm(Actual~ num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920 + eFG., data = filtered_data)
summary(new_model2) #R^2 increased from .6492 to .6506

anova(new_model2,new_model1) #partial F- test supports dropping AST variable due to P-value>.05. 

vif(new_model2) #num_teams_1920 and eFG. have extremely high VIF. must consider dropping one of these

new_model3 <- lm(Actual ~  num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920, data = filtered_data) #model with eFG. dropped
vif(new_model3) #no multicollinearity issues
summary(new_model3) #R^2 of .6444 and all predictors are significant
anova(new_model3,new_model2) #partial F-test supports keeping full model, but dropping the predictors num_teams_1920 and eFg since the 
#new_model_3 has R^2 of .6444. Although partial-F test supports keeping ANOVA, we must drop these terms since the reduction in R^2 is only about .05%.

#checking linearity assumptions of new_model3

boxcox(new_model3) #this plot suggests a .25 response variable transformation
acf(new_model3$residuals) #shows slight lag issue around 13
qqnorm(new_model3$residuals)
qqline(new_model3$residuals, col = "red") #data is mostly normal. except for a few data points

plot(new_model3$fitted.values,new_model3$residuals, main = "residual plot of model") #residual plot shows variance issues for lower points
abline(h=0, col = 'red')

#Transforming new filtered data with Actual^.25 response variable transformation

trans_model2 <- lm((Actual^.25) ~  num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920, data = filtered_data)
boxcox(trans_model2) #boxcox contains 1 within interval after transformation. transformation fixed variance issue
acf(trans_model2$residuals) #slight lag issue around lag 7 and lag 13 but very small/insignificant
qqnorm(trans_model2$residuals)
qqline(trans_model2$residuals, col = 'red') #normality improved alot after transformation

plot(trans_model2$fitted.values,trans_model2$residuals, main = "residual plot of model")
abline(h = 0, col= "red") #residual plot is now fixed after transformation
vif(trans_model2)

#R^2 of model below is .5968
summary(trans_model2) #this model has bad fit, will try pursuing other models to consider dropping insignificant variables

#dropping insignificant predictors from above
new_model4 <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS.+ NBA_titles + PF + num_teams_1920, data = filtered_data)
summary(new_model4) #fit went from .5968 to .5961 o we can keep this model as R^2 is essentially same

#consider dropping insignificant predictors from above.
new_model5 <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS.+ PF , data = filtered_data)
summary(new_model5) #R^2 of .5928. keep this model since R^2 is basically same as model before

#R^2 of .5911
final_model <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS., data = filtered_data)
summary(final_model) #this is final model that has best fit with no significance issues

#check linearity assumptions of this model.

boxcox(final_model) #boxcox contains 1 within interval. No variance issue
acf(final_model$residuals) #slight lag issue around lag 7 and lag 13 but very small/insignificant
qqnorm(final_model$residuals)
qqline(final_model$residuals, col = 'red') #normality assumption is met

plot(final_model$fitted.values,final_model$residuals, main = "residual plot of model")
abline(h = 0, col= "red") #residual plot shows no variance issue

#checking scatter plot of predictors and response variable to determine if any predictor variable transformation can improve fit


plot((filtered_data$Actual)^.25,filtered_data$yrs_exp, main = "yrs_exp vs Actual salary")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(final_model)



