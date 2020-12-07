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

reduced <- lm(data$Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB +GS, data = data)
summary(reduced)
anova(reduced,full_model)

summary(reduced)

#rename reduced as model_1 as this is the intial model that will be purused and linearity assumptions analyzed

model_1 <- lm( Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB +GS, data = data)

#model_1 above has R-squared of .6512. The R-squared of the full_model above is .6543
#Therefore. Choosing model_1 is appropriate since it has 4 less predictors and only ~1% reduction in fit

#check lineartiy assumptions of model with boxcox

library(faraway)
library(MASS)
vif(model_1) #GS has vif over 10, so removing that from model and comparing
plot(model_1$fitted.values,model_1$residuals, main = "residual plot of model")
abline(h=0,col="red")
acf(model_1$residuals) #acf looks fine, but slight lag around 14. #residuals not so great around lower
qqnorm(model_1$residuals)
qqline(model_1$residuals, col = "red") #normality issue
boxcox(model_1,lambda = seq(.1, .4, by = 0.05)) #variance issue



model_2 <- lm( Actual ~  num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB, data = data)
summary(model_2) #R^2 of .6485
anova(model_2,model_1) #F-test suggests keeping GS as predictor
vif(model_2) #no VIF issues on model 2. Therefor use this model over model 1 since there is less than 1% difference in R^2. Remove GS from model, but keep GS.


#checking model 2 linearity assumptions
plot(model_2$fitted.values,model_2$residuals, main = "residual plot of model") #slight issue around lower fitted values
abline(h=0,col="red")
acf(model_2$residuals) #acf looks fine, but slight lag around 14. #residuals not so great around lower
qqnorm(model_2$residuals)
qqline(model_2$residuals, col = "red") #normality issue
boxcox(model_2,lambda = seq(.1, .4, by = 0.05)) #variance issue
plot(residuals(model_2))


#transforming model using (Salary^.25) response variable transformation based on box cox plot
trans_model <- lm((Actual^.25) ~ num_accolades + PTS + yrs_exp + num_teams_played + GS. + NBA_titles + num_teams_1920 + PF + DRB, data = data)

boxcox(trans_model) #new boxcox shows that variance issue has been fixed.
acf(trans_model$residuals) #shows slight lag issue around 24
qqnorm(trans_model$residuals)
qqline(trans_model$residuals, col = "red")

plot(trans_model$fitted.values,trans_model$residuals, main = "residual plot of model") #residual plot looks good after transformation. 
abline(h=0, col = 'red')
summary(trans_model) #R^2 of .6295

vif(trans_model)

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

vif(new_model2) #num_teams_1920 and eFG. have extremely high VIF. must consider dropping these

new_model3 <- lm(Actual ~  num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920, data = filtered_data)
summary(new_model3)
anova(new_model3,new_model2) #partial F-test supports keeping full model
#new_model_3 has R^2 of .6444. Although partial-F test supports keeping ANOVA, we must drop these terms since the reduction in R^2 is only about .05%.

#checking linearity assumptions of new_model3

boxcox(new_model3) #
acf(new_model3$residuals) #shows slight lag issue around 14
qqnorm(new_model3$residuals)
qqline(new_model3$residuals, col = "red") #data is mostly normal. except for a few data points

plot(new_model3$fitted.values,new_model3$residuals, main = "residual plot of model") #residual plot shows variance issues for lower points
abline(h=0, col = 'red')

#Transforming new filtered data with Actual^.25 response variable transformation

trans_model2 <- lm((Actual^.25) ~  num_accolades + yrs_exp + num_teams_played + PTS + GS. + NBA_titles + PF + num_teams_1920, data = filtered_data)
boxcox(trans_model2) #boxcox contains 1 within interval after transformation. transformation fixed variance issue
acf(trans_model2$residuals) #slight lag issue around lag 7 and lag 14 but very small/insignificant
qqnorm(trans_model2$residuals)
qqline(trans_model2$residuals, col = 'red') #normality improved alot after transformation

plot(trans_model2$fitted.values,trans_model2$residuals, main = "residual plot of model")
abline(h = 0, col= "red") #residual plot is now fixed after transformation
vif(trans_model2)
summary(trans_model2) #this model has bad fit, will try pursuing other model. 


#using stepwise regression on filtered and transformed data
regfull <- lm((Actual^.25)~.-Player-Guaranteed-url, data=filtered_data)
summary(regfull)
regnull <- lm((Actual^.25) ~1, data=filtered_data)

step_reg <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

new_model4 <- lm((Actual^.25)~ num_accolades + yrs_exp + num_teams_played + PTS + GS.+ NBA_titles + PF + num_teams_1920, data = filtered_data)
summary(new_model4) #model from stepwise regression on transformed and filtered data is exact same as trans_model2. 
vif(new_model4)

new_model5 <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS.+ NBA_titles + PF + num_teams_1920, data = filtered_data)
summary(new_model5)

new_model6 <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS.+ PF , data = filtered_data)
summary(new_model6)

new_model7 <- lm((Actual^.25)~yrs_exp + num_teams_played + PTS + GS., data = filtered_data)
summary(new_model7) #this is final model that has best fit with no significance issues

#check linearity assumptions of this model.

boxcox(new_model7) #boxcox contains 1 within interval. No variance issue
acf(new_model7$residuals) #slight lag issue around lag 7 and lag 13 but very small/insignificant
qqnorm(new_model7$residuals)
qqline(new_model7$residuals, col = 'red') #normality assumption is met

plot(new_model7$fitted.values,new_model7$residuals, main = "residual plot of model")
abline(h = 0, col= "red") #residual plot is now fixed after transformation

#checking scatter plot of predictors and response variable to determine if any predictor variable transformation can improve fit


plot((filtered_data$Actual)^.25,filtered_data$yrs_exp, main = "yrs_exp vs Actual salary")
plot((filtered_data$Actual)^.25,filtered_data$num_teams_played, main = "num_teams_played vs Actual salary")
plot((filtered_data$Actual)^.25,filtered_data$PTS, main = "PTS vs Actual salary")
plot((filtered_data$Actual)^.25,filtered_data$GS., main = "GS. vs Actual salary")



