#Mohammed A. Al Muhaymin
#Econometric and Statistics

#####Question 1########

#H not = Man and woman are both equally live in the same state that they are born
#H alternative = Man and woman DO NOT live equally in the same state that they are born

#z = I will use z statistic for proportion 

#x1bar(female mean) <= 0.5557
#x2bar (male mean) <= 0.5384 
#zigma1 <= 
#zigma2 <= 
#n1    <= 45087
#n2  <= 50579

#Using the formula to get test_statistic <- (x1bar - x2bar) / sqrt((zigma1^2 / n1) + (zigma2^2 / n2))

#Unable

#####Question 2 ########

summary(acs2021)
my_subset <- use_varb <- (acs2021$AGE>=25) & (acs2021$AGE <= 55) & (acs2021$MARST == 1) & (acs2021$FAMSIZE > 3) & (acs2021$NCHILD > 0) & (acs2021$CITIZEN = 2) & (acs2021$EMPSTAT = 1)
dat_use <- subset(acs2021,my_subset)
dat_use <- subset(acs2021,my_subset)
summary(my_subset)

#Mode   FALSE    TRUE 
#logical 2916268  336331 

#My subset has people age 25 to 55 which is the prime age, they are citizen
#They are married with spouses present
#Their family size is greater than 3
#They have more than 1 children at least
#They are employed

#I choose this subset because I want to look at family members and one of the reason people move 
#is because of getting married or having children(family reasons). It looked interesting to me
#it focuses a specific sub group(family) which people values.


######Question 3 ######
ols_out1 <- lm(live_same_state_born ~ SEX + AGE + EDUC, data=dat_use)
summary(ols_out1)
#For now trying with age and sex and education

#Residuals:
  #Min      1Q  Median      3Q     Max 
#-0.6838 -0.4705 -0.2031  0.4973  0.9925 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    0.4782896  0.0080531  59.392  < 2e-16 ***
  #SEXFemale                     -0.0087633  0.0016973  -5.163 2.43e-07 ***
  #AGE                           -0.0068427  0.0001184 -57.813  < 2e-16 ***
 # EDUCNursery school to grade 4 -0.0856523  0.0146050  -5.865 4.51e-09 ***
  #EDUCGrade 5, 6, 7, or 8       -0.0036749  0.0082591  -0.445  0.65635    
#EDUCGrade 9                    0.0278057  0.0102187   2.721  0.00651 ** 
 # EDUCGrade 10                   0.2208626  0.0110832  19.928  < 2e-16 ***
  #EDUCGrade 11                   0.2610964  0.0106475  24.522  < 2e-16 ***
 # EDUCGrade 12                   0.3206185  0.0063330  50.627  < 2e-16 ***
  #EDUC1 year of college          0.3442090  0.0066185  52.007  < 2e-16 ***
  #EDUC2 years of college         0.3765939  0.0066974  56.230  < 2e-16 ***
  #EDUC4 years of college         0.2816109  0.0063289  44.496  < 2e-16 ***
  #EDUC5+ years of college        0.2179326  0.0064015  34.044  < 2e-16 ***
  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4887 on 336318 degrees of freedom
#Multiple R-squared:  0.04101,	Adjusted R-squared:  0.04097 
#F-statistic:  1198 on 12 and 336318 DF,  p-value: < 2.2e-16

#This is just a simple a ols and it run 
#The p value is 2.2 e ^ -16 and Pr(|t|) is very small. This means the sex of the person 
#and the person's age and education as well has great significant on whether they move or not

#Type 1 and Type 2 Error
pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = dat_use$live_same_state_born)

#true
#pred   0     1
#FALSE 92789 57663
#TRUE  86035 99844
#We can see that that getting the correction is higher than getting the type 1 or type 2 error in this confusion matrix.

#####Now ATTEMPT at using Mixing and separating into female and male, 

# for now, really simplify the education dummy
dat_use$BA_plus <- dat_use$educ_college + dat_use$educ_advdeg


# Here we are people who are moving, people who have bachelor degree plus, 
#their age, and mixed of female, and ba plus, and mix of age and female
model_lpm_v1 <- lm(live_same_state_born ~ female + BA_plus + AGE + I(female*BA_plus) + I(AGE * female) + I(male * BA_plus) + I(AGE * male), data = dat_use)
summary(model_lpm_v1)

dat_use_female <- subset(dat_use,as.logical(dat_use$female))
dat_use_male <- subset(dat_use,!(dat_use$female))

##Here the data is splitting two parts -- female and male
model_lpm_v1f <- lm(live_same_state_born ~ BA_plus + AGE, data = dat_use_female)
summary(model_lpm_v1f)
model_lpm_v1m <- lm(live_same_stare_born ~ BA_plus + AGE, data = dat_use_male)
summary(model_lpm_v1m)


#OLS Type 1 and Type 2 Error
pred_vals_ols2 <- predict(olsmodel, dat_use)
pred_model_ols2 <- (pred_vals_ols2 > mean(pred_vals_ols2))
table(pred = pred_model_ols1, true = dat_use$live_same_state_born)


########Question 4#########
#ATTEMPT at using more dummies to see the impact
ols_out3 <- lm(live_same_state_born ~ female + male + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE + white + african-american + american indian + chinese + japanese, data=dat_use2)
summary(ols_out3)



###I try to make it better by adding more dummy variable using race dummy variables
#This regression will let us know how each race moves in addition to people with education and gender


###### Question 5 ##########
model_logit1 <- glm(live_same_state_born~ AGE + SEX+ EDUC, data = dat_use,family = binomial)
summary(model_logit1)

#Coefficients:
  #Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                   -0.2876058  0.0392126  -7.335 2.23e-13 ***
# AGE                           -0.0285783  0.0004985 -57.324  < 2e-16 ***
  #SEXFemale                     -0.0370210  0.0071102  -5.207 1.92e-07 ***
  #EDUCNursery school to grade 4 -0.7983878  0.1015318  -7.863 3.74e-15 ***
  #EDUCGrade 5, 6, 7, or 8       -0.0144053  0.0442256  -0.326 0.744634    
#EDUCGrade 9                    0.1945396  0.0524634   3.708 0.000209 ***
 # EDUCGrade 10                   1.1269487  0.0506325  22.257  < 2e-16 ***
 # EDUCGrade 11                   1.2929194  0.0486907  26.554  < 2e-16 ***
  #EDUCGrade 12                   1.5343124  0.0334282  45.899  < 2e-16 ***
 # EDUC1 year of college          1.6298201  0.0343593  47.435  < 2e-16 ***
 # EDUC2 years of college         1.7623826  0.0346442  50.871  < 2e-16 ***
#  EDUC4 years of college         1.3773591  0.0334223  41.211  < 2e-16 ***
 # EDUC5+ years of college        1.1164189  0.0336947  33.133  < 2e-16 ***
  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 464902  on 336330  degrees of freedom
#Residual deviance: 450307  on 336318  degrees of freedom
#AIC: 450333

#Number of Fisher Scoring iterations: 4

#In the logit model, age and sex also has huge significance. In addition, education has great
#significance on moving. It is plausible because people with higher age just move to
#differnt place due to job and personal reason. Also, people with education tend to move
#because of school or attending college.

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$live_same_state_born)

#true
#pred   0      1
#FALSE 118622  82212
#TRUE   60202  75295
#The number of correction is also higher than getting a type 1 or type 2 in this confusion matrix.




#Other Models
#I will attempt to use random forest and another model with package e2071 

#Random Forest
set.seed(54321)
model_randFor <- randomForest(as.factor(live_same_state_born) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$live_same_state_born)


#Other model
install.packages("e1071")
library(e1071)
# tuned_parameters <- tune.svm(as.factor(pub_work) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:2)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(live_same_state_born) ~ ., data = sobj$data, cost = 1, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$live_same_state_born)

