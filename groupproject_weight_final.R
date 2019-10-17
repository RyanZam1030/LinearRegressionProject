###### Multiple Linear Regression Model ###################
###### Published BY: Ryan Zamora ##########################
###########################################################


###### Reading the dataset and putting into a dataframe ###
###########################################################
library(readr)
body_dat <- read_delim("C:/Users/Ryan/Desktop/Linear Reggression/Project/body.dat.txt",
                       " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)


###### Summary and dimensions of the dataframe ############
###########################################################
View(body_dat)
summary(body_dat)
write.table(t(summary(body_dat)), "summary", sep="\t") 
boxplot(body_dat)
dim(body_dat)


###### Splitting the data into a training/test set ########
###########################################################
indexes = sample(1:nrow(body_dat), size = 0.1*nrow(body_dat))
test = body_dat[indexes,]
dim(test)
View(test)
body_dat = body_dat[-indexes,]
dim(body_dat)

###### Assigning the variables ############################
###########################################################
# Dependent variable
weight = body_dat$X23

# Regressor variables
biacromial_diam = body_dat$X1
biiliac_diam = body_dat$X2
birtrochan_diam = body_dat$X3
chest_depth = body_dat$X4
chest_diam = body_dat$X5
elbow_diam = body_dat$X6
wrist_diam = body_dat$X7
knee_diam = body_dat$X8
ankle_diam = body_dat$X9
shoulder_girth = body_dat$X10
chest_girth = body_dat$X11
waist_girth = body_dat$X12
navel_girth = body_dat$X13
hip_girth = body_dat$X14
thigh_girth = body_dat$X15
bicep_girth = body_dat$X16
forearm_girth = body_dat$X17
knee_girth = body_dat$X18
calf_max_girth = body_dat$X19
ankle_min_girth = body_dat$X20
wrist_min_girth = body_dat$X21
age = body_dat$X22
height = body_dat$X24
gender = body_dat$X25


###### Checking for nomality of response variable ##########
############################################################
plot(weight)
boxplot(weight, horizontal = TRUE, xlab = "Weight (kg)")
title(" Boxplot of Weight w/o Outliers")
hist(weight, xlab = "Weight (kg)")
qqnorm(weight, main = "Normal Q-Q Plot of Weight")
qqline(weight)


###### Making corrilation plots ############################
############################################################
a = cor(biacromial_diam, weight)
b = cor(biiliac_diam, weight)
c = cor(birtrochan_diam, weight)
d = cor(chest_depth, weight)
e = cor(chest_diam, weight)
f = cor(elbow_diam, weight)
g = cor(wrist_diam, weight)
h = cor(knee_diam, weight)
i = cor(ankle_diam, weight)
j = cor(shoulder_girth, weight)
k = cor(chest_girth, weight)
l = cor(waist_girth, weight)
m = cor(navel_girth, weight)
n = cor(hip_girth, weight)
o = cor(thigh_girth, weight)
p = cor(bicep_girth, weight)
q = cor(forearm_girth, weight)
r = cor(knee_girth, weight)
s = cor(calf_max_girth, weight)
t = cor(ankle_min_girth, weight)
u = cor(wrist_min_girth, weight)
v = cor(age, weight)
w = cor(height, weight)
x = cor(gender, weight)

corr = c(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

# Making the corrilation plots
library(corrgram)
corrgram(body_dat, lower.panel = panel.cor,
         col.regions = colorRampPalette(c("green","blue", "purple", "black")))
title("Multi-correlation Chart")

###### Original Regrestion model ###########################
############################################################
regmodel = lm(weight ~ biacromial_diam + biiliac_diam + birtrochan_diam + 
              chest_depth + chest_diam + elbow_diam + wrist_diam + 
              knee_diam + ankle_diam + shoulder_girth + chest_girth + 
              waist_girth + navel_girth + hip_girth + thigh_girth + 
              bicep_girth + forearm_girth + knee_girth + calf_max_girth + 
              ankle_min_girth + wrist_min_girth + age + height + as.factor(gender))


####### Original Regression model analysis #################
############################################################
summary(regmodel)
anova(regmodel)

# Placing the term in variable names
terms1 = summary(regmodel)
r_sq1 = terms1$r.squared
r_sq_ad1 = terms1$adj.r.squared
fstat1 = terms1$fstatistic


# Comparing coeffecients vs correlation 
coeff = regmodel$coefficients
coeff = coeff[2:24]
comp = data.frame(coeff, corr)

# Residual plots
plot(regmodel)

# Partial residual plots
library(car)
crPlots(regmodel)

# Transforming the response variable
transy = log(weight)
boxplot(transy, horizontal = TRUE, xlab = "log(Weight) (kg)")
title(" Boxplot of Log Transformation of Weight")
hist(transy, main = paste("Histogram of Log Transformation of Weight"), xlab = "Log(Weight) (kg)")
qqnorm(transy, main = "Normal Q-Q Plot of Log Transformation of Weight")
qqline(transy)

tregmodel = lm(transy ~ biacromial_diam + biiliac_diam + birtrochan_diam + 
                 chest_depth + chest_diam + elbow_diam + wrist_diam + 
                 knee_diam + ankle_diam + shoulder_girth + chest_girth + 
                 waist_girth + navel_girth + hip_girth + thigh_girth + 
                 bicep_girth + forearm_girth + knee_girth + calf_max_girth + 
                 ankle_min_girth + wrist_min_girth + age + height + as.factor(gender))
summary(tregmodel)
anova(tregmodel)
plot(tregmodel)


###### Variable selection procedure ########################
############################################################
# Reassigning the Dependent variable
weight = body_dat$X23
full = regmodel
null = lm(weight ~ 1, data = body_dat)
foward = step(null, scope = list(lower = null, upper = full), direction = "forward")
backward = step(full, data = body_dat, direction = "backward")
stepwise = step(null, scope = list(upper = full), direction = "both")
summary(foward)
summary(backward)
summary(stepwise)
anova(foward)
anova(backward)
anova(stepwise)

for_coeff = foward$coefficients
back_coeff = backward$coefficients
step_coeff = stepwise$coefficients
vspdf = data.frame(for_coeff, back_coeff, step_coeff)

terms2 = summary(stepwise)
r_sq2 = terms2$r.squared
r_sq_ad2 = terms2$adj.r.squared
fstat2 = terms2$fstatistic


###### All possible regressors #############################
############################################################
library(leaps)
leaps = regsubsets(formula((regmodel)), data = body_dat, nbest = 1,nvmax = NULL, 
                   force.in = NULL, force.out = NULL, intercept = TRUE, 
                   method = "exhaustive")
regsub_out = summary(leaps)
r_sq3 = regsub_out$rsq
rss = regsub_out$rss
r_adj3 = regsub_out$adjr2
cp1 = regsub_out$cp
bic1 = regsub_out$bic
outmat = regsub_out$outmat

# Vector to make Msres
n_p = c(506, 505, 504, 503, 502, 501, 500, 499, 498, 497, 496, 495, 
        494, 493, 492, 491, 490, 489, 488, 487, 486, 485,484, 483)
msres1 = rss / (n_p-1)

# Data frame of all possible regressors metrix
metrixdf = data.frame(r_sq3, rss, r_adj3, msres1, cp1, bic1)
write.table(metrixdf, file = "Original APR",sep = "\t")

######## Taking out the outliers ##########################
###########################################################
fit = influence.measures(regmodel)
outliers = which(apply(fit$is.inf,1,any))
remove_out = body_dat[-outliers,]


###### Summary and dimensions of new dataframe ############
###########################################################
View(remove_out)
t(summary(remove_out))
dim(remove_out)


###### Assigning the new variables ########################
###########################################################
weight = remove_out$X23
biacromial_diam = remove_out$X1
biiliac_diam = remove_out$X2
birtrochan_diam = remove_out$X3
chest_depth = remove_out$X4
chest_diam = remove_out$X5
elbow_diam = remove_out$X6
wrist_diam = remove_out$X7
knee_diam = remove_out$X8
ankle_diam = remove_out$X9
shoulder_girth = remove_out$X10
chest_girth = remove_out$X11
waist_girth = remove_out$X12
navel_girth = remove_out$X13
hip_girth = remove_out$X14
thigh_girth = remove_out$X15
bicep_girth = remove_out$X16
forearm_girth = remove_out$X17
knee_girth = remove_out$X18
calf_max_girth = remove_out$X19
ankle_min_girth = remove_out$X20
wrist_min_girth = remove_out$X21
age = remove_out$X22
height = remove_out$X24
gender = remove_out$X25


###### Checking for nomality of response variable ##########
############################################################
plot(weight)
hist(weight)
qqnorm(weight)
qqline(weight)

###### New regression model without outliers ###############
############################################################
newregmodel = lm(weight ~ biacromial_diam + biiliac_diam + birtrochan_diam + 
                chest_depth + chest_diam + elbow_diam + wrist_diam + 
                knee_diam + ankle_diam + shoulder_girth + chest_girth + 
                waist_girth + navel_girth + hip_girth + thigh_girth + 
                bicep_girth + forearm_girth + knee_girth + calf_max_girth + 
                ankle_min_girth + wrist_min_girth + age + height + as.factor(gender))

summary(newregmodel)
anova(newregmodel)
coeff = t(newregmodel$coefficients)

# Making the corrilation plots
library(corrgram)
corrgram(remove_out, lower.panel = panel.cor,
         col.regions = colorRampPalette(c("green","blue", "purple", "black")))
title("Multi-correlation Chart w/o Outliers")


# New residual plots
plot(newregmodel)


######## Taking out the outliers ##########################
###########################################################
fit = influence.measures(tregmodel)
outliers = which(apply(fit$is.inf,1,any))
remove_out = body_dat[-outliers,]

dim(remove_out)

###### Assigning the new variables ########################
###########################################################
weight = remove_out$X23
biacromial_diam = remove_out$X1
biiliac_diam = remove_out$X2
birtrochan_diam = remove_out$X3
chest_depth = remove_out$X4
chest_diam = remove_out$X5
elbow_diam = remove_out$X6
wrist_diam = remove_out$X7
knee_diam = remove_out$X8
ankle_diam = remove_out$X9
shoulder_girth = remove_out$X10
chest_girth = remove_out$X11
waist_girth = remove_out$X12
navel_girth = remove_out$X13
hip_girth = remove_out$X14
thigh_girth = remove_out$X15
bicep_girth = remove_out$X16
forearm_girth = remove_out$X17
knee_girth = remove_out$X18
calf_max_girth = remove_out$X19
ankle_min_girth = remove_out$X20
wrist_min_girth = remove_out$X21
age = remove_out$X22
height = remove_out$X24
gender = remove_out$X25

newtransy = log(weight)

boxplot(newtransy, horizontal = TRUE, xlab = "log(Weight) (kg)")
title(" Boxplot of Log Transformation of Weight w/o Outliers")
hist(newtransy, main = paste("Histogram of Log Transformation of Weight W/o Outliers"), xlab = "Log(Weight) (kg)")
qqnorm(newtransy, main = "Normal Q-Q Plot of Log Transformation of Weight W/o Outliers")
qqline(newtransy)

tnewregmodel = lm(newtransy ~ biacromial_diam + biiliac_diam + birtrochan_diam + 
                 chest_depth + chest_diam + elbow_diam + wrist_diam + 
                 knee_diam + ankle_diam + shoulder_girth + chest_girth + 
                 waist_girth + navel_girth + hip_girth + thigh_girth + 
                 bicep_girth + forearm_girth + knee_girth + calf_max_girth + 
                 ankle_min_girth + wrist_min_girth + age + height + as.factor(gender))
summary(tnewregmodel)
anova(tnewregmodel)
plot(tnewregmodel)

###### New variable selection ##############################
############################################################
full = newregmodel
null = lm(weight ~ 1, data = remove_out)
foward = step(null, scope = list(lower = null, upper = full), direction = "forward")
backward = step(full, data = remove_out, direction = "backward")
stepwise = step(null, scope = list(upper = full), direction = "both")
summary(foward)
summary(backward)
summary(stepwise)
anova(foward)
anova(backward)
anova(stepwise)

for_coeff2 = foward$coefficients
back_coeff2 = backward$coefficients
step_coeff2 = stepwise$coefficients
vspdf2 = data.frame(for_coeff2, back_coeff2, step_coeff2)

coeff4 = regmodel$coefficients
terms4 = summary(regmodel)
r_sq4 = terms1$r.squared
r_sq_ad4 = terms1$adj.r.squared
fstat4 = terms1$fstatistic

###### New all posible regressor ###########################
############################################################
library(leaps)
leaps = regsubsets(formula((newregmodel)), data = remove_out, nbest = 1, 
                   nvmax = NULL, force.in = NULL, force.out = NULL, 
                   intercept = TRUE, method = "exhaustive")
regsub_out = summary(leaps)
r_sq5 = regsub_out$rsq
rss = regsub_out$rss
r_adj5 = regsub_out$adjr2
cp2 = regsub_out$cp
bic2 = regsub_out$bic
outmat = regsub_out$outmat

n_p = n_p - 23
msres2 = rss / n_p

# New data frame of all possible regressors metrix
newmetrixdf = data.frame(r_sq5, rss, r_adj5, msres2, cp2, bic2)
write.table(newmetrixdf, file = "Second APR",sep = "\t")

###### COmparing the final r_sq, rsq_adj ################
#########################################################
final_r_sqdf = data.frame(r_sq1, r_sq2, r_sq3, r_sq4, r_sq5)
final_r_sq_ad = data.frame(r_sq_ad1, r_sq_ad2, r_adj3, r_sq_ad4, r_adj5)

###### Variable Iflation Factors ########################
#########################################################
vifdf = data.frame(vif(regmodel), vif(newregmodel),vif(tregmodel), vif(tnewregmodel))
write.table(vifdf, file = "Vif",sep = "\t")

###### Cross validating Models ##########################
#########################################################
conf = predict.lm(regmodel, newdata = test, interval = "confidence",level=0.95)
pred = predict(regmodel, newdata = test, interval = "prediction",level=0.95)
summary(pred) 

# Assigning the actual y values
y = test$X23

# Using the test set to find the difference between pred values and actual for all 6 models
z = as.vector(regmodel$coefficients)
x = within(test, rm(X23))
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - y
error = sum(resid^2)
error
plot(predy, y)
plot(predy, resid)

z = as.vector(newregmodel$coefficients)
x = within(test, rm(X23))
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - y
error2 = sum(resid^2)
error2
plot(predy, y)
plot(predy, resid)

z = as.vector(step_coeff)
x = within(test, rm(X1,X3, X6, X7, X9, X13, X16,X20, X21,X23))
x = data.frame(test$X12, test$X18, test$X24, test$X15, test$X11, 
               test$X19, test$X17, test$X14, test$X8, test$X22, 
               test$X4, test$X25, test$X10, test$X2, test$X5)
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - y
error5 = sum(resid^2)
error5
plot(predy, y)
plot(predy, resid)

z = as.vector(step_coeff2)
x = within(test, rm(X1,X3, X7, X9, X10, X13, X20, X21, X23))

x = data.frame(test$X11, test$X18, test$X24, test$X14, test$X12, 
               test$X15, test$X17, test$X19, test$X2, test$X22, 
               test$X4, test$X8, test$X5, test$X6, test$X25, test$X16)
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - y
error6 = sum(resid^2)
error6
plot(predy, y)
plot(predy, resid)

ty = log(y)
z = as.vector(tregmodel$coefficients)
x = within(test, rm(X23))
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - ty
error3 = sum(resid^2)
error3
plot(predy, ty)
plot(predy, resid)

z = as.vector(tnewregmodel$coefficients)
x = within(test, rm(X23))
ones = rep(1, 50)
newx = cbind(ones,x)
predmat = t(newx) * z
predy = colSums(predmat)
resid = predy - ty
error4 = sum(resid^2)
error4
plot(predy, ty)
plot(predy, resid)

errordf = data.frame(error, error2, error5, error6, error3, error4)
write.table(errordf, file = "Error",sep = "\t")

