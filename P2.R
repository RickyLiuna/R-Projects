library(dplyr)
library(ggplot2)
library(ggfortify)
library(caret)
library(sjPlot)

########### Multiple Linear Regression ########
white=read.csv("~/Desktop/DATA2002/A2/winequality-white.csv" ,sep = ";")
old_names = colnames(white)
whitewine = white %>%
  janitor::clean_names()

#Data Description
GGally::ggpairs(whitewine) + theme_bw()

#Linear model
whitewine_lm= lm(formula = quality ~ . , data=whitewine)

#Checking p-value:citric_acid, total_sulfur_dioxide,chloride are insignificant for 5% alpha
summary(whitewine_lm)$coefficients %>% round(4)

#AIC:citric_acid, total_sulfur_dioxide,chloride dropped by step() function
whitewine_step=step(whitewine_lm)

#backward
step.back.aic = step(whitewine_lm,
                     direction = "backward"
                     ,
                     trace = FALSE)
round(summary(step.back.aic)$coef,3)
drop1(step.back.aic, test = "F")

#forwards
M0 = lm(quality ~ 1, data = whitewine) # Null model
M1 = whitewine_lm
step.fwd.aic = step(M0, scope = list(lower = M0, upper = M1), direction = "forward", trace = F)
round(summary(step.fwd.aic)$coef,3)
add1(step.fwd.aic, test = "F", scope = whitewine_lm)
drop1(step.fwd.aic, test = "F")

#comparison
sjPlot::tab_model(
  step.fwd.aic, step.back.aic,
  show.ci = FALSE,
  show.aic = TRUE,
  dv.labels = c("Forward model"
                ,
                "Backward model")
)

#Fitted model
summary(whitewine_step)$coefficients %>% round(4)
summary(whitewine_step)

#checking assumptions
autoplot(whitewine_step, which = 1:2) + theme_bw()
autoplot(step.back.aic, which = 1:2) + theme_bw()
autoplot(step.fwd.aic, which = 1:2) + theme_bw()


#Performance:
cv_full = train(
  quality ~ ., whitewine,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = FALSE))
cv_full







