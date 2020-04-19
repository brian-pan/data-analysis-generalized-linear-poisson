# load data
File = 'fijiDownload.RData'
load(File)

# explore data
head(fijiFull)

# First get rid of newly married women, and those who are not educated:
fijiSub = fiji[fiji$monthsSinceM > 0 & !is.na(fiji$literacy), ]
# make a offset and make it log-based:
fijiSub$logYrsSinceM = log(fijiSub$monthsSinceM/12)
# make 15-18 years old the reference group:
fijiSub$ageMarried = relevel(fijiSub$ageMarried, '15to18')
# make the woman in rural area the reference group:
fijiSub$urban = relevel(fijiSub$residence, 'rural')

# glm regression:
fijiGlm = glm(
  children ~ offset(logYrsSinceM) + ageMarried + urban + literacy + ethnicity,
  family = poisson (link = log), data = fijiSub)

summary(fijiGlm)

# make a 95% CI:
rate = cbind(est = fijiGlm$coefficients, confint(fijiGlm, level = 0.95))

# table
knitr::kable(cbind(summary(
  fijiRes)$coefficients,
  exp(rate)),
  digits = 2) 

# glm regression 2:
fijiGlm2 = glm(
  children ~ offset(logYrsSinceM) + ageMarried + urban + ethnicity,
  family = poisson(link = log), data = fijiSub)

summary(fijiGlm2)

# table 2
rate2 = cbind(est = fijiGlm2$coef, confint(fijiGlm2, level = 0.95))
knitr::kable(cbind(
  summary(fijiGlm2)$coef,
  exp(rate2)),
  digits=2)
