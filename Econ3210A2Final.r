library(readr)
job_training <- read_csv("job_training.csv")

#creating DiD dataframe
newdf <- subset(job_training, select = -c(re14, re18))
newdf <- rbind(newdf, newdf)
newdf$tgroup <- ifelse(newdf$treated == 1,1,0)
re18.df <- data.frame(realIncome = job_training$re18, period = c(rep(1,nrow(job_training))))
re14.df <- data.frame(realIncome = job_training$re14, period = c(rep(0,nrow(job_training))))
newdf <- cbind(newdf,rbind(re14.df,re18.df))
newdf$treated[newdf$period == 0] <- 0 

library(fixest)
#first estimation
feols(realIncome ~ tgroup:period | factor(...1) + period[age, educ, black, hisp, married, nodegree, urban, fsize], data = newdf, vcov = "iid") #-58.6718

#ebalance
X <- subset(newdf, select = -c(...1,treated,realIncome,period,tgroup))
eb <- ebalance(Treatment = newdf$tgroup, X = X)
newdf$weights <- 1
newdf$weights[newdf$tgroup==0] <- eb$w 
#estimate with ebalance
feols(realIncome ~ tgroup:period | factor(...1) + period[age, educ, black, hisp, married, nodegree, urban, fsize], data = newdf, vcov = "iid", weights = newdf$weights) #463.193


#running double ML
job_training$incChange <- job_training$re18 - job_training$re14

job_training$age2 <- (job_training$age)^2
job_training$age3 <- (job_training$age)^3
job_training$educ2 <- (job_training$educ)^2
job_training$educ3 <- (job_training$educ)^3
job_training$fsize2 <- (job_training$fsize)^2
job_training$fsize3 <- (job_training$fsize)^3

newdf$age2 <- (newdf$age)^2
newdf$age3 <- (newdf$age)^3
newdf$educ2 <- (newdf$educ)^2
newdf$educ3 <- (newdf$educ)^3
newdf$fsize2 <- (newdf$fsize)^2
newdf$fsize3 <- (newdf$fsize)^3


XML <- (model.matrix(~ -1 + age + age2 + age3 + log(age) + educ + educ2 + educ3 + log(educ) + fsize + log(fsize) + fsize2 + fsize3 + (fsize + age + educ + black + hisp + married + nodegree + urban)^2, data = job_training))
XML.long <- (model.matrix(~ -1 + age + age2 + age3 + log(age) + educ + educ2 + educ3 + log(educ) + fsize + log(fsize) + fsize2 + fsize3 + (fsize + age + educ + black + hisp + married + nodegree + urban)^2, data = newdf))

DPred <- cv.glmnet(XML,job_training$treated)
coef(DPred)
YPred <- cv.glmnet(XML,job_training$incChange)
coef(YPred)

DML.DF <- subset(as.data.frame(XML.long),select = c(1,4,13,15,28,31,37,38))
ebML <- ebalance(Treatment = newdf$tgroup, X = DML.DF)
newdf$weightsML <- 1
newdf$weightsML[newdf$tgroup==0] <- ebML$w 

#double ML
feols(realIncome ~ tgroup:period | factor(...1) + period[age, log(age), black, married, age:married, educ:black, black:married, black:nodegree], data = newdf, vcov = "iid") 
#double ML + ebalance weights
feols(realIncome ~ tgroup:period | factor(...1) + period[age, log(age), black, married, age:married, educ:black, black:married, black:nodegree], data = newdf, vcov = "iid", weights = newdf$weightsML)


