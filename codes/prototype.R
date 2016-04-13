
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(Metrics)

Xtt <- read.csv("data/student-mat.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")
Xtt[, c("G1", "G2", "G3")] <- Xtt[, c("G1", "G2", "G3")] * 5
save(list = "Xtt", file = "data/Xtt.RData")

num_vars <- c("age", "failures", "absences", "G1", "G2", "G3")
cate_vars <- setdiff(colnames(Xtt), num_vars)

# Should be factors
# be_factors <- c("Medu", "Fedu", "famrel", "goout", "Dalc", "Walc", "health")
# for(i in seq(ncol(Xtt))){
#     if(colnames(Xtt)[i] %in% be_factors){
#         Xtt[,i] <- factor(Xtt[,i])
#     }
# }

actionable_vars <- c("studytime", "schoolsup", "activities", "Dalc", "absences", "paid", "traveltime", "freetime")
static_vars <- setdiff(colnames(Xtt), actionable_vars)
save(list = c("num_vars", "cate_vars", "actionable_vars", "static_vars"), file = "data/vars.RData")

# Required response variables:
# @ G1_cate
# @ G2
# @ G3MinusG2 (students who failed G2 or G1)

# G1_cate
Xtt$G1_cate <- unlist(sapply(Xtt$G1, function(x){
    if(x <= 50) return("Fail")
    else return("Pass")
}))
Xtt$G1_cate <- factor(Xtt$G1_cate)

# G3MinusG2
Xtt$G3MinusG2 <- Xtt$G3 - Xtt$G2

write.csv(Xtt, "data/Xtt.csv", row.names = FALSE)

# ====
# Prep for cross-validation

set.seed(86647)

# G1_cate
# Remove students who didnt take G1
r <- which(Xtt$G1 > 0)
Xtt_took_G1 <- Xtt[r,]

tr_idx          <- createDataPartition(Xtt_took_G1$G1_cate, p = 0.7, list = FALSE)
Xtrain_G1       <- Xtt_took_G1[ tr_idx,]
Xtest_G1        <- Xtt_took_G1[-tr_idx,]
ytrain_G1       <- Xtrain_G1$G1_cate
ytest_G1        <- Xtest_G1$G1_cate
Xtrain_G1       <- subset(Xtrain_G1, select = -c(G1, G1_cate, G2, G3, G3MinusG2))
Xtest_G1        <- subset(Xtest_G1, select = -c(G1, G1_cate, G2, G3, G3MinusG2))

# G2
# Remove students who didnt take G2 and G1
r <- which(Xtt$G1 > 0)
#r <- intersect(r, which(Xtt$G2 > 0))
Xtt_took_both <- Xtt[r,]

tr_idx          <- createDataPartition(Xtt_took_both$G2, p = 0.7, list = FALSE)
Xtrain_G2       <- Xtt_took_both[ tr_idx,]
Xtest_G2        <- Xtt_took_both[-tr_idx,]
ytrain_G2       <- Xtrain_G2$G2
ytest_G2        <- Xtest_G2$G2
Xtrain_G2       <- subset(Xtrain_G2, select = -c(G2, G3, G3MinusG2))
Xtest_G2        <- subset(Xtest_G2, select = -c(G2, G3, G3MinusG2))

# G3 (students who failed either G1 or G2)
# Also remove students who did not take the last exam (presumably G3 == 0)
r <- seq(1:nrow(Xtt))
r <- union(r, which(Xtt$G2 < 60))
r <- union(r, which(Xtt$G1 < 60))
r <- intersect(r, which(Xtt$G1 != 0))
r <- intersect(r, which(Xtt$G2 != 0))
#r <- intersect(r, which(Xtt$G3 != 0))
Xtt_fail <- Xtt[r,]

# Only use some variables
# lm0_G3_vars <- c("absences", "failures", "G1", "G2", "studytime",
#                  "activities", "famsup", "Dalc", "G3MinusG2")

lm0_G3_vars <- c("G3MinusG2", "age", "failures", "activities", "famrel", "G1_cate", "G2",
                 "schoolsup", "paid", "famsup", "romantic")

# lm0_G3_vars <- union(lm0_G3_vars, actionable_vars)
Xtt_fail <- Xtt_fail[,lm0_G3_vars]

tr_idx          <- createDataPartition(Xtt_fail$G3MinusG2, p = 0.85, list = FALSE)
Xtrain_G3       <- Xtt_fail[ tr_idx,]
Xtest_G3        <- Xtt_fail[-tr_idx,]
ytrain_G3       <- Xtrain_G3$G3MinusG2
ytest_G3        <- Xtest_G3$G3MinusG2
Xtrain_G3       <- subset(Xtrain_G3, select = -c(G3MinusG2))
Xtest_G3        <- subset(Xtest_G3, select = -c(G3MinusG2))

save(list = c("Xtrain_G3", "Xtest_G3", "ytrain_G3", "ytest_G3"), file = "data/G3.RData")
save(list = c("Xtrain_G2", "Xtest_G2", "ytrain_G2", "ytest_G2"), file = "data/G2.RData")
save(list = c("Xtrain_G1", "Xtest_G1", "ytrain_G1", "ytest_G1"), file = "data/G1.RData")


# ====
# Three models
# @ tr0_G1
# @ tr0_G2
# @ lm0_G3

# tr0_G1
tr0_G1 <- rpart(data = cbind(Xtrain_G1, ytrain_G1), ytrain_G1 ~.)
save(list = "tr0_G1", file = "models/tr0_G1.RData")

rpart.plot(tr0_G1)

tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
print(tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1))
print(tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb))

tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
print(tr0_G1_auc <- pROC::auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2]))

# tr0_G2
tr0_G2 <- rpart(data = cbind(Xtrain_G2, ytrain_G2), ytrain_G2 ~.)
save(list = "tr0_G2", file = "models/tr0_G2.RData")

rpart.plot(tr0_G2)

tr0_G2_pred <- predict(tr0_G2, newdata = Xtest_G2)
rmse(actual = ytest_G2, predicted = tr0_G2_pred)
plot(ytest_G2 ~ tr0_G2_pred)
abline(a = 0, b = 1)

# lm0_G3
lm0_G3 <- lm(data = cbind(Xtrain_G3, ytrain_G3), ytrain_G3 ~.)
summary(lm0_G3)

save(list = c("lm0_G3", "lm0_G3_vars"), file = "models/lm0_G3.RData")


lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
rmse(actual = ytest_G3, predicted = lm0_G3_pred)

#print(anova(lm0_G3))
plot(ytest_G3 ~ lm0_G3_pred)
abline(a = 0, b = 1)

# ====

# # Stepwise regression for G3
# r <- seq(1:nrow(Xtt))
# r <- union(r, which(Xtt$G2 < 50))
# r <- union(r, which(Xtt$G1 < 50))
# r <- intersect(r, which(Xtt$G1 != 0))
# r <- intersect(r, which(Xtt$G2 != 0))
# #r <- intersect(r, which(Xtt$G3 != 0))
# Xtt_step <- Xtt[r,]
# 
# Xtt_step <- subset(Xtt, select = -c(G3))
# step0 <- step(lm(data = Xtt_step, G3MinusG2~.), direction = "both")
# summary(step0)
# 
# # final model
# mod <- lm(data = Xtt_step, G3MinusG2 ~ age + failures + activities + famrel + G1_cate + G2
#           + schoolsup + paid + famsup + romantic)
# summary(mod)
