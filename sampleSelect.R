require(NoiseFiltersR)
evalq({
#--------DT---------------------
out11 <- ORBoosterFilter(Class~., data = DT$train, N= 10, useDecisionStump = True)
DT$train_clean1 <- out11$cleanData
#-----DTTanh.n------------------------
  out1 <- ORBoostFilter(Class~., data = DTTanh.n$train, N = 10, useDecisionStump = TRUE)
  DTTanh.n$train_clean1 <- out1$cleanData
  #------DTn--------------------------------
  out12 <- ORBoostFilter(Class~., data = DTn$train, N = 10, useDecisionStump = TRUE)
  DTn$train_clean1 <- out12$cleanData
},
env)

#--------------------
require(funModeling)
evalq({
  par(mfrow = c(1,3))
  par(las = 1)
  boxplot(DT$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DT$train_clean1")
  boxplot(DTn$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DTn$train_clean1")
  boxplot(DTTanh.n$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DTTanh.n$train_clean1")
  par(mfrow = c(1,1))
}, env)
#-----------------------
require(GGally)
evalq(ggpairs(DT$train_clean1 %>% select(-Data), 
              columns = c(1:6, 13), 
              mapping = aes(color = Class),
              title = "DT$train_clean1/1"), 
      env)