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

