#----Prepare-------------
library(anytime)
library(rowr)
library(elmNN)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
library(clusterSim)
#---prepare--------------
evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt, 4000, 1000, 500, 250, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth = meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
}, env)
#---Data X --------------------------

evalq({
  list(
    pretrain = list(
      x = DTcap.n$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$pretrain$Class %>% as.numeric() %>% subtract(1) 
    ),
    train = list(
      x = DTcap.n$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$train$Class %>% as.numeric() %>% subtract(1) 
    ),
    test = list(
      x = DTcap.n$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$val$Class %>% as.numeric() %>% subtract(1) 
    ),
    test1 = list(
      x = DTcap.n$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
      y = DTcap.n$test$Class %>% as.numeric() %>% subtract(1) 
   )
  ) -> X
  
}, env)

#------------Order Predictor----------------------------------
require(clusterSim)
evalq({
  numFeature <- 10
  HINov.Mod(x = X$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4,
            distance = NULL, #"d1" - Manhattan, "d2" -Euclidean,
            #"d3" - Chebychev(max), "d4" - squared Euclidean, 
            #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
            method = "kmeans" ,#"kmeans" (default) , "single", 
            #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
            #"median", "centroid", "pam"
            Index = "cRAND") -> r
  r$stopri[ ,1] %>% head(numFeature) -> bestF
}, env)
print(env$r$stopri)

#-------Train--------------------------------
evalq({
  n <- 500
  r <- 7
  nh <- 5
  Xtrain <- X$pretrain$x[, bestF]
  Ytrain <- X$pretrain$y
  Ens <- foreach(i=i:n, .packages="elmNN") %do% {
    idx <- rminer:holdout(Ytrain, ratio=r/10, mode= "random")$tr
    elmtrain(x=Xtrain[idx, ], y= train[idx],
             nhid = nh, actfun = "sin")
  }
}, env)

#------Predict----------------------
evalq({
  Xtest <- X$train$x[, bestF]
  Ytest <- X$train$y
  foreach(i = i:n, .packages="elmNN", .combine = "cbind") %do% {
    predict(Ens[[i]], newdata= Xtest)
  } -> y.pr #[, n]
}, env)

evalq({
  numEns <- 3
  foreach(i = 1:n, .combine = "c") %do% {
    ifelse(y.pr[, i] > 0.5, 1, 0) -> Ypred
    Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
      mean()
  } -> Score
  Score %>% order(decreasing = TRUE) %>% head((numEns*2 + 1)) -> bestNN
  Score[bestNN] %>% round(3)
}, env)


