#-------Prepare---------
library(anytime)
library(rowr)
library(darch)
library(rBayesianOptimization)
library(forEach)
library(magrittr)
#source(file = "")

evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt, 4000, 1000, 500, 100, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth=meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
} env)
#-------Data DT -----------
evalq({
  foreach(i = 1:4) %do% {
    DTcap.n[[i]] %>% dplyr::select(-c(v.rstl, v.pcci))
  } -> DT
  list(pretrain = DT[[1]],
       train  = DT[[2]],
       val = DT[[3]],
       test = DT[[4]]) -> DT
} env)

#---Data X-------------
evalq({
  list(
    pretrain = list(
      x = DT$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$pretrain$Class %>% as.data.frame()
    ),
    train = list(
      x = DT$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$train$Class %>% as.data.frame()
    ),
    test = list(
      x = DT$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$val$Class %>% as.data.frame()
    ),
    test1 = list(
      x = DT$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
      y = DT$test$Class %>% as.vector()
    )
  ) -> X
}, env)