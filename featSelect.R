#-----Cleave--------
require(caret)
require(pipeR)
evalq(
{
	train = 1:2000
	val = 2001:3000
	test = 3001:4000
	DT <- list()
	dataSet %>%
	preProcess(., method=c("zv", "nzv", "conditionalX")) %>%
	predict(., dataSet) %>%
	na.omit -> dataSetClean
	list(train = dataSetClean[train, ], 
	val = dataSetClean[val,],
	test = dataSetClean[test,]) -> DT
	rm(dataSetClean, train, val, test)

}, 
env)

#------outlier------------
evalq({
# define new list for the result
	DTcap <- list()
#go through the three sets of data
	foreach(i = 1:3) %do% {
	DT[[i]] %>%

#remove columns with (Data, Class)
	select(-c(Data, Class)) %>%
#transform into data.frame and store in temporary variable x
	as.data.frame() -> x
if(i == 1) {
#define params of outliers in the first input
foreach(i = 1:ncol(x), .combine = "cbind") %do% {
	  prep.outlier(x[ , i]) %>% unlist()

} -> pre.outl
  colnames(pre.outl) <- colnames(x)

}

#substitute outliers for 5/95 % and store the result in x.cap
foreach(i = 1:ncol(x), .combine = "cbind") %do% {
      stopifnot(exists("pre.outl", envir = env))
      lower = pre.outl['lower.25%', i] 
      upper = pre.outl['upper.75%', i]
      med = pre.outl['med', i]
      cap1 = pre.outl['cap1.5%', i] 
      cap2 = pre.outl['cap2.95%', i] 
      treatOutlier(x = x[ ,i], impute = T, fill = T, 
                   lower = lower, upper = upper, 
                   med = med, cap1 = cap1, cap2 = cap2) 
      } %>% as.data.frame() -> x.cap
    colnames(x.cap) <- colnames(x)
    return(x.cap)
  } -> DTcap
  rm(lower, upper, med, cap1, cap2, x, x.cap)
}, env)

#----logTransform-----
evalq({
	DTLn <- list()
	foreach(i = 1:3) %do% {
	DTcap[[ i ]] %>%
	apply(., 2, function(x) log2(x+1)) %>%
	as.data.frame() %>%
	cbind(., Class= DT[[i]] $Class)
	} -> DTLn

}, env)

#------sintrans--------------
evalq({
  DTSin <- list()
  foreach(i = 1:3) %do% {
    DTcap[[i]] %>% 
      apply(., 2, function(x) sin(2*pi*x)) %>%
      as.data.frame() %>%
      cbind(., Class = DT[[i]]$Class)
  } -> DTSin
},
env)
#------tanhTrans----------
evalq({
  DTTanh <- list()
  foreach(i = 1:3) %do% {
    DTcap[[i]] %>% 
      apply(., 2, function(x) tanh(x)) %>%
      as.data.frame() %>%
      cbind(., Class = DT[[i]]$Class)
  } -> DTTanh
},
env)
#------normalize-----------
evalq(
  {
#define parameters of normalization
    preProcess(DT$train, method = "spatialSign") -> preproc 
    list(train = predict(preproc, DT$train), 
         val = predict(preproc, DT$val),
         test = predict(preproc, DT$test)
    ) -> DTn
  }, 
  env) 
#--ln---
evalq(
  {
    preProcess(DTLn[[1]], method = "spatialSign") -> preprocLn 
    list(train = predict(preprocLn, DTLn[[1]]), 
         val = predict(preprocLn, DTLn[[2]]),
         test = predict(preprocLn, DTLn[[3]])
    ) -> DTLn.n
  }, 
  env)
#---sin---
evalq(
  {
    preProcess(DTSin[[1]], method = "spatialSign") -> preprocSin 
    list(train = predict(preprocSin, DTSin[[1]]), 
         val = predict(preprocSin, DTSin[[2]]),
         test = predict(preprocSin, DTSin[[3]])
    ) -> DTSin.n
  }, 
  env)
#-----tanh-----------------
evalq(
  {
    preProcess(DTTanh[[1]], method = "spatialSign") -> preprocTanh 
    list(train = predict(preprocTanh, DTTanh[[1]]), 
         val = predict(preprocTanh, DTTanh[[2]]),
         test = predict(preprocTanh, DTTanh[[3]])
    ) -> DTTanh.n
  }, 
  env)
##------discretize----------
#--------preCut---------------------
require(pipeR)
require(discretization)
evalq(
  #require(pipeR) time !!!
  pipeline({
    DT$train
    select(-Data)
    as.data.frame()
    mdlp() 
  }) -> mdlp.train, 
  env)
#-------cut_opt----------
evalq(
  {
    DTd <- list()
    mdlp.train$cutp %>% 
# define the columns that have to be discretized.
      lapply(., function(x) is.numeric(x)) %>%
      unlist -> idx   # bool
    #----train-----------------
    mdlp.train$Disc.data[ ,idx] -> DTd$train 
    #---test------------
    DT$test %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> test.d
    #rearrange data according to calculated ranges
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {
        findInterval(test.d[ ,i], 
        vec = mdlp.train$cutp[[i]],
        rightmost.closed = FALSE, 
        all.inside = F,
        left.open = F)
        }
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$test$Class) -> DTd$test
    colnames(DTd$test) <- colnames(DTd$train)
    #-----val-----------------
    DT$val %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> val.d
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {
        findInterval(val.d[ ,i], 
        vec = mdlp.train$cutp[[i]],
        rightmost.closed = FALSE, 
        all.inside = F,
        left.open = F)
        }
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$val$Class) -> DTd$val 
    colnames(DTd$val) <- colnames(DTd$train)
    rm(test.d, val.d)
  }, 
  env
)



}})