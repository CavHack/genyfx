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





}})