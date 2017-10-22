##---Cross-correlation------------------
require(caret)
evalq({preProClean <- preProcess(x = dataSet, 
		      		 method = c("zv", "nzv", "conditionalX", "corr"))
dataSetClean <- predict(preProClean, dataSet %>%
	     na.omit)},
	     env)
env$preProClean$method$remove
#[1] "v.rbci"
colnames(env$dataSetClean)
##-------outlier-------------------------------
