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
