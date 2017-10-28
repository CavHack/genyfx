##-------Cleave-------
require(magrittr)
require(caret)

evalq({

	train = 1:2000
	val = 2001:3000
	test = 3001:4000
	DT <- list()
	dataSet %>% 
	preProcess(., method = c("zv", "nzv", "conditionalX")) %>%
	predict(., dataSet) %>%
	na.omit -> dataSetClean
	list(train = dataSetClean[train, ], 
	val = dataSetClean[val, ],
	test = dataSetClean[test, ]) -> DT
	rm(dataSetClean, train, val, test)

}, env)

#-----outlier---------------
require(foreach)
require(dplyr)
evalq({
	DTcap <- list()
	foreach(i = 1:3) %do% {
		  DT[[i]] %>%
		  	  select(-c(Data, Class)) %>%
			  as.data.frame() -> x

			  if ( i == 1) {
			     foreach(i = 1:ncol(x), .combine = "cbind") %do% {

			     	       prep.outlier(x[, i]) %>% unlist()
			     

			     } -> pre.outl
			     colnames(pre.outl) <- colnames(x)
			  
			  }
			  foreach(i = 1:ncol(x), .combine = "cbind") %do% {

			  	    stopifnot(exists("pre.outl", envir=env))
				    lower = pre.outl['lower .25%', i]
				    upper = pre.outl['upper.75%', i]
				    med = []
			  

			  }

	}

})
