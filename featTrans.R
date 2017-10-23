require(VIM)
require(foreach)
require(GGally)
require(ggvis)
require(discretization)
require(caret)
require(pipeR)
require(funModeling)
require(lubridate)

#Get rid of right skewness of the x data set
#take log base 2 and then remove outliers
#for values smaller than 1 add 1 and take log

evalq({x.ln <- apply(x, 2, function(x) log2(x+1))
	    sk.ln <- skewness(x.ln)},
	    env)
env$sk.ln

#for stlm, rbci, and v.ftlm remove and impute outliers 
#observe skewness

evalq({
	foreach(i = 1:ncol(x.ln), .combine="cbind") %do%
	{
		remove_outliers(x.ln[ , i])

	}->x.ln.out
	colnames(x.ln.out)<- colnames(x.ln)
}, env)
evalq({
  foreach(i = 1:ncol(x.ln), .combine = "cbind") %do% {
    capping_outliers(x.ln[ ,i])
  } -> x.ln.cap
  colnames(x.ln.cap) <- colnames(x.ln)
},  env)
evalq({
  sk.ln.out <- skewness(x.ln.out) 
  sk.ln.cap <- skewness(x.ln.cap)
}, 
env)
env$sk.ln.out
env$sk.ln.cap