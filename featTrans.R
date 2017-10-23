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