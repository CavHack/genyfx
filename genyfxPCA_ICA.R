require(caret)
evalq({
	prePCA <- preProcess(x.cap, 
	       	  	     pcaComp = 5,
			     method = Hmisc::Cs(center,
			     scale, pca))
	preICA <- preProcess(x.cap,
	       	             n.comp = 3,
			     method = "ica")


}, env)

str(env$prePCA)
str(env$preICA)