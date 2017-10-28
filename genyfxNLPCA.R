source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
library(pcaMethods)
evalq({
	DTcap.n$train %>% tbl_df %>%
	select(-Class) %>% as.matrix() %>%
	prep(scale = "none", center = TRUE) -> train
	resNLPCA <- pca((train, 
                  method = "nlpca", weightDecay = 0.01,
                  unitsPerLayer = c(3, 8, 12),
                  center = TRUE, scale = "none",# c("none", "pareto", "vector", "uv")
                  nPcs = 3, completeObs = FALSE, 
                  subset = NULL, cv = "none", # "none""q2"), ...) 
                  maxSteps = 1100)
  rm(train)},
  env)

#------------------------------

eval(pcTrain <- resNLPCA@scores %>% tbl_df %>%
	cbind(., Class = DTcap.n$train$Class), env)
	
#----graph------
require(GGally)
evalq({
	
	(pcTrain, columns = 1:ncol(pcTrain),
		mapping = aes(color = Class),
			title = "pcTrain -> NLPCA(3-8-12) wd = 0.01"
	
)}, env)

#-----------

load <- loadings(env$resNLPCA)
#------------------------------

print(env$resNLPCA)
summary(env$resNLPCA)
evalq(plot(x = resNLPCA, y = NULL,
				main = deparse(substitute(object)),
				col = gray(c(0.9, 0.5))), env)
				
				evalq(slplot(resNLPCA), env)
				nPcs()
				nObs()
				cvstat()
				nVar()
				loadings(env$resNLPCA) #Get loadings
				scores() # Get the scores
				dim() #Get the dimensions (number of observations, number of features)
				centered() #get a logical heuristic of centroid approx.
					center() #avg of original variables
						completeObs() #Get the imputed data set
						method() #Get a string naming the used PCA method
						sDev() #get the standard deviation
						scaled()	#get a logical heuristic indicating if scaling was done.