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