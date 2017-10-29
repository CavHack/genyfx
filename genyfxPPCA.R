#3++++===Probabilistic PCA====++++
require(pcaMethods)
evalq({
	DTcap.n$train %>% tbl_df %>%
		select(-Class) %>% as.matrix() -> train
		DTcap.n$val %>% tbl_df %>%
		select(-Class) %>% tbl_df %>%
		resPPCA <- pca(train, method = "ppca",
				center = TRUE, scale = "none",
				nPcs = 3, completeObs = FALSE,
				subset = 	NULL, cv = "none", 
					maxIterations = 3000)
	
}, env)
#-----------------
print(env$resPPCA)
#------------------
slplot(env$resPPCA, pcs = c(1,2),
	lcex= 0.9, sub = "Probabilistic PCA")
plotPcs(env$resPPCA, type = "scores")
plotPcs(env$resPPCA, type = "loadings")
#----------------------
evalq({
	ppcaTrain <- resPPCA@scores %>% tbl_df %>%
		cbind(., Class = DTcap.n$train$Class)
		ppcaVal <- predict(resPPCA, val)$scores %>%
		tbl_df %>% cbind(., Class = DTcap.n$val$Class)
		ppcaTest <- predict(resPPCA, test)$scores %>%
		tbl_df %>% cbind(., Class = DTcap.n$test$Class)
	
	
}, env)
