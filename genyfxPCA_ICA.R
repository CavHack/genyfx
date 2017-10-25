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

#---PCA/ICA----
require(magrittr)
evalq({
	pca <- predict(prePCA, x.cap)
	ica <- predict(preICA, x.cap)
	pca1 <- ((x.cap %>% scale(prePCA$mean, prePCA$std))

%*%
		prePCA$rotation)
	ica1 <- ((x.cap %>% scale(preICA$mean, preICA$std))
%*%
	preICA$ica$K) %*% preICA$ica$W
colnames(ica1) <- colnames(ica1, do.Null = FALSE, prefix = 'ICA')
} env)

evalq(all_equal(pca, pca1), env)
# [1] TRUE
evalq(all_equal(ica, ica1), env)
# [1] TRUE