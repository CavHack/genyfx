library(anytime)
library(rowr)
library(elmNN)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
library(clusterSim)
library(doRNG)

#----prepare-----
evalq({
	dt <- PrepareData(Data, Open, High, Low, Close, Volume)
	DT <- SplitData(dt, 4000, 1000, 500, 250, start = 1)
	pre.outl <- PreOutlier(DT$pretrain)
	DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
	preproc <- PreNorm(DTcap, meth = meth)
	DTcap.n <- NormData(DTcap, preproc = preproc)
	#---1--Data X -----------
	list(
			pretrain = list(
			x = DTcap.n$pretrain %>% dplyr::select(-c(Data, Class)) %>%
			as.data.frame(),
			y = DTcap.n$pretrain$class %>% as.numeric() %>% subtract(1)),
			
			train = list(
			x = DTcap.n$train %>% dlpyr::select(-c(Data, Class)) %>%
			as.data.frame(),
			y = DTcap.n$train$Class %>% as.numeric() %>% subtract(1)),
			test = list(
      		x = DTcap.n$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
    	    y = DTcap.n$val$Class %>% as.numeric() %>% subtract(1) 
   		 ),
   			 test1 = list(
    		  x = DTcap.n$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
    		  y = DTcap.n$test$Class %>% as.numeric() %>% subtract(1) 
    )
  ) -> X
	
#---2----MetricSelection------
  #require(clusterSim)
  numFeature <- 10
  HINoV.Mod(x = X$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
            distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
            #"d3" - Chebychev (max), "d4" - squared Euclidean, 
            #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
            method = "kmeans" ,#"kmeans" (default) , "single", 
            #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
            #"median", "centroid", "pam"
            Index = "cRAND") %$% stopri[ ,1] -> orderX
  orderX %>% head(numFeature) -> bestF
}, env)

	
