##---Cross-correlation------------------
require(caret)
evalq({preProClean <- preProcess(x = dataSet, 
		      		 method = c("zv", "nzv", "conditionalX", "corr"))
dataSetClean <- predict(preProClean, dataSet %>%
	     na.omit)},
	     env)
env$preProClean$method$remove
#[1] "v.rbci"
colnames(env$dataSetClean)
##-------outlier-------------------------------
#-------boxplot---Ris9---------
evalq(ggplot(dataSetClean, aes(x = factor(0), 
                               y = ftlm,
                               color = 'red')) + 
        geom_boxplot() + xlab("") + 
        scale_x_discrete(breaks = NULL) + 
        coord_flip(),
      env)
#------------------------
evalq({dataSetClean$ftlm -> x  
  out.ftlm <- x[!x %in% boxplot.stats(x)$out]}, 
  env)