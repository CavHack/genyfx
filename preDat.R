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
#-----
evalq({dataSetClean$ftlm -> x 
  out.ftlm1 <- x[x > quantile(x, .25) - 1.5*IQR(x) & 
          x < quantile(x, .75) + 1.5*IQR(x)]},
  env)
evalq(all.equal(out.ftlm, out.ftlm1), env)
nrow(env$dataSetClean) - length(env$out.ftlm)
#-------Ris10-------------
boxplot(env$out.ftlm, main = "ftlm  without outliers", 
        boxwex = 0.5)
#---remove--outliers------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), 
                  na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#----------