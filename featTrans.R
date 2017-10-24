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
#------Plot Skewness----------
par(mfrow = c(2,2))
boxplot(env$x.ln, 
        main = "x.ln with outliers",
        xlab = "")
boxplot(env$x.ln.out, 
        main = "x.ln.out without outliers",
        xlab = "")
boxplot(env$x.ln.cap, 
        main = "x.ln.cap with imputed outliers",
        xlab = "")
par(mfrow = c(1,1))
#------------------------
evalq(x.ln.cap %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetLnCap, 
      env)

require(GGally)
evalq(ggpairs(dataSetLnCap, columns = 2:7, 
              mapping = aes(color = Class),
              title = "PredLnCap1"), 
      env)
evalq(ggpairs(dataSetLnCap, columns = 8:13, 
              mapping = aes(color = Class),
              title = "PredLnCap2"), 
      env)
#######Fourier-Sinusoid-Transform############
evalq({x.sin <- apply(x, 2, function(x) sin(2*pi*x))
	     sk.sin <- skewness(x.sin)
},
	env)
#------------------------------------
evalq({foreach(i = 1:ncol(x.sin), .combine="cbind") %do% {
		 capping_outliers(x.sin[ , i])
} -> x.sin.out
  colnames(x.sin.out) <- colnames(x.sin)
}
, env})


evalq({
	foreach(i = 1:ncol(x.sin), .combine="cbind") %do% {
	capping_outliers(x.sin[, i])
}-> x.sin.cap
    colnames(x.sin.cap) <- colnames(x.sin)
}, env)

#-----------
evalq({
  sk.sin.out <- skewness(x.sin.out) 
  sk.sin.cap <- skewness(x.sin.cap)
}, 
env)
#-----------
env$sk.sin
env$sk.sin.out
sin.cap
#------Symmetric Transform-----
par(mfrow = c(2, 2))
boxplot(env$x.sin, main = "x.sin with outlier")
abline(h = 0, col = 2)
boxplot(env$x.sin.out, main = "x.sin.out without outlier")
abline(h = 0, col = 2)
boxplot(env$x.sin.cap, main = "x.sin.cap with capping outlier")
abline(h = 0, col = 2)
par(mfrow = c(1, 1))
##---------------------------
require(VIM)
evalq(a <- aggr(x.sin.out), env)
print(env$a)
#--------------------------
par(mfrow= c(3, 4))
evalq(
	foreach(i= 1:ncol(x.sin.out)) %do% {
	barMiss(x.sin.out, pos = i, only.miss= TRUE,
	main = "x.sin.out without outlier")
	
	}, env
)
par(mfrow=c(1,1))
evalq(x.sin.cap %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetSinCap, 
      env) 
require(GGally)
evalq(ggpairs(dataSetSinCap, columns = 2:7, 
              mapping = aes(color = Class),
              title = "dataSetSinCap1 with capping outlier "), 
      env)
evalq(ggpairs(dataSetSinCap, columns = 8:13, 
              mapping = aes(color = Class),
              title = "dataSetSinCap2 with capping outlier"), 
      env)
#----------------------------
##===========Normalize==========
evalq(
  {
    train = 1:2000
    val = 2001:3000
    test = 3001:4000
    DT <- list()
    clean = data.frame(dataSet) %>% na.omit()
    list(clean = clean, 
         train = clean[train, ], 
         val = clean[val, ], 
         test = clean[test, ]) -> DT
    rm(clean)
  }, 
env)
#---------------------
require(foreach)
evalq(
  {
    preProcess(DT$train, method = "spatialSign") -> preproc 
    list(train = predict(preproc, DT$train), 
         val = predict(preproc, DT$val),
         test = predict(preproc, DT$test)
    ) -> DTn
  }, 
  env)
#--------total statistics of the train set----
table.Stats(env$DTn$train %>% tk_xts())
#----Graph distribution-----------------
boxplot(env$DTn$train %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Train")
abline(v = 0, col = 2)
boxplot(env$DTn$test %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Test")
abline(v = 0, col = 2)
boxplot(env$DTn$val %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Val")
abline(v = 0, col = 2)
##--------correlation/covariation-------
require(GGally)
evalq(ggpairs(DTn$train, columns = 2:7, 
              mapping = aes(color = Class),
              title = "DTn$train1 "), 
      env)
evalq(ggpairs(DTn$train, columns = 8:14, 
              mapping = aes(color = Class),
              title = "DTn$train2"), 
      env)
#--------correlation of predictors------
funModeling::correlation_table(env$DTn$train %>% 
                                 tbl_df %>%
                                 select(-Data), str_target = 'Class')

#-----------relevance of v.fatl---------
require(ggvis)
evalq(
  DTn$train %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$train$v.fatl"),
  env)
evalq(
  DTn$val %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$val$v.fatl"),
  env)
evalq(
  DTn$test %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$test$v.fatl"),
  env) 
##=======discretization=============
require(discretization)
require(caret)
require(pipeR)
evalq(
  {
    dataSet %>%
      preProcess(., method = c("zv", "nzv", "conditionalX")) %>%
      predict(., dataSet) %>%
      na.omit -> dataSetClean
    train = 1:2000
    val = 2001:3000
    test = 3001:4000
    DT <- list()
    list(train = dataSetClean[train, ], 
         val = dataSetClean[val, ], 
         test = dataSetClean[test, ]) -> DT
  }, 
env)
#--------------------
evalq(
  pipeline({
    DT$train
    select(-Data)
    as.data.frame()
    mdlp()}) -> mdlp.train, envir = env)
#--------------------
env$mdlp.train%>%str()
#---------------------
evalq(
  {
    mdlp.train$cutp %>% 
      lapply(., function(x) is.numeric(x)) %>%
      unlist -> idx   # bool
    #----train-----------------
    mdlp.train$Disc.data[ ,idx] -> train.d
    #---test------------
    DT$test %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> test.d
    
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {findInterval(test.d[ ,i], 
                                vec = mdlp.train$cutp[[i]],
                                rightmost.closed = FALSE, 
                                all.inside = F,
                                left.open = F)}
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$test$Class) -> test.d
    colnames(test.d) <- colnames(train.d)
    #-----val-----------------
    DT$val %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> val.d
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {findInterval(val.d[ ,i], 
                                vec = mdlp.train$cutp[[i]],
                                rightmost.closed = FALSE, 
                                all.inside = F,
                                left.open = F)}
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$val$Class) -> val.d
    colnames(val.d) <- colnames(train.d)
  },env
)

#-----------------
env$train.d %>% head()
env$test.d %>% head()
env$val.d %>% head()
env$train.d$v.fatl %>% table()
env$test.d$v.fatl %>% table()
env$val.d$v.fatl %>% table()
#-----cross plotting---------
require(funModeling)
evalq(
	cross_plot(data= train.d,
	str_input = c("v.fatl", "ftlm", "v.satl"),
	str_target = "Class",
	auto_binning = F,
	plot_type = "both"), #quantity 'perceptron'
	env
)	

evalq(
  cross_plot(
      DT$train  %>% select(-Data) %>%
      select(c(v.satl, ftlm, v.fatl, stlm, v.rstl, Class)) %>%
      as.data.frame(), 
      str_input = Cs(v.satl, ftlm, v.fatl, stlm, v.rstl), 
      str_target = "Class", 
      auto_binning = T,
      plot_type = "both"), #'quantity' 'percentual'
  env
)

##----BayesianTraining-----------
evalq(
  {
    bayesian_plot(train.d, input = "v.fatl", 
                  target = "Class", 
                  title = "Bayesian comparison train$v.fatl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(train.d, input = "ftlm", 
                  target = "Class", 
                  title = "Bayesian comparison train$ftlm/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(train.d, input = "v.satl", 
                  target = "Class", 
                  title = "Bayesian comparison train$v.satl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env