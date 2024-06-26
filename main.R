library(reticulate)
library(plotly)
library(ramify)
library(mlegp)
library(wordspace)
library(rmarkdown)
library(MASS)
library(foreach)
library(umap)


KL <- function(u1, u2, std1, std2){
  kl_divergence <- log(std1 / std2) + (std2^2 + (u1 - u2)^2) / (2 * std1^2) - 0.5
  return(kl_divergence)
}

KL_all <- function(u1_list, u2, std1_list, std2){
  # if first calculate KL distance then take the mean
  KLs = c()
  for (i in 1:length(u1_list)){
    distance = KL(u1_list[i], u2, std1_list[i], std2)
    KLs = c(KLs, distance)
  }
  return(mean(KLs, na.rm = TRUE))
  
  # # if first calculate mean of cv_mean and cv_std, then calculate KL distance once
  # u1 = mean(u1_list)
  # std1 = mean(std1_list)
  # distance = KL(u1, u2, std1, std2)
  # return(distance)
}

model_fit_test <- function(trainset = "MNIST", testsets = c("FashionMNIST"), n_tr = 1000, n_ts = 1000, f = 16){
  df = read.csv(paste0("data_", toString(f), "/", trainset, "/train.csv")) # [,-1]
  set.seed(430)
  feature_names <- paste("f", 1:32, sep = "")
  label_names <- paste("l", 1:10, sep = "")
  column_names <- c(feature_names, label_names, "label")
  colnames(df) <- column_names

  # Train dataset  
  select.index <- sample(1:nrow(df), n_tr, replace = FALSE)
  train.df <- df[select.index, ]
  train.df <- df[1:n_tr, ]
  CNN_train_score = train.df[, (f+1):(f+10)]
  CNN_train_score = normalize.rows(as.matrix(exp(CNN_train_score)), method = "manhattan") #normalize
  
  
  Y <- rep(0,nrow(train.df))
  
  
  for (i in 1:10){  # weighted average of Y 
    Y <- Y + train.df[, (f+i)] * CNN_train_score[,i]
  }
  
  models = vector("list", 10)
  cv_results = vector("list", 10)
  
  # 10 different clusters 
  foreach (i=1:10) %do% {
  #for(i in 1:10){  
    X <- train.df[train.df[,"label"]==i-1, 1:f]
    y <- train.df[train.df[,"label"]==i-1, (f+i)] # only look at the scores of correct label
    
    fit <- mlegp(data.matrix(X), data.matrix(y)) #, nugget.known = 0, nugget = NULL)  # , nugget = 1e-4
    
    
    
    cv_results[[i]] = CV(fit)  # mean # VARIANCE
    cv_results[[i]][,2] = sqrt(cv_results[[i]][,2]) # calculate std based on variance
    
    models[[i]] = fit
      
    rm(fit)
  } 
  
  print("Train Finished")
  
  directory_path <- paste0("Rdata_", toString(n_tr), "_", toString(f))
  
  # Use dir.create() to create the directory
  if (!dir.exists(directory_path)) {
    if (dir.create(directory_path)) {
      cat("Directory created successfully:", directory_path, "\n")
    } else {
      cat("Failed to create the directory:", directory_path, "\n")
    }
  } else {
    cat("Directory already exists:", directory_path, "\n")
  }
  
  for (testset in testsets){
    results_test = vector("list", 10)
    results_ood = vector("list", 10)
    
    t.df = read.csv(paste0("data_", toString(f), "/", trainset, "/", testset, "_test.csv"))# [,-1]
    
    test.df = t.df[t.df[,"class"]=='test', ]
    ood.df = t.df[t.df[,"class"]=='OOD', ]    
    
    test.X <- test.df[1:n_ts, 1:f]
    OOD.X <- ood.df[1:n_ts, 1:f]
    
    CNN_test_score = test.df[1:n_ts, (f+1):(f+10)]
    CNN_test_score = normalize.rows(as.matrix(exp(CNN_test_score)), method = "manhattan") #normalize
    CNN_OOD_score = ood.df[1:n_ts, (f+1):(f+10)]
    CNN_OOD_score = normalize.rows(as.matrix(exp(CNN_OOD_score)), method = "manhattan") #normalize
    
    for(i in 1:10){  # 10 different clusters 
      fit = models[[i]]
      results_test[[i]] <- predict(fit, data.matrix(test.X),se.fit=TRUE)
      results_ood[[i]] <- predict(fit, data.matrix(OOD.X),se.fit=TRUE)
    }
    
    # compute the predictive mean 
    y.test <- y.ood <- s2.test <- s2.ood <- rep(0, n_ts)
    for(i in 1:10){
      y.test <- y.test + results_test[[i]]$fit * CNN_test_score[,i]
      y.ood <- y.ood + results_ood[[i]]$fit * CNN_OOD_score[,i]
    }
    
    # compute the predictive variance 
    for(i in 1:10){
      s2.test <- s2.test + (results_test[[i]]$se.fit^2 + results_test[[i]]$fit^2) * CNN_test_score[,i]
      s2.ood <- s2.ood + (results_ood[[i]]$se.fit^2 + results_ood[[i]]$fit^2) * CNN_OOD_score[,i]
    }  
    
    test.df = test.df[1:n_ts, ]
    ood.df = ood.df[1:n_ts, ]
    print('save trained model and data to:', paste0("Rdata_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData") )
    save(results_test, test.df, results_ood, ood.df, cv_results,
         file=paste0("Rdata_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData"))
  }
  
  print("Test Finished")
  # save(Y, models, file=paste0("Rdata/", trainset, "_models.RData"))
}  
  


get_argmax <- function(x) {
  return(which.max(x) - 1)
}


score_function <- function(trainset = "MNIST", testset = "FashionMNIST", q = 0.9, f = 16, n_tr = 1000, n_ts = 1000){

  load(file=paste0("Rdata_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData"))
  
  test.df$predictions = apply(test.df[, (f+1):(f+10)], 1, get_argmax) + 1
  ood.df$predictions = apply(ood.df[, (f+1):(f+10)], 1, get_argmax) + 1
  
  
  
  test.df$KL = 0
  ood.df$KL = 0
  test.df$mean = 0
  ood.df$mean = 0
  test.df$std = 0
  ood.df$std = 0
  for (i in (1:nrow(test.df))){
    class = test.df$predictions[i]
    test.df$mean[i] = results_test[[class]]$fit[i]
    test.df$std[i] = results_test[[class]]$se.fit[i]
    test.df$KL[i] = KL_all(cv_results[[class]][,1], results_test[[class]]$fit[i], # mean
                           cv_results[[class]][,2], results_test[[class]]$se.fit[i]) # SD
  }
  
  # Calculate KL for train data
  KL_list = c()
  for (i in (1:10)){
    cv_train = cv_results[[i]]
    kl_ = c()
    for (j in (1:nrow(cv_train))){
      kl_train = KL_all(cv_train[,1], cv_train[j,1], cv_train[,2], cv_train[j,2])
      kl_ = c(kl_, kl_train)
    }
    KL_list = c(KL_list, quantile(kl_, q))
  }
  
  
  for (i in (1:nrow(ood.df))){
    class = ood.df$predictions[i]
    ood.df$mean[i] = results_ood[[class]]$fit[i]
    ood.df$std[i] = results_ood[[class]]$se.fit[i]
    ood.df$KL[i] = KL_all(cv_results[[class]][,1], results_ood[[class]]$fit[i], # mean
                           cv_results[[class]][,2], results_ood[[class]]$se.fit[i]) # SD
  }
  
  ID_acc_list = c()
  OOD_acc_list = c()
  ID_sum = 0
  OOD_sum = 0

  for (i in 1:10){
    ID_acc = mean(test.df[test.df$predictions == i, ]$KL < KL_list[i])
    ID_sum = ID_sum + sum(test.df[test.df$predictions == i, ]$KL < KL_list[i])
    ID_acc_list = c(ID_acc_list, ID_acc)
    OOD_acc = mean(ood.df[ood.df$predictions == i, ]$KL > KL_list[i])
    OOD_sum = OOD_sum + sum(ood.df[ood.df$predictions == i, ]$KL > KL_list[i])
    OOD_acc_list = c(OOD_acc_list, OOD_acc)
  }

  # Create a list to store the dataframes
  result <- list(test.df = test.df, ood.df = ood.df, cv_results = cv_results,
                 ID_acc = ID_acc_list, OOD_acc = OOD_acc_list, ID_all = ID_sum/n_ts, OOD_all = OOD_sum/n_ts)
  
  return(result)
}

# 
args <- commandArgs(trailingOnly = TRUE)
InD_Dataset = args[1]
n_tr = as.integer(args[2])
n_ts = as.integer(args[3])

f = as.integer(args[4])
if (InD_Dataset == "MNIST"){
    OOD_Datasets = c("FashionMNIST", "Cifar_10", "SVHN", "Imagenet_r", "Imagenet_c")
} else if (InD_Dataset == "FashionMNIST"){
    OOD_Datasets = c("MNIST", "Cifar_10", "SVHN", "Imagenet_r", "Imagenet_c")
} else if (InD_Dataset == "Cifar_10"){
    OOD_Datasets = c("SVHN", "Imagenet_r", "Imagenet_c")
} else if (InD_Dataset == "ImageNet"){
    OOD_Datasets = c("DTD", "iSUN", "LSUN", "Places", "SUN") # c("INaturalist", "SUN", "Places", "DTD")
}
print("########")
print(OOD_Datasets)

model_fit_test(trainset = InD_Dataset, testsets = OOD_Datasets, n_tr = n_tr, n_ts = n_ts, f = f)  # Run only once, specify the training samples to use

# df = read.csv(paste0("data_", toString(f), "/", InD_Dataset, "/", OOD_Datasets[1], "_test.csv"))[,-1]
# plot <- plot_ly(data = df, x = ~df[, f+11], y = ~df[, f+12], text = ~label, mode = "markers")

# # Customize the appearance (optional)
# plot <- plot %>%
#   layout(
#     title = "Scatter Plot",
#     xaxis = list(title = "X1"),
#     yaxis = list(title = "X2")
#   )

# # Save the plot as an HTML file
# htmlwidgets::saveWidget(plot, "scatter_plot.html")



list0.95_InD = c()
list0.95_OOD = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.95, f = f, n_tr = n_tr)
  list0.95_InD = c(list0.95_InD, pred$ID_all)
  list0.95_OOD = c(list0.95_OOD, pred$OOD_all)
}

list0.9_InD = c()
list0.9_OOD = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.9, f = f, n_tr = n_tr)
  list0.9_InD = c(list0.9_InD, pred$ID_all)
  list0.9_OOD = c(list0.9_OOD, pred$OOD_all)
}

list0.8_InD = c()
list0.8_OOD = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.8, f = f, n_tr = n_tr)
  list0.8_InD = c(list0.8_InD, pred$ID_all)
  list0.8_OOD = c(list0.8_OOD, pred$OOD_all)
}

df = data.frame(InD_0.95 = list0.95_InD, 
                   OOD_0.95 = list0.95_OOD,
                   InD_0.9 = list0.9_InD, 
                   OOD_0.9 = list0.9_OOD,
                   InD_0.8 = list0.8_InD, 
                   OOD_0.8 = list0.8_OOD)
rownames(df) <- OOD_Datasets
print(paste0("InD - ", InD_Dataset))
print(paste0("features - ", args[4]))
print(paste0("n_tr - ", args[2]))
paged_table(df)


# try