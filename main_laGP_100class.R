library(reticulate)
# library(plotly)
library(ramify)
library(mlegp)
library(wordspace)
library(rmarkdown)
library(MASS)
library(foreach)
library(umap)
library(laGP)
library(rmarkdown)

# setwd("/path/to/your/directory") # if run locally, set this to the directory of main.R
# setwd("/Users/yangchen/Desktop/test")

KL <- function(u1, u2, std1, std2){
  kl_divergence <- log(std1 / std2) + (std2^2 + (u1 - u2)^2) / (2 * std1^2) - 0.5 # log(std1 / std2) +
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
  ## Change here if you want to use different dataset
  df = read.csv(paste0("data100class_", toString(f), "/", trainset, "/train-large.csv")) # [,-1]
  set.seed(430)
  num_class = 100
  # print(dim(df))

  # Train dataset  
  # select.index <- sample(1:nrow(df), n_tr, replace = FALSE)
  # train.df <- df[select.index, ]
  # since the training set is already shuffled, so we just choose the first n_tr samples for training
  train.df <- df[1:n_tr, ]

  CNN_train_score = train.df[, (f+1):(f+num_class)]
  CNN_train_score = normalize.rows(as.matrix(exp(CNN_train_score)), method = "manhattan") #normalize
  
  Y <- rep(0,nrow(train.df))
  
  
  for (i in 1:num_class){  # weighted average of Y 
    Y <- Y + train.df[, (f+i)] * CNN_train_score[,i]
  }
  
  models = vector("list", num_class)
  cv_results = vector("list", num_class)
  
  
  # 100 different clusters 
  foreach (i=1:num_class) %do% {
    # print(i)
    #for(i in 1:10){  
    X <- train.df[train.df[,"label"]==i-1, 1:f]
    y <- train.df[train.df[,"label"]==i-1, (f+i)] # only look at the scores of correct label
    
    gpisep <- newGP(X, y, d=100, g=1e-4, dK=TRUE)
    
    cv_res = predGP(gpisep, X, lite=TRUE)
    
    cv_results[[i]] = cbind(cv_res$mean, sqrt(cv_res$s2)) # mean # VARIANCE

    models[[i]] = gpisep
    
    rm(gpisep)
  } 
  print("Train Finished")
  
  
  directory_path <- paste0("Rdata100class_laGP_", toString(n_tr), "_", toString(f))
  
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

  test.df = df[(n_tr+1):(n_tr+n_ts), ]
  
  for (testset in testsets){
    results_test = vector("list", 100)
    results_ood = vector("list", 100)
    
    t.df = read.csv(paste0("data100class_", toString(f), "/", trainset, "/", testset, "-large.csv")) # [,-1]
    
    # test.df = t.df[t.df[,"class"]=='test', ]
    # ood.df = t.df[t.df[,"class"]=='OOD', ]   
    
    # again ensure the size if right
    test.df = test.df[1:n_ts,]
    ood.df = t.df[1:n_ts,]
    print("test")
    
    test.X <- test.df[, 1:f]
    OOD.X <- ood.df[, 1:f]
    
    CNN_test_score = test.df[1:n_ts, (f+1):(f+100)]
    CNN_test_score = normalize.rows(as.matrix(exp(CNN_test_score)), method = "manhattan") #normalize
    CNN_OOD_score = ood.df[1:n_ts, (f+1):(f+100)]
    CNN_OOD_score = normalize.rows(as.matrix(exp(CNN_OOD_score)), method = "manhattan") #normalize
    
    for(i in 1:num_class){  # 10 different clusters 
      gpisep = models[[i]]
      results_test[[i]] <- predGP(gpisep, test.X, lite=TRUE)
      results_ood[[i]] <- predGP(gpisep, OOD.X, lite=TRUE)
    }
    
    # compute the predictive mean 
    y.test <- y.ood <- s2.test <- s2.ood <- rep(0, n_ts)
    for(i in 1:num_class){
      y.test <- y.test + results_test[[i]]$mean * CNN_test_score[,i]
      y.ood <- y.ood + results_ood[[i]]$mean * CNN_OOD_score[,i]
    }
    
    # compute the predictive variance 
    for(i in 1:num_class){
      s2.test <- s2.test + (results_test[[i]]$s2 + results_test[[i]]$mean^2) * CNN_test_score[,i]
      s2.ood <- s2.ood + (results_ood[[i]]$s2 + results_ood[[i]]$mean^2) * CNN_OOD_score[,i]
    }  
    
    #test.df = test.df #[1:n_ts, ]
    #ood.df = ood.df #[1:n_ts, ]
    # print('save trained model and data to:', paste0("Rdata_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData") )
   
    save(results_test, test.df, results_ood, ood.df, cv_results,
         file=paste0("Rdata100class_laGP_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData"))
  }
  
  print("Test Finished")
  # save(Y, models, file=paste0("Rdata/", trainset, "_models.RData"))
}  
  


get_argmax <- function(x) {
  return(which.max(x) - 1)
}


score_function <- function(trainset = "MNIST", testset = "FashionMNIST", q_ = 0.9, f = 16, n_tr = 1000, n_ts = 1000){
  setwd(getwd())
  load(file=paste0("Rdata100class_laGP_", toString(n_tr), "_", toString(f), "/", trainset, "_", testset, ".RData"))
  
  test.df$predictions = apply(test.df[, (f+1):(f+100)], 1, get_argmax) + 1
  ood.df$predictions = apply(ood.df[, (f+1):(f+100)], 1, get_argmax) + 1
  
  
  
  test.df$KL = 0
  ood.df$KL = 0
  test.df$mean = 0
  ood.df$mean = 0
  test.df$std = 0
  ood.df$std = 0
  for (i in (1:nrow(test.df))){
    class = test.df$predictions[i]
    test.df$mean[i] = results_test[[class]]$mean[i]
    test.df$std[i] = sqrt(results_test[[class]]$s2[i])
    test.df$KL[i] = KL_all(cv_results[[class]][,1], results_test[[class]]$mean[i], # mean
                           cv_results[[class]][,2], sqrt(results_test[[class]]$s2[i])) # SD
  }
  
  # Calculate KL for train data
  KL_list = c()
  for (i in (1:100)){
    cv_train = cv_results[[i]]
    kl_ = c()
    for (j in (1:nrow(cv_train))){
      kl_train = KL_all(cv_train[,1], cv_train[j,1], cv_train[,2], cv_train[j,2])
      kl_ = c(kl_, kl_train)
    }
    KL_list = c(KL_list, quantile(kl_, q_))
  }
  
  
  for (i in (1:nrow(ood.df))){
    class = ood.df$predictions[i]
    ood.df$mean[i] = results_ood[[class]]$mean[i]
    ood.df$std[i] = sqrt(results_ood[[class]]$s2[i])
    ood.df$KL[i] = KL_all(cv_results[[class]][,1], results_ood[[class]]$mean[i], # mean
                           cv_results[[class]][,2], sqrt(results_ood[[class]]$s2[i])) # SD
  }
  
  ID_acc_list = c()
  OOD_acc_list = c()
  ID_sum = 0
  OOD_sum = 0
  columns = 0

  for (i in 1:100){
    ID_acc = mean(test.df[test.df$predictions == i, ]$KL < KL_list[i])
    ID_sum = ID_sum + sum(test.df[test.df$predictions == i, ]$KL < KL_list[i])
    columns = columns + nrow(test.df[test.df$predictions == i, ])
    
    ID_acc_list = c(ID_acc_list, ID_acc)
    OOD_acc = mean(ood.df[ood.df$predictions == i, ]$KL > KL_list[i])
    OOD_sum = OOD_sum + sum(ood.df[ood.df$predictions == i, ]$KL > KL_list[i])
    OOD_acc_list = c(OOD_acc_list, OOD_acc)
  }
  print("n_ts:")
  print(n_ts)
  print("ind_acc")
  print(ID_acc_list)
  print("ood_acc")
  print(OOD_acc_list)



  # Create a list to store the dataframes
  result <- list(test.df = test.df, ood.df = ood.df, cv_results = cv_results,
                 ID_acc = ID_acc_list, OOD_acc = OOD_acc_list, ID_all = ID_sum/n_ts, OOD_all = OOD_sum/n_ts, 
                 AUROC = 1/2 - (1-OOD_sum/n_ts)/2 + ID_sum/n_ts/2)
  
  return(result)
}



# if run with arguments (Rscript --save main.R "ImageNet" 3000 1000 32)
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
    OOD_Datasets = c("DTD", "iNAT", "LSUN-C", "Places365") 
}

###########################  filter data  ############################
# InD_Dataset == "ImageNet"
# OOD_Datasets = c("DTD", "iSUN", "LSUN", "Places", "SUN") 


# train.df.selected = read.csv(paste0("data_", toString(f), "/", InD_Dataset, "/train.csv")) # [,-1]

# split_data <- split(train.df.selected, train.df.selected$label)

# top_10_indices_per_label <- list()

# for(label in names(split_data)) {
#   subset_df <- split_data[[label]]
#   distance_matrix <- as.matrix(dist(subset_df[, 1:32]))
#   median_distances <- apply(distance_matrix, 1, median)
#   sorted_data <- sort(median_distances, decreasing = TRUE)
#   index_90th_percentile <- round(length(sorted_data) * 0.9)
#   first_90_percent <- sorted_data[1:index_90th_percentile]
#   top_10_indices_per_label[[label]] <- first_90_percent
# }

# all_top_10_indices <- unlist(lapply(top_10_indices_per_label, FUN = function(x) as.numeric(names(x))))
# filtered_df <- train.df.selected[all_top_10_indices, ]
# print(dim(filtered_df))
# write.csv(filtered_df, "data_32/ImageNet/train_10.csv", row.names = FALSE)


###########################  fit model  ############################


model_fit_test(trainset = InD_Dataset, testsets = OOD_Datasets, n_tr = n_tr, n_ts = n_ts, f = f)  # Run only once



###########################  score function  ############################

list0.95_InD = c()
list0.95_OOD = c()
list0.95_AUROC = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.95, f = f, n_tr = n_tr, n_ts = n_ts)
  list0.95_InD = c(list0.95_InD, pred$ID_all)
  list0.95_OOD = c(list0.95_OOD, pred$OOD_all)
  list0.95_AUROC = c(list0.95_AUROC, pred$AUROC)
}

list0.9_InD = c()
list0.9_OOD = c()
list0.9_AUROC = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.9, f = f, n_tr = n_tr, n_ts = n_ts)
  list0.9_InD = c(list0.9_InD, pred$ID_all)
  list0.9_OOD = c(list0.9_OOD, pred$OOD_all)
  list0.9_AUROC = c(list0.9_AUROC, pred$AUROC)
}

list0.8_InD = c()
list0.8_OOD = c()
list0.8_AUROC = c()
for (OOD_Dataset in OOD_Datasets){
  pred = score_function(InD_Dataset, OOD_Dataset, q = 0.8, f = f, n_tr = n_tr, n_ts = n_ts)
  list0.8_InD = c(list0.8_InD, pred$ID_all)
  list0.8_OOD = c(list0.8_OOD, pred$OOD_all)
  list0.8_AUROC = c(list0.8_AUROC, pred$AUROC)
}

df = data.frame(InD_0.95 = list0.95_InD,
                   OOD_0.95 = list0.95_OOD,
                   AUROC_0.95 = list0.95_AUROC,
                   InD_0.9 = list0.9_InD,
                   OOD_0.9 = list0.9_OOD,
                   AUROC_0.9 = list0.9_AUROC,
                   InD_0.8 = list0.8_InD,
                   OOD_0.8 = list0.8_OOD,
                   AUROC_0.8 = list0.8_AUROC)
rownames(df) <- OOD_Datasets
print(paste0("InD - ", InD_Dataset))
print(paste0("features - ", args[4]))
print(paste0("n_tr - ", args[2]))
print(rownames(df))
print(df)
paged_table(df)

