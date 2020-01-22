multiplecolumnImputation = function(data, k, ntree, rows, columns = c(1:dim(data)[2]), n_columns, iterations, histogram = FALSE) {

  library(DMwR)
  library(missForest)
  library(mice)
  library(mi)
  library(Amelia)
  library(Metrics)
  library(TSdist)
  library(ggplot2)

  EmptyDataFrame = as.data.frame(matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2]))

  list_indexes_multiple = list()
  for( h in 1:iterations ) {
    tmp = matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2])
    C = sample(columns, size = n_columns)
    for( i in C ) {
      tmp[,i] = sample(which(!is.na(data[,i])), size = dim(data)[1] * rows) }
    list_indexes_multiple[[h]] = tmp }



  list_observed_KNN_multiple = list()
  list_predicted_KNN_multiple = list()
  observed_multiple_KNN = EmptyDataFrame
  predicted_multiple_KNN = EmptyDataFrame
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_KNN[,i] = data[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = NA }
    set.seed(8)
    KNN = knnImputation(data[,columns], k = k)
    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      KNN$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(KNN)[2] < dim(data)[2] ) {
        KNN = KNN[,c(1:(j-1), dim(KNN)[2], j:(dim(KNN)[2] - 1))]
      } else { KNN = KNN[,c(1:(j-1), dim(KNN)[2])] }
      colnames(KNN)[j] = colnames(data)[j] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_KNN[,i] = KNN[list_indexes_multiple[[h]][,i], i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_KNN[,i] }
    head(predicted_multiple_KNN)
    list_observed_KNN_multiple[[h]] = observed_multiple_KNN
    list_predicted_KNN_multiple[[h]] = predicted_multiple_KNN
    print(paste("Done with KNN, iteration", h, sep = "\ ")) }




  list_observed_amelia_multiple = list()
  list_predicted_amelia_multiple = list()
  observed_multiple_amelia = EmptyDataFrame
  predicted_multiple_amelia = EmptyDataFrame
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_amelia[,i] = data[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = NA }
    set.seed(8)
    amelia = amelia(data[,columns])
    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      amelia$imputations$imp5$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(amelia$imputations$imp5)[2] < dim(data)[2] ) {
        amelia$imputations$imp5 = amelia$imputations$imp5[,c(1:(j-1), dim(amelia$imputations$imp5)[2], j:(dim(amelia$imputations$imp5)[2] - 1))]
      } else { amelia$imputations$imp5 = amelia$imputations$imp5[,c(1:(j-1), dim(amelia$imputations$imp5)[2])] }
      colnames(amelia$imputations$imp5)[j] = colnames(data)[j] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_amelia[,i] = amelia$imputations$imp5[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_amelia[,i] }
    head(predicted_multiple_amelia)
    list_observed_amelia_multiple[[h]] = observed_multiple_amelia
    list_predicted_amelia_multiple[[h]] = predicted_multiple_amelia
    print(paste("Done with Amelia, iteration", h, sep = "\ ")) }





  list_observed_missForest_multiple = list()
  list_predicted_missForest_multiple = list()
  observed_multiple_missForest = EmptyDataFrame
  predicted_multiple_missForest = EmptyDataFrame
  for( h in 1:iterations ) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_missForest[,i] = data[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = NA }
    set.seed(8)
    missForest = missForest(data[,columns], ntree = ntree)
    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      missForest$ximp$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(missForest$ximp)[2] < dim(data)[2] ) {
        missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2], j:(dim(missForest$ximp)[2] - 1))]
      } else { missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2])] }
      colnames(missForest$ximp)[j] = colnames(data)[j] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_missForest[,i] = missForest$ximp[list_indexes_multiple[[h]][,i], i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_missForest[,i] }
    head(predicted_multiple_missForest)
    list_observed_missForest_multiple[[h]] = observed_multiple_missForest
    list_predicted_missForest_multiple[[h]] = predicted_multiple_missForest
    print(paste("Done with missForest, iteration", h, sep = "\ ")) }





  list_observed_mice_multiple = list()
  list_predicted_mice_multiple = list()
  observed_multiple_mice = EmptyDataFrame
  predicted_multiple_mice = EmptyDataFrame
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_mice[,i] = data[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = NA }
    set.seed(8)
    mice = mice(data[,columns])
    mice_completed = mice::complete(mice, 5)
    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      mice_completed$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(mice_completed)[2] < dim(data)[2] ) {
        mice_completed = mice_completed[,c(1:(j-1), dim(mice_completed)[2], j:(dim(mice_completed)[2] - 1))]
      } else { mice_completed = mice_completed[,c(1:(j-1), dim(mice_completed)[2])] }
      colnames(mice_completed)[j] = colnames(data)[j] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_mice[,i] = mice_completed[list_indexes_multiple[[h]][,i], i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_mice[,i] }
    head(predicted_multiple_mice)
    list_observed_mice_multiple[[h]] = observed_multiple_mice
    list_predicted_mice_multiple[[h]] = predicted_multiple_mice
    print(paste("Done with mice, iteration", h, sep = "\ ")) }






  list_observed_MI_multiple = list()
  list_predicted_MI_multiple = list()
  observed_multiple_MI = EmptyDataFrame
  predicted_multiple_MI = EmptyDataFrame
  data_scal_multiple = scale(data[,columns], scale = TRUE)
  data_scal_multiple = as.data.frame(data_scal_multiple)
  for ( i in seq(1:length(colnames(data)))[-columns] ) {
    data_scal_multiple$NEW = c(rep(x = NA, times = dim(data)[1]))
    if ( dim(data_scal_multiple)[2] < dim(data)[2] ) {
      data_scal_multiple = data_scal_multiple[,c(1:(i-1), dim(data_scal_multiple)[2], i:(dim(data_scal_multiple)[2] - 1))]
    } else { data_scal_multiple = data_scal_multiple[,c(1:(i-1), dim(data_scal_multiple)[2])] }
    colnames(data_scal_multiple)[i] = colnames(data)[i] }
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_MI[,i] = data_scal_multiple[list_indexes_multiple[[h]][,i],i]
      data_scal_multiple[list_indexes_multiple[[h]][,i], i] = NA }
    set.seed(8)
    MI = mi::mi(missing_data.frame(data_scal_multiple[,columns]))
    MI_completed = mi::complete(MI, 1)
    MI_completed = MI_completed[,1:length(columns)]
    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      MI_completed$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(MI_completed)[2] < dim(data)[2] ) {
        MI_completed = MI_completed[,c(1:(j-1), dim(MI_completed)[2], j:(dim(MI_completed)[2] - 1))]
      } else { MI_completed = MI_completed[,c(1:(j-1), dim(MI_completed)[2])] }
      colnames(MI_completed)[j] = colnames(data)[j] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_MI[,i] = MI_completed[list_indexes_multiple[[h]][,i], i]
      data_scal_multiple[list_indexes_multiple[[h]][,i], i] = observed_multiple_MI[,i] }
    head(predicted_multiple_MI)
    list_observed_MI_multiple[[h]] = observed_multiple_MI
    list_predicted_MI_multiple[[h]] = predicted_multiple_MI
    print(paste("Done with mi, iteration", h, sep = "\ ")) }



  list_observed_mean_multiple = list()
  list_predicted_mean_multiple = list()
  observed_multiple_mean = EmptyDataFrame
  predicted_multiple_mean = EmptyDataFrame
  M = c()
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_mean[,i] = data[list_indexes_multiple[[h]][,i],i]
      data[list_indexes_multiple[[h]][,i], i] = NA
      M[i] = mean(data[which(!is.na(data[,i])),i]) }
    for( i in C[which(!is.na(C))] ) {
      data[which(is.na(data[,i])),i] = M[i] }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_mean[,i] = data[list_indexes_multiple[[h]][,i], i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_mean[,i] }
    head(predicted_multiple_mean)
    list_observed_mean_multiple[[h]] = observed_multiple_mean
    list_predicted_mean_multiple[[h]] = predicted_multiple_mean }





  list_observed_random_multiple = list()
  list_predicted_random_multiple = list()
  observed_multiple_random = EmptyDataFrame
  predicted_multiple_random = EmptyDataFrame
  M = matrix(nrow = dim(data)[1], ncol = dim(data)[2])
  for( h in 1:iterations) {
    C = c()
    for( i in columns ) {
      if( anyNA(list_indexes_multiple[[h]][,i]) == FALSE ) {
        C[i] = i } }
    for( i in C[which(!is.na(C))] ) {
      observed_multiple_random[,i] = data[list_indexes_multiple[[h]][,i],i]
      M[,i] = seq(from = min(data[,i]), to = max(data[,i]), length.out=length(data[,i]))
      data[list_indexes_multiple[[h]][,i], i] = NA }
    for( i in C[which(!is.na(C))] ) {
      data[which(is.na(data[,i])),i] = sample(M[,i], size = dim(data)[1] * rows) }
    for( i in C[which(!is.na(C))] ) {
      predicted_multiple_random[,i] = data[list_indexes_multiple[[h]][,i], i]
      data[list_indexes_multiple[[h]][,i], i] = observed_multiple_random[,i] }
    head(predicted_multiple_random)
    list_observed_random_multiple[[h]] = observed_multiple_random
    list_predicted_random_multiple[[h]] = predicted_multiple_random }

  MCAR_multiple_random = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_random_multiple[[i]][,h], list_predicted_random_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_random_multiple[[i]][,h], list_predicted_random_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_random_multiple[[i]][,h], list_predicted_random_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_random[[i]] = tmp }

  tmp_MAE_random = c()
  tmp_RMSE_random = c()
  tmp_Euclidean_random = c()

  for ( i in 1:iterations ) {
    tmp_MAE_random[i] = mean(MCAR_multiple_random[[i]][which(!is.na(MCAR_multiple_random[[i]][,1])),1])
    tmp_RMSE_random[i] = mean(MCAR_multiple_random[[i]][which(!is.na(MCAR_multiple_random[[i]][,2])),2])
    tmp_Euclidean_random[i] = mean(MCAR_multiple_random[[i]][which(!is.na(MCAR_multiple_random[[i]][,3])),3]) }

  Histograms = list()
  multiplecolumn = list()
  multiplecolumn_random = list()

  MCAR_multiple_mean = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_mean_multiple[[i]][,h], list_predicted_mean_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_mean_multiple[[i]][,h], list_predicted_mean_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_mean_multiple[[i]][,h], list_predicted_mean_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_mean[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times = iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$mean = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,3])),3]) }
    multiplecolumn$mean[1] = mean(tmp[,1])
    multiplecolumn$mean[2] = mean(tmp[,2])
    multiplecolumn$mean[3] = mean(tmp[,3]) }

  tmp_MAE_mean = c()
  tmp_RMSE_mean = c()
  tmp_Euclidean_mean = c()
  MAE_multiple_mean = c()
  RMSE_multiple_mean = c()
  Euclidean_multiple_mean = c()

  for ( i in 1:iterations ) {
    tmp_MAE_mean[i] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,1])),1])
    tmp_RMSE_mean[i] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,2])),2])
    tmp_Euclidean_mean[i] = mean(MCAR_multiple_mean[[i]][which(!is.na(MCAR_multiple_mean[[i]][,3])),3])
    MAE_multiple_mean[i] = tmp_MAE_random[i] - tmp_MAE_mean[i]
    RMSE_multiple_mean[i] = tmp_RMSE_random[i] - tmp_RMSE_mean[i]
    Euclidean_multiple_mean[i] = tmp_Euclidean_random[i] - tmp_Euclidean_mean[i] }

  multiplecolumn_random$mean = list(MAE_multiple_mean = MAE_multiple_mean, RMSE_multiple_mean = RMSE_multiple_mean, Euclidean_multiple_mean = Euclidean_multiple_mean)



  MCAR_multiple_KNN = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_KNN_multiple[[i]][,h], list_predicted_KNN_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_KNN_multiple[[i]][,h], list_predicted_KNN_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_KNN_multiple[[i]][,h], list_predicted_KNN_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_KNN[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times =iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$KNN = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,3])),3]) }
    multiplecolumn$KNN[1] = mean(tmp[,1])
    multiplecolumn$KNN[2] = mean(tmp[,2])
    multiplecolumn$KNN[3] = mean(tmp[,3]) }

  tmp_MAE_KNN = c()
  tmp_RMSE_KNN = c()
  tmp_Euclidean_KNN = c()
  MAE_multiple_KNN = c()
  RMSE_multiple_KNN = c()
  Euclidean_multiple_KNN = c()

  for ( i in 1:iterations ) {
    tmp_MAE_KNN[i] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,1])),1])
    tmp_RMSE_KNN[i] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,2])),2])
    tmp_Euclidean_KNN[i] = mean(MCAR_multiple_KNN[[i]][which(!is.na(MCAR_multiple_KNN[[i]][,3])),3])
    MAE_multiple_KNN[i] = tmp_MAE_random[i] - tmp_MAE_KNN[i]
    RMSE_multiple_KNN[i] = tmp_RMSE_random[i] - tmp_RMSE_KNN[i]
    Euclidean_multiple_KNN[i] = tmp_Euclidean_random[i] - tmp_Euclidean_KNN[i] }

  multiplecolumn_random$KNN = list(MAE_multiple_KNN = MAE_multiple_KNN, RMSE_multiple_KNN = RMSE_multiple_KNN, Euclidean_multiple_KNN = Euclidean_multiple_KNN)


  if(histogram == "MAE") {
    Plot_KNN = ggplot() + geom_histogram(data = as.data.frame(MAE_multiple_mean), aes(x = MAE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mean), y = 0, xend = median(MAE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(MAE_multiple_KNN), aes(x = MAE_multiple_KNN), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_KNN), y = 0, xend = median(MAE_multiple_KNN), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$KNN = Plot_KNN
  }
  if(histogram == "RMSE"){
    Plot_KNN = ggplot() + geom_histogram(data = as.data.frame(RMSE_multiple_mean), aes(x = RMSE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mean), y = 0, xend = median(RMSE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(RMSE_multiple_KNN), aes(x = RMSE_multiple_KNN), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_KNN), y = 0, xend = median(RMSE_multiple_KNN), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$KNN = Plot_KNN
  }





  MCAR_multiple_amelia = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_amelia_multiple[[i]][,h], list_predicted_amelia_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_amelia_multiple[[i]][,h], list_predicted_amelia_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_amelia_multiple[[i]][,h], list_predicted_amelia_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_amelia[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times =iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$amelia = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,3])),3]) }
    multiplecolumn$amelia[1] = mean(tmp[,1])
    multiplecolumn$amelia[2] = mean(tmp[,2])
    multiplecolumn$amelia[3] = mean(tmp[,3]) }


  tmp_MAE_amelia = c()
  tmp_RMSE_amelia = c()
  tmp_Euclidean_amelia = c()
  MAE_multiple_amelia = c()
  RMSE_multiple_amelia = c()
  Euclidean_multiple_amelia = c()

  for ( i in 1:iterations ) {
    tmp_MAE_amelia[i] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,1])),1])
    tmp_RMSE_amelia[i] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,2])),2])
    tmp_Euclidean_amelia[i] = mean(MCAR_multiple_amelia[[i]][which(!is.na(MCAR_multiple_amelia[[i]][,3])),3])
    MAE_multiple_amelia[i] = tmp_MAE_random[i] - tmp_MAE_amelia[i]
    RMSE_multiple_amelia[i] = tmp_RMSE_random[i] - tmp_RMSE_amelia[i]
    Euclidean_multiple_amelia[i] = tmp_Euclidean_random[i] - tmp_Euclidean_amelia[i] }

  multiplecolumn_random$amelia = list(MAE_multiple_amelia = MAE_multiple_amelia, RMSE_multiple_amelia = RMSE_multiple_amelia, Euclidean_multiple_amelia = Euclidean_multiple_amelia)

  if(histogram == "MAE") {
    Plot_amelia = ggplot() + geom_histogram(data = as.data.frame(MAE_multiple_mean), aes(x = MAE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mean), y = 0, xend = median(MAE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(MAE_multiple_amelia), aes(x = MAE_multiple_amelia), fill = "chocolate1", col = "chocolate3", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_amelia), y = 0, xend = median(MAE_multiple_amelia), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$amelia = Plot_amelia
  }
  if(histogram == "RMSE") {
    Plot_amelia = ggplot() + geom_histogram(data = as.data.frame(RMSE_multiple_mean), aes(x = RMSE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mean), y = 0, xend = median(RMSE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(RMSE_multiple_amelia), aes(x = RMSE_multiple_amelia), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_amelia), y = 0, xend = median(RMSE_multiple_amelia), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$amelia = Plot_amelia
  }




  MCAR_multiple_mice = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_mice_multiple[[i]][,h], list_predicted_mice_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_mice_multiple[[i]][,h], list_predicted_mice_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_mice_multiple[[i]][,h], list_predicted_mice_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_mice[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times =iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$mice = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,3])),3]) }
    multiplecolumn$mice[1] = mean(tmp[,1])
    multiplecolumn$mice[2] = mean(tmp[,2])
    multiplecolumn$mice[3] = mean(tmp[,3]) }

  tmp_MAE_mice = c()
  tmp_RMSE_mice = c()
  tmp_Euclidean_mice = c()
  MAE_multiple_mice = c()
  RMSE_multiple_mice = c()
  Euclidean_multiple_mice = c()

  for ( i in 1:iterations ) {
    tmp_MAE_mice[i] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,1])),1])
    tmp_RMSE_mice[i] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,2])),2])
    tmp_Euclidean_mice[i] = mean(MCAR_multiple_mice[[i]][which(!is.na(MCAR_multiple_mice[[i]][,3])),3])
    MAE_multiple_mice[i] = tmp_MAE_random[i] - tmp_MAE_mice[i]
    RMSE_multiple_mice[i] = tmp_RMSE_random[i] - tmp_RMSE_mice[i]
    Euclidean_multiple_mice[i] = tmp_Euclidean_random[i] - tmp_Euclidean_mice[i] }

  multiplecolumn_random$mice = list(MAE_multiple_mice = MAE_multiple_mice, RMSE_multiple_mice = RMSE_multiple_mice, Euclidean_multiple_mice = Euclidean_multiple_mice)

  if(histogram == "MAE"){
    Plot_mice = ggplot() + geom_histogram(data = as.data.frame(MAE_multiple_mean), aes(x = MAE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mean), y = 0, xend = median(MAE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(MAE_multiple_mice), aes(x = MAE_multiple_mice), fill = "indianred1", col = "indianred3", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mice), y = 0, xend = median(MAE_multiple_mice), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$mice = Plot_mice
  }
  if(histogram == "RMSE"){
    Plot_mice = ggplot() + geom_histogram(data = as.data.frame(RMSE_multiple_mean), aes(x = RMSE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mean), y = 0, xend = median(RMSE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(RMSE_multiple_mice), aes(x = RMSE_multiple_mice), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mice), y = 0, xend = median(RMSE_multiple_mice), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$mice = Plot_mice
  }



  MCAR_multiple_missForest = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_missForest_multiple[[i]][,h], list_predicted_missForest_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_missForest_multiple[[i]][,h], list_predicted_missForest_multiple[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_missForest_multiple[[i]][,h], list_predicted_missForest_multiple[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_multiple_missForest[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times =iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$missForest = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,3])),3]) }
    multiplecolumn$missForest[1] = mean(tmp[,1])
    multiplecolumn$missForest[2] = mean(tmp[,2])
    multiplecolumn$missForest[3] = mean(tmp[,3]) }

  tmp_MAE_missForest = c()
  tmp_RMSE_missForest = c()
  tmp_Euclidean_missForest = c()
  MAE_multiple_missForest = c()
  RMSE_multiple_missForest = c()
  Euclidean_multiple_missForest = c()

  for ( i in 1:iterations ) {
    tmp_MAE_missForest[i] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,1])),1])
    tmp_RMSE_missForest[i] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,2])),2])
    tmp_Euclidean_missForest[i] = mean(MCAR_multiple_missForest[[i]][which(!is.na(MCAR_multiple_missForest[[i]][,3])),3])
    MAE_multiple_missForest[i] = tmp_MAE_random[i] - tmp_MAE_missForest[i]
    RMSE_multiple_missForest[i] = tmp_RMSE_random[i] - tmp_RMSE_missForest[i]
    Euclidean_multiple_missForest[i] = tmp_Euclidean_random[i] - tmp_Euclidean_missForest[i] }

  multiplecolumn_random$missForest = list(MAE_multiple_missForest = MAE_multiple_missForest, RMSE_multiple_missForest = RMSE_multiple_missForest, Euclidean_multiple_missForest = Euclidean_multiple_missForest)

  if(histogram == "MAE") {
    Plot_missForest = ggplot() + geom_histogram(data = as.data.frame(MAE_multiple_mean), aes(x = MAE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mean), y = 0, xend = median(MAE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(MAE_multiple_missForest), aes(x = MAE_multiple_missForest), fill = "lightsalmon1", col = "lightsalmon3", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_missForest), y = 0, xend = median(MAE_multiple_missForest), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$missForest = Plot_missForest
  }
  if(histogram == "RMSE"){
    Plot_missForest = ggplot() + geom_histogram(data = as.data.frame(RMSE_multiple_mean), aes(x = RMSE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mean), y = 0, xend = median(RMSE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(RMSE_multiple_missForest), aes(x = RMSE_multiple_missForest), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_missForest), y = 0, xend = median(RMSE_multiple_missForest), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$missForest = Plot_missForest
  }



  MCAR_multiple_MI = list()
  for( i in 1:iterations ) {
    C = c()
    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))
    rownames(tmp) = colnames(data)[1:max(columns)]
    for( h in columns ) {
      if( anyNA(list_indexes_multiple[[i]][,h]) == FALSE ) {
        C[h] = h } }
    for( h in C[which(!is.na(C))] ) {
      tmp[h, 1] = mae(list_observed_MI_multiple[[i]][,h], list_predicted_MI_multiple[[i]][,h]) / (abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[1]) + abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_MI_multiple[[i]][,h], list_predicted_MI_multiple[[i]][,h]) / (abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[1]) + abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_MI_multiple[[i]][,h], list_predicted_MI_multiple[[i]][,h], p = 2) / (abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[1]) + abs(range(data_scal_multiple[which(!is.na(data_scal_multiple[,h])),h])[2])) }
    MCAR_multiple_MI[[i]] = tmp }

  tmp = data.frame(MAE = c(rep(x = NA, times = iterations)), RMSE = c(rep(x = NA, times =iterations)), Euclidean = c(rep(x = NA, times = iterations)))
  multiplecolumn$MI = c(MAE = NA, RMSE = NA, Euclidean = NA)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,1])),1])
      tmp[i,2] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,2])),2])
      tmp[i,3] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,3])),3]) }
    multiplecolumn$MI[1] = mean(tmp[,1])
    multiplecolumn$MI[2] = mean(tmp[,2])
    multiplecolumn$MI[3] = mean(tmp[,3]) }

  tmp_MAE_MI = c()
  tmp_RMSE_MI = c()
  tmp_Euclidean_MI = c()
  MAE_multiple_MI = c()
  RMSE_multiple_MI = c()
  Euclidean_multiple_MI = c()

  for ( i in 1:iterations ) {
    tmp_MAE_MI[i] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,1])),1])
    tmp_RMSE_MI[i] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,2])),2])
    tmp_Euclidean_MI[i] = mean(MCAR_multiple_MI[[i]][which(!is.na(MCAR_multiple_MI[[i]][,3])),3])
    MAE_multiple_MI[i] = tmp_MAE_random[i] - tmp_MAE_MI[i]
    RMSE_multiple_MI[i] = tmp_RMSE_random[i] - tmp_RMSE_MI[i]
    Euclidean_multiple_MI[i] = tmp_Euclidean_random[i] - tmp_Euclidean_MI[i] }

  multiplecolumn_random$MI = list(MAE_multiple_MI = MAE_multiple_MI, RMSE_multiple_MI = RMSE_multiple_MI, Euclidean_multiple_MI = Euclidean_multiple_MI)

  if(histogram == "MAE") {
    Plot_MI = ggplot() + geom_histogram(data = as.data.frame(MAE_multiple_mean), aes(x = MAE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_mean), y = 0, xend = median(MAE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(MAE_multiple_MI), aes(x = MAE_multiple_MI), fill = "springgreen3", col = "springgreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(MAE_multiple_MI), y = 0, xend = median(MAE_multiple_MI), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$MI = Plot_MI
  }
  if(histogram == "RMSE"){
    Plot_MI = ggplot() + geom_histogram(data = as.data.frame(RMSE_multiple_mean), aes(x = RMSE_multiple_mean), fill = "gray82", col = "gray53", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_mean), y = 0, xend = median(RMSE_multiple_mean), yend = Inf), lty = 3, lwd = 0.5) + geom_histogram(data = as.data.frame(RMSE_multiple_MI), aes(x = RMSE_multiple_MI), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.8, bins = 25) + geom_segment(aes(x = median(RMSE_multiple_MI), y = 0, xend = median(RMSE_multiple_MI), yend = Inf), lty = 3, lwd = 0.5)
    Histograms$MI = Plot_MI
  }


  if(is.null(Histograms) == FALSE) {
  return(list(multiplecolumn = multiplecolumn, multiplecolumn_random = multiplecolumn_random, histograms = Histograms))
  } else {return(list(multiplecolumn = multiplecolumn, multiplecolumn_random = multiplecolumn_random))}
  }
