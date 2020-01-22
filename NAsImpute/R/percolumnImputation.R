percolumnImputation = function(data, k, ntree, rows, columns = c(1:dim(data)[2]), iterations, histogram = FALSE) {

  library(DMwR)
  library(missForest)
  library(mice)
  library(mi)
  library(Amelia)
  library(Metrics)
  library(TSdist)
  library(ggplot2)


  EmptyDataFrame = as.data.frame(matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2]))

  list_indexes = list()
  for( h in 1:iterations ) {
    tmp = matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2])
    for( i in columns ) {
      tmp[,i] = sample(which(!is.na(data[,i])), size = dim(data)[1] * rows) }
    list_indexes[[h]] = tmp }



  observed_KNN = EmptyDataFrame
  predicted_KNN = EmptyDataFrame
  list_observed_KNN = list()
  list_predicted_KNN = list()
  for( h in 1:iterations) {
    for( i in columns ) {
      observed_KNN[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = NA
      set.seed(8)
      KNN = knnImputation(data[,columns], k = k)
      for ( j in seq(1:length(colnames(data)))[-columns] ) {
        KNN$NEW = c(rep(x = NA, times = dim(data)[1]))
        if ( dim(KNN)[2] < dim(data)[2] ) {
          KNN = KNN[,c(1:(j-1), dim(KNN)[2], j:(dim(KNN)[2] - 1))]
        } else { KNN = KNN[,c(1:(j-1), dim(KNN)[2])] }
        colnames(KNN)[j] = colnames(data)[j] }
      predicted_KNN[,i] = KNN[list_indexes[[h]][,i], i]
      print(head(predicted_KNN[,i]))
      data[list_indexes[[h]][,i], i] = observed_KNN[,i] }
    list_observed_KNN[[h]] = observed_KNN
    list_predicted_KNN[[h]] = predicted_KNN
    print(paste("Done with KNN, iteration", h, sep = "\ ")) }



  observed_amelia = EmptyDataFrame
  predicted_amelia = EmptyDataFrame
  list_observed_amelia = list()
  list_predicted_amelia = list()
  for( h in 1:iterations) {
    for( i in columns ) {
      observed_amelia[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = NA
      set.seed(8)
      amelia = amelia(data[,columns])
      for ( j in seq(1:length(colnames(data)))[-columns] ) {
        amelia$imputations$imp5$NEW = c(rep(x = NA, times = dim(data)[1]))
        if ( dim(amelia$imputations$imp5)[2] < dim(data)[2] ) {
          amelia$imputations$imp5 = amelia$imputations$imp5[,c(1:(j-1), dim(amelia$imputations$imp5)[2], j:(dim(amelia$imputations$imp5)[2] - 1))]
        } else { amelia$imputations$imp5 = amelia$imputations$imp5[,c(1:(j-1), dim(amelia$imputations$imp5)[2])] }
        colnames(amelia$imputations$imp5)[j] = colnames(data)[j] }
      predicted_amelia[,i] = amelia$imputations$imp5[list_indexes[[h]][,i],i]
      print(head(predicted_amelia[,i]))
      data[list_indexes[[h]][,i], i] = observed_amelia[,i] }
    list_observed_amelia[[h]] = observed_amelia
    list_predicted_amelia[[h]] = predicted_amelia
    print(paste("Done with Amelia, iteration", h, sep = "\ ")) }



  observed_mice = EmptyDataFrame
  predicted_mice = EmptyDataFrame
  list_observed_mice = list()
  list_predicted_mice = list()
  for( h in 1:iterations) {
    for( i in columns ) {
      observed_mice[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = NA
      set.seed(8)
      mice = mice(data[,columns])
      mice_completed = mice::complete(mice, 5)
      for ( j in seq(1:length(colnames(data)))[-columns] ) {
        mice_completed$NEW = c(rep(x = NA, times = dim(data)[1]))
        if ( dim(mice_completed)[2] < dim(data)[2] ) {
          mice_completed = mice_completed[,c(1:(j-1), dim(mice_completed)[2], j:(dim(mice_completed)[2] - 1))]
        } else { mice_completed = mice_completed[,c(1:(j-1), dim(mice_completed)[2])] }
        colnames(mice_completed)[j] = colnames(data)[j] }
      predicted_mice[,i] = mice_completed[list_indexes[[h]][,i], i]
      print(head(predicted_mice[,i]))
      data[list_indexes[[h]][,i], i] = observed_mice[,i] }
    list_observed_mice[[h]] = observed_mice
    list_predicted_mice[[h]] = predicted_mice
    print(paste("Done with mice, iteration", h, sep = "\ ")) }



  observed_missForest = EmptyDataFrame
  predicted_missForest = EmptyDataFrame
  list_observed_missForest = list()
  list_predicted_missForest = list()
  for( h in 1:iterations) {
    for ( i in columns ) {
      observed_missForest[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = NA
      set.seed(8)
      missForest = missForest(data[,columns], ntree = ntree)
      for ( j in seq(1:length(colnames(data)))[-columns] ) {
        missForest$ximp$NEW = c(rep(x = NA, times = dim(data)[1]))
        if ( dim(missForest$ximp)[2] < dim(data)[2] ) {
          missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2], j:(dim(missForest$ximp)[2] - 1))]
        } else { missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2])] }
        colnames(missForest$ximp)[j] = colnames(data)[j] }
      predicted_missForest[,i] = missForest$ximp[list_indexes[[h]][,i], i]
      print(head(predicted_missForest[,i]))
      data[list_indexes[[h]][,i], i] = observed_missForest[,i] }
    list_observed_missForest[[h]] = observed_missForest
    list_predicted_missForest[[h]] = predicted_missForest
    print(paste("Done with missForest, iteration", h, sep = "\ ")) }



  data_scal = scale(data[,columns], scale = TRUE)
  data_scal = as.data.frame(data_scal)
  for ( i in seq(1:length(colnames(data)))[-columns] ) {
    data_scal$NEW = c(rep(x = NA, times = dim(data)[1]))
    if ( dim(data_scal)[2] < dim(data)[2] ) {
      data_scal = data_scal[,c(1:(i-1), dim(data_scal)[2], i:(dim(data_scal)[2] - 1))]
    } else { data_scal = data_scal[,c(1:(i-1), dim(data_scal)[2])] }
    colnames(data_scal)[i] = colnames(data)[i] }
  observed_MI = EmptyDataFrame
  predicted_MI = EmptyDataFrame
  list_observed_MI = list()
  list_predicted_MI = list()
  for( h in 1:iterations) {
    for( i in columns ) {
      observed_MI[,i] = data_scal[list_indexes[[h]][,i],i]
      data_scal[list_indexes[[h]][,i], i] = NA
      set.seed(8)
      MI = mi::mi(missing_data.frame(data_scal[,columns]))
      MI_completed = mi::complete(MI, 1)
      MI_completed = MI_completed[,1:length(columns)]
      for ( j in seq(1:length(colnames(data)))[-columns] ) {
        MI_completed$NEW = c(rep(x = NA, times = dim(data)[1]))
        if ( dim(MI_completed)[2] < dim(data)[2] ) {
          MI_completed = MI_completed[,c(1:(j-1), dim(MI_completed)[2], j:(dim(MI_completed)[2] - 1))]
        } else { MI_completed = MI_completed[,c(1:(j-1), dim(MI_completed)[2])] }
        colnames(MI_completed)[j] = colnames(data)[j] }
      predicted_MI[,i] = MI_completed[list_indexes[[h]][,i],i]
      data_scal[list_indexes[[h]][,i], i] = observed_MI[,i] }
    list_observed_MI[[h]] = observed_MI
    list_predicted_MI[[h]] = predicted_MI
    print(paste("Done with mi, iteration", h, sep = "\ ")) }



  observed_mean = EmptyDataFrame
  predicted_mean = EmptyDataFrame
  list_observed_mean = list()
  list_predicted_mean = list()
  for( h in 1:iterations) {
    for (i in columns) {
      observed_mean[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = NA
      M = mean(data[which(!is.na(data[,i])),i])
      set.seed(8)
      data[which(is.na(data[,i])),i] = M
      predicted_mean[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = observed_mean[,i] }
    list_observed_mean[[h]] = observed_mean
    list_predicted_mean[[h]] = predicted_mean }



  observed_random = EmptyDataFrame
  predicted_random = EmptyDataFrame
  list_observed_random = list()
  list_predicted_random = list()
  for( h in 1:iterations) {
    for (i in columns) {
      observed_random[,i] = data[list_indexes[[h]][,i],i]
      M = seq(from = min(data[,i]), to = max(data[,i]), length.out=length(data[,i]))
      data[list_indexes[[h]][,i], i] = NA
      data[which(is.na(data[,i])),i] = sample(M, size = dim(data)[1] * rows)
      predicted_random[,i] = data[list_indexes[[h]][,i],i]
      data[list_indexes[[h]][,i], i] = observed_random[,i] }
    list_observed_random[[h]] = observed_random
    list_predicted_random[[h]] = predicted_random }


  MCAR_random = list()
  tmp = data.frame(MAE = 1:max(columns), RMSE = 1:max(columns), Euclidean = 1:max(columns))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_random[[i]][,h], list_predicted_random[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_random[[i]][,h], list_predicted_random[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_random[[i]][,h], list_predicted_random[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_random[[i]] = tmp }

  MCAR_random[[iterations + 1]] = MCAR_random[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_random[[i]][h,1]
      tmp[i,2] = MCAR_random[[i]][h,2]
      tmp[i,3] = MCAR_random[[i]][h,3] }
    MCAR_random[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_random[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_random[[iterations + 1]][h,3] = mean(tmp[,3]) }


  percolumn = list()
  percolumn_random = list()

  MCAR_KNN = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_KNN[[i]][,h], list_predicted_KNN[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_KNN[[i]][,h], list_predicted_KNN[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_KNN[[i]][,h], list_predicted_KNN[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_KNN[[i]] = tmp }

  MCAR_KNN[[iterations + 1]] = MCAR_KNN[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_KNN[[i]][h,1]
      tmp[i,2] = MCAR_KNN[[i]][h,2]
      tmp[i,3] = MCAR_KNN[[i]][h,3] }
    MCAR_KNN[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_KNN[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_KNN[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_KNN[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$KNN = MCAR_KNN[[iterations + 1]]

  MAE_KNN = matrix(ncol = max(columns), nrow = iterations)
  RMSE_KNN = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_KNN = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_KNN) = colnames(data)[1:max(columns)]
  colnames(RMSE_KNN) = colnames(data)[1:max(columns)]
  colnames(Euclidean_KNN) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_KNN[i, h] = MCAR_random[[i]][h,1] - MCAR_KNN[[i]][h,1]
      RMSE_KNN[i, h] = MCAR_random[[i]][h,2] - MCAR_KNN[[i]][h,2]
      Euclidean_KNN[i, h] = MCAR_random[[i]][h,3] - MCAR_KNN[[i]][h,3] } }
  percolumn_random$KNN = list(MAE_KNN = MAE_KNN, RMSE_KNN = RMSE_KNN, Euclidean_KNN = Euclidean_KNN)




  MCAR_amelia = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_amelia[[i]][,h], list_predicted_amelia[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_amelia[[i]][,h], list_predicted_amelia[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_amelia[[i]][,h], list_predicted_amelia[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_amelia[[i]] = tmp }

  MCAR_amelia[[iterations + 1]] = MCAR_amelia[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_amelia[[i]][h,1]
      tmp[i,2] = MCAR_amelia[[i]][h,2]
      tmp[i,3] = MCAR_amelia[[i]][h,3] }
    MCAR_amelia[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_amelia[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_amelia[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_amelia[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$amelia = MCAR_amelia[[iterations + 1]]

  MAE_amelia = matrix(ncol = max(columns), nrow = iterations)
  RMSE_amelia = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_amelia = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_amelia) = colnames(data)[1:max(columns)]
  colnames(RMSE_amelia) = colnames(data)[1:max(columns)]
  colnames(Euclidean_amelia) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_amelia[i, h] = MCAR_random[[i]][h,1] - MCAR_amelia[[i]][h,1]
      RMSE_amelia[i, h] = MCAR_random[[i]][h,2] - MCAR_amelia[[i]][h,2]
      Euclidean_amelia[i, h] = MCAR_random[[i]][h,3] - MCAR_amelia[[i]][h,3] } }

  percolumn_random$amelia = list(MAE_amelia = MAE_amelia, RMSE_amelia = RMSE_amelia, Euclidean_amelia = Euclidean_amelia)




  MCAR_mice = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_mice[[i]][,h], list_predicted_mice[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_mice[[i]][,h], list_predicted_mice[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_mice[[i]][,h], list_predicted_mice[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_mice[[i]] = tmp }

  MCAR_mice[[iterations + 1]] = MCAR_mice[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_mice[[i]][h,1]
      tmp[i,2] = MCAR_mice[[i]][h,2]
      tmp[i,3] = MCAR_mice[[i]][h,3] }
    MCAR_mice[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_mice[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_mice[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_mice[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$mice = MCAR_mice[[iterations + 1]]

  MAE_mice = matrix(ncol = max(columns), nrow = iterations)
  RMSE_mice = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_mice = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_mice) = colnames(data)[1:max(columns)]
  colnames(RMSE_mice) = colnames(data)[1:max(columns)]
  colnames(Euclidean_mice) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_mice[i, h] = MCAR_random[[i]][h,1] - MCAR_mice[[i]][h,1]
      RMSE_mice[i, h] = MCAR_random[[i]][h,2] - MCAR_mice[[i]][h,2]
      Euclidean_mice[i, h] = MCAR_random[[i]][h,3] - MCAR_mice[[i]][h,3] } }

  percolumn_random$mice = list(MAE_mice = MAE_mice, RMSE_mice = RMSE_mice, Euclidean_mice = Euclidean_mice)




  MCAR_missForest = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_missForest[[i]][,h], list_predicted_missForest[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_missForest[[i]][,h], list_predicted_missForest[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_missForest[[i]][,h], list_predicted_missForest[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_missForest[[i]] = tmp }

  MCAR_missForest[[iterations + 1]] = MCAR_missForest[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_missForest[[i]][h,1]
      tmp[i,2] = MCAR_missForest[[i]][h,2]
      tmp[i,3] = MCAR_missForest[[i]][h,3] }
    MCAR_missForest[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_missForest[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_missForest[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_missForest[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$missForest = MCAR_missForest[[iterations + 1]]

  MAE_missForest = matrix(ncol = max(columns), nrow = iterations)
  RMSE_missForest = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_missForest = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_missForest) = colnames(data)[1:max(columns)]
  colnames(RMSE_missForest) = colnames(data)[1:max(columns)]
  colnames(Euclidean_missForest) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_missForest[i, h] = MCAR_random[[i]][h,1] - MCAR_missForest[[i]][h,1]
      RMSE_missForest[i, h] = MCAR_random[[i]][h,2] - MCAR_missForest[[i]][h,2]
      Euclidean_missForest[i, h] = MCAR_random[[i]][h,3] - MCAR_missForest[[i]][h,3] } }

  percolumn_random$missForest = list(MAE_missForest = MAE_missForest, RMSE_missForest = RMSE_missForest, Euclidean_missForest = Euclidean_missForest)




  MCAR_MI = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_MI[[i]][,h], list_predicted_MI[[i]][,h]) / (abs(range(data_scal[which(!is.na(data_scal[,h])),h])[1]) + abs(range(data_scal[which(!is.na(data_scal[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_MI[[i]][,h], list_predicted_MI[[i]][,h]) / (abs(range(data_scal[which(!is.na(data_scal[,h])),h])[1]) + abs(range(data_scal[which(!is.na(data_scal[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_MI[[i]][,h], list_predicted_MI[[i]][,h], p = 2) / (abs(range(data_scal[which(!is.na(data_scal[,h])),h])[1]) + abs(range(data_scal[which(!is.na(data_scal[,h])),h])[2])) }
    MCAR_MI[[i]] = tmp }

  MCAR_MI[[iterations + 1]] = MCAR_MI[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_MI[[i]][h,1]
      tmp[i,2] = MCAR_MI[[i]][h,2]
      tmp[i,3] = MCAR_MI[[i]][h,3] }
    MCAR_MI[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_MI[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_MI[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_MI[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$MI = MCAR_MI[[iterations + 1]]

  MAE_MI = matrix(ncol = max(columns), nrow = iterations)
  RMSE_MI = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_MI = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_MI) = colnames(data)[1:max(columns)]
  colnames(RMSE_MI) = colnames(data)[1:max(columns)]
  colnames(Euclidean_MI) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_MI[i, h] = MCAR_random[[i]][h,1] - MCAR_MI[[i]][h,1]
      RMSE_MI[i, h] = MCAR_random[[i]][h,2] - MCAR_MI[[i]][h,2]
      Euclidean_MI[i, h] = MCAR_random[[i]][h,3] - MCAR_MI[[i]][h,3] } }

  percolumn_random$MI = list(MAE_MI = MAE_MI, RMSE_MI = RMSE_MI, Euclidean_MI = Euclidean_MI)




  MCAR_mean = list()
  tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

  for( i in 1:iterations ) {
    for( h in columns ) {
      tmp[h, 1] = mae(list_observed_mean[[i]][,h], list_predicted_mean[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 2] = rmse(list_observed_mean[[i]][,h], list_predicted_mean[[i]][,h]) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2]))
      tmp[h, 3] = MinkowskiDistance(list_observed_mean[[i]][,h], list_predicted_mean[[i]][,h], p = 2) / (abs(range(data[which(!is.na(data[,h])),h])[1]) + abs(range(data[which(!is.na(data[,h])),h])[2])) }
    MCAR_mean[[i]] = tmp }

  MCAR_mean[[iterations + 1]] = MCAR_mean[[iterations]]
  tmp = matrix(nrow = iterations, ncol = 3)
  for( h in columns ) {
    for( i in 1:iterations ) {
      tmp[i,1] = MCAR_mean[[i]][h,1]
      tmp[i,2] = MCAR_mean[[i]][h,2]
      tmp[i,3] = MCAR_mean[[i]][h,3] }
    MCAR_mean[[iterations + 1]][h,1] = mean(tmp[,1])
    MCAR_mean[[iterations + 1]][h,2] = mean(tmp[,2])
    MCAR_mean[[iterations + 1]][h,3] = mean(tmp[,3]) }

  rownames(MCAR_mean[[iterations + 1]]) = colnames(data)[1:max(columns)]
  percolumn$mean = MCAR_mean[[iterations + 1]]

  MAE_mean = matrix(ncol = max(columns), nrow = iterations)
  RMSE_mean = matrix(ncol = max(columns), nrow = iterations)
  Euclidean_mean = matrix(ncol = max(columns), nrow = iterations)
  colnames(MAE_mean) = colnames(data)[1:max(columns)]
  colnames(RMSE_mean) = colnames(data)[1:max(columns)]
  colnames(Euclidean_mean) = colnames(data)[1:max(columns)]

  for ( h in columns ) {
    for ( i in 1:iterations ) {
      MAE_mean[i, h] = MCAR_random[[i]][h,1] - MCAR_mean[[i]][h,1]
      RMSE_mean[i, h] = MCAR_random[[i]][h,2] - MCAR_mean[[i]][h,2]
      Euclidean_mean[i, h] = MCAR_random[[i]][h,3] - MCAR_mean[[i]][h,3] } }

  percolumn_random$mean = list(MAE_mean = MAE_mean, RMSE_mean = RMSE_mean, Euclidean_mean = Euclidean_mean)

  Histograms = list()

  if(histogram == "MAE") {
  for( j in columns ) {
    Plot_tmp = ggplot() + geom_histogram(data = as.data.frame(percolumn_random$mean[[1]][,j]), aes(x = percolumn_random$mean[[1]][,j]), fill = "gray82", col = "gray53", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random$KNN[[1]][,j]), aes(x = percolumn_random$KNN[[1]][,j]), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random$amelia[[1]][,j]), aes(x = percolumn_random$amelia[[1]][,j]), fill = "chocolate1", col = "chocolate3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random$mice[[1]][,j]), aes(x = percolumn_random$mice[[1]][,j]), fill = "indianred1", col = "indianred3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random$missForest[[1]][,j]), aes(x = percolumn_random$missForest[[1]][,j]), fill = "lightsalmon1", col = "lightsalmon3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random$MI[[1]][,j]), aes(x = percolumn_random$MI[[1]][,j]), fill = "springgreen3", col = "springgreen4", alpha = 0.5, bins = 25) + geom_segment(aes(x = median(percolumn_random$mean[[1]][,j]), y = 0, xend = median(percolumn_random$mean[[1]][,j]), yend = Inf), colour = "gray53", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random$KNN[[1]][,j]), y = 0, xend = median(percolumn_random$KNN[[1]][,j]), yend = Inf), colour = "darkseagreen4", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random$amelia[[1]][,j]), y = 0, xend = median(percolumn_random$amelia[[1]][,j]), yend = Inf), colour = "chocolate3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random$mice[[1]][,j]), y = 0, xend = median(percolumn_random$mice[[1]][,j]), yend = Inf), colour = "indianred3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random$missForest[[1]][,j]), y = 0, xend = median(percolumn_random$missForest[[1]][,j]), yend = Inf), colour = "lightsalmon3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random$MI[[1]][,j]), y = 0, xend = median(percolumn_random$MI[[1]][,j]), yend = Inf), colour = "springgreen4", lty = 5, lwd = 0.5)
    Plot_nombre = paste("Plot", j, sep = "_")
    Histograms[[j]] = Plot_tmp
    names(Histograms)[j] = print(Plot_nombre)
  }}

  if(histogram == "RMSE") {
    for( j in columns ) {
      Plot_tmp = ggplot() + geom_histogram(data = as.data.frame(percolumn_random_mean[[2]][,j]), aes(x = percolumn_random_mean[[2]][,j]), fill = "gray82", col = "gray53", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random_KNN[[2]][,j]), aes(x = percolumn_random_KNN[[2]][,j]), fill = "darkseagreen3", col = "darkseagreen4", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random_amelia[[2]][,j]), aes(x = percolumn_random_amelia[[2]][,j]), fill = "chocolate1", col = "chocolate3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random_mice[[2]][,j]), aes(x = percolumn_random_mice[[2]][,j]), fill = "indianred1", col = "indianred3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random_missForest[[2]][,j]), aes(x = percolumn_random_missForest[[2]][,j]), fill = "lightsalmon1", col = "lightsalmon3", alpha = 0.5, bins = 25) + geom_histogram(data = as.data.frame(percolumn_random_MI[[2]][,j]), aes(x = percolumn_random_MI[[2]][,j]), fill = "springgreen3", col = "springgreen4", alpha = 0.5, bins = 25) + geom_segment(aes(x = median(percolumn_random_mean[[2]][,j]), y = 0, xend = median(percolumn_random_mean[[2]][,j]), yend = Inf), colour = "gray53", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random_KNN[[2]][,j]), y = 0, xend = median(percolumn_random_KNN[[2]][,j]), yend = Inf), colour = "darkseagreen4", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random_amelia[[2]][,j]), y = 0, xend = median(percolumn_random_amelia[[2]][,j]), yend = Inf), colour = "chocolate3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random_mice[[2]][,j]), y = 0, xend = median(percolumn_random_mice[[2]][,j]), yend = Inf), colour = "indianred3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random_missForest[[2]][,j]), y = 0, xend = median(percolumn_random_missForest[[2]][,j]), yend = Inf), colour = "lightsalmon3", lty = 5, lwd = 0.5) +  geom_segment(aes(x = median(percolumn_random_MI[[2]][,j]), y = 0, xend = median(percolumn_random_MI[[2]][,j]), yend = Inf), colour = "springgreen4", lty = 5, lwd = 0.5)
      Plot_nombre = paste("Plot", j, sep = "_")
      Histograms[[j]] = Plot_tmp
      names(Histograms)[j] = print(Plot_nombre)
    }}

  if(is.null(Histograms) == FALSE) {
  return(list(percolumn = percolumn, percolumn_random = percolumn_random, histograms = Histograms))
  } else {return(list(percolumn = percolumn, percolumn_random = percolumn_random))}

}
