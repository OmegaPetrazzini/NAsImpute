select_ntree = function(data, maxtree, rows, columns = c(1:dim(data)[2]), n_columns) {

  library(missForest)
  library(Metrics)
  library(TSdist)

  EmptyDataFrame = as.data.frame(matrix(nrow = dim(data)[1] * rows, ncol = max(columns)))

  tmp = matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2])
  C = sample(columns, size = n_columns)
  for( i in C ) {
    tmp[,i] = sample(which(!is.na(data[,i])), size = dim(data)[1] * rows) }
  indexes_multiple = tmp

  tested_ntrees = list()

  for( h in seq(from = 3, to = maxtree, by = 2) ) {
    observed_multiple_missForest = EmptyDataFrame
    predicted_multiple_missForest = EmptyDataFrame
    for( i in C ) {
      observed_multiple_missForest[,i] = data[indexes_multiple[,i],i]
      data[indexes_multiple[,i], i] = NA }

    set.seed(8)
    missForest = missForest(data[,columns], ntree = h)

    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      missForest$ximp$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(missForest$ximp)[2] < dim(data)[2] ) {
        missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2], j:(dim(missForest$ximp)[2] - 1))]
      } else { missForest$ximp = missForest$ximp[,c(1:(j-1), dim(missForest$ximp)[2])] }
      colnames(missForest$ximp)[j] = colnames(data)[j] }

    for( i in C ) {
      predicted_multiple_missForest[,i] = missForest$ximp[indexes_multiple[,i], i]
      data[indexes_multiple[,i], i] = observed_multiple_missForest[,i] }

    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

    for( i in C ) {
      tmp[i, 1] = mae(observed_multiple_missForest[, i], predicted_multiple_missForest[, i]) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tmp[i, 2] = rmse(observed_multiple_missForest[, i], predicted_multiple_missForest[, i]) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tmp[i, 3] = MinkowskiDistance(observed_multiple_missForest[, i], predicted_multiple_missForest[, i], p = 2) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tested_ntrees[[h]] = c(MAE = NA, RMSE = NA, Euclidean = NA)
      tested_ntrees[[h]][1] = mean(tmp[which(!is.na(tmp[,1])),1])
      tested_ntrees[[h]][2] = mean(tmp[which(!is.na(tmp[,2])),2])
      tested_ntrees[[h]][3] = mean(tmp[which(!is.na(tmp[,3])),3]) }
    names(tested_ntrees)[h] = h }

    tested_ntrees = tested_ntrees[which(!is.na(names(tested_ntrees)))]

    return(tested_ntrees)

  }
