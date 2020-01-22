select_k = function(data, biggestk, rows, columns = c(1:dim(data)[2]), n_columns) {

  library(DMwR)
  library(Metrics)
  library(TSdist)

  EmptyDataFrame = as.data.frame(matrix(nrow = dim(data)[1] * rows, ncol = max(columns)))

  tmp = matrix(nrow = dim(data)[1] * rows, ncol = dim(data)[2])
  C = sample(columns, size = n_columns)
  for( i in C ) {
    tmp[,i] = sample(which(!is.na(data[,i])), size = dim(data)[1] * rows) }
  indexes_multiple = tmp

  tested_k = list()

  for( h in seq(from = 2, to = biggestk, by = 1) ) {
    observed_multiple_KNN = EmptyDataFrame
    predicted_multiple_KNN = EmptyDataFrame
    for( i in C ) {
      observed_multiple_KNN[,i] = data[indexes_multiple[,i],i]
      data[indexes_multiple[,i], i] = NA }

    set.seed(8)
    KNN = knnImputation(data[,columns], k = h)

    for ( j in seq(1:length(colnames(data)))[-columns] ) {
      KNN$NEW = c(rep(x = NA, times = dim(data)[1]))
      if ( dim(KNN)[2] < dim(data)[2] ) {
        KNN = KNN[,c(1:(j-1), dim(KNN)[2], j:(dim(KNN)[2] - 1))]
      } else { KNN = KNN[,c(1:(j-1), dim(KNN)[2])] }
      colnames(KNN)[j] = colnames(data)[j] }

    for( i in C ) {
      predicted_multiple_KNN[,i] = KNN[indexes_multiple[,i], i]
      data[indexes_multiple[,i], i] = observed_multiple_KNN[,i] }

    tmp = data.frame(MAE = c(rep(x = NA, times = max(columns))), RMSE = c(rep(x = NA, times = max(columns))), Euclidean = c(rep(x = NA, times = max(columns))))

    for( i in C ) {
      tmp[i, 1] = mae(observed_multiple_KNN[, i], predicted_multiple_KNN[, i]) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tmp[i, 2] = rmse(observed_multiple_KNN[, i], predicted_multiple_KNN[, i]) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tmp[i, 3] = MinkowskiDistance(observed_multiple_KNN[, i], predicted_multiple_KNN[, i], p = 2) / (abs(range(data[which(!is.na(data[,i])),i])[1]) + abs(range(data[which(!is.na(data[,i])),i])[2]))
      tested_k[[h]] = c(MAE = NA, RMSE = NA, Euclidean = NA)
      tested_k[[h]][1] = mean(tmp[which(!is.na(tmp[,1])),1])
      tested_k[[h]][2] = mean(tmp[which(!is.na(tmp[,2])),2])
      tested_k[[h]][3] = mean(tmp[which(!is.na(tmp[,3])),3]) }
    names(tested_k)[h] = h }

  tested_k = tested_k[which(!is.na(names(tested_k)))]
  return(tested_k)

}
