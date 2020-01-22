\name{multiplecolumnImputation}
\alias{multiplecolumnImputation}

\title{
Test the performance of multiple imputation methods on a realistic scenario where multiple columns of your dataset have missing values
}
\description{
'multiplecolumnImputation' generates artificial missing values (NA) on multiple columns which are then predicted altogether with six imputations methods ('knnImputation', 'missForest', 'mice', 'mi', 'Amelia' and mean-value). Predicted values are compared with observed values on each column to determine three error metrics (MAE, RMSE and Euclidean distance) for each algorithm. The average of each metric for all imputed columns is reported as the error of each algorithm. We also compare each methods performance with a random imputation.
'multiplecolumnImputation' is a usefull tool to determine the best performing algorithm to impute missing values in a single run for your particular dataset.
}
\usage{
multiplecolumnImputation(data, k, ntree, rows, columns = c(1:dim(data)[2]), n_columns, iterations, histogram = c(FALSE, "MAE", "RMSE"))
}

\arguments{
  \item{data}{
A data frame where columns correspond to variables and rows to observations.
}
  \item{k}{
Number of neighbors to use in 'knnImputation' method.
ej: k = 10
}
  \item{ntree}{
Number of trees to grow in 'missForest' imputation method.
ej: ntree = 18
}
  \item{rows}{
Proportion of observations in which imputation will be performed. This observations will be replaced by missing values (NA) and imputed using each algorithm. 'rows' must be between 0 and 1.
ej: 'rows' = 0.1
}
  \item{columns}{
Object of type vector specifying the columns (variables) to be used during imputation. Only numerical variables sould be used. Default is all columns in 'data'.
ej: 'columns' = c(1:4, 7, 9:15)
}
  \item{n_columns}{
Number of columns to use for imputation. length('columns') - 'n_columns' should be greater than 2.
ej: 'n_columns' = 4
}
  \item{iterations}{
Number of times each algorithm will be tested.
ej: 'iterations' = 50
}
  \item{histogram}{
Should histograms be created to compare the distance of each imputation method to a random imputation and the distance of the mean-value imputation method to a random imputation. If histogram = 'MAE' then MAE will be used to compare each algorithm to the mean-value imputation method. If histogram = 'RMSE' then RMSE will be used to compare each algorithm to the mean-value imputation method. Default is FALSE where no histogram will be created.
ej: histogram = "MAE"
}
}

\value{

multiplecolumn: List containing six vectors (KNN, amelia, mice, missForest, MI and mean). Each vector shows the averaged error metrics for n = 'iterations' tests. Each iteration's value is the average of metrics in all n = 'n_columns' selected.

multiplecolumn_random: List containing six lists (KNN, amelia, mice, missForest, MI and mean), each list has three vectors (MAE, RMSE and Euclidean distance). Each vector shows the difference in error metrics when comparing with a random imputation. Each iteration's value is the average of metrics in all n = 'n_columns' selected, this values are then averaged by the number of interations ('iterations'). This process is done for a random imputations and for each methods imputation. Reported values are the diference between the average of the random imputations and the average of the methods imputation.

histograms: List containing a histogram ('ggplot' object) for each method tested. Each histogram plots the distance of the mean imputation to a random imputation and the methods imputation to a random imputation.
}
\references{
El paper
}
\author{
Ben Omega Petrazzini, Lucia Spangenberg, Fernando López-Bello, Hugo Naya and Gustavo Vázquez.
}

\examples{
##Upload dataframe:
data(iris)
summary(iris)

##Run function:
MultipleColumnImputations = multiplecolumnImputation(data = iris, rows = 0.1, k = 3, ntree = 8, iterations=10, columns = c(1:4), n_columns = 2, histogram="MAE")

##Inspect results:
head(MultipleColumnImputations$multiplecolumn$KNN)
head(MultipleColumnImputations$multiplecolumn_random$KNN$MAE_KNN)

##Create histograms:
jpeg("KNN.jpeg", res = 500, width=5.5, height=5.5, units="in")
MultipleColumnImputations$histograms$KNN
dev.off()
}
