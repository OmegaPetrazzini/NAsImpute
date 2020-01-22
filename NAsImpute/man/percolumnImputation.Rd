\name{percolumnImputation}
\alias{percolumnImputation}

\title{
Test the performance of multiple imputation methods on each column of your dataset
}
\description{
'percolumnImputation' generates artificial missing values (NA) on every column which are then predicted separetly with six imputations methods ('knnImputation', 'missForest', 'mice', 'mi', 'Amelia' and mean-value). Predicted values are compared with observed values to determine three error metrics (MAE, RMSE and Euclidean distance) for each algorithm on each column of your dataset. We also compare each methods performance with a random imputation.
'percolumnImputation' is a usefull tool to determine the best performing algorithm on each particular distribution of variables in your dataset.
}
\usage{
percolumnImputation(data, k, ntree, rows, columns = c(1:dim(data)[2]), iterations, histogram = c(FALSE, "MAE", "RMSE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame where columns correspond to variables and rows to observations.
}
  \item{k}{
Number of neighbors to use in 'knnImputation' method.
ej: k = 11
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
  \item{iterations}{
Number of times each algorithm will be tested.
ej: 'iterations' = 50
}
  \item{histogram}{
Should histograms be created for each column in 'columns'. If histogram = 'MAE' then MAE will be used to compare algorithms in each column. If histogram = 'RMSE' then RMSE will be used to compare algorithms in each column. Default is FALSE where no histogram will be created.
ej: histogram = "MAE"
}
}

\value{

percolumn: List containing six data frames (KNN, amelia, mice, missForest, MI and mean). Each data frame shows the averaged error metrics (columns) for each column tested (rows) n = 'iterations' times.

percolumn_random: List containing six lists (KNN, amelia, mice, missForest, MI and mean), each list has three data frames (MAE, RMSE and Euclidean distance). Each data frame shows the difference in error metrics when comparing with a random imputation for each variable (columns) in each iteration (rows).

histograms: List containing a histogram ('ggplot' object) for each column in 'columns'.
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
PerColumnImputations = percolumnImputation(data = iris, rows = 0.1, k = 3, ntree = 8, iterations=10, columns = c(1:4), histogram="MAE")

##Inspect results:
head(PerColumnImputations$percolumn$KNN)
head(PerColumnImputations$percolumn_random$KNN$MAE_KNN)

##Create histograms:
jpeg("SepalLength.jpeg", res = 500, width=5.5, height=5.5, units="in")
PerColumnImputations$histograms$Plot_1
dev.off()
}
