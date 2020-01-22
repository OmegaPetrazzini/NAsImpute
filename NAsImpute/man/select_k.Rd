\name{select_k}
\alias{select_k}

\title{
Determine the optimum number of neighbors (k) to use with 'knnImputation'
}
\description{
'select_k' finds the optimum number of neighbors to use on a Nearest-Neighbors clasifier using your own dataset. The optimum number of neighbors is then used to test 'knnImputation' on its best posible performance.
}
\usage{
select_k(data, biggestk, rows, columns = c(1:dim(data)[2]), n_columns)
}

\arguments{
  \item{data}{
A data frame where columns correspond to variables and rows to observations.
}
  \item{biggestk}{
Maximum number of neighbors to test on. First iteration will test k = 2, the following will increase by one (k = k + 1) until reaching k = 'biggestk'.
ej: 'biggestk' = 10
}
  \item{rows}{
Proportion of observations in which imputation will be performed. This observations will be replaced by missing values (NA) and imputed using 'knnImputation' algorithm. 'rows' must be between 0 and 1.
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
}

\value{
The average error metrics in all columns for each tested number of neighbors.
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
Choose_k = select_k(data = iris, biggestk = 10, rows = 0.1, columns = c(1:4), n_columns = 2)

##Select optimum number of neighbors:
Choose_k
}
