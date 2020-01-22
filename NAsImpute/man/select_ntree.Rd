\name{select_ntree}
\alias{select_ntree}

\title{
Determine the optimum number of trees to use in 'missForest' imputation
}
\description{
'select_ntree' finds the optimum number of trees to grow on a Random Forest clasifier using your own dataset. The optimum number of trees is then used to test 'missForest' imputation on its best posible performance.
}
\usage{
select_ntree(data, maxtree, rows, columns = c(1:dim(data)[2]), n_columns)
}

\arguments{
  \item{data}{
A data frame where columns correspond to variables and rows to observations.
}
  \item{maxtree}{
Maximum number of trees to test on. First iteration will test ntree = 3, the following will increase by two (ntree = ntree + 2) until reaching ntree = 'maxtree'.
ej: 'maxtree' = 20
}
  \item{rows}{
Proportion of observations in which imputation will be performed. This observations will be replaced by missing values (NA) and imputed using 'missForest' algorithm. 'rows' must be between 0 and 1.
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
The average error metrics in all columns for each tested number of trees.
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
Choose_ntree = select_ntree(data = iris, maxtree = 9, rows = 0.1, columns = c(1:4), n_columns = 2)

##Select optimum number of trees:
Choose_ntree
}
