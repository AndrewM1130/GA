% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_custom_function.R
\name{test_user_function}
\alias{test_user_function}
\title{Tests User_Provided Custom Function}
\usage{
test_user_function(func, generation_matrix, data)
}
\arguments{
\item{func}{user provided function (needs generation_matrix followed by data)}

\item{generation_matrix}{generation matrix with all 0s and 1s with ncol = gene_length and nrow = pop}

\item{data}{a matrix or dataframe with ncol = gene_length plus 1}
}
\value{
TRUE if it passes and an error message otherwise
}
\description{
This function confirms that a user-provided function takes genes from
a generation_matrix and a data matrix and returns a score for each gene
It also confirms there are not NAs or infinity values.
This function does not confirm that the scores are in descending order
aka higher fitness creatures receive a higher fitness scores. It also
does not confirm that all possible scores are defined.
}
\details{
User-provided function must have its first two arguments be
(1) generation_matrix and (2) data. Function needs to take a single row of
 a generation_matrix and return a single numeric without NAs or  infinities.
}
\examples{
generation_matrix <- matrix(rbinom(2*10,1,.5),ncol=10)
data <- matrix(rnorm(11*3),nc=11)
func <- function(generation_matrix,data) {sum(generation_matrix)}

test <- test_user_function(func,generation_matrix,data)

}
