% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_fitness_V2.R
\name{score_fitness2}
\alias{score_fitness2}
\title{Scores fitness for an entire generation of creatures}
\usage{
score_fitness2(
  gene,
  data,
  metric = "AIC",
  family = "gaussian",
  custom_function = NULL,
  fittest = "high"
)
}
\arguments{
\item{gene}{matrix with all 0s and 1s with ncol = gene_length and nrow = pop}

\item{data}{a matrix or dataframe with ncol = gene_length plus 1}

\item{metric}{a character, a user specified statistic such as R2, AIC, BIC, or AICc}

\item{family}{, model family corresponding to GLM. Defaults to 'gaussian'}

\item{custom_function}{defaults to NULL, if defined takes a single row from a generation_matrix and returns a numeric value}

\item{fittest}{whether a custom function has the highest value corresponding to the fittest or the lowest value}
}
\value{
returns a list with the first element being a vector of processed fitness scores to for continued use by the algorithm and element 2 being true (user specified) values user reference, for instance true AIC values
}
\description{
This takes a generation_matrix representing all the genes of a single
generation, the data, and user input describing the metric such as R2,
AIC, BIC, and AICC and can even use a custom function defined by the user.
The underlying fitness function for R2 is lm() and the underlying fitness
function for AIC, BIC, and AICc is glm().  AIC, BIC, and AICC has the option
to specify a specific family of functions however, the data needs to be
defined across the support for the given family for instance exponential
must be greater than 0.
}
\examples{
gene <- matrix(rbinom(2*10,1,.5),nc=10)
data <- matrix(rnorm(11*3),nc=11)
pop_required <- 10
metric <- 'AIC'

score_vec <- score_fitness2(data = data,gene = gene,metric = metric)

}
