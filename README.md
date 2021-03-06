
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GA <a href='https://github.com/AndrewM1130/GA'><img src='Analysis/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/GA)](https://cran.r-project.org/package=GA)
[![R build
status](https://github.com/AndrewM1130/GA/workflows/R-CMD-check/badge.svg)](https://github.com/tidyverse/dplyr/actions?workflow=R-CMD-check)
[![Coverage Status](https://coveralls.io/repos/github/AndrewM1130/GA/badge.svg)](https://coveralls.io/github/AndrewM1130/GA)
<!-- badges: end -->

## Overview

GA is an exploration into the development of a customizable package
for genetic algorithm with the goal of solving multivariate linear
regression at speeds faster than step-wise methods. The main function
and supporting functions are:

-   `select()` adds new variables that are functions of existing
    variables

-   `create_initial_generation()` picks variables based on their names

-   `apply_elitism()` is a sub-function which processes the Highest
    Fitness Creatures to ensure that they Make it into the Next
    Generation

-   `breed_next_gen()` breeds the next generation

-   `create_initial_generation()` creates the Initial Gene Pool for
    Genetic Algorithm

-   `score_fitness2()` scores fitness for an entire generation of
    creatures

-   `see_if_terminate()` tests if early termination conditions are met.

-   `select_parent()` selects parents for the next generation of
    creatures

-   `test_user_function()` tests User_Provided Custom Function

-   `generate_data()` generates normal data with 50 Covariates -
    utilized for demo/setup & benchmarking purposes

GA requires the stats, tesstthat, and assertthat packages**; these
should automatically be installed and loaded when installing the ‘GA’
pacakge. A in-depth paper on the development process can be found
[here](https://github.com/AndrewM1130/GA/blob/main/release_process/GAReport.pdf).

Users can enter a custom function or select a metric from (AIC, BIC,
AICC, R2) to optimize features to include in the linear regression. This
function has a broad range of genetic algorithm features including
multiple parent section methods, crossover options, mutation options and
other features such as elitism, minimizing-inbreeding, using more than
two parents and selecting from a range of early termination options.

## Installation

``` r
# The easiest way to get dplyr is to clone install the 'GA' package with the tar.gz file:
install.packages('GA_0.1.1.tgz', repos = NULL, type ='source')

# Alternatively, install using the command line:
R CMD INSTALL 'GA_0.1.1.tgz'

# install directly through github
devtools::install_github("AndrewM1130/GA")
```

### Steps for using `select()`

1.  Load response & covariate variables
2.  **Required parameters without default values** are gene length,
    desired number of generations, and population size for each
    generation
3.  Tune & add additional parameters, which include:

<table class="tg">
<thead>
<tr>
<th class="tg-0lax">
Parameter
</th>
<th class="tg-0lax">
Variable Name
</th>
<th class="tg-0lax">
Available Options
</th>
</tr>
</thead>
<tbody>
<tr>
<td class="tg-0lax">
Crossover Method
</td>
<td class="tg-0lax">
crossover
</td>
<td class="tg-0lax">
(‘uniform’,‘fitness’,‘k_point’)
</td>
</tr>
<tr>
<td class="tg-0lax">
Parent Selection Method
</td>
<td class="tg-0lax">
metric
</td>
<td class="tg-0lax">
(‘roulette’,‘rank’,‘tournament’,‘sus’)
</td>
</tr>
<tr>
<td class="tg-0lax">
Mutation Method
</td>
<td class="tg-0lax">
mutation_rate
</td>
<td class="tg-0lax">
(‘fixed’,‘adaptive’)
</td>
</tr>
<tr>
<td class="tg-0lax">
Number of Parents
</td>
<td class="tg-0lax">
number_of_parents
</td>
<td class="tg-0lax">
integer value
</td>
</tr>
<tr>
<td class="tg-0lax">
Elitism & Diversity
</td>
<td class="tg-0lax">
elitism & minimize_inbreeding
</td>
<td class="tg-0lax">
TRUE/FALSE
</td>
</tr>
<tr>
<td class="tg-0lax">
Termination Conditions
</td>
<td class="tg-0lax">
<span
style="font-weight:normal;font-style:normal;text-decoration:none">‘pause_length’,
‘score_threshold’, ‘percent_converge’)</span>
</td>
<td class="tg-0lax">
integer values
</td>
</tr>
</tbody>
</table>

A comprehensive list of available options for each parameter and their
default values within the select() function can be found within the
[documentation
manual](https://github.com/AndrewM1130/GA/blob/main/Analysis/GADocumentation.pdf).

In the following example, gene_length is set to 20 to match the number
of independent variables. Population size is set to 25 & we set AIC as
the metric and use uniform crossover and rank based parent selection.
Moreover, we’ve set early termination criteria that if our estimator
(mean) remains unchanged for 4 iterations the program will terminate.

``` r
## example deployment for covariate-weight estimation 
response_vec <- rnorm(100)
independent_vars <- matrix(rnorm(100*20),ncol=20)
gene_length <- 20
pop <- 25
total_number_generations <- 50
metric <- 'AIC'
crossover <- 'uniform'
method <- 'rank'
estimator <-'Mean'
pause_length <- 4

select(total_number_generations = total_number_generations,
     response_vec = response_vec,
     independent_vars = independent_vars,
     pop = pop,
     gene_length = gene_length,
     metric = metric,
     crossover = crossover,
     method = method,
     estimator = estimator,
     pause_length = pause_length)
```

## GA::select() Parameters

The following section contains clarifications and an overview of common
parameter changes when developing and testing genetic algorithms.

<h3 align="center">
<u> Custom user-genes & fitness functions </u>
</h3>

-   User_genes allows the user specific genes to the initial generation
    provided that the matrix consist only of 1s and 0s has no all-zero
    rows, ncol = gene_length, and has nrow less than or equal the total
    population pop custom_function allows the user to specify a custom
    function instead of lm() or glm(). User-provided function must have
    its first two arguments be: (1) generation_matrix & (2) data.

-   Function needs to take a single row of a generation_matrix and
    return a single numeric without NAs or infinities.Otherwise, user
    should specify a metric from R2, AIC, BIC, or AICC. The underlying
    fitness function for R2 is lm() and the underlying fitness function
    for AIC, BIC, and AICc is glm(). AIC, BIC, and AICC has the option
    to specify a specific family of functions however, the data needs to
    be defined across the support for the given family for instance
    exponential must be greater than 0.

-   Finally, one can also change the **estimator** parameter; can be
    nominated from the summary() function aka Min., 1st Qu., Median,
    Mean, 3rd Qu., or Max. so for instance estimator = Max. metric =
    ‘AIC’ and score_threshold = 500 would terminate when the Max. AIC
    falls below 500. diversity is defined as sum(unique(genes))/total
    genes.

<h3 align="center">
<u> Parent selection method </u>
</h3>

-   Roulette Method selects parents randomly with a probability
    proportional to their fitness

-   Rank Method selects parents randomly with a probability proportional
    to the rank of their fitness

-   Tournament uses tourn_size to randomly group that many creatures
    together. The most fit in that group becomes a parent.

-   Stochastic Universal Sampling (susN) works like roulette but also
    selects a number (susN) more candidates at a fixed width from the
    first draw to increase diversity

<h3 align="center">
<u> Available Crossover Methods </u>
</h3>

-   Uniform - each gene is randomly selected from 2 or more parents from
    a PMF proportional to the number of parents.

-   Fitness - each gene is randomly selected from 2 or more parents from
    a PMF proportional to the parent’s fitness.

-   K-Point - parents genes are broken into k+1 segments, then the
    offspring inherits portions randomly from the parents. Takes the
    parameter number_of_crossovers which must be less than 1/2
    gene_length.For all methods candidate offspring are accepted /
    rejected so that they don’t have completely 0-vector genes.

<h3 align="center">
<u> Mutation parameters </u>
</h3>

-   Fixed - Each offspring has a mutation_rate chance of being selected
    for mutation. Once selected one gene is switched form one to zero or
    zero to one.

-   Adaptive - the overall population is measured for diversity. As
    diversity becomes lower, the mutation rate increases. Once selected
    for mutation a single gene is switched from one to zero or zero to
    one. The adaptive function is controlled by a simple logistic
    function with parameters ad_min and ad_max describing the minimum
    and maximum mutation rates. ad_inflection controls where the
    logistics point pivots, and ad_curve controls how rapidly the
    logistics curve increase.

For all mutation methods, candidate offspring are accepted/rejected so
that they don’t have completely 0-vector genes.

<h3 align="center">
<u> ‘Inbreeding’ & Elitism </u>
</h3>

-   The ‘Inbreeding’ parameter attempts to reduce the chance of similar
    creatures creating offspring together. Each parent is assigned new
    partner(s) randomly from a PMF proportional to how different their
    genes are. Parents are drawn without replacement so that if they
    were selected to become parents, they will still remain parents.

-   Elitism preserves the most-fit creatures from each generation. by
    selecting a number of most fit creatures equal to the ceiling(pop \*
    elite_prop) and guarantees that they make it to the next generation.
    Additionally, this features makes a copy of each most-fit creature
    and conducts one gene mutation randomly on each creature. If the
    copy is more fit than the original, it is returned instead of the
    original so that the final returned matrix of elite creatures are at
    least as fit as the incoming elite creatures.

<h3 align="center">
<u> Early Termination Conditions </u>
</h3>

-   Users can define early termination criteria to include a
    percentage_convergence such that when diversity falls below a
    specific threshold the program terminates. User can identify a
    estimate (from the standard summary() function that if the estimate
    reaches a specific threshold the program terminates or if the
    estimate pauses for a certain number of iterations terminates.

## Getting Help

You can read more in our functions’ documentation using ?function (such
as ?select). If you encounter a clear bug, please file an issue with a
minimal reproducible example on
[GitHub](https://github.com/AndrewM1130/GA). For questions and other
discussion, please reach out to me via email or direct messaging!
