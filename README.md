# GA
On the utilization of Genetic Algorithms for Feature Selection in Multivariate Regression Models

Last Updated: 17 DEC 2021

Andrew Ma - James Hall - Sky Qiu

[![R build
status](https://github.com/tidyverse/dplyr/workflows/R-CMD-check/badge.svg)](https://github.com/tidyverse/dplyr/actions?workflow=R-CMD-check)

<img src="http://hexb.in/vector/dotnetbot.svg" style="width:10.0%" />

## Overview:

-   Description
-   Git Layout
-   Dependencies
-   How to Download
-   Steps for Using Select and Example
-   Functions and Supporting Functions
-   Parameters for Select
-   Additional Details
-   Acknowledgements

## Description:

User can enter a custom function or select a metric from (AIC, BIC,
AICC, R2) to optimize features to include in the linear regression. This
function has a broad range of genetic algorithm features including
multiple parent section methods, crossover options, mutation options and
other features such as elitism, minimizing-inbreeding, using more than
two parents and selecting from a range of early termination options. See
below for details)

## Git Layout:

-   ‘GA’ folder contains R package

-   ‘Development’ folder contains past R code and our group’s past
    commits, e.t.c.

-   .tar.gz and .tgz for package download

## Dependencies:

GA requires stats, tesstthat, and assertthat packages.

## How/where to download your program:

1.  Download GA_0.1.0.tar.gz from the GA github
2.  Unzip .gz file onto working directory
3.  ‘R CMD INSTALL GA’

## Steps for Using Select:

1.  Load response variables
2.  Load Independent variables
3.  Select number of generations
4.  Select Initial Population Size
5.  Select and/or tune additional features including:

-   crossover method (‘uniform’,‘fitness’,‘k_point’)
-   parent selection method (‘roulette’,‘rank’,‘tournament’,‘sus’)
-   mutation method (‘fixed’,‘adaptive’)
-   number of parents per offspring
-   elitism
-   minimize inbreeding
-   Early termination conditions (‘pause_length’, ‘score_threshold’,
    ‘percent_converge’)

## An Example / Your first Genetic Algorithm using Select

-   Concept: Below, gene_length is set to 20 to match the number of
    independent variables. Pop is set to 25. We set AIC as the metric
    and use uniform crossover and rank based parent selection. Also,
    we’ve set early termination criteria that if our estimator (mean)
    remains unchanged for 4 iterations the program will terminate.

``` r
response_vec <- rnorm(100)
independent_vars <- matrix(rnorm(100*20),ncol=20)
gene_length <- 20
pop <- 25
total_number_generations <- 25
metric <- 'AIC'
crossover <- 'uniform'
method <- 'rank'
estimator <-'Mean'
pause_length <-4

select(total_number_generations = total_number_generations,
     response_vec = response_vec,independent_vars = independent_vars,pop = pop,
     gene_length = gene_length,metric = metric,crossover = crossover,
     method = method,estimator = estimator,pause_length = pause_length)
```

## Functions and Supporting Functions:

-   apply_elitism - Processes the Highest Fitness Creatures to ensure
    that they Make it into the Next Generation

-   breed_next_gen - Breeds Next Generation

-   create_initial_generation - Creates the Initial Gene Pool for
    Genetic Algorithm

-   generate_data - Generates normal data with 50 Covariates.

-   score_fitness2 - Scores fitness for an entire generation of
    creatures

-   see_if_terminate - Tests if early termination conditions are met.

-   select - Runs a Genetic Algorithm

-   select_parent - Selects parents for the next generation of creatures

-   test_user_function - Tests User_Provided Custom Function

## Help:

You can read more in our functions’ documentation using ?function (such
as ?select)

## GA::select() Parameters:

-   total_number_generations - an integer representing the maximum
    number of iterations number_of_parents - an integer defining the
    number of parents per offspring

-   pop - an integer defining the number of creatures per a generation

-   gene_length - an integer representing the number of genes per a
    creature

-   response_vec - a numeric vector without infinite or NA values

-   independent_vars - a matrix or dataframe with ncol equal to number
    of genes and nrow with the same length as the response_vec

-   prob - a number 0-1 representing the chance that an individual has
    any given gene

-   user_genes - a matrix consisting of 1s and 0s without any rows with
    all zeros ncol equal to gene_length and nrow less than or equal to
    pop

-   metric - a character from the following ‘R2’, ‘AIC’, ‘BIC’, or
    ‘AICc’, the statistic LM or GLM is returning

-   family - the GLM family of distributions (must have the same support
    as the data for instance exponential cannot have negative numbers)

-   method - the method of choosing parents, ‘roulette’, ‘rank’,
    ‘tournament’, and ‘sus’

-   susN - if sus is selected how many parents are chosen scholastically

-   tourn_size - if tournament is chosen how many candidates are in the
    tournament

-   mutation - a character either ‘fixed’ or ‘adaptive’ indicating the
    type of mutation

-   mutation_rate - a numeric between 0 and 1 indicating how often a
    creature is selected for mutation.

-   minimize_inbreeding - a logical indicating whether or not we
    minimize inbreeding.

-   crossover - a character that can be ‘uniform’, ‘fitness’, or
    ‘k_point’ indicating how to carry out crossover.

-   number_of_crossovers - number of k_point crossovers, needs to be
    less than gene_length

-   elitism - a logical representing whether elitism is requested

-   elite_prop - the proportion of each generation that is selected for
    elitism

-   ad_max_mutate - max mutation rate for adaptive mutation, numeric
    between 0 and 1 and more than ad_min

-   ad_min_mutate - min mutation rate for adaptive mutation, numeric
    between 0 and 1 and less than ad_max

-   ad_inflection - percentage of diversity in population where adaptive
    mutation begins to increase rapidly

-   ad_curve - rate that influence how intensely adaptive mutation
    changes

-   custom_function - user defined custom mast take vector representing
    a single creatures genes, data, and return a single numeric

-   estimator - character string from ‘Min.’,‘1st
    Qu.’,‘Median’,‘Mean’,‘3rd Qu.’, or ‘Max.’; this term controls
    termination conditions for instance, terminating after the Mean AIC
    is above a certain threshold

-   pause_length - a numeric indicating how many iterations without
    improvement in the estimator before you terminate

-   percent_converge - percentage of diversity when algorithm terminates
    for instance .1 will terminate if there are only .90 of the
    population is represented by the same genes

-   score_threshold - a numeric indicating a threshold to cutoff if the
    estimator reaches that value

-   fittest - a character either ‘high’ or ‘low’ defining whether a
    custom functions

## Additional Details:

user_genes allows the user specific genes to the initial generation
provided that the matrix consist only of 1s and 0s has no all-zero rows,
ncol = gene_length, and has nrow less than or equal the total population
pop

custom_function allows the user to specify a custom function instead of
lm() or glm(). User-provided function must have its first two arguments
be (1) generation_matrix and (2) data. Function needs to take a single
row of a generation_matrix and return a single numeric without NAs or
infinities.

Otherwise, user should specify a metric from R2, AIC, BIC, or AICC. The
underlying fitness function for R2 is lm() and the underlying fitness
function for AIC, BIC, and AICc is glm(). AIC, BIC, and AICC has the
option to specify a specific family of functions however, the data needs
to be defined across the support for the given family for instance
exponential must be greater than 0.

### Parents are selected via a number of methods:

Roulette Method selects parents randomly with a probability proportional
to their fitness

Rank Method selects parents randomly with a probability proportional to
the rank of their fitness

Tournament uses tourn_size to randomly group that many creatures
together. The most fit in that group becomes a parent.

Stochastic Universal Sampling works like roulette but also selects a
number (susN) more candidates at a fixed width from the first draw to
increase diversity

### Crossover Methods

Uniform - each gene is randomly selected from 2 or more parents from a
PMF proportional to the number of parents.

Fitness - each gene is randomly selected from 2 or more parents from a
PMF proportional to the parent’s fitness.

K-Point - parents genes are broken into k+1 segments, then the offspring
inherits portions randomly from the parents. Takes the parameter
number_of_crossovers which must be less than 1/2 gene_length.

For all methods candidate offspring are accepted / rejected so that they
don’t have completely 0-vector genes.

### Mutation

Fixed - Each offspring has a mutation_rate chance of being selected for
mutation. Once selected one gene is switched form one to zero or zero to
one.

Adaptive - the overall population is measured for diversity. As
diversity becomes lower, the mutation rate increases. Once selected for
mutation a single gene is switched from one to zero or zero to one. The
adaptive function is controlled by a simple logistic function with
parameters ad_min and ad_max describing the minimum and maximum mutation
rates. ad_inflection controls where the logistics point pivots, and
ad_curve controls how rapidly the logistics curve increase.

For all mutation methods, candidate offspring are accepted/rejected so
that they don’t have completely 0-vector genes.

### Minimize inbreeding

This option reduces (though does not remove) the chance of similar
creatures creating offspring together. Each parent is assigned new
partner(s) randomly from a PMF proportional to how different their genes
are. Parents are drawn without replacement so that if they were selected
to become parents, they will still remain parents.

### Elitism

This factor preserves the most-fit creatures from each generation. by
selecting a number of most fit creatures equal to the ceiling(pop \*
elite_prop) and guarantees that they make it to the next generation.
Additionally, this features makes a copy of each most-fit creature and
conducts one gene mutation randomly on each creature. If the copy is
more fit than the original, it is returned instead of the original so
that the final returned matrix of elite creatures are at least as fit as
the incoming elite creatures.

### Early Termination Conditions

User can define early termination criteria to include a
percentage_convergence such that when diversity falls below a specific
threshold the program terminates. User can identify a estimate (from the
standard summary() function that if the estimate reaches a specific
threshold the program terminates or if the estimate pauses for a certain
number of iterations terminates.

### Estimator

Can be nominated from the summary() function aka Min., 1st Qu., Median,
Mean, 3rd Qu., or Max. so for instance estimator = Max. metric = ‘AIC’
and score_threshold = 500 would terminate when the Max. AIC falls below
500. diversity is defined as sum(unique(genes))/total genes.
