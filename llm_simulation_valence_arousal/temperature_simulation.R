## recommendation: run script as Batch job
## systemd-inhibit R CMD BATCH --vanilla temperature_simulation.R sim_results/sim_results_documentation_temperature_analysis.Rout

## load packages ##
###################
## automate LLM usage
library(ellmer)

## packages for data loading
library(readxl)

## package for displaying progress bar of the simulation
library(utils)

## data analysis
library(tidyverse)

## load simulation code
source("simulation_code.R")

## create directory for storing results (if it does not yet exist)
dir.create("sim_results/")
dir.create("sim_results/tmp/")

## document session information
sessionInfo()

set.seed(582358926)


## read statements to be rated ##
#################################

## check that files exist
file_names <- c(
  "96_selected_stimuli_human_validation_valence.xlsx",
  "96_selected_stimuli_human_validation_arousal.xlsx"
)

if (!all(file.exists(file_names))) {
  stop("To run all analyses, you need to download the files from the following links: https://osf.io/k5a4x/files/qp2u8 and https://osf.io/k5a4x/files/kuz5s")
}

## valence ##
##---------##

valence_statements <- read_excel("96_selected_stimuli_human_validation_valence.xlsx")
valence_statements <- valence_statements[, "Word"]
names(valence_statements) <- "statement"
valence_statements$stimulus_id <- 1:nrow(valence_statements)
valence_statements


## define parameters of prompt design ##
########################################

## valence ##
##---------##

prompt_params_valence <- list(
  system_prompts = "",
  example = c(
    "",
    "Examples of words that would get a rating of {scale_min} are pedophile, AIDS, and wreck.",
    "Examples of words that would get a rating of {scale_max} are vacation, fantastic, and laugh.",
    "Examples of words that would get a rating of {scale_min} are pedophile, AIDS, and wreck. Examples of words that would get a rating of {scale_max} are vacation, fantastic, and laugh."
  )
)

prompt_params_valence$expressions <- valence_statements


## define models used for the rating task ##
############################################

models <- c(
  "llama3.1:8b",
  "phi3:3.8b", "phi3:14b", "phi4:14b"
)


## execute simulation ##
########################

# identical for both simulations
iterations <- 20
design <- expand.grid(
  system_prompt = 1,
  format = 1:2,
  example = 1:4,
  models = models,
  scale_min = NA,
  scale_max = NA
)

design$scale_min <- ifelse(design$format == 1, 0, 1)
design$scale_max <- ifelse(design$format == 1, 100, 9)

design
design |> nrow()


## valence ##
##---------##

## run simulation
sim_results_valence_tmp_default <- do_llm_simulation(
  design,
  iterations,
  prompt_params_valence,
  construct = "valence",
  fill_cols = c("scale_min", "scale_max"),
  temperature = NULL
)

sim_results_valence_tmp_0_8 <- do_llm_simulation(
  design,
  iterations,
  prompt_params_valence,
  construct = "valence",
  fill_cols = c("scale_min", "scale_max"),
  temperature = 0.8
)

sim_results_valence_tmp_1_0 <- do_llm_simulation(
  design,
  iterations,
  prompt_params_valence,
  construct = "valence",
  fill_cols = c("scale_min", "scale_max"),
  temperature = 1
)

all_temperature_results_valence <- cbind(
  temperature = rep(c("default", 0.8, 1), each = nrow(sim_results_valence_tmp_default)),
  rbind(
    sim_results_valence_tmp_default,
    sim_results_valence_tmp_0_8,
    sim_results_valence_tmp_1_0
  )
)

entropy <- function(x) - sum(table(x)/sum(!is.na(x)) * log2(table(x)/sum(!is.na(x))))
#' standard error (se)
#'
#' @param x values for which to compute the standard error
#' @returns standard error of x
se <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}

#' confidence interval (ci) that uses the \code{se} function
#'
#' @param x values for which to compute the confidence interval bounds
#' @returns two values in a vector with the first being the lower and the second
#' being the upper bound of the confidence interval for x
ci <- function(x) {
  c(mean(x, na.rm = TRUE) - 1.96 * se(x), mean(x, na.rm = TRUE) + 1.96 * se(x))
}

entropy_stats <- all_temperature_results_valence |>
  group_by(temperature, models, example, format, stimulus_id) |>
  summarize(e = entropy(rating)) |>
  group_by(temperature, models) |>
  summarize(entropy = mean(e), ci_lb = ci(e)[1], ci_ub = ci(e)[2]) |>
  arrange(models)
entropy_stats

## conducting this simulation is analogous for the ratings of arousal but not presented
## here due to the exemplary nature of this script
