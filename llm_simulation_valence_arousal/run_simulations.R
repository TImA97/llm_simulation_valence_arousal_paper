## recommendation: run script as Batch job
## systemd-inhibit R CMD BATCH --vanilla run_simulations.R sim_results/sim_results_documentation.Rout

## load packages ##
###################
## automate LLM usage
library(ellmer)

## packages for data loading
library(readxl)

## package for displaying progress bar of the simulation
library(utils)

## load simulation code
source("simulation_code.R")

## create directory for storing results (if it does not yet exist)
dir.create("sim_results/")
dir.create("sim_results/tmp/")

## document session information
sessionInfo()


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


## arousal ##
##---------##

arousal_statements <- read_excel("96_selected_stimuli_human_validation_arousal.xlsx")
arousal_statements <- arousal_statements[, "Word"]
names(arousal_statements) <- "statement"
arousal_statements$stimulus_id <- 1:nrow(arousal_statements)
arousal_statements


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


## arousal ##
##---------##

prompt_params_arousal <- list(
  system_prompts = "",
  example = c(
    "",
    "Examples of words that would get a rating of {scale_min} are grain, dull, and rest.",
    "Examples of words that would get a rating of {scale_max} are gun, lover, and thrill.",
    "Examples of words that would get a rating of {scale_min} are grain, dull, and rest. Examples of words that would get a rating of {scale_max} are gun, lover, and thrill."
  )
)

prompt_params_arousal$expressions <- arousal_statements


## define models used for the rating task ##
############################################

models <- c(
  "gemma3:4b", "gemma3:12b", "gemma3:27b",
  "llama3.1:8b",
  "phi3:3.8b", "phi3:14b", "phi4:14b",
  "gpt-oss:20b"
)


## execute simulation ##
########################

# identical for both simulations
iterations <- 100
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
sim_results_valence <- do_llm_simulation(
  design,
  iterations,
  prompt_params_valence,
  construct = "valence",
  fill_cols = c("scale_min", "scale_max")
)

save(sim_results_valence, file = "sim_results/sim_results_valence.RData")


## arousal ##
##---------##

sim_results_arousal <- do_llm_simulation(
  design,
  iterations,
  prompt_params_arousal,
  construct = "arousal",
  fill_cols = c("scale_min", "scale_max")
)

save(sim_results_arousal, file = "sim_results/sim_results_arousal.RData")
