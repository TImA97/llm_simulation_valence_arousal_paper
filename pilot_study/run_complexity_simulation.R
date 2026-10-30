## recommendation: run script as Batch job
## systemd-inhibit R CMD BATCH --vanilla run_complexity_simulation.R sim_results/2025_12_09_sim_results.Rout

## load packages ##
###################
## automate LLM usage
library(ellmer)

## packages for data loading
library(readr)

## package for displaying progress bar of the simulation
library(utils)

## package for data-handling
library(tidyverse)

## load simulation code
source("simulation_code.R")

## create directory for storing results (if it does not yet exist)
dir.create("sim_results/")

## path where to save simulation results
save_path <- "sim_results/simulation_results.RData"


## check that files exist
file_names <- c(
  "study_3_statements-statistics.csv",
  "mixed_models_item_filtered_study_3.RData"
)

if (!all(file.exists(file_names))) {
  stop("To run all analyses, you need to download the files from the following links: https://osf.io/vp2nu/files/t5rxp and https://osf.io/vp2nu/files/a7gj2")
}


## read statements to be rated ##
#################################

data <- read_delim(
  "study_3_statements-statistics.csv",
  delim = "\t", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Windows-1252"))

data$statement


## load results from previous studies to obtain human ratings of statements ##
##############################################################################

load("mixed_models_item_filtered_study_3.RData")
human_ratings_by_item <- study3_long_item %>%
  left_join(data, by = c("item" = "key")) %>%
  mutate(
    item2 = factor(item2)
  ) %>%
  group_by(item2, statement) %>%
  summarize(complexity = mean(complexity, na.rm = TRUE)) %>%
  arrange(item2)

human_ratings_by_item %>% head(10)

## identify least and most complex statement as well as an average complexity
human_ratings_by_item %>% arrange(complexity) %>% head() # min 1.32
human_ratings_by_item %>% arrange(complexity) %>% tail() # max 4.52
## min complexity statement: "Thomas Edison hat den ersten elektrischen Stuhl entwickelt." (c = 1.32, id = tu3)
## max complexity statement: "Schöpfungsgötter gewesen seien griechischer Mythologie nach Gaia, Erebos und Kreon." (c = 4.52, id = fu10)

## remove all statements with corresponding item content of example statements
human_ratings_by_item <- human_ratings_by_item %>%
  filter(!item2 %in% c("tu3", "fu10"))

human_ratings_by_item %>% nrow() ## 4 statements less because 2 examples * 2 = 4


## replace double quotation marks, e.g., \"\" with \"
human_ratings_by_item$statement_old <- human_ratings_by_item$statement # save old version
human_ratings_by_item$statement <- lapply(human_ratings_by_item$statement, function(x) gsub("\"\"", "\"", x)) |> Reduce(f = "c")
human_ratings_by_item$statement_old |> tail()
human_ratings_by_item$statement |> tail()


## define parameters of prompt design ##
########################################

prompt_parameters <- list()

prompt_parameters$human_ratings <- human_ratings_by_item
prompt_parameters$human_ratings$stimulus_id <- 1:nrow(prompt_parameters$human_ratings)

prompt_parameters$system_prompts <- c("")
prompt_parameters$tasks <- c(
  "Sie werden gleich die syntaktische Komplexität einer Aussage auf einer Skala von 0 bis 100 bewerten, wobei 0 eine maximal simple und 100 eine maximal komplexe Aussage ist. Syntaktische Komplexität bezieht sich auf das Vorhandensein von Textmarkern wie Wortschatzreichtum, lexikalischer Vielfalt, Lesbarkeit und syntaktischer Vielfalt.",
  "Sie werden gleich die syntaktische Komplexität einer Aussage auf einer Skala von 1 bis 7 bewerten, wobei 1 eine maximal simple und 7 eine maximal komplexe Aussage ist. Syntaktische Komplexität bezieht sich auf das Vorhandensein von Textmarkern wie Wortschatzreichtum, lexikalischer Vielfalt, Lesbarkeit und syntaktischer Vielfalt."
)
prompt_parameters$examples <- c(
  "",
  " Ein Beispiel für eine simple Aussage ist: Thomas Edison hat den ersten elektrischen Stuhl entwickelt.",
  " Ein Beispiel für eine komplexe Aussage ist: Schöpfungsgötter gewesen seien griechischer Mythologie nach Gaia, Erebos und Kreon.",
  paste0(
    " Ein Beispiel für eine simple Aussage ist: Thomas Edison hat den ersten elektrischen Stuhl entwickelt.",
    " Ein Beispiel für eine komplexe Aussage ist: Schöpfungsgötter gewesen seien griechischer Mythologie nach Gaia, Erebos und Kreon."
  )
)

## example prompt
create_prompt(prompt_parameters$tasks[1], prompt_parameters$examples[4], prompt_parameters$human_ratings$statement[1])


## define models used for the rating task ##
############################################

models <- c(
  "gemma3:4b", "gemma3:12b", "gemma3:27b",
  "llama3.1:8b",
  "phi3:3.8b", "phi3:14b", "phi4:14b",
  "gpt-oss:20b"
)

design <- expand.grid(
  system_prompt = 1:length(prompt_parameters$system_prompts),
  task = 1:length(prompt_parameters$tasks),
  example = 1:length(prompt_parameters$examples),
  models = models
)

design
design |> nrow()


## execute simulation ##
########################

## include possibility to specify random seeds in simulation

iterations <- 100
human_ratings_by_item$stimulus_id <- 1:nrow(human_ratings_by_item)
prompt_parameters$human_ratings <- human_ratings_by_item

## run simulation
sim_results <- do_llm_simulation(design, iterations, prompt_parameters)



## prepare simulation results for further analysis ##
#####################################################

## combine with empirical ratings done by humans
results <- sim_results %>%
  ## integrate with human-generated ratings
  left_join(human_ratings_by_item, by = "stimulus_id") %>%
  ## assign meaningful names to variables and labels to factors
  rename(
    "llm_rating" = "rating",
    "human_rating" = "complexity"
  ) %>%
  mutate(
    task = factor(task, labels = c("101pt", "7pt")),
    example = factor(example, labels = c("no", "lower", "upper", "both")),
    item2 = factor(item2),
    stimulus_id = factor(stimulus_id)
  )

## rescale responses in the rating task to be on the same scale from 0 to 100
#' function that rescales the provided input to the range of 0 to 100
#'
#' @param x values to be rescaled
#' @param min theoretical minimum of provided values
#' @param max theoretical maximum of provided values
#' @returns rescaled vector of \code{x}
rescale100 <- function(x, min, max) {
  (x - min) / (max - min) * 100
}

results$llm_rating_re <- results$llm_rating
results[results$task == "7pt", "llm_rating_re"] <- rescale100(
  results[results$task == "7pt", "llm_rating"], min = 1, max = 7
)

head(results)

## save results
save(results, file = save_path)
