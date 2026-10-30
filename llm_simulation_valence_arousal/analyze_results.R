## load packages and simulation results ##
##########################################

## packages for data loading and analysis
library(readxl)
library(tidyverse)
library(ggplot2)
library(gtheory)
library(papaja)

sessionInfo()

## set seed to make all analyses and plots that may contain a random component replicable
set.seed(1772625073) # as.numeric(Sys.time()) used for creating the seed

## load simulation results (depending on the dataset under analysis)
construct <- "valence"
if (construct == "valence") {
  load("sim_results/sim_results_valence.RData")
  results <- sim_results_valence
  rm(sim_results_valence)
} else if (construct == "arousal") {
  load("sim_results/sim_results_arousal.RData")
  results <- sim_results_arousal
  rm(sim_results_arousal)
}

head(results)

## check that files exist
file_names <- c(
  "Evaluation validity valence with human raters on Prolific for R analysis.xlsx",
  "Evaluation validity arousal with human raters on Prolific for R analysis.xlsx"

)

if (!all(file.exists(file_names))) {
  stop("To run all analyses, you need to download the files from the following links: https://osf.io/k5a4x/files/cn5g9 and https://osf.io/k5a4x/files/y7bpf")
}


## definition of helper functions for data analysis ##
######################################################

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

#' extract confidence bounds for correlation coefficient
#'
#' @param x first variable x
#' @param y second variable y
#' @returns two values in a vector with the first being the lower and the second
#' being the upper bound of the confidence interval for the correlation between x and y
cor_conf_bounds <- function(x, y) {
  test_result <- cor.test(x, y)
  test_result$conf.int |> as.numeric()
}

#' function that rescales the provided input to the range of 0 to 100
#'
#' @param x values to be rescaled
#' @param min theoretical minimum of provided values
#' @param max theoretical maximum of provided values
#' @returns rescaled vector of \code{x}
rescale100 <- function(x, min, max) {
  (x - min) / (max - min) * 100
}


## prepare data analysis ##
###########################

## rename variable containing the LLM ratings so that it can be distinguished later
## from human ratings of the same stimuli
results <- results |>
  rename(
    "llm_rating" = "rating",
    "llm_rating_with_invalid" = "rating_with_invalid"
  )

## rescale rating variable so that the values lie on a range from 0 to 100
## (this has to be done only for the format with a scale from 1 to 9)
results$llm_rating_re <- results$llm_rating
results$llm_rating_re[results$format == 2] <- rescale100(
  results$llm_rating_re[results$format == 2],
  min = 1,
  max = 9
)

## convert variables indicating simulation conditions to factors
results$format <- factor(results$format, labels = c("0-100", "1-9"))
results$example <- factor(results$example, labels = c("none", "lower anchor", "upper anchor", "both anchors"))
results$stimulus_factor <- factor(results$stimulus_id)


## analyze missing values ##
############################

## analyze missing values due to LLM errors
tapply(results$llm_rating_with_invalid, results$models, function(x) sum(is.na(x)))
tapply(results$llm_rating_with_invalid, results$models, function(x) mean(is.na(x)))

## analyze remaining missing values (LLM errors and values outside the allowed range) by model
tapply(results$llm_rating, results$models, function(x) sum(is.na(x))) -
  tapply(results$llm_rating_with_invalid, results$models, function(x) sum(is.na(x)))
tapply(results$llm_rating, results$models, function(x) mean(is.na(x))) -
  tapply(results$llm_rating_with_invalid, results$models, function(x) mean(is.na(x)))

## analyze invalid values by response format
tapply(results$llm_rating, results$format, function(x) sum(is.na(x))) -
  tapply(results$llm_rating_with_invalid, results$format, function(x) sum(is.na(x)))

## analyze invalid values by example type
tapply(results$llm_rating, results$example, function(x) sum(is.na(x))) -
  tapply(results$llm_rating_with_invalid, results$example, function(x) sum(is.na(x)))

## total percentage of missings
mean(is.na(results$llm_rating))


## explain variance in LLM ratings ##
#####################################

gstudy_results <- gstudy(
  formula = llm_rating_re ~ (1|stimulus_factor*models*example*format),
  data = results)
gstudy_results

dstudy(
  gstudy_results,
  colname.objects = "stimulus_factor",
  colname.scores = "llm_rating_re",
  data = results |> filter(iteration == 1) # D study with extracted variance components
)

# D study with extracted variance components but include information about iterations
dstudy(
  gstudy_results,
  colname.objects = "stimulus_factor",
  colname.scores = "llm_rating_re",
  data = results
)

# D study but with only each condition per facet (n = 1 design)
dstudy(
  gstudy_results,
  colname.objects = "stimulus_factor",
  colname.scores = "llm_rating_re",
  data = results |> # select arbitrary n = 1 configuration
    filter(iteration == 1 & models == "gemma3:4b" & format == "1-9" & example == "none")
)


## combine simulation results with human ratings ##
###################################################

if (construct == "valence") {
  data <- read_excel("Evaluation validity valence with human raters on Prolific for R analysis.xlsx")
} else if (construct == "arousal") {
  data <- read_excel("Evaluation validity arousal with human raters on Prolific for R analysis.xlsx")
}

data <- as.data.frame(data)
data[, 10:105] <- sapply(data[, 10:105], as.numeric)

## transform human data into a row-wise format for the statements
human_ratings <- data.frame(
  stimulus_id = 1:96,
  human_rating = colMeans(data[, 10:105])
)

## join simulation results with human ratings
results <- results |>
  left_join(human_ratings, by = "stimulus_id")



## evaluate inter-rater reliability in human ratings ##
#######################################################

## conduct generalizability theory analysis for human ratings
raw_human_ratings <- data[, 10:105]
n_human <- nrow(raw_human_ratings)

human_ratings_long <- raw_human_ratings |>
  mutate(rater = 1:n_human) |>
  pivot_longer(cols = all_of(colnames(raw_human_ratings)), names_to = "stimulus", values_to = "rating")

g <- gstudy(data = human_ratings_long, formula = rating ~ (1|rater) + (1|stimulus))
g
g$components$var / sum(g$components$var) # to avoid rounding

## compute generalizability and dependability coefficients
item_var <- g$components$percent[g$components$source == "stimulus"]
person_var <- g$components$percent[g$components$source == "rater"]
resid_var <- g$components$percent[g$components$source == "Residual"]

item_var / (item_var + resid_var / n_human) # generalizability coefficient
item_var / (item_var + person_var / n_human + resid_var / n_human) # dependability coefficient


## evaluate correlations of LLM ratings with human ratings ##
#############################################################

## define color theme for models for all plots
colors <- c(rep("purple", 3), "orange", rep("skyblue", 3), "#69b3a2")
stopifnot(length(colors) == length(levels(results$models)))

aggregated_results <- results |>
  ## conduct analysis across iterations for each condition
  group_by(models, format, example, iteration) |>
  ## compute correlation between human and LLM ratings
  summarize(
    cor = cor.test(llm_rating_re, human_rating)$estimate,
    cor_uncertainty_lower = cor_conf_bounds(llm_rating_re, human_rating)[1],
    cor_uncertainty_upper = cor_conf_bounds(llm_rating_re, human_rating)[2]
  ) |>
  ungroup() |>
  ## aggregate correlation statistics across simulation conditions
  group_by(models, format, example) |>
  summarize(
    correlation = mean(cor, na.rm = TRUE),
    cor_lb = ci(cor)[1],
    cor_ub = ci(cor)[2],
    cor_uncertainty_lower = mean(cor_uncertainty_lower),
    cor_uncertainty_upper = mean(cor_uncertainty_upper)
  )

ggplot(aggregated_results, aes(x = models, y = correlation, col = models)) +
  theme_apa() +
  geom_tile(
    data = aggregated_results,
    aes(
      x = models,
      y = (cor_uncertainty_lower + cor_uncertainty_upper) / 2,
      height = cor_uncertainty_upper - cor_uncertainty_lower,
      width = 0.7
    ),
    inherit.aes = FALSE,
    fill = "grey",
    alpha = 0.4
  ) +
  geom_abline(intercept = c(0.2, 0.4, 0.6, 0.8), slope = 0, linetype = "solid", linewidth = 0.5, color = "grey") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = cor_lb, ymax = cor_ub), width = 0.5, linewidth = 0.8) +
  scale_y_continuous(
    name = "correlation between human and LLM ratings",
    limits = c(0,1),
    breaks = seq(0.2, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.01),
    expand = FALSE
  ) +
  facet_grid(format ~ example, labeller = label_both) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 22)) +
  geom_abline(intercept = 0, slope = 0, linetype = "solid") +
  geom_abline(intercept = 1, slope = 0, linetype = "solid") +
  geom_abline(intercept = ifelse(construct == "valence", 0.95, 0.92), slope = 0, linetype = "dashed", alpha = 0.5)

ggsave(paste0("sim_results/", construct, "_human_llm_correlations.pdf"), dpi = 600, width = 15, height = 12)
write.csv(aggregated_results, paste0("sim_results/correlations_", construct, ".csv"), row.names = FALSE)

## split-half analysis by stimuli ##
##--------------------------------##

bootstrap_config_cors <- function(df, n_boot = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stim <- levels(droplevels(df$stimulus_factor))
  n_items <- length(stim)
  df <- df |>
    select(models, format, example, iteration,
           stimulus_factor, llm_rating_re, human_rating)

  out <- vector("list", n_boot)
  for (i in seq_len(n_boot)) {
    ## random split of the stimuli
    s_levels <- sample(stim, size = floor(n_items / 2))

    ## per-iteration correlation in each half, then average per condition
    stats <- df |>
      mutate(half = if_else(stimulus_factor %in% s_levels, "a", "b")) |>
      group_by(models, format, example, iteration, half) |>
      summarize(cor = cor(llm_rating_re, human_rating, use = "complete.obs"),
                .groups = "drop") |>
      group_by(models, format, example, half) |>
      summarize(cor = mean(cor, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = half, values_from = cor)

    ## stability of the 64 averaged correlations between halves
    out[[i]] <- tibble(
      boot     = i,
      pearson  = cor(stats$a, stats$b, use = "complete.obs"),
      spearman = cor(stats$a, stats$b, method = "spearman", use = "complete.obs")
    )
  }
  bind_rows(out) |> as.data.frame()
}

split_half_cors <- bootstrap_config_cors(results, n_boot = 1000)
spearman_brown <- function(r) 2 * r / (1 + r)
split_half_cors |>
  summarize(
    across(c(pearson, spearman),
           list(mean = mean,
                q025 = ~ quantile(.x, 0.025),
                q975 = ~ quantile(.x, 0.975))),
    pearson_full  = spearman_brown(pearson_mean),
    spearman_full = spearman_brown(spearman_mean)
  )


## scatter plot: human and best LLM ratings ##
##------------------------------------------##

best_idx <- which.max(aggregated_results$correlation)
best_row <- aggregated_results[best_idx, ]
best_row

results |>
  group_by(format, example, models, stimulus_id) |>
  summarize(
    human_rating = mean(human_rating),
    llm_rating = mean(llm_rating, na.rm = TRUE)
  ) |>
  filter(format == best_row$format & example == best_row$example & models == best_row$models) |>
  ggplot(aes(x = human_rating, y = llm_rating)) +
  theme_apa() +
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "salmon", level = 0.95) +
  scale_x_continuous(name = paste0("Average human rating of ", construct), limits = c(1,7), breaks = 1:7) +
  scale_y_continuous(name = paste0("Average LLM rating of ", construct), limits = c(0,9), breaks = 1:9) +
  theme(text = element_text(size = 15))

ggsave(paste0("sim_results/", construct, "_scatter_plot_human_best_llm.pdf"), dpi = 600, width = 10,height = 7)



## assess correlation by number of estimates per stimulus (iterations) ##
##---------------------------------------------------------------------##

#' function repeatedly computes mean LLM ratings across different numbers of iterations of the simulation
#'
#' @param input results data frame from simulation containing LLM ratings for each stimulus for each simulation iteration
#' @param repetitions number of times this analysis will be repeated
#' @returns data frame that contains mean LLM-generated ratings of valence/arousal
#' for different orders of aggregation (number of randomly drawn simulation iterations
#' of the same valence/arousal rating in the respective condition).
#' For the sake of completeness, the data frame also contains mean human ratings of statements.
get_aggregated_ratings <- function(input, repetitions) {
  ## function requires the variables format, example, models, stimulus_id, and iteration
  ## and llm_rating_re to be defined in input data frame (function tailored to this analysis)
  output <- NULL
  for (i in 1:repetitions) {
    suppressMessages(
      stats <- input |>
        group_by(format, example, models, stimulus_id) |>
        summarize(
          human_rating = mean(human_rating), # always the same across iterations of simulation because it is never varied
          llm_mean_complete =  mean(sample(llm_rating_re, size = 100, replace = TRUE), na.rm = TRUE),
          llm_mean_50 = mean(sample(llm_rating_re, size = 50, replace = TRUE), na.rm = TRUE),
          llm_mean_25 = mean(sample(llm_rating_re, size = 25, replace = TRUE), na.rm = TRUE),
          llm_mean_10 = mean(sample(llm_rating_re, size = 10, replace = TRUE), na.rm = TRUE),
          llm_mean_05 = mean(sample(llm_rating_re, size = 5, replace = TRUE), na.rm = TRUE),
          llm_mean_01 = mean(sample(llm_rating_re, size = 1, replace = TRUE), na.rm = TRUE)
        )
    )
    stats$repetition <- factor(i)
    output <- rbind(output, stats)
  }
  output
}

## aggregate across ratings of the same models across different numbers of simulation iterations
item_order_stats <- get_aggregated_ratings(results, repetitions = 100)
item_order_stats

## compute correlation between human-generated and LLM-generated statements for
## different orders of aggregation
cor_order_stats <- item_order_stats |>
  pivot_longer(
    cols = matches("llm_mean"),
    names_to = "draws",
    values_to = "llm_rating_re"
  ) |>
  mutate(
    draws =
      factor(
        draws,
        levels = c("llm_mean_01", "llm_mean_05", "llm_mean_10", "llm_mean_25", "llm_mean_50", "llm_mean_complete"),
        labels = c("1", "5", "10", "25", "50", "100")
      )
  ) |>
  ## compute correlation of human and LLM ratings
  group_by(format, example, models, draws, repetition) |>
  summarize(
    cor = cor.test(human_rating, llm_rating_re)$estimate
  ) |>
  ## finally, aggregate correlations for each simulation condition across repetitions
  group_by(format, example, models, draws) |>
  summarize(
    correlation = mean(cor),
    cor_lb = ci(cor)[1],
    cor_ub = ci(cor)[2]
  )

cor_order_stats |>
  filter(format == "1-9" & example == "both anchors") |>
  ggplot(aes(x = draws, y = correlation, col = models, group = 1)) +
  theme_apa() +
  geom_abline(intercept = c(0.6, 0.8), slope = 0, linetype = "solid", linewidth = 0.5, color = "grey") +
  geom_point(size = 5) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = cor_lb, ymax = cor_ub), width = 0.6) +
  scale_x_discrete(
    expression(n["est"])
  ) +
  scale_y_continuous(
    name = "correlation between human and LLM ratings",
    limits = c(ifelse(construct == "valence", 0.5, 0.4),1),
    breaks = seq(0.5, 1, by = 0.1),
    labels = scales::number_format(accuracy = 0.01),
    expand = FALSE
  ) +
  facet_grid(~models) +
  scale_color_manual(values = colors) +
  geom_abline(intercept = 1, slope = 0, linetype = "solid", alpha = 1) +
  geom_abline(intercept = ifelse(construct == "valence", 0.95, 0.92), slope = 0, linetype = "dashed", alpha = 0.5) +
  theme(text = element_text(size = 20), legend.position = "none", axis.title = element_text(size = 32), strip.text.x = element_text(size = 26))

ggsave(paste0("sim_results/", construct, "_human_llm_correlations_by_aggregation.pdf"), dpi = 600, width = 20, height = 15)


## overall best performance including aggregation procedures ##
##-----------------------------------------------------------##

best_idx_aggregated <- which.max(cor_order_stats$correlation)
best_cor_aggregated <- cor_order_stats[best_idx_aggregated, ]
best_cor_aggregated


## compare valence and arousal results ##
#########################################

correlations_valence <- read.csv("sim_results/correlations_valence.csv")
correlations_arousal <- read.csv("sim_results/correlations_arousal.csv")

cor.test(correlations_valence$correlation, correlations_arousal$correlation)
cor.test(correlations_valence$correlation, correlations_arousal$correlation, method = "spearman")

all_correlations <- correlations_valence
all_correlations <- all_correlations |> rename("correlation_valence" = "correlation")
all_correlations$correlation_arousal <- correlations_arousal$correlation

all_correlations <- all_correlations |>
  mutate(
    models = factor(models, levels = unique(models))
  )

ggplot(all_correlations, aes(x = correlation_valence, y = correlation_arousal, col = models, shape = models)) +
  theme_apa() +
  geom_point() +
  geom_smooth(aes(group = NA), method = "lm", fill = "grey", color = "black", linetype = "dashed", linewidth = 1) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_color_manual(values = colors) +
  scale_x_continuous(
    name = "correlations between human and LLM ratings (valence)",
    limits = c(0.3, 1),
    breaks = c(0.2, 0.4, 0.6, 0.8, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(
    name = "correlations between human and LLM ratings (arousal)",
    limits = c(0.3, 1),
    breaks = c(0.2, 0.4, 0.6, 0.8, 1),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_color_manual(
    name = "models",
    labels = levels(all_correlations$models),
    values = colors
  ) +
  scale_shape_manual(
    name = "models",
    labels = levels(all_correlations$models),
    values = c(19, 15, 17, 19, 19, 15, 17, 19)
  ) +
  theme(text = element_text(size = 15))

ggsave(paste0("sim_results/", "valence_arousal_comparison.pdf"), dpi = 600, width = 12, height = 9)
