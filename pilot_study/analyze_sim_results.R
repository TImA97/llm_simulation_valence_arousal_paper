## This script is for analyzing the data from the LLM simulation

## load packages and simulation results ##
##########################################

## packages for data loading and analysis
library(tidyverse)
library(ggplot2)
library(car)
library(gtheory)

## load simulation results
warning("running this script requires running the simulation beforehand")
load("sim_results/simulation_results.RData")


## definition of helper functions for data analysis ##
######################################################

#' standard error (se)
#'
#' @param x values for which to compute the standard error
#' @returns standard error of x
se <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / sqrt(length(x))
}

#' confidence interval (ci) that uses the \code{se} function
#'
#' @param x values for which to compute the confidence interval bounds
#' @returns two values in a vector with the first being the lower and the second
#' being the upper bound of the confidence interval for x
ci <- function(x) {
  c(mean(x) - 1.96 * se(x), mean(x) + 1.96 * se(x))
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


## analyze missing values ##
############################

## look for percentages of missing values in ratings
tapply(results$llm_rating, results$models, function(x) sum(is.na(x)))
tapply(results$llm_rating, results$models, function(x) mean(is.na(x)))


## explain variance in LLM ratings ##
#####################################

#' function compute partial eta squared for Anova model
#' @param anova_model anova model without partial eta squared computed yet
#' @returns anova table including partial eta squared
partial_eta_squared <- function(anova_model) {
  # extract sum of squares
  ss <- anova_model[["Sum Sq"]]

  # get the error/residual row
  error_row <- rownames(anova_model) == "Residuals"
  ss_error <- ss[error_row]

  # SS_effect / (SS_effect + SS_error)
  partial_eta_sq <- ss / (ss + ss_error)

  # Set the residual row to NA (not meaningful)
  partial_eta_sq[error_row] <- NA

  # Add as a new column to the anova model
  anova_model$partial_eta_sq <- partial_eta_sq

  return(anova_model)
}

## set effect coding for categorical predictors
contrasts(results$task) <- contr.sum(2)
contrasts(results$example) <- contr.sum(4)
contrasts(results$models) <- contr.sum(8)

results$stimulus_factor <- factor(results$stimulus_id)
contrasts(results$stimulus_factor) <- contr.sum(length(levels(results$stimulus_factor)))

## normalize ratings
results$llm_rating_re_z <- c(scale(results$llm_rating_re))

lm_2x <- lm(llm_rating_re_z ~ (stimulus_factor + models + task + example)^2, data = results)
lm_3x <- lm(llm_rating_re_z ~ (stimulus_factor + models + task + example)^3, data = results)
lm_4x <- lm(llm_rating_re_z ~ (stimulus_factor + models + task + example)^4, data = results)

anova_2x <- partial_eta_squared(Anova(lm_2x, type = 3))
anova_3x <- partial_eta_squared(Anova(lm_3x, type = 3))
anova_4x <- partial_eta_squared(Anova(lm_4x, type = 3))

## investigate effects including an interaction with the stimulus relative to the
## main effect of the stimulus on ratings
anova_2x[grepl("stimulus_factor", rownames(anova_2x)), ]
anova_3x[grepl("stimulus_factor", rownames(anova_3x)), ]
anova_4x[grepl("stimulus_factor", rownames(anova_4x)), ]


## conduct G theory study ##
############################

gstudy_results <- gstudy(
  formula = llm_rating_re ~ (1|stimulus_factor*models*example*task),
  data = results
)

gstudy_results

dstudy(
  gstudy_results,
  colname.objects = "stimulus_factor",
  colname.scores = "llm_rating_re",
  data = results
)


## evaluate correlations of LLM ratings with human ratings ##
#############################################################

## define color theme for models
colors <- c(rep("purple", 3), "orange", rep("skyblue", 3), "#69b3a2")
stopifnot(length(colors) == length(levels(results$models)))


## correlation between LLM and human ratings ##
##-------------------------------------------##

aggregated_results <- results %>%
  ## conduct analysis across replications for each condition
  group_by(models, task, example, replication) %>%
  ## compute correlation between human and LLM ratings
  summarize(
    cor = cor.test(llm_rating, human_rating, use = "complete.obs")$estimate,
    cor_uncertainty_lower = cor_conf_bounds(llm_rating, human_rating)[1],
    cor_uncertainty_upper = cor_conf_bounds(llm_rating, human_rating)[2]
  ) %>%
  ungroup() %>%
  ## aggregate correlation statistics across simulation conditions
  group_by(models, task, example) %>%
  summarize(
    correlation = mean(cor, na.rm = TRUE),
    cor_lb = ci(cor)[1],
    cor_ub = ci(cor)[2],
    cor_uncertainty_lower = mean(cor_uncertainty_lower),
    cor_uncertainty_upper = mean(cor_uncertainty_upper)
  )

ggplot(aggregated_results, aes(x = models, y = correlation, col = models)) +
  theme_light() +
  geom_tile(
    data = aggregated_results,
    aes(
      x = models,
      y = (cor_uncertainty_lower + cor_uncertainty_upper) / 2,
      height = cor_uncertainty_upper - cor_uncertainty_lower,
      width = 0.4
    ),
    inherit.aes = FALSE,
    fill = "grey",
    alpha = 0.4
  ) +
  geom_point() +
  geom_errorbar(aes(ymin = cor_lb, ymax = cor_ub), width = 0.2) +
  ggtitle(
    "Comparing Human and LLM ratings of Syntactic Complexity",
    paste0("88 statements, 100 replications")
  ) +
  scale_y_continuous(
    name = "correlation between human and LLM ratings",
    limits = c(-0.4,1),
    breaks = seq(-0.2, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  facet_grid(task ~ example, labeller = label_both) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_abline(intercept = -1, slope = 0, linetype = "solid", alpha = 0.4) +
  geom_abline(intercept = 1, slope = 0, linetype = "solid", alpha = 0.4)

ggsave("sim_results/human_llm_correlations.pdf", dpi = 600, width = 15, height = 9)


## assess correlation by number of estimates per stimulus (replications) ##
##-----------------------------------------------------------------------##

#' function repeatedly computes mean LLM ratings across different numbers of replications of the simulation
#'
#' @param input results data frame from simulation containing LLM ratings for each stimulus for each simulation iteration
#' @param repetitions number of times this analysis will be repeated
#' @param replace indicates whether to draw with or without replacement from the
#' provided ratings during the resampling procedure
#' @returns data frame that contains mean LLM-generated ratings of syntactic complexity
#' for different orders of aggregation (number of randomly drawn simulation replications
#' of the same complexity rating in the respective condition).
#' For the sake of completeness, the data frame also contains mean human ratings of statements.
get_aggregated_ratings <- function(input, repetitions, replace) {
  ## function requires the variables task, example, models, stimulus_id, and replication
  ## and llm_rating_re to be defined in input data frame (function tailored to this analysis)
  output <- NULL
  for (i in 1:repetitions) {
    suppressMessages(
      stats <- input %>%
        group_by(task, example, models, stimulus_id) %>%
        summarize(
          human_rating = mean(human_rating), # always the same across replications of simulation because it is never varied
          llm_mean_complete =  mean(sample(llm_rating_re, size = 100, replace = TRUE), na.rm = TRUE),
          llm_mean_50 = mean(sample(llm_rating_re, size = 50, replace = TRUE)),
          llm_mean_25 = mean(sample(llm_rating_re, size = 25, replace = TRUE)),
          llm_mean_10 = mean(sample(llm_rating_re, size = 10, replace = TRUE)),
          llm_mean_05 = mean(sample(llm_rating_re, size = 5, replace = TRUE)),
          llm_mean_01 = mean(sample(llm_rating_re, size = 1, replace = TRUE))
        )
    )
    stats$repetition <- factor(i)
    output <- rbind(output, stats)
  }
  output
}

## aggregate across ratings of the same models across different numbers of simulation replications
item_order_stats <- get_aggregated_ratings(results, repetitions = 100, replace = TRUE)
item_order_stats

## compute correlation between human-generated and LLM-generated statements for
## different orders of aggregation
cor_order_stats <- item_order_stats %>%
  pivot_longer(
    cols = matches("llm_mean"),
    names_to = "draws",
    values_to = "llm_rating_re"
  ) %>%
  mutate(
    draws =
      factor(
        draws,
        levels = c("llm_mean_01", "llm_mean_05", "llm_mean_10", "llm_mean_25", "llm_mean_50", "llm_mean_complete"),
        labels = c("1", "5", "10", "25", "50", "100")
      )
  ) %>%
  ## compute correlation of human and LLM ratings
  group_by(task, example, models, draws, repetition) %>%
  summarize(
    cor = cor.test(human_rating, llm_rating_re)$estimate
  ) %>%
  ## finally, aggregate correlations for each simulation condition across repetitions
  group_by(task, example, models, draws) %>%
  summarize(
    correlation = mean(cor),
    cor_lb = ci(cor)[1],
    cor_ub = ci(cor)[2]
  )

cor_order_stats %>%
  filter(task == "101pt" & example == "no") %>%
  ggplot(aes(x = draws, y = correlation, col = models, group = 1)) +
  theme_light() +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = cor_lb, ymax = cor_ub), width = 0.2) +
  ggtitle(
    "Effect on Human-LLM-correlation when aggregating LLM ratings across replications",
    paste0("88 statements, ",
           " 100 replications, condition with no example and a 101-point response format")
  ) +
  scale_y_continuous(
    name = "correlation between human and LLM ratings",
    limits = c(-0.1,1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  facet_grid(~models) +
  scale_color_manual(values = colors) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_abline(intercept = 1, slope = 0, linetype = "solid", alpha = 0.4) +
  theme(legend.position = "none")

ggsave("sim_results/human_llm_correlations_by_aggregation.pdf", dpi = 600, width = 12, height = 9)

## compare with model means of aggregated results in previous step (approximately identical results)
aggregated_results %>%
  group_by(models) %>%
  summarize(cor = mean(correlation))

cor_order_stats %>%
  filter(draws == "1") %>%
  group_by(models) %>%
  summarize(cor = mean(correlation))



## scatter plot: human and best LLM ratings ##
##------------------------------------------##

results %>%
  group_by(task, example, models, stimulus_id) %>%
  summarize(
    human_rating = mean(human_rating),
    llm_rating_re = mean(llm_rating_re)
  ) %>%
  filter(task == "101pt" & example == "both" & models == "gemma3:27b") %>%
  ggplot(aes(x = human_rating, y = llm_rating_re)) +
  theme_light() +
  geom_point() +
  geom_smooth(method = "lm", color = "red", fill = "salmon") +
  xlab("human rating of complexity (1-5)") +
  ylab("LLM rating of complexity (0 to 100)") +
  ggtitle(
    "Highest correlation between LLM ratings and human ratings of syntactic complexity",
    "condition: gemma3:27b on a scale from 0 to 100 and with both examples aggregated across 100 bootstrapped replications"
  )

ggsave("sim_results/scatter_plot_human_best_llm.pdf", dpi = 600, width = 10,height = 7)


