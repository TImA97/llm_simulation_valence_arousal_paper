## the packages 'ellmer' and 'utils' should be loaded for this script to run.

#' wrapper function used for running the simulation
#'
#' @param conditions data frame containing simulation conditions in a row-wise order
#' @param iterations number of simulation iterations per condition
#' @param material list object containing additional data used during the simulation
#' @param construct construct to be assessed in simulation (can be either 'valence' or 'arousal').
#' @param seeds provided seeds for each simulation condition in conditions
#' @param fill_cols indicate which columns will be used to fill the expression
#' @returns simulation results for all simulation conditions with the specified
#' number of repetitions
do_llm_simulation <- function(conditions, iterations, material, construct, seeds = NULL, fill_cols = NULL, temperature = NULL) {

  ## generate seeds if none are provided
  n_seeds <- nrow(conditions) * iterations * nrow(material$expressions)
  if (is.null(seeds)) {
    ## each llm call gets its own seed
    seeds <- array(
      sample(1:1e+09, size = n_seeds, replace = FALSE),
      dim = c(
        nrow(material$expressions),
        iterations,
        nrow(conditions)
      )
    )
  } else {

    if (length(seeds) != n_seeds) {
      stop("the number of provided seeds does not correspond to the number of simulation conditions")
    }

    seeds <- array(
      seeds,
      dim = c(
        nrow(material$expressions),
        iterations,
        nrow(conditions)
      )
    )
  }

  ## save seeds in separate folder
  save(seeds, file = paste0("sim_results/tmp/simulation_seeds_", construct, ".RData"))

  ## prepare output data frame including all relevant conditions
  results <- NULL

  ## number of rows each simulation iteration produces
  n_ratings <- nrow(material$expressions)

  ## loop across simulation conditions and obtain ratings
  rownames(conditions) <- paste0(1:nrow(conditions), " / ", nrow(conditions))
  for (i in 1:nrow(conditions)) {
    cat("\n")

    ## extract relevant condition
    condition <- conditions[i, ]

    ## initialize progress bar
    print(condition)
    cat("\n")

    ## in first iteration, initialize the progress bar
    if (i == 1) {
      progress_bar <- txtProgressBar(min = 0, max = iterations, initial = 0, style = 3, width = 50)
    }
    setTxtProgressBar(progress_bar, 0)

    ## conduct a specified number of iterations per simulation condition
    for (j in 1:iterations) {
      condition$iteration <- j ## add iteration information to data frame

      ## obtain simulation results for each loop iteration
      ratings <- get_llm_rating(condition,
                                fixed_objects = material,
                                construct = construct,
                                seeds = seeds[, j, i] |> as.numeric(),
                                fill_cols = fill_cols,
                                temperature = temperature
      )

      ## combine condition parameters with simulation results
      results <- rbind(
        results,
        cbind(
          repeat_data_frame(condition, n_ratings),
          ratings
        )
      )
      setTxtProgressBar(progress_bar, j)
    }
    cat("\n\n")

    ## save already computed results
    rownames(results) <- 1:nrow(results)
    save(results, file = paste0("sim_results/tmp/sim_results_tmp_", construct, ".RData"))
    gc()
  }

  rownames(results) <- 1:nrow(results)
  results
}

#' function that takes a data frame and repeats it using the rbind function
#'
#' @param df data frame to be repeated
#' @param n number of repetitions of df
#' @returns stacked data frame with n repetitions of the input data frame df
repeat_data_frame <- function(df, n) {
  new_df <- NULL
  for (i in 1:n) {
    new_df <- rbind(new_df, df)
  }
  new_df
}

#' function that executes all relevant code for each simulation condition and
#' obtains LLM ratings of valence or arousal for the provided statements
#'
#' @param condition row of data frame that contains the parameters of the simulation
#' @param fixed_objects list object that contains data being used repeatedly during
#' the course of this simulation
#' @returns data frame containing LLM ratings per stimulus (one per row)
#' for one iteration of the condition
get_llm_rating <- function(condition, fixed_objects, construct, seeds, fill_cols = NULL, temperature) {
  ## extract model parameters for creating the complete prompt
  model <- condition$models |> as.character()
  system_prompt <- fixed_objects$system_prompts[condition$system_prompt]
  example <- fixed_objects$example[condition$example]

  expressions <- fixed_objects$expressions

  responses <- data.frame(
    stimulus_id = expressions$stimulus_id,
    rating = NA,
    rating_with_invalid = NA,
    seed = seeds
  )

  for (i in 1:nrow(expressions)) {
    ## create chat using ollama to rate the statements
    rater <- chat_ollama(
      system_prompt = system_prompt,
      model = model,
      params = params(keep_alive = 0, seed = responses$seed[i], temperature = temperature)
    )

    ## extract current statement
    statement <- expressions[i, "statement"]

    ## create prompt and feed it into model
    if (is.null(fill_cols)) {
      fill_expr <- NULL
    } else {
      fill_expr <- condition[, fill_cols]
    }
    task_prompt <- create_prompt(example, statement, construct, fill_expr = fill_expr)

    ## provide structure for response
    rating <- tryCatch({
      output <- rater$chat_structured(
        task_prompt,
        type = type_object(rating = type_integer("Rating"))
      )
      output$rating
    }, error = function(e) {
      message("Error in iteration ", condition$iteration, " of expression nr. ", i, " due to error: ", e$message)
      NA
    })

    responses[i, "rating_with_invalid"] <- rating

    ## clean environment
    rm(rater)
    rm(rating)
    gc()
  }

  ## remove responses outside the allowed range
  format_nr <- condition$format |> as.numeric()
  valid_ind <- check_if_valid(responses$rating_with_invalid, format_nr)
  responses$rating <- responses$rating_with_invalid
  responses$rating[!valid_ind] <- NA

  responses
}

#' helper function that helps to create the prompt fed to the LLMs
#'
#' @param example example statement provided for context
#' @param expression statement to be rated by LLM
#' @param fill_expr filler expression to be inserted provided as a data frame row
#' @returns complete prompt as one string of characters
create_prompt <- function(example, expression, construct, fill_expr = NULL) {
  if (example == "") example <- NULL

  if (construct == "valence") {
    prompt <- paste0(c(
      "Could you please rate how reading the following multi-word expression makes a person feel.",
      "Use a scale from {scale_min} to {scale_max}, where {scale_min} means very negative, bad and {scale_max} means very positive, good.",
      example,
      paste0("The expression is: ", expression, "."),
      "Only answer a number from {scale_min} to {scale_max}. Please limit your answer to numbers."
    ), collapse = " ") |> trimws()

    if (!is.null(fill_expr)) { # early return if no expression needs to be filled
      prompt <- fill_expression(prompt, fill_expr)
    }
  } else if (construct == "arousal") {
    prompt <- paste0(c(
      "Could you please rate how reading the following multi-word expression makes a person feel.",
      "Use a scale from {scale_min} to {scale_max}, where {scale_min} means very calm, relaxed and {scale_max} means very aroused, energized.",
      example,
      paste0("The expression is: ", expression, "."),
      "Only answer a number from {scale_min} to {scale_max}. Please limit your answer to numbers."
    ), collapse = " ") |> trimws()

    ## insert expressions into prompt (if required)
    if (!is.null(fill_expr)) {
      prompt <- fill_expression(prompt, fill_expr)
    }
  }

  prompt
}

#' function for replacing placeholders in expressions
#'
#' @param expression complete phrase including expressions to be filled
#' @param row_data data frame row that contains columns corresponding to the
#' the expressions to be filled
#' @returns returns the complete (filled) expression
fill_expression <- function(expression, row_data) {
  result <- expression

  # find all placeholders in the expression
  placeholders <- regmatches(expression, gregexpr("\\{([^}]+)\\}", expression))[[1]]

  # replace each placeholder with corresponding value
  for (placeholder in placeholders) {
    # extract the variable name (remove curly braces)
    var_name <- gsub("[{}]", "", placeholder)

    # get the value from the row
    if (var_name %in% names(row_data)) {
      value <- row_data[[var_name]]
      if (!is.na(value)) {
        result <- gsub(placeholder, value, result, fixed = TRUE)
      }
    }
  }

  result
}

#' function that checks for validity of LLM responses
#' It is assumed that the numerical code 1 denotes a scale from 0 to 100, and
#' that a 2 denotes a 9-point Likert scale
#'
#' @param x values to be checked for validity
#' @param task numerical code of task (1 = 0 to 100, 2 = 1 to 9)
#' @returns returns a logical vector indicating whether a provided value is valid
#' with TRUE indicating a valid value and FALSE indicating an invalid value
check_if_valid <- function(x, task) {
  if (task == 1) {
    valid <- 0:100
  } else if (task == 2) {
    valid <- 1:9
  }

  x %in% valid # return whether a response was valid or not
}
