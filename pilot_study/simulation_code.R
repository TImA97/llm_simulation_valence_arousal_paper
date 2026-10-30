## the packages 'ellmer' and 'utils' should be loaded for this script to run.

#' wrapper function used for running the simulation
#'
#' @param conditions data frame containing simulation conditions in a row-wise order
#' @param replications number of simulation replications/iterations per condition
#' @param material list object containing additional data used during the simulation
#' (depends on goal and implementation of function; here, statements to be rated regarding complexity)
#' @param seeds provided seeds for each simulation condition in conditions
#' @returns simulation results for all simulation conditions with the specified
#' number of repetitions
do_llm_simulation <- function(conditions, replications, material, seeds = NULL) {

  ## generate seeds if none are provided
  n_seeds <- nrow(conditions) * replications * nrow(material$human_ratings)
  if (is.null(seeds)) {
    ## each llm call gets its own seed
    seeds <- array(
      sample(1:2147483647, size = n_seeds, replace = FALSE),
      dim = c(
        nrow(material$human_ratings),
        replications,
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
        nrow(material$human_ratings),
        replications,
        nrow(conditions)
      )
    )
  }

  ## prepare output data frame including all relevant conditions
  results <- NULL

  ## number of rows each simulation iteration produces
  n_ratings <- nrow(material$human_ratings)

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
      progress_bar <- txtProgressBar(min = 0, max = replications, initial = 0, style = 3, width = 50)
    }
    setTxtProgressBar(progress_bar, 0)

    ## conduct a specified number of replications per simulation condition
    for (j in 1:replications) {
      condition$replication <- j ## add replication info to data frame

      ## obtain simulation results for each loop iteration
      ratings <- get_llm_rating(condition,
                                fixed_objects = material,
                                seed = seeds[, j, i] |> as.numeric()
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
#' obtains LLM ratings of verbal complexity for the provided statements
#'
#' @param condition row of data frame that contains the parameters of the simulation
#' @param fixed_objects list object that contains data being used repeatedly during
#' the course of this simulation
#' @returns data frame containing LLM ratings per stimulus (one per row)
#' for one replication of the condition
get_llm_rating <- function(condition, fixed_objects, seeds) {
  ## extract model parameters for creating the complete prompt
  model <- condition$models |> as.character()
  system_prompt <- fixed_objects$system_prompts[condition$system_prompt]
  task <- fixed_objects$tasks[condition$task]
  example <- fixed_objects$examples[condition$example]

  human_ratings <- fixed_objects$human_ratings

  responses <- data.frame(
    stimulus_id = human_ratings$stimulus_id,
    rating = NA,
    seed = seeds
  )

  for (i in 1:nrow(human_ratings)) {
    ## create chat using ollama to rate the statements
    rater <- chat_ollama(
      system_prompt = system_prompt,
      model = model,
      params = params(keep_alive = 0, seed = responses$seed[i])
    )

    ## extract current statement
    statement <- human_ratings[i, "statement"]

    ## create prompt and feed it into model
    task_prompt <- create_prompt(task, example, statement)

    ## provide structure for response
    output <- rater$chat_structured(
      task_prompt,
      type = type_object(
        rating = type_integer("Bewertung")
      )
    )

    responses[i, "rating"] <- output$rating
  }

  ## remove responses outside the allowed range
  task_number <- condition$task |> as.numeric()
  valid_ind <- check_if_valid(responses$rating, task_number)
  responses$rating[!valid_ind] <- NA

  responses
}

#' helper function that helps to create the prompt fed to the LLMs
#'
#' @param task rating task that the LLM has to do
#' @param example example statement provided for context
#' @param expression statement to be rated by LLM
#' @param sep character separator between different parts of the prompt (empty by default)
#' @returns complete prompt as one string of characters
create_prompt <- function(task, example, expression, sep = "") {
  expression <- paste0(" Bewerten Sie die folgende Aussage: ", expression)
  paste0(task, sep, example, sep, expression) |> trimws()
}


#' function that checks for validity of LLM responses
#' It is assumed that the numerical code 1 denotes a scale from 0 to 100, and
#' that a 2 denotes a 7-point Likert scale
#'
#' @param x values to be checked for validity
#' @param task numerical code of task (1 = 0 to 100, 2 = 1 to 7)
#' @returns returns a logical vector indicating whether a provided value is valid
#' with TRUE indicating a valid value and FALSE indicating an invalid value
check_if_valid <- function(x, task) {
  if (task == 1) {
    valid <- 0:100
  } else if (task == 2) {
    valid <- 1:7
  }

  x %in% valid # return whether a response was valid or not
}
