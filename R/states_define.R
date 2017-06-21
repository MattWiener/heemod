#' Define a Markov Model State
#' 
#' Define the values characterising a Markov Model state for
#' 1 cycle.
#' 
#' As with [define_parameters()], state values are
#' defined sequentially. Later state definition can thus
#' only refer to values defined earlier.
#' 
#' For the `modify` function, existing values are 
#' modified, no new values can be added. Values order 
#' matters since only values defined earlier can be 
#' referenced in later expressions.
#' 
#' @param ... Name-value pairs of expressions defining state
#'   values.
#' @param .OBJECT An object of class `state`.
#' @param .dots Used to work around non-standard evaluation.
#'   
#' @return An object of class `state` (actually a named
#'   list of `lazy` expressions).
#' @export
#' 
#' @example inst/examples/example_define_state.R
#'   
define_state <- function(..., count_types) {
  .dots <- lazyeval::lazy_dots(...)
  define_state_(.dots, count_types)
}

#' @export
#' @rdname define_state
define_state_ <- function(.dots, count_types) {
  check_names(names(.dots))
  if(missing(count_types)){
    count_types <- rep("", length(.dots))
  }
  if(is.null(names(count_types))){
    names(count_types) <- names(.dots)
  }
  acceptable_count_types <- c("beginning", "end", "life-table", "")
  bad_count_types <- setdiff(count_types, acceptable_count_types)
  if(length(bad_count_types)){
    stop(sprintf("bad count type entr%s %s;\n all count types should be one of %s.",
                 plur_y(length(bad_count_types)),
                 paste(bad_count_types, collapse = ", "),
                 paste(acceptable_count_types, collapse = ", ")
                 )
    )
  }
  structure(.dots,
            class = c("state", class(.dots)),
            value_count_types = count_types[match(names(count_types),
                                                  names(.dots))])
}

#' @export
#' @rdname define_state
modify.state <- function(.OBJECT, ...) {
  .dots <- lazyeval::lazy_dots(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.state <- function(.OBJECT, .dots) {
  check_names(names(.dots))
  # !mod!
  # message d'erreur informatif quand valeurs pas dans
  # bon ordre
  
  if (! all(names(.dots) %in% names(.OBJECT))) {
    stop(sprintf(
      "The following state values are not defined: %s.",
      names(.dots)[names(.dots) %in% names(.OBJECT)]
    ))
  }
  
  utils::modifyList(.OBJECT, .dots)
}

#' Define Markov Model State List
#' 
#' Define the states of a Markov model by combining 
#' `state` objects.
#' 
#' State names have to correspond to those specified through
#' [define_transition()].
#' 
#' All states should have the same value names.
#' 
#' The `modify` function can modify existing states or 
#' add new ones.
#' 
#' @param ... Name-value pairs of expressions defining model
#'   states.
#' @param .OBJECT An `uneval_states` object.
#' @param .dots List of states, only used by 
#'   `define_state_list_` to avoid using `...`.
#'   
#' @return An object of class `uneval_state_list` (a 
#'   list of `state` objects).
#'   
#' @examples
#' \dontrun{
#' s1 <- define_state(cost = 1, util = 1)
#' s2 <- define_state(cost = 3, util = .4)
#' 
#' states_mod <- define_state_list(
#'   healthy = s1,
#'   sick = s2
#' )
#' 
#' states_mod
#' 
#' s1_bis <- define_state(cost = 0, util = 1)
#' s3 <- define_state(cost = 10, util = .1)
#' 
#' modify(
#'   states_mod,
#'   healthy = s1_bis,
#'   sicker = s3
#' )
#' }
#'   
#' @keywords internal
define_state_list <- function(...) {
  .dots <- list(...)
  
  define_state_list_(.dots)
}

#' @rdname define_state_list
define_state_list_ <- function(.dots) {
  
  state_names <- names(.dots)
  
  if (is.null(state_names)) {
    message("No named state -> generating names.")
    state_names <- LETTERS[seq_along(.dots)]
    names(.dots) <- state_names
  }
  
  if (any(state_names == "")) {
    warning("Not all states are named -> generating names.")
    state_names <- LETTERS[seq_along(.dots)]
    names(.dots) <- state_names
  }
  
  if (any(duplicated(names(.dots)))) {
    stop("Some state names are duplicated.")
  }
  
  if (! all(unlist(lapply(.dots,
                          function(x) "state" %in% class(x))))) {
    
    .x <- names(.dots)[! unlist(lapply(
      .dots,
      function(x) "state" %in% class(x)))]
    
    stop(sprintf(
      "Incorrect state object%s: %s",
      plur(length(.x)),
      paste(.x, collapse = ", ")
    ))
  }
  check_states(.dots)
  
  res <-   structure(
      .dots,
      class = c("uneval_state_list", class(.dots))
    )
  check_value_count_types_consistent(res)
  res
}

#' @rdname define_state_list
modify.uneval_state_list <- function(.OBJECT, ...) {
  .dots <- list(...)
  
  modify_(.OBJECT = .OBJECT, .dots = .dots)
}

modify_.uneval_state_list <- function(.OBJECT, .dots) {
  res <- utils::modifyList(.OBJECT, .dots)
  check_states(res)
  
  res
}

#' Check Model States for Consistency
#' 
#' For internal use.
#' 
#' All states should have the same value names.
#' 
#' @param x An object of class `uneval_states`.
#'   
#' @return `NULL`
#'   
#' @keywords internal
check_states <- function(x){
  if (! list_all_same(lapply(x, length))) {
    stop("Number of state values differ between states.")
  }
  
  if (! list_all_same(lapply(x, function(y) sort(names(y))))) {
    stop("State value names differ between states.")
  }
  NULL
}

#' Return Number of State
#' 
#' For internal use.
#' 
#' Work with both `uneval_states` and
#' `eval_states`.
#' 
#' @param x An object containing states.
#'   
#' @return An integer: number of states.
#'   
#' @keywords internal
get_state_number <- function(x){
  # !mod!
  # rename get_state_count
  length(get_state_names(x))
}

#' Return Names of State Values
#' 
#' @param x An object containing states.
#' @param ... Additional arguments passed to methods.
#'   
#' @return A character vector of state value names.
#'   
#' @keywords internal
get_state_value_names <- function(x){
  UseMethod("get_state_value_names")
}

get_state_value_names.uneval_state_list <- function(x) {
  names(x[[1]])
}

get_state_value_names.state <- function(x){
  names(x)
}

#' Get State Names
#' 
#' Retrieve state names from an object containing states.
#' 
#' @param x An object containing states.
#' @param ... Additional arguments passed to methods.
#'   
#' @return A character vector of state names.
#'   
#' @keywords internal
get_state_names <- function(x, ...){
  UseMethod("get_state_names")
}

get_state_names.default <- function(x, ...){
  names(x)
}

#' Get value count types
#'
#' @param x A `state` or `lazy_dot` or `uneval_state_list` object.
#'
#' @return a named vector of count types
#' @details The value count types will be passed to
#'   [correct_counts()] as the `method` argument.
#' @export
#'
#' @examples
get_state_value_count_types <- function(x){
  UseMethod("get_state_value_count_types")
  }
get_state_value_count_types.uneval_state_list <-
  function(x){
    check_value_count_types_consistent(x)
    get_state_value_count_types(x[[1]])
  }
get_state_value_count_types.state <-
  function(x){
    attributes(x)[["value_count_types"]]
  }
get_state_value_count_types.lazy_dots <-
  get_state_value_count_types.state

check_value_count_types_consistent <- function(x){
  if(!inherits(x, "uneval_state_list"))
    stop("function check_value_count_types_consistent ", 
         "requires an uneval_state_list")
  types_by_state <- 
    lapply(x, function(y)get_state_value_count_types(y))
  
  type_matrix <- 
    matrix(unlist(types_by_state),
           ncol = length(x),
           byrow = TRUE
    )
  num_types_per_val <- 
    apply(type_matrix, 2, function(x){length(unique(x))})
  if(any(num_types_per_val > 1))
    stop("state value types do not agree")
  else 
    NULL
  }