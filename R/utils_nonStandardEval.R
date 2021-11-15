# A helper to substitute_q() to evaluate in all parent environments
substitute_stack <- function(expr) {
  expr <- substitute_q(expr)

  # Substitute in all envs in the call stack
  envs <- rev(sys.frames())
  for (e in envs) {
    expr <- substitute_q(expr, e)
  }

  # sys.frames() does not include globalenv() and
  # substitute() doesn't "substitute" there
  e <- as.list(globalenv())
  substitute_q(expr, e)
}

#' A version of substitute that evaluates its first argument
#' This version of substitute is needed because substitute does not evaluate it's first argument, and it's often useful to be able to modify a quoted call.
#' @param call a quoted call
#' @param env an environment
#' @export
substitute_q <- function(call, env = parent.frame()) {
  force(env) #prevent lazy evaluation of env
  # as.list(env) allows for the global environment,
  # which will not work with substitute() unless as a list
  eval(substitute(substitute(x, as.list(env)), list(x = call)))
}

#' Automatically re-evaluates a bquote call in a local (or specified) environment
#'
#' Also checks if the argument is already a call  to bquote. If not, then wraps the argument in bquote.
#' @param expr Either a bquote call, or an argument to be passed to bquote.
#' @param env an environment
#' @export
bquote_remask <- function(expr, env = parent.frame()) {
  force(env)

  expr <- substitute(expr)
  expr <- eval(substitute(substitute(x, env), list(x = expr)))

  if (!(is.call(expr) && expr[[1]] == as.name("bquote")))
    expr <- substitute(bquote(x), list(x = expr))

  eval(expr, envir = env)
}
