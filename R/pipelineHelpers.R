
#' Function to test executeNode
#'
#' Simple function to test executeNode.  Add 3 to a number
#'
#' @param n A number
#'
#' @export
add3 <- function(n = 5) {
  return(n + 3)
}
comment(add3) <- '[{"n": "numeric"}]'

#' Check that function is in the function list
#'
#' Ensure that the node is included in the evaluation list
#'
#' @param fn Name of the function to run.
#'
checkfnList <- function(fn) {
  return(fn %in% pkg.env$allNodes)
}

#' Add package names to node inclusion list
#'
#' Add a series of package names to the node inclusion list.
#' This defines the functions that can be executed within the workflow.
#' The function needs to be run before any nodes execute.  It creates two pkg.env
#' variables: \code{pkg.env$packageFunctions} which contains a list of nodes by package
#' and \code{pkg.env$allNodes} which contains all the allowed nodes
#'
#' @export
includePackages <- function(pkgs = NULL) {
  out <- setNames(lapply(pkgs, function(x) try(ls(paste0('package:', x)))), pkgs)
  out <- out[which(!sapply(out, function(x) inherits(x, 'try-error')))]
  pkg.env$packageFunctions <- out
  pkg.env$allNodes <- unlist(out, use.names = FALSE)
  return()
}

#' Process node details
#'
#' Process the comments atttached to the nodes that have been added through includePackages
#' @return list of parameters associated with each node
#'
#' @importFrom rjson fromJSON
#' @export
nodeParameters <- function() {

  l.nodes <- list()
  for (p in seq(pkg.env$packageFunctions)) {
    l.parents <- list()
    for (n in pkg.env$packageFunctions[[p]]) {
      if (!is.null(comment(eval(parse(text = n))))) {
        l.parents[[n]] <- fromJSON(comment(eval(parse(text = n))))
      }
    }
    if (length(l.parents) > 0) l.nodes[[names(pkg.env$packageFunctions)[[p]]]] <- l.parents
  }
  return(l.nodes)
}
# nodeParameters <- function() {
#   l.nodes <- list()
#   for (n in pkg.env$allNodes) {
#     if (!is.null(comment(eval(parse(text = n))))) l.nodes[[n]] <- comment(eval(parse(text = n)))
#   }
#   return(lapply(l.nodes, fromJSON))
# }

#' Execute a node
#'
#' This is a function used to execute a node in the Cobralingus engine.
#' The code outputs a list containing two elements.  The first reports on the
#' success of running the node (\code{error} or \code{success}) and the second
#' contains either an error report or the output of the node execution.
#'
#' @param fn Name of the function to run.
#' @param params List of parameters to pass to the function.
#' @return List containing output from the function.  First element (\code{result})
#' will be \code{error} or \code{success} and second element (\code{output}) is
#' an error report or the output of the node execution.
#'
#' @examples
#' \dontrun{
#' executeNode('scrambleNouns', list(text, 50))
#' }
#'
#' @export
executeNode <- function(fn = NULL, params = list()) {
  if (is.null(fn)) return(list(result = 'error', output = 'no function specified'))
  if (!is.list(params)) return(list(result = 'error', output = 'parameters should be specified as a list'))
  if (!checkfnList(fn)) return(list(result = 'error', output = 'function not in list'))
  nodeOut <- tryCatch({
    do.call(what = fn, args = params)
  }, error = function(e) {
    list(success = FALSE, output = 'error in running node') ## error caught whilst running node
  })
  if (!nodeOut$success) {
    return(list(result = 'error', output = nodeOut$output))  ## error reported by node
  } else {
    return(list(result = 'success', output = nodeOut$output))  ## Success !!!
  }
}
