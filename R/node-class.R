#' Node class
#'
#' Simple S3 class to hold node information
#'
#' @param id Unique node identifier
#' @param type Type of node (used to define internal parameters)
#' @param name Name of node
#' @param parameters List of parameters and their arguments (see details)
#' @param output Output of calculation for display or input into subsequent node
#'
#' @details
#' This is a simple S3 class to hold parameters relating to a single node.
#' The parameters argument contains information required to run the function associated with
#' the node.  It specifies the name and type of variable along with data required to generate
#' a user interface using Shiny.  For example, a numeric input may be specified as
#' list(name = 'n', type = 'numeric', min = 1, max = 100, step = 1, value = 50)
#'
#' @export
Node <- function(id = NA_character_,
                 type = NA_character_,
                 name = NA_character_,
                 parameters = list(),
                 status = 'queued',
                 output = NULL)
{

  me <- list(
    id = id,
    type = type,
    name = name,
    parameters = parameters,
    status = status,
    output = output
  )

  ## Set the name for the class
  class(me) <- append(class(me),"Node")
  return(me)
}

