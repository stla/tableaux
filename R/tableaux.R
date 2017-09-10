.asciiTableau <- function(tableau){
  .C("asciiTableauR", tableau=tableau, l=length(tableau), result="")$result
}

.dualTableau <- function(tableau){
  .C("dualTableauR", tableau=tableau, l=length(tableau), result=list(0L))$result[[1L]]
}

.isPartition <- function(partition){
  .C("isPartitionR", partition=partition, l=length(partition), result=0L)$result
}

.countStandardYoungTableaux <- function(shape){
  .C("countStandardYoungTableauxR", partition=shape, l=length(shape),
     result=0L)$result
}

.standardYoungTableaux <- function(shape){
  .C("standardYoungTableauxR", partition=shape, l=length(shape),
     result=list(0L))$result[[1L]]
}

.hookLengths <- function(shape){
  .C("hookLengthsR", partition=shape, l=length(shape), result=list(0L))$result[[1L]]
}

#' ASCII diagram of a tableau
#'
#' Returns the ASCII diagram of a tableau.
#'
#' @param tableau a tableau
#'
#' @return A character string.
#' @export
#' @useDynLib tableaux
#'
#' @examples
#' tableau <- list(c(1,3,4,6,7), c(2,5,10,8), 9)
#' cat(asciiTableau(tableau))
asciiTableau <- function(tableau){
  .asciiTableau(lapply(tableau, as.integer))
}

#' Dual tableau
#'
#' Returns the dual tableau (mirror image to the main diagonal).
#'
#' @param tableau a tableau
#'
#' @return A tableau.
#' @export
#'
#' @examples
#' tableau <- list(c(1,3,4,6,7), c(2,5,10,8), 9)
#' cat(asciiTableau(tableau))
#' cat(asciiTableau(dualTableau(tableau)))
dualTableau <- function(tableau){
  .dualTableau(lapply(tableau, as.integer))
}

#' Number of standard Young tableaux
#'
#' Count the number of standard Young tableaux of a given shape.
#'
#' @param shape partition of integers
#'
#' @return An integer.
#' @export
#' @importFrom purrr map_int
#'
#' @examples
#' countStandardYoungTableaux(c(5,3,1))
countStandardYoungTableaux <- function(shape){
  shape <- purrr::map_int(shape, as.integer)
  if(.isPartition(shape)==0L){
    warning("`shape` is not a partition")
  }
  .countStandardYoungTableaux(shape)
}

#' Standard Young tableaux
#'
#' Returns the list of standard Young tableaux of a given shape.
#'
#' @param shape partition of integers
#'
#' @return A list of tableaux
#' @export
#'
#' @examples
#' shape <- c(3,2)
#' standardYoungTableaux(shape)
standardYoungTableaux <- function(shape){
  shape <- purrr::map_int(shape, as.integer)
  if(.isPartition(shape)==0L){
    warning("`shape` is not a partition")
  }
  .standardYoungTableaux(shape)
}

#' Hook lengths
#'
#' Hook lengths of a partition.
#'
#' @param shape a partition
#'
#' @return The tableau of hook lengths.
#' @export
#' @importFrom purrr map_int
#'
#' @examples
#' hookLengths(c(5,2,1))
hookLengths <- function(shape){
  shape <- purrr::map_int(shape, as.integer)
  if(.isPartition(shape)==0L){
    warning("`shape` is not a partition")
  }
  .hookLengths(shape)
}
