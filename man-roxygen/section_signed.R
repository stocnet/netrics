#' @section Signed networks:
#'   These algorithms read a tie's weight as the strength of a pull into the
#'   same community, and a negative tie is hostility rather than such a pull.
#'   Where the network is signed, they therefore consider only the positive
#'   ties, and say so. Only [node_in_spinglass()] reads a sign as a sign.
#'   Use [manynet::to_unsigned()] first to control this yourself.
