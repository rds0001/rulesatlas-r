\name{validate_state}
\alias{validate_state}
\title{Check a Rule State}
\description{Check the state envelope, typed fields and the selected profile's consistency and
outcome constraints without changing the input.}
\usage{
validate_state(state)
}
\arguments{
  \item{state}{A named list containing game_id, ruleset_id, ruleset_version, variant, players
and data, with optional metadata. Use \link{new_state} to create the envelope. See
\link{ruleatlas-formats}.}
}
\value{A named list with valid (logical), scope (character) and issues (a list). Each issue
contains code, path, message and severity. A successful result has valid = TRUE and an empty
issues list. A failed result has valid = FALSE and a diagnostic issue.}
\details{Errors encountered during state validation are converted to validation results. This
function does not establish facts omitted from the state, infer hidden observations or
reconstruct an arbitrary game history. Other operations require a valid state and may raise an
error condition.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")),
                   ruleset_version = "1.0.0")
validate_state(state)$valid
bad <- state
bad$data$board[[1]] <- 99L
validate_state(bad)$valid
validate_state(state)$valid
}
