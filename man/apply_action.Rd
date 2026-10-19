\name{apply_action}
\alias{apply_action}
\title{Apply One Validated Rule Transition}
\description{Validate and apply one submitted move or observed event, including its rule
consequences and postconditions.}
\usage{
apply_action(state, action)
}
\arguments{
  \item{state}{A named list containing game_id, ruleset_id, ruleset_version, variant, players
and data, with optional metadata. Use \link{new_state} to create the envelope. See
\link{ruleatlas-formats}.}
  \item{action}{A named list with type (one character string) and params (a named list).
Optional actor is a participant ID; optional metadata is an object. Use a named empty list for
parameterless actions. Profile-specific parameters are described by \link{legal_actions}.}
}
\value{A new validated state envelope of the same ruleset. The input state remains unchanged.
Invalid actions raise a ruleatlas_error condition; no partial transition is committed.}
\details{Terminal states reject further actions. Numeric indices in params are zero-based. An
optional actor must identify a participant; this is not an authentication or
private-information boundary. In the custom DSL, effect expressions read the old state and
commit atomically after validation.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")),
                   ruleset_version = "1.0.0")
action <- list(type = "place", params = list(cell = 0L))
after <- apply_action(state, action)
state$data$board[[1]]
after$data$board[[1]]
tryCatch(apply_action(after, action),
         ruleatlas_error = function(e) e$code)
}
