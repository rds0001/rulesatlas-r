\name{get_capabilities}
\alias{get_capabilities}
\title{Inspect Rule-Checking Capabilities}
\description{Describe available operations and the scope of legality checks for a selected
ruleset.}
\usage{
get_capabilities(game_id, ruleset_version = NULL, ruleset_id = NULL)
}
\arguments{
  \item{game_id}{One case-sensitive character string identifying a retained built-in profile or
a registered custom game in the user. namespace. See \link{ruleatlas-profiles}.}
  \item{ruleset_version}{NULL or one character string selecting an exact version, for example
"1.0.0". NULL succeeds only when selection is unambiguous. Pin versions when persisting
states.}
  \item{ruleset_id}{NULL or one character string selecting a ruleset belonging to game_id.
Unknown selections fail instead of falling back to another ruleset.}
}
\value{A named list containing new_state, validate_state, legal_actions, apply_action, outcome
and validation_scope. Custom rulepacks also report max_enumeration = 512. legal_actions
describes the capability; it is not a list of currently allowed actions.}
\details{Validation may be structural, local_rule_consistency or history_consistency. None of
these implies a solver or a general proof of historical reachability. Read the profile
limitations and inspect the current state with legal_actions().}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
cap <- get_capabilities("G105")
cap$legal_actions
cap$validation_scope
}
