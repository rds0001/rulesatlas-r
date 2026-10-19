\name{legal_actions}
\alias{legal_actions}
\title{Inspect Currently Permitted Actions or Events}
\description{Return finite legal actions where supported, or typed event constraints for the
current phase.}
\usage{
legal_actions(state)
}
\arguments{
  \item{state}{A named list containing game_id, ruleset_id, ruleset_version, variant, players
and data, with optional metadata. Use \link{new_state} to create the envelope. See
\link{ruleatlas-formats}.}
}
\value{A named list with mode, actions, action_types, constraints, completeness and reason. In
enumerated mode, actions contains executable type/params objects. In event_types mode,
action_types contains parameter schemas and preconditions; it is not an exhaustive list of
valid parameter values. Terminal states return no actions.}
\details{For custom rules, finite parameter combinations are enumerated only up to 512
combinations; larger or unbounded domains expose event types. A listed event type can still
reject a particular parameter combination. External adjudication and missing information are
explicit in profile capabilities and outcomes. Always submit an action through apply_action().}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")),
                   ruleset_version = "1.0.0")
legal <- legal_actions(state)
legal$mode
length(legal$actions)
legal$actions[[1]]
}
