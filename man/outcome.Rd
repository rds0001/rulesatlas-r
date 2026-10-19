\name{outcome}
\alias{outcome}
\title{Determine the Current Rule Outcome}
\description{Determine whether the state is ongoing, terminal or requires further information
or official adjudication under the selected ruleset.}
\usage{
outcome(state)
}
\arguments{
  \item{state}{A named list containing game_id, ruleset_id, ruleset_version, variant, players
and data, with optional metadata. Use \link{new_state} to create the envelope. See
\link{ruleatlas-formats}.}
}
\value{A named list containing status, result_kind, winners, losers, placements, reason,
evidence_required and rule_id. winners and losers are lists of participant IDs, not numeric
player indices. placements and evidence_required are profile-specific lists. rule_id can be
NULL when no result rule applies.}
\details{An ongoing result is not a prediction. Ties, rankings, forfeits and adjudicated
results depend on the declared variant. Invalid states raise an error instead of producing a
guessed outcome. The custom DSL rejects states in which multiple outcome predicates hold
simultaneously.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")),
                   ruleset_version = "1.0.0")
for (cell in c(0L, 3L, 1L, 4L, 2L)) {
  state <- apply_action(state, list(type = "place",
                                    params = list(cell = cell)))
}
result <- outcome(state)
result$status
result$winners
length(legal_actions(state)$actions)
}
