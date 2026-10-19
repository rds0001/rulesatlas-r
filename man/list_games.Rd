\name{list_games}
\alias{list_games}
\title{List Available Rule Profiles}
\description{List installed accepted profiles and session-local custom rulepacks. This does not
run a game or search possible future states.}
\usage{
list_games(include_candidates = FALSE)
}
\arguments{
  \item{include_candidates}{One logical value. If TRUE, also include catalog entries without
accepted implementations. In this distribution all 212 catalog entries are implemented; removed
profiles are not candidates.}
}
\value{A list of named lists. Built-in entries contain game_id, profile_id, name,
implementation_status, rulesets, capabilities, coverage_scope, known_limitations, provenance
and evidence. Custom summaries contain game_id, name, ruleset_id, ruleset_version and
implementation_status. Candidate records, when present, identify their unsupported status.}
\details{Custom versions may produce multiple records for one game ID. Use explicit ruleset
selection for reproducible calls. Profile acceptance describes the declared implementation
scope, not federation certification or legal clearance.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
games <- list_games()
length(games)
head(vapply(games, function(x) x$game_id, character(1)))
}
