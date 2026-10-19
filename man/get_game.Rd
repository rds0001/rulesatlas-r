\name{get_game}
\alias{get_game}
\title{Inspect a Profile and Its Declared Variant}
\description{Retrieve the supported variant, capabilities, scope and limitations before
supplying a state or action.}
\usage{
get_game(game_id, ruleset_version = NULL, variant = NULL, ruleset_id = NULL)
}
\arguments{
  \item{game_id}{One case-sensitive character string identifying a retained built-in profile or
a registered custom game in the user. namespace. See \link{ruleatlas-profiles}.}
  \item{ruleset_version}{NULL or one character string selecting an exact version, for example
"1.0.0". NULL succeeds only when selection is unambiguous. Pin versions when persisting
states.}
  \item{variant}{NULL or an empty named list. Nonempty variant overrides are currently
rejected; the selected profile describes its fixed variant and any configuration actions.}
  \item{ruleset_id}{NULL or one character string selecting a ruleset belonging to game_id.
Unknown selections fail instead of falling back to another ruleset.}
}
\value{For a built-in game, a named profile record with profile_id, default_ruleset_id,
rulesets, capabilities, coverage_scope, known_limitations, provenance and evidence. For a
custom game, the validated registered rulepack, including state, actions and outcomes.}
\details{Returned built-in metadata is not an editable configuration object. Configuration,
where supported, uses the actions declared by that profile. Modifying returned data does not
modify the registry.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
game <- get_game("G105", ruleset_version = "1.0.0")
game$name
game$coverage_scope
}
