\name{new_state}
\alias{new_state}
\title{Create an Initial Rule State}
\description{Create and validate an initial state for the selected built-in or registered
custom ruleset.}
\usage{
new_state(game_id, players = NULL, variant = NULL, ruleset_version = NULL,
          ruleset_id = NULL)
}
\arguments{
  \item{game_id}{One case-sensitive character string identifying a retained built-in profile or
a registered custom game in the user. namespace. See \link{ruleatlas-profiles}.}
  \item{players}{A list of participant records, each a named list with a distinct, nonempty
character id. The number must satisfy the selected profile. Although the default is NULL,
participant records are required.}
  \item{variant}{NULL or an empty named list. Nonempty variant overrides are currently
rejected; the selected profile describes its fixed variant and any configuration actions.}
  \item{ruleset_version}{NULL or one character string selecting an exact version, for example
"1.0.0". NULL succeeds only when selection is unambiguous. Pin versions when persisting
states.}
  \item{ruleset_id}{NULL or one character string selecting a ruleset belonging to game_id.
Unknown selections fail instead of falling back to another ruleset.}
}
\value{A named state-envelope list with game_id, ruleset_id, ruleset_version, variant, players
and profile-specific data. The initial phase may require explicit setup actions before play.}
\details{Player IDs identify results and actors. Numeric player and position indices in rule
data and action parameters are zero-based even though R list indexing is one-based. Random
draws, official decisions and observations are supplied by the caller. The function does not
start clocks or generate random events.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")),
                   ruleset_version = "1.0.0")
state$data
validate_state(state)$valid
}
