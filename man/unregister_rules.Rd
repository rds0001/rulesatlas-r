\name{unregister_rules}
\alias{unregister_rules}
\title{Remove a Session-Local Custom Ruleset}
\description{Remove one matching custom ruleset from the session registry. Built-in profiles
cannot be unregistered.}
\usage{
unregister_rules(game_id, ruleset_version = NULL, ruleset_id = NULL)
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
\value{TRUE when one custom ruleset was removed, or FALSE if the game or selected version was
not registered. An ambiguous selection or attempted built-in removal raises RULESET_CONFLICT.}
\details{This function does not delete a rulepack file. States referring to a removed custom
version cannot be processed until that version is registered again. Pin both identifiers where
necessary to remove only the intended version.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
path <- system.file("extdata", "examples", "first_to_three.json",
                    package = "ruleatlas", mustWork = TRUE)
local({
  registered <- register_rules(path)
  on.exit(unregister_rules(registered$game_id,
                           ruleset_version = "1.0.0"))
  print(unregister_rules(registered$game_id,
                         ruleset_version = "1.0.0"))
  print(unregister_rules(registered$game_id,
                         ruleset_version = "1.0.0"))
})
}
