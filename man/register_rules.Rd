\name{register_rules}
\alias{register_rules}
\title{Register an Immutable Custom Ruleset Version}
\description{Validate and register a user-defined rulepack in the current R session so it can
use the same state/action/outcome API as built-in games.}
\usage{
register_rules(rules_or_path, overwrite = FALSE)
}
\arguments{
  \item{rules_or_path}{A named list describing a custom rulepack, or one character string
naming a regular local JSON file. Symbolic links, directories, URLs and files larger than 1 MiB
are rejected. Use jsonlite::fromJSON(..., simplifyVector = FALSE) when parsing externally.}
  \item{overwrite}{One logical value retained for API compatibility. TRUE does not allow
replacing an existing version with different contents; register a new version instead.}
}
\value{The validated registered rulepack as a named list. Registering identical contents for
the same game_id/ruleset_id/ruleset_version is idempotent. Conflicting contents raise
RULESET_CONFLICT.}
\details{Registration is process-local and does not write files or change the installed
package. Built-in IDs are reserved. Multiple versions may coexist; select explicitly when more
than one matches. Changing a version requires a new version identifier. Returned list
modifications do not replace the registry entry.}
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
                           ruleset_version = registered$ruleset_version))
  state <- new_state(registered$game_id,
                     players = list(list(id = "alice"), list(id = "bob")),
                     ruleset_version = "1.0.0")
  state <- apply_action(state, list(type = "score",
                                    params = list(player = 0L)))
  print(state$data$scores)
})
}
