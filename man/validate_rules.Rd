\name{validate_rules}
\alias{validate_rules}
\title{Validate a Local Custom Rulepack}
\description{Check a custom rulepack against the bundled JSON schema and bounded interpreter
restrictions without registering it.}
\usage{
validate_rules(rules_or_path)
}
\arguments{
  \item{rules_or_path}{A named list describing a custom rulepack, or one character string
naming a regular local JSON file. Symbolic links, directories, URLs and files larger than 1 MiB
are rejected. Use jsonlite::fromJSON(..., simplifyVector = FALSE) when parsing externally.}
}
\value{A named validation result with valid, scope = "structural" and issues. Each issue
contains code, path, message and severity. Invalid files or rulepacks return valid = FALSE
instead of being registered.}
\details{Validation checks supported expression operators, references, parameter and state
types, effects and resource bounds. It is not a proof of termination, fairness or reachability.
No rulepack code is executed and no network resources are retrieved. See
\link{ruleatlas-formats} for the DSL and bundled schema.}
\section{Errors}{Except for validation results as documented above, failures use a
\code{ruleatlas_error} condition with \code{message}, \code{code} and optional \code{path}.
Catch this class with \code{tryCatch}; see \link{ruleatlas-formats} for stable error codes.}
\seealso{\link{ruleatlas-package}, \link{ruleatlas-formats}, \link{ruleatlas-profiles}}
\examples{
path <- system.file("extdata", "examples", "first_to_three.json",
                    package = "ruleatlas", mustWork = TRUE)
validate_rules(path)$valid
bad <- jsonlite::fromJSON(path, simplifyVector = FALSE)
bad$game_id <- "invalid_namespace"
validate_rules(bad)$valid
}
