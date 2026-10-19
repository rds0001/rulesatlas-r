\name{ruleatlas-formats}
\alias{ruleatlas-formats}
\title{State, Action, Outcome and Custom Rulepack Formats}
\description{Portable data conventions, error handling and the bounded custom
rule language shared by the native R and Python implementations.}
\details{
\strong{R representation.} JSON objects are named lists; arrays are unnamed
lists, including arrays of scalar numbers. Scalars are length-one character,
numeric or logical values. JSON null is R NULL. Use
\code{structure(list(), names = character())} for an empty object and
\code{list()} for an empty array. Do not substitute data frames, factors,
NA, NaN or infinite numbers. When reading JSON externally use
\code{jsonlite::fromJSON(path, simplifyVector = FALSE)}. Numeric values are
bounded by 2^53 - 1 in absolute value. Integer fields require whole values.
Array and player indices in public rule data are zero-based; R list access is
one-based. Results identify players by their character IDs.

\strong{State envelopes.} A state is a named list with \code{game_id},
\code{ruleset_id}, \code{ruleset_version}, \code{variant}, \code{players} and
\code{data}, with optional \code{metadata}. Players are a list of records
with distinct nonempty \code{id} strings. Data depend on the selected profile.
Start with \code{new_state()} and submit setup or play actions through
\code{apply_action()}. Some profiles need an external deal, course, lexicon,
measurements or official decisions. These are caller inputs, never guessed.
Imported states receive only the checks described by the profile; they do not
receive a general historical-reachability proof.

\strong{Actions and legality.} An action contains \code{type} and
\code{params}, with optional \code{actor} and \code{metadata}.
An actor identifies a participant but does not implement authentication.
\code{legal_actions()} returns \code{mode}, \code{actions},
\code{action_types}, \code{constraints}, \code{completeness} and \code{reason}.
Use complete action objects in enumerated mode. For typed events, follow the
parameter schema and preconditions; enumeration is not promised and a particular
submission can still fail. The state is authoritative and may contain hidden
cards or secrets. Applications must implement their own private views.

\strong{Results.} \code{outcome()} returns \code{status}, \code{result_kind},
\code{winners}, \code{losers}, \code{placements}, \code{reason},
\code{evidence_required} and \code{rule_id}. Status can be ongoing, terminal,
indeterminate or awaiting_adjudication. An official observation required by a
profile is not inferred from physical movement. Read \code{reason} and
\code{evidence_required} before treating a result as final. Rankings and ties
follow the selected profile. No probabilities or strategies are calculated.

\strong{Validation.} \code{validate_state()} and \code{validate_rules()}
return \code{valid}, \code{scope} and \code{issues}. Every issue has
\code{code}, \code{path}, \code{message} and \code{severity}.
Other operations may raise a \code{ruleatlas_error} condition with
\code{code}, \code{message} and optional \code{path}. Stable codes include
UNKNOWN_GAME, UNKNOWN_RULESET, UNSUPPORTED_CAPABILITY, INVALID_STATE,
ILLEGAL_ACTION, MISSING_INFORMATION, RULESET_CONFLICT, INVALID_RULEPACK,
RESOURCE_LIMIT and INTERNAL_ERROR. Catch the condition class instead of matching
human-readable messages. Rejected actions do not partially modify the state.
}
\section{Custom DSL version 1.0}{
A custom rulepack has \code{schema_version = "1.0"}, a \code{game_id} starting
with \code{user.}, \code{name}, \code{ruleset_id}, \code{ruleset_version},
\code{family}, \code{participants}, \code{state}, \code{actions},
\code{outcomes} and \code{provenance}. Optional description and capabilities
are metadata. Nonempty variants are unsupported. A ruleset ID starts with the
game ID followed by a dot. Participants declare integer min and max from 1 to
512. Family is finite_turn, event_scoring, measured_rank, adjudicated or custom.
Provenance supplies basis and verification_status strings and source references.

State declares a named object of field schemas and an initial state. Supported
field types are integer, number, boolean, string, enum, array and object.
Numeric bounds use minimum/maximum, strings minLength/maxLength and arrays
minItems/maxItems. Enum uses a values array; array uses items; object uses
properties and required. Nullable fields permit null. All top-level state fields
are required by the runtime.

Each action declares type, a named params schema, a boolean when expression and
an effects array. Expressions are scalar constants, an object with ref such as
\code{list(ref = "state.turn")}, or an op/args object. References address only
state or params; numeric path components are zero-based array indices. Allowed
operators are eq, ne, lt, lte, gt, gte, not, len, add, sub, mul, mod, in, and, or.
Boolean operators require booleans; arithmetic requires numbers. Modulo requires
integers and a positive divisor. Expressions are data, not R code.

Effects are set with value, inc with by, append with value, or remove_at with
index. Each has a slash-separated path starting at the state data, such as
\code{"/scores/0"}. A path segment \code{"\{player\}"} can use a nonnegative
integer parameter as an array index. Overlapping target paths are rejected.
All effect expressions read the old state; results commit atomically after
postcondition checks. File paths, shell commands and executable R are not DSL
operations.

An outcome declares when, result_kind and a winners array of zero-based player
indices or numeric expressions, with optional reason. More than one matching
outcome is invalid. No match means ongoing. Terminal states reject actions.
Registration is session-local and version-immutable. The same version and same
contents can be registered repeatedly; changed contents require a new version.

Rulepacks are bounded to 1 MiB, JSON depth 32, 50,000 value nodes, 512 actions,
512 outcomes, 100 effects per action and 2,000 expression-check nodes. Runtime
expression evaluation also uses bounded budgets. Custom legal-action enumeration
is limited to 512 parameter combinations; otherwise typed event schemas are
returned. Structural validation does not prove fairness or eventual termination.
}
\section{Bundled schemas and example}{
The extdata/contracts directory contains the language-neutral JSON schemas for
states, actions, results, validation, profile records and rulepacks. The original
extdata/examples/first_to_three.json rulepack is a small runnable template.
It uses alternating zero-based player indices and a three-point win threshold.
}
\seealso{\link{new_state}, \link{legal_actions}, \link{outcome},
\link{validate_rules}, \link{register_rules}, \link{ruleatlas-profiles}}
\examples{
path <- system.file("extdata", "examples", "first_to_three.json",
                    package = "ruleatlas", mustWork = TRUE)
validate_rules(path)$valid
schema <- system.file("extdata", "contracts", "rulepack.schema.json",
                      package = "ruleatlas", mustWork = TRUE)
file.exists(schema)
tryCatch(get_game("user.not_registered"),
         ruleatlas_error = function(e) e$code)
}
