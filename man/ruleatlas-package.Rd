\name{ruleatlas-package}
\alias{ruleatlas}
\alias{ruleatlas-package}
\docType{package}
\title{Native Syntactic Sport and Game Rules}
\description{Validate rule-relevant states, inspect permitted next moves or
observed events, apply deterministic consequences and determine outcomes.
Includes 212 retained profiles and a bounded declarative custom-rule interpreter.
No simulation, movement model, strategy, solver or prediction is included.}
\details{The public API is the same as the native Python library. R does not
call Python or a remote service. Each profile implements one declared variant;
inspect its scope, observations and limitations before use. Errors and returned
objects follow the portable conventions in \link{ruleatlas-formats}.
The profile index is in \link{ruleatlas-profiles}.}
\section{Workflow}{
Use \link{list_games}, \link{get_game} and \link{get_capabilities} to select a
profile. Create a state with \link{new_state}; inspect it with
\link{validate_state}, \link{legal_actions} and \link{outcome}. Submit one
move or official event with \link{apply_action}. Its result is a new state;
the input is unchanged. Custom rulepacks use \link{validate_rules},
\link{register_rules} and \link{unregister_rules}.
}
\examples{
state <- new_state("G105", players = list(list(id = "alice"),
                                         list(id = "bob")))
next_state <- apply_action(state, list(type = "place",
                                       params = list(cell = 0L)))
validate_state(next_state)$valid
outcome(next_state)$status
}
\keyword{package}
