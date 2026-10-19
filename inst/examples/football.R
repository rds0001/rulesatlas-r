# ruleatlas-r: referee-confirmed events and automatic rule consequences.
library(ruleatlas)
state <- new_state("S001", players = list(list(id = "home"), list(id = "away")))
state <- apply_action(state, list(type = "start_period", params = structure(list(), names = character())))
state <- apply_action(state, list(type = "goal", params = list(team = 0L)))
for (i in 1:2) {
  state <- apply_action(state, list(type = "card", params = list(team = 1L, player = 3L, color = "yellow")))
}
stopifnot(state$data$teams[[2]]$red[[4]])
print(state$data$goals)
print(legal_actions(state)$constraints)
print(outcome(state))
