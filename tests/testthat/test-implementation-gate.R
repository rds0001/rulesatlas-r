test_that("the retained catalog creates valid native initial states", {
  path <- system.file("extdata", "catalog", "target_profiles.json",
                      package = "ruleatlas", mustWork = TRUE)
  ids <- vapply(jsonlite::fromJSON(path, simplifyVector = FALSE)$entries,
                function(x) x$id, character(1))
  games <- list_games()
  expect_length(games, 212L)
  expect_setequal(vapply(games, function(x) x$game_id, character(1)), ids)
  expect_length(list_games(TRUE), 212L)
  # Participant minima come from the packaged native registry, not guessed values.
  registry_path <- system.file("extdata", "profiles", "builtin.json",
                               package = "ruleatlas", mustWork = TRUE)
  registry <- jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
  valid <- vapply(ids, function(id) {
    players <- lapply(seq_len(registry[[id]]$participants$min),
                       function(i) list(id = paste0("p", i)))
    state <- new_state(id, players = players, ruleset_version = "1.0.0")
    identical(state$game_id, id)
  }, logical(1))
  expect_true(all(valid))
  error <- tryCatch(get_game("user.not_registered"),
                    ruleatlas_error = function(e) e)
  expect_identical(error$code, "UNKNOWN_GAME")
})

test_that("finite turns enforce legality, immutability and victory", {
  state <- new_state("G105", players = list(list(id = "a"), list(id = "b")))
  original <- state
  expect_true(validate_state(state)$valid)
  expect_identical(get_capabilities("G105")$legal_actions, "enumerated")
  expect_length(legal_actions(state)$actions, 9L)
  for (cell in c(0L, 3L, 1L, 4L, 2L)) {
    state <- apply_action(state, list(type = "place", params = list(cell = cell)))
  }
  expect_equal(original$data$board, as.list(rep(-1, 9)))
  expect_identical(outcome(state)$winners, list("a"))
  expect_identical(outcome(state)$status, "terminal")
  expect_length(legal_actions(state)$actions, 0L)
  error <- tryCatch(apply_action(state, list(type = "place", params = list(cell = 8L))),
                    ruleatlas_error = function(e) e)
  expect_identical(error$code, "ILLEGAL_ACTION")
  bad <- original
  bad$data$board[[1]] <- 99L
  expect_false(validate_state(bad)$valid)
})

test_that("observed football events apply disciplinary consequences", {
  state <- new_state("S001", players = list(list(id = "home"), list(id = "away")))
  state <- apply_action(state, list(type = "start_period",
                                    params = structure(list(), names = character())))
  state <- apply_action(state, list(type = "goal", params = list(team = 0L)))
  for (i in 1:2) {
    state <- apply_action(state, list(type = "card",
      params = list(team = 1L, player = 3L, color = "yellow")))
  }
  expect_equal(state$data$goals, list(1, 0))
  expect_true(state$data$teams[[2]]$red[[4]])
  expect_identical(legal_actions(state)$mode, "event_types")
  expect_identical(outcome(state)$status, "ongoing")
})

test_that("custom rules validate, version, execute and unregister", {
  path <- system.file("extdata", "examples", "first_to_three.json",
                      package = "ruleatlas", mustWork = TRUE)
  expect_true(validate_rules(path)$valid)
  pack <- register_rules(path)
  on.exit(unregister_rules(pack$game_id, ruleset_version = "1.0.0"))
  expect_identical(register_rules(path), pack)
  altered <- pack
  altered$state$initial$turn <- 1
  error <- tryCatch(register_rules(altered, overwrite = TRUE),
                    ruleatlas_error = function(e) e)
  expect_identical(error$code, "RULESET_CONFLICT")
  state <- new_state(pack$game_id, players = list(list(id = "a"), list(id = "b")),
                     ruleset_version = "1.0.0")
  for (player in c(0L, 1L, 0L, 1L, 0L)) {
    state <- apply_action(state, list(type = "score", params = list(player = player)))
  }
  expect_identical(outcome(state)$winners, list("a"))
  bad <- pack
  bad$actions[[1]]$when <- list(op = "system", args = list("invalid"))
  expect_false(validate_rules(bad)$valid)
  expect_true(unregister_rules(pack$game_id, ruleset_version = "1.0.0"))
  expect_false(unregister_rules(pack$game_id, ruleset_version = "1.0.0"))
})
