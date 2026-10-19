# ruleatlas-r

All and only the state needed to check game rules: plausible states, permitted next events, rule consequences and outcomes. No simulation or physical model.


A native rule library for game-state validation, legal next actions, transitions
and outcomes, with a bounded interpreter for user-defined JSON rules.
`list_games()` lists accepted profiles. All 212 retained profiles have accepted, explicitly bounded variants. Read `get_game(id)` for the exact
variant, required inputs and limitations; acceptance does not certify every
commercial or federation edition.
The R implementation does not invoke Python.


```r
library(ruleatlas)
register_rules("first_to_three.json")
state <- new_state("user.first_to_three",
                   players = list(list(id = "alice"), list(id = "bob")),
                   ruleset_version = "1.0.0")
state <- apply_action(state, list(type = "score", params = list(player = 0L)))
outcome(state)
```

Use unnamed lists for JSON arrays and named lists for objects. An empty JSON
object is `structure(list(), names = character())`; an empty array is `list()`.
Indices in rules and actions are zero-based. Public winners contain participant
IDs. `NA`, nonfinite numbers and implicit type coercions are rejected.

Registration is session-local. Existing versions are immutable, including when
`overwrite=TRUE`; register a new version for changed rules. Nonempty custom
variants are rejected until explicit variant support is implemented.

Expressions use a bounded whitelist. Effects commit atomically and cannot execute
code, shell commands or network requests. Finite action domains are enumerated up
to 512 combinations; larger domains return guarded event types. Structured errors
inherit `ruleatlas_error` and expose the same codes as the Python library.

This is a development package, not a CRAN release. Real maintainer and license
holder details are required before public distribution. Local check results and
submission status are recorded in the repository release/RELEASE_READINESS.md.

The library product is named `ruleatlas-r`. Its R package identifier is
`ruleatlas`, since R package identifiers cannot contain hyphens.

## Built-in match rules

`S001` implements compact football match rules: goals, per-player cards,
eligibility, substitutions, phases and optional knockout progression.
Use `get_game("S001")` for its declared variant and limitations, and
`legal_actions(state)` for phase-dependent event types and remaining allowances.
The repository includes an R football example in `inst/examples/football.R`.
No ball motion, possession or injury state is stored.

## GitHub source repository

Repository: `rds0001/rulesatlas-r`. The package name remains unchanged.

Install from this repository root:

```sh
R CMD INSTALL .
```

Source repository: https://github.com/rds0001/rulesatlas-r

CI results: https://github.com/rds0001/rulesatlas-r/actions

This source publication is not a PyPI or CRAN release.
