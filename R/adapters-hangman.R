.ra_hm_letters <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ","",fixed=TRUE)[[1]]
.ra_hm_word <- list(type="string",minLength=1,maxLength=64)
.ra_hm_wrong <- function(d) if(is.null(d$word)) 0L else sum(!unlist(d$guesses) %in% strsplit(d$word,"",fixed=TRUE)[[1]])
.ra_hm_solved <- function(d) !is.null(d$word) && all(strsplit(d$word,"",fixed=TRUE)[[1]] %in% unlist(d$guesses))
.ra_hm_validate <- function(d) {
  if(!is.null(d$word) && any(!strsplit(d$word,"",fixed=TRUE)[[1]] %in% .ra_hm_letters)) .ra_fail("Word must contain uppercase Latin letters only",code="INVALID_STATE")
  if(anyDuplicated(unlist(d$guesses))) .ra_fail("Repeated letter guess",code="INVALID_STATE")
  if(is.null(d$word) && length(d$guesses)) .ra_fail("A word is required before guessing",code="INVALID_STATE")
  if(.ra_hm_wrong(d)>6) .ra_fail("Too many incorrect guesses",code="INVALID_STATE")
  if(length(d$guesses)) {
    previous<-list(word=d$word,guesses=head(d$guesses,-1))
    if(.ra_hm_solved(previous) || .ra_hm_wrong(previous)==6) .ra_fail("Guesses continued after the end",code="INVALID_STATE")
  }
}
.ra_hm_apply <- function(state,action) {
  d<-state$data;setup<-is.null(d$word)
  if(!is.null(action$actor) && action$actor!=state$players[[if(setup) 1L else 2L]]$id) .ra_fail("Wrong actor")
  if(setup) {
    if(action$type!="set_word") .ra_fail("Supply the word first")
    .ra_fields(action$params,list(word=.ra_hm_word),"/params");d$word<-action$params$word
  } else {
    if(action$type!="guess") .ra_fail("Unknown action")
    .ra_fields(action$params,list(letter=list(type="enum",values=as.list(.ra_hm_letters))),"/params");c<-action$params$letter
    if(c %in% unlist(d$guesses)) .ra_fail("Letter already guessed")
    d$guesses[[length(d$guesses)+1L]]<-c
  }
  d
}
.ra_hm_legal <- function(state) {
  if(is.null(state$data$word)) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_word",params_schema=list(word=.ra_hm_word),preconditions=list("Only uppercase Latin letters A through Z."),external_observations=list("An externally chosen word."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_word_required"))
  lapply(setdiff(.ra_hm_letters,unlist(state$data$guesses)),function(c) list(type="guess",params=list(letter=c)))
}
.ra_hm_outcome <- function(state,pack) {
  d<-state$data;win<-.ra_hm_solved(d);terminal<-win || .ra_hm_wrong(d)==6;w<-if(win) 2L else 1L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=if(terminal) list(state$players[[w]]$id) else list(),losers=if(terminal) list(state$players[[3L-w]]$id) else list(),placements=list(),reason=if(win) "word_completed" else if(terminal) "miss_limit_reached" else if(is.null(d$word)) "awaiting_word" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
