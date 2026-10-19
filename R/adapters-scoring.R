# Fixed sport-specific awards; officiating and segment boundaries are external.
.ra_scoring_validate <- function(data, awards) {
  scores<-unlist(data$scores)
  if(any(scores>0 & scores<min(unlist(awards)))) .ra_fail("Score is below the smallest scoring award",code="INVALID_STATE")
}
.ra_scoring_legal <- function(state, awards) {
  list(mode="event_types",actions=list(),action_types=list(
    list(type="award",params_schema=list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="enum",values=as.list(names(awards)))),preconditions=list("Scoring segment is open."),external_observations=list("An official has adjudicated this scoring event, including any prerequisites.")),
    list(type="close_segment",params_schema=structure(list(),names=character()),preconditions=list("Scoring segment is open."),external_observations=list("The supplied scoring segment has ended."))
  ),constraints=structure(list(),names=character()),completeness=FALSE,reason="externally_adjudicated_events")
}
.ra_scoring_apply <- function(state, action, awards) {
  data<-state$data
  if(action$type=="close_segment") {
    .ra_fields(action$params,structure(list(),names=character()),"/params");data$closed<-TRUE
  } else if(action$type=="award") {
    .ra_fields(action$params,list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="enum",values=as.list(names(awards)))),"/params")
    p<-action$params$player+1L;data$scores[[p]]<-data$scores[[p]]+awards[[action$params$kind]]
    if(max(unlist(data$scores))>1000000) .ra_fail("Score exceeds supported bound")
  } else .ra_fail("Unknown action")
  data
}
.ra_scoring_outcome <- function(state, pack, awards) {
  d<-state$data;terminal<-d$closed;tie<-d$scores[[1]]==d$scores[[2]]
  w<-if(!terminal || tie) NULL else if(d$scores[[1]]>d$scores[[2]]) 1L else 2L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(tie) "draw" else "win"} else "none",winners=if(is.null(w)) list() else list(state$players[[w]]$id),losers=if(is.null(w)) list() else list(state$players[[3L-w]]$id),placements=list(),reason=if(terminal) "scoring_segment_closed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
