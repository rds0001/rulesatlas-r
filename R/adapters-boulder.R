.ra_boulder_advance <- function(d) {
  while(d$current<10) {
    p<-d$current%%2+1L;b<-d$current%/%2+1L;c<-d$results[[p]][[b]]
    if(d$disqualified[[p]]) {c$closed<-TRUE;c$active<-FALSE;d$results[[p]][[b]]<-c}
    if(!c$closed) break
    d$current<-d$current+1L
  }
  d
}
.ra_boulder_validate <- function(d) {
  for(p in 1:2) for(b in 1:5) {
    c<-d$results[[p]][[b]];index<-(b-1L)*2L+p-1L
    if(c$closed!=(index<d$current)) .ra_fail("Problem order mismatch",code="INVALID_STATE")
    if(c$active && (index!=d$current || c$attempts==0)) .ra_fail("Invalid active attempt",code="INVALID_STATE")
    for(key in c("zone_attempt","top_attempt")) if(!is.null(c[[key]]) && (c[[key]]<1 || c[[key]]>c$attempts)) .ra_fail("Scoring attempt is outside the attempt count",code="INVALID_STATE")
    if(!is.null(c$top_attempt) && (!c$closed || is.null(c$zone_attempt) || c$zone_attempt>c$top_attempt)) .ra_fail("Top and zone state mismatch",code="INVALID_STATE")
    if(index>d$current && (c$attempts!=0 || !is.null(c$zone_attempt) || !is.null(c$top_attempt))) .ra_fail("Future problem has attempts",code="INVALID_STATE")
  }
  if(d$current<10 && d$disqualified[[d$current%%2+1L]]) .ra_fail("Disqualified climber must be skipped",code="INVALID_STATE")
}
.ra_boulder_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="disqualify") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");t<-p$player+1L
    if(d$disqualified[[t]]) .ra_fail("Climber is already disqualified")
    d$disqualified[[t]]<-TRUE;return(.ra_boulder_advance(d))
  }
  .ra_fields(p,structure(list(),names=character()),"/params");t<-d$current%%2+1L;b<-d$current%/%2+1L;c<-d$results[[t]][[b]]
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  if(kind=="start_attempt") {
    if(c$active) .ra_fail("An attempt is already active")
    c$attempts<-c$attempts+1L;c$active<-TRUE
  } else if(kind %in% c("record_zone","record_top","fail_attempt")) {
    if(!c$active) .ra_fail("An active attempt is required")
    if(kind=="record_zone") {
      if(!is.null(c$zone_attempt)) .ra_fail("Zone has already been recorded")
      c$zone_attempt<-c$attempts
    } else if(kind=="record_top") {
      if(is.null(c$zone_attempt)) c$zone_attempt<-c$attempts
      c$top_attempt<-c$attempts;c$closed<-TRUE;c$active<-FALSE
    } else c$active<-FALSE
  } else if(kind=="end_problem") {c$closed<-TRUE;c$active<-FALSE} else .ra_fail("Unknown action")
  d$results[[t]][[b]]<-c;.ra_boulder_advance(d)
}
.ra_boulder_score <- function(row) c(-sum(vapply(row,function(c) !is.null(c$top_attempt),logical(1))),-sum(vapply(row,function(c) !is.null(c$zone_attempt),logical(1))),sum(vapply(row,function(c) if(is.null(c$top_attempt)) 0 else c$top_attempt,numeric(1))),sum(vapply(row,function(c) if(is.null(c$zone_attempt)) 0 else c$zone_attempt,numeric(1))))
.ra_boulder_less <- function(a,b) {different<-which(a!=b);length(different)>0 && a[different[1]]<b[different[1]]}
.ra_boulder_legal <- function(state) {
  d<-state$data;c<-d$results[[d$current%%2+1L]][[d$current%/%2+1L]];kinds<-"end_problem"
  if(c$active) {kinds<-c(kinds,"fail_attempt","record_top");if(is.null(c$zone_attempt)) kinds<-c(kinds,"record_zone")} else kinds<-c(kinds,"start_attempt")
  types<-lapply(kinds,function(k) list(type=k,params_schema=structure(list(),names=character()),preconditions=list("The current attempt and problem must permit the event."),external_observations=list("Official attempt, controlled hold, failure or end of climbing period.")))
  types[[length(types)+1L]]<-list(type="disqualify",params_schema=list(player=list(type="integer",minimum=0,maximum=1)),preconditions=list("Climber is not already disqualified."),external_observations=list("Official disciplinary decision."))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(player=d$current%%2,problem=d$current%/%2),completeness=FALSE,reason="externally_adjudicated_climbing_events")
}
.ra_boulder_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$current==10;indices<-if(terminal) which(!unlist(d$disqualified)) else integer();scores<-lapply(indices,function(i) .ra_boulder_score(d$results[[i]]))
  if(length(indices)==2 && .ra_boulder_less(scores[[2]],scores[[1]])) {indices<-rev(indices);scores<-rev(scores)}
  winners<-if(length(indices)) lapply(indices[vapply(scores,function(s) all(s==scores[[1]]),logical(1))],function(i) state$players[[i]]$id) else list()
  places<-lapply(seq_along(indices),function(k) {s<-scores[[k]];list(player_id=state$players[[indices[k]]]$id,place=1L+sum(vapply(scores,function(other) .ra_boulder_less(other,s),logical(1))),tops=-s[1],zones=-s[2],top_attempts=s[3],zone_attempts=s[4])})
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(length(indices)) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=places,reason=if(terminal) "qualification_round_completed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
