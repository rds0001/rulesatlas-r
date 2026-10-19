.ra_fencing_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_fencing_schemas<-function() {
 e<-setNames(list(),character());team<-.ra_fencing_i(0,1)
 list(start=e,exchange=list(touches=list(type="array",minItems=2,maxItems=2,items=list(type="boolean")),elapsed_ms=.ra_fencing_i(0,180000)),elapse=list(milliseconds=.ra_fencing_i(1,180000)),start_priority=list(priority=team),infringement=list(team=team,group=.ra_fencing_i(1,4)),withdraw=list(team=team),confirm_result=e,confirm_no_contest=e)
}
.ra_fencing_award<-function(d,t) {d$scores[[t+1L]]<-d$scores[[t+1L]]+1L;if(d$scores[[t+1L]]==5 || d$phase %in% c("priority","priority_setup")) {d$phase<-"review";d$winner<-t};d}
.ra_fencing_end_clock<-function(d) {
 if(d$phase=="fencing" && d$remaining_ms==0) {if(d$scores[[1]]==d$scores[[2]]) d$phase<-"priority_setup" else {d$phase<-"review";d$winner<-if(d$scores[[1]]>d$scores[[2]]) 0L else 1L}} else if(d$phase=="priority" && d$remaining_ms==0) {d$phase<-"review";d$winner<-d$priority};d
}
.ra_fencing_exclude<-function(d,t) {d$excluded[[t+1L]]<-TRUE;d$phase<-"review";d["winner"]<-list(if(all(unlist(d$excluded))) NULL else 1L-t);d}
.ra_fencing_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");phase<-d$phase
 for(t in 1:2) if(d$red_count[[t]]>d$scores[[3L-t]] || (d$group_three_red[[t]] && d$red_count[[t]]==0)) bad("Penalty record and awarded touches disagree")
 if(!is.null(d$priority) && !(phase %in% c("priority","review","finished"))) bad("Priority exists outside the extra minute")
 if(phase=="priority" && (is.null(d$priority) || d$remaining_ms>60000)) bad("A priority holder and one-minute clock are required")
 if(phase %in% c("fencing","priority") && (d$remaining_ms==0 || max(unlist(d$scores))==5 || any(unlist(d$excluded)))) bad("Resolved bout cannot remain active")
 if(phase %in% c("priority_setup","priority") && d$scores[[1]]!=d$scores[[2]]) bad("Extra time requires equal scores")
 if(phase=="priority_setup" && d$remaining_ms!=0) bad("Regulation time must expire before priority selection")
 if(phase=="ready" && (d$remaining_ms!=180000 || !is.null(d$priority) || any(unlist(d$excluded)) || max(unlist(d$scores))==5)) bad("Invalid pre-bout state")
 if(!(phase %in% c("review","finished")) && !is.null(d$winner)) bad("Winner is premature")
 if(phase %in% c("review","finished")) {
  if(all(unlist(d$excluded))) {if(!is.null(d$winner)) bad("Double exclusion has no winner")} else if(any(unlist(d$excluded))) {if(is.null(d$winner) || d$winner!=as.integer(d$excluded[[1]])) bad("The non-excluded fencer wins")} else if(is.null(d$winner)) bad("Bout winner is unresolved") else if(d$scores[[1]]!=d$scores[[2]]) {
   if(d$winner!=(if(d$scores[[1]]>d$scores[[2]]) 0 else 1)) bad("Score and winner disagree")
   if(max(unlist(d$scores))<5 && d$remaining_ms>0 && is.null(d$priority)) bad("Regulation bout ended before score or time limit")
  } else if(is.null(d$priority) || d$priority!=d$winner || d$remaining_ms!=0) bad("A tied bout is decided by expired priority time")
 }
 if(d$no_contest && (phase!="finished" || !all(unlist(d$excluded)))) bad("No contest requires an official double-exclusion resolution")
 if(phase=="finished" && all(unlist(d$excluded)) && !d$no_contest) bad("Resolve both exclusions explicitly")
 invisible(TRUE)
}
.ra_fencing_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;schemas<-.ra_fencing_schemas()
 if(!(k %in% names(schemas))) .ra_fail("Unknown fencing event")
 .ra_fields(p,schemas[[k]],"/params")
 if(k=="start") {
  if(phase!="ready") .ra_fail("Bout has already started")
  d$phase<-"fencing"
 } else if(k %in% c("exchange","elapse")) {
  if(!(phase %in% c("fencing","priority"))) .ra_fail("An active fencing interval is required")
  elapsed<-if(k=="exchange") p$elapsed_ms else p$milliseconds
  if(elapsed>d$remaining_ms) .ra_fail("Elapsed fencing time exceeds the remaining interval")
  d$remaining_ms<-d$remaining_ms-elapsed
  if(k=="exchange") {
   touches<-unlist(p$touches)
   if(all(touches)) {
    if(phase=="fencing" && !all(unlist(d$scores)==4)) {d$scores<-as.list(unlist(d$scores)+1L);if(max(unlist(d$scores))==5) {d$phase<-"review";d$winner<-which(unlist(d$scores)==5)[1]-1L}}
   } else if(any(touches)) d<-.ra_fencing_award(d,if(touches[1]) 0L else 1L)
  }
  d<-.ra_fencing_end_clock(d)
 } else if(k=="start_priority") {
  if(phase!="priority_setup") .ra_fail("No tied regulation result awaits priority")
  d$phase<-"priority";d$priority<-p$priority;d$remaining_ms<-60000
 } else if(k=="infringement") {
  t<-p$team;group<-p$group
  if(d$excluded[[t+1L]] || (phase=="review" && group!=4)) .ra_fail("This sanction cannot be issued in the current bout phase")
  black<-group==4 || (group==3 && d$group_three_red[[t+1L]])
  if(black) d<-.ra_fencing_exclude(d,t) else if(group==1 && !d$yellow[[t+1L]] && d$red_count[[t+1L]]==0) d$yellow[[t+1L]]<-TRUE else {
   if(group==3) d$group_three_red[[t+1L]]<-TRUE
   d$red_count[[t+1L]]<-d$red_count[[t+1L]]+1L;d<-.ra_fencing_award(d,1L-t)
  }
 } else if(k=="withdraw") {
  if(d$excluded[[p$team+1L]]) .ra_fail("Fencer is already excluded")
  d<-.ra_fencing_exclude(d,p$team)
 } else if(k=="confirm_result") {
  if(phase!="review" || all(unlist(d$excluded))) .ra_fail("Resolve the bout winner before confirmation")
  d$phase<-"finished"
 } else if(k=="confirm_no_contest") {
  if(phase!="review" || !all(unlist(d$excluded))) .ra_fail("Official no-contest decision requires both exclusions")
  d$phase<-"finished";d$no_contest<-TRUE
 };d
}
.ra_fencing_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,ready="start",fencing=c("exchange","elapse"),priority_setup="start_priority",priority=c("exchange","elapse"),review=if(all(unlist(d$excluded))) "confirm_no_contest" else "confirm_result",character())
 if(phase!="finished") names<-c(names,"infringement","withdraw")
 s<-.ra_fencing_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Bout phase, score limit, elapsed time and penalty group must permit the event."),external_observations=list("Official valid touch signals, penalty classification or drawn priority."))),constraints=list(phase=phase,remaining_ms=d$remaining_ms,scores=d$scores,priority=d$priority),completeness=FALSE,reason="official_epee_bout_events")
}
.ra_fencing_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$no_contest) "no_contest" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) "epee_bout_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
