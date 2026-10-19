.ra_decathlon_events<-function() c("100m","long_jump","shot_put","high_jump","400m","110m_hurdles","discus","pole_vault","javelin","1500m")
.ra_decathlon_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);e<-setNames(list(),character());r<-list(player=i(0,7),status=list(type="string",enum=as.list(c("ok","nm","dnf","dq","dns","absent"))),mark=c(i(1,60000),list(nullable=TRUE)))
 list(start_event=e,submit_result=r,amend_result=r,withdraw=list(player=i(0,7)),disqualify=list(player=i(0,7)),confirm_event=e)
}
.ra_decathlon_points<-function(index,r) {
 if(is.null(r) || r$status!="ok") return(0L)
 coefficients<-list(c(25.4347,18,1.81),c(.14354,220,1.4),c(51.39,1.5,1.05),c(.8465,75,1.42),c(1.53775,82,1.81),c(5.74352,28.5,1.92),c(12.91,4,1.1),c(.2797,100,1.35),c(10.14,7,1.08),c(.03768,480,1.85));v<-coefficients[[index+1L]];track<-index %in% c(0L,4L,5L,9L);m<-if(track || index %in% c(2L,6L,8L)) r$mark/100 else r$mark;x<-if(track) v[[2]]-m else m-v[[2]];floor(v[[1]]*max(0,x)^v[[3]])
}
.ra_decathlon_eligible<-function(d,i,current=TRUE) !d$withdrawn[[i+1L]] && !d$disqualified[[i+1L]] && all(vapply(d$results,function(row) !(row[[i+1L]]$status %in% c("dns","absent")),logical(1))) && (!current || length(d$current)==0 || is.null(d$current[[i+1L]]) || !(d$current[[i+1L]]$status %in% c("dns","absent")))
.ra_decathlon_totals<-function(d) lapply(0:7,function(i) sum(vapply(seq_along(d$results),function(j) .ra_decathlon_points(j-1L,d$results[[j]][[i+1L]]),numeric(1)))+if(length(d$current)>0) .ra_decathlon_points(d$event_index,d$current[[i+1L]]) else 0L)
.ra_decathlon_check<-function(index,r,bad) {
 if((r$status=="ok")!=!is.null(r$mark)) bad("Only a valid performance carries a mark")
 track<-index %in% c(0L,4L,5L,9L)
 if(r$status=="nm" && track || r$status=="dnf" && !track) bad("No-mark and did-not-finish statuses must match the event type")
 if(!is.null(r$mark) && !track && r$mark>20000) bad("Field marks use centimetres within the declared input bound")
}
.ra_decathlon_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");n<-length(d$results);phase<-d$phase
 for(j in seq_along(d$results)) for(r in d$results[[j]]) .ra_decathlon_check(j-1L,r,bad)
 if(phase=="finished") {if(length(d$current)>0 || n!=d$event_index+1L || n<10 && any(vapply(0:7,function(i) .ra_decathlon_eligible(d,i),logical(1)))) bad("Final classification requires ten events or no remaining eligible athletes")} else {
  if(n!=d$event_index) bad("Completed event count must match the next event index")
  if(phase=="ready" && length(d$current)>0 || phase %in% c("event","review") && length(d$current)!=8) bad("Current result slots must match the event phase")
 }
 if(length(d$current)>0) {
  for(r in d$current) if(!is.null(r)) .ra_decathlon_check(d$event_index,r,bad)
  if((phase=="review")!=all(vapply(d$current,function(r) !is.null(r),logical(1)))) bad("Review begins when every event result is resolved")
 }
 for(i in 1:8) {absent<-FALSE;for(row in d$results) {if(absent && row[[i]]$status!="absent") bad("An athlete who failed to start cannot return in a subsequent event");if(row[[i]]$status %in% c("dns","absent")) absent<-TRUE}};invisible(TRUE)
}
.ra_decathlon_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_decathlon_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown decathlon event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished") .ra_fail("Competition is finished")
 if(k=="start_event") {
  if(phase!="ready") .ra_fail("Current event must be confirmed before another starts")
  d$current<-lapply(0:7,function(i) if(.ra_decathlon_eligible(d,i)) NULL else list(status="absent",mark=NULL));d$phase<-if(any(vapply(d$current,is.null,logical(1)))) "event" else "review"
 } else if(k %in% c("submit_result","amend_result")) {
  i<-p$player;r<-list(status=p$status,mark=p$mark)
  if(phase!=if(k=="submit_result") "event" else "review") .ra_fail("Submit an eligible athlete once, or amend the current event during review")
  if(!.ra_decathlon_eligible(d,i,FALSE) || k=="submit_result" && !is.null(d$current[[i+1L]]) || p$status=="absent") .ra_fail("Submit an eligible athlete once, or amend the current event during review")
  .ra_decathlon_check(d$event_index,r,.ra_fail);d$current[[i+1L]]<-r
  if(all(vapply(d$current,function(x) !is.null(x),logical(1)))) d$phase<-"review"
 } else if(k %in% c("withdraw","disqualify")) {
  i<-p$player+1L;field<-if(k=="withdraw") "withdrawn" else "disqualified"
  if(d[[field]][[i]]) .ra_fail("Athlete already has this overall status")
  d[[field]][[i]]<-TRUE
  if(length(d$current)>0 && is.null(d$current[[i]])) d$current[[i]]<-list(status="dns",mark=NULL)
  if(length(d$current)>0 && all(vapply(d$current,function(x) !is.null(x),logical(1)))) d$phase<-"review"
 } else if(k=="confirm_event") {
  if(phase!="review") .ra_fail("Resolve all current results before confirming the event")
  d$results[[length(d$results)+1L]]<-d$current;d$current<-list()
  if(d$event_index==9 || !any(vapply(0:7,function(i) .ra_decathlon_eligible(d,i),logical(1)))) d$phase<-"finished" else {d$event_index<-d$event_index+1L;d$phase<-"ready"}
 };d
}
.ra_decathlon_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,ready="start_event",event="submit_result",review=c("amend_result","confirm_event"),character());if(phase!="finished") names<-c(names,"withdraw","disqualify");s<-.ra_decathlon_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Event order, participation eligibility and result-review phase must permit the event."),external_observations=list("Official completed event marks or statuses; race and field-attempt judgments are supplied externally."))),constraints=list(phase=phase,event=.ra_decathlon_events()[[d$event_index+1L]],event_number=d$event_index+1L,mark_unit=if(d$event_index %in% c(0L,4L,5L,9L)) "centiseconds" else "centimetres",eligible=as.list(Filter(function(i) .ra_decathlon_eligible(d,i),0:7)),totals=.ra_decathlon_totals(d)),completeness=FALSE,reason="official_decathlon_event_results")
}
.ra_decathlon_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";scores<-unlist(.ra_decathlon_totals(d));ids<-Filter(function(i) .ra_decathlon_eligible(d,i),0:7);best<-if(length(ids)>0) max(scores[ids+1L]) else NULL;w<-ids[scores[ids+1L]==best];ordered<-ids[order(-scores[ids+1L],ids)]
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) {if(length(ids)==0) "no_contest" else if(length(w)>1) "draw" else "win"} else "none",winners=if(done) lapply(w,function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(setdiff(ids,w),function(i) state$players[[i+1L]]$id) else list(),placements=if(done) lapply(ordered,function(i) list(player_id=state$players[[i+1L]]$id,place=1L+sum(scores[ids+1L]>scores[[i+1L]]),points=scores[[i+1L]])) else list(),reason=if(done && length(ids)>0) "highest_total" else if(done) "no_classified_athletes" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
