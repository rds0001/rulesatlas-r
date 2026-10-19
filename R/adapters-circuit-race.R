.ra_circuit_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);p<-i(0,7);time<-i(1,1000000000);e<-setNames(list(),character())
 list(configure=list(laps=i(1,200)),start=e,cross_line=list(player=p,elapsed_ms=time),withdraw=list(player=p),disqualify=list(player=p),time_penalty=list(player=p,milliseconds=i(0,1000000)),suspend=e,resume=e,cancel_race=e,end_early=list(cutoff_ms=i(0,1000000000)),confirm=e,amend_timing=list(player=p,laps=list(type="array",minItems=0,maxItems=200,items=time),status=list(type="string",enum=as.list(c("finished","retired","dns","dq")))))
}
.ra_circuit_classified<-function(d,kind) {
 if(is.null(d$end_mode)) return(integer())
 lengths<-vapply(Filter(function(r) !(r$status %in% c("dns","dq")),d$runners),function(r) length(r$laps),integer(1));maxlaps<-max(c(0L,lengths));if(maxlaps==0) return(integer())
 threshold<-floor(maxlaps*(if(kind=="circuit") 90L else 0L)/100)
 Filter(function(i) {r<-d$runners[[i+1L]];!(r$status %in% c("ready","dns","dq","running")) && length(r$laps)>=threshold && (kind!="circuit" || length(r$laps)>0)},0:7)
}
.ra_circuit_key<-function(r) c(-length(r$laps),(if(length(r$laps)>0) r$laps[[length(r$laps)]] else 0L)+r$penalty_ms)
.ra_circuit_less<-function(a,b) a[[1]]<b[[1]] || a[[1]]==b[[1]] && a[[2]]<b[[2]]
.ra_circuit_settle<-function(d) {if(!any(vapply(d$runners,function(r) r$status=="running",logical(1)))) d$phase<-if(!is.null(d$end_mode)) "review" else "suspended";d}
.ra_circuit_validate<-function(d,kind) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");phase<-d$phase
 for(r in d$runners) {
  times<-unlist(r$laps)
  if(length(times)>d$target_laps || any(diff(times)<=0) || length(times)>0 && tail(times,1)>d$elapsed_ms) bad("Lap crossings must increase and stay within race timing and distance")
  if(r$status %in% c("ready","dns") && length(times)>0) bad("A nonstarter cannot have completed laps")
  if(r$status=="ready" && phase!="ready" || r$status=="running" && !(phase %in% c("racing","suspended"))) bad("Participant status disagrees with race phase")
  if(r$status=="finished" && is.null(d$end_mode)) bad("Finisher requires an official race-end condition")
  if(r$status=="finished" && identical(d$end_mode,"normal") && (length(times)==0 || tail(times,1)<d$checkered_ms)) bad("Normal finisher must cross after the leader receives the checkered flag")
  if(identical(d$end_mode,"early") && length(times)>0 && tail(times,1)>d$cutoff_ms) bad("Early result cannot retain timing beyond the official countback cutoff")
 }
 if(identical(d$end_mode,"normal")!=!is.null(d$checkered_ms) || identical(d$end_mode,"early")!=!is.null(d$cutoff_ms)) bad("Race-end mode must match its flag or countback timestamp")
 if(!is.null(d$checkered_ms) && d$checkered_ms>d$elapsed_ms) bad("Checkered flag cannot occur after the latest official timing event")
 if(phase=="ready" && (d$elapsed_ms>0 || !is.null(d$end_mode) || any(vapply(d$runners,function(r) length(r$laps)>0,logical(1))))) bad("Unstarted race cannot contain timing or a result")
 if(phase %in% c("review","finished") && (is.null(d$end_mode) || any(vapply(d$runners,function(r) r$status %in% c("ready","running"),logical(1))))) bad("Result review requires every entrant resolved and an end condition");invisible(TRUE)
}
.ra_circuit_apply<-function(state,action,kind) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_circuit_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown circuit-race event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished") .ra_fail("Result is final")
 if(k=="configure") {if(phase!="ready") .ra_fail("Race distance is fixed after the start");d$target_laps<-p$laps
 } else if(k=="start") {
  if(phase!="ready" || !any(vapply(d$runners,function(r) r$status=="ready",logical(1)))) .ra_fail("Start requires an eligible grid")
  for(i in 1:8) if(d$runners[[i]]$status=="ready") d$runners[[i]]$status<-"running";d$phase<-"racing"
 } else if(k=="cross_line") {
  i<-p$player+1L;r<-d$runners[[i]];time<-p$elapsed_ms
  if(phase!="racing" || r$status!="running" || time<d$elapsed_ms || length(r$laps)>0 && time<=r$laps[[length(r$laps)]]) .ra_fail("Timing crossings must be chronological for an active racer")
  r$laps[[length(r$laps)+1L]]<-time;d$elapsed_ms<-time
  if(is.null(d$end_mode) && length(r$laps)==d$target_laps) {d$end_mode<-"normal";d$checkered_ms<-time}
  if(identical(d$end_mode,"normal")) r$status<-"finished"
  d$runners[[i]]<-r;d<-.ra_circuit_settle(d)
 } else if(k %in% c("withdraw","disqualify")) {
  i<-p$player+1L;r<-d$runners[[i]]
  if(k=="withdraw") {if(!(r$status %in% c("ready","running")) || !(phase %in% c("ready","racing","suspended"))) .ra_fail("Only an unstarted or active racer may withdraw");r$status<-if(phase=="ready") "dns" else "retired"} else {if(r$status=="dq") .ra_fail("Racer is already disqualified");r$status<-"dq"}
  d$runners[[i]]<-r;if(phase %in% c("racing","suspended")) d<-.ra_circuit_settle(d)
 } else if(k=="time_penalty") {
  if(!(phase %in% c("racing","suspended","review")) || d$runners[[p$player+1L]]$status %in% c("dns","dq")) .ra_fail("A time penalty requires a started, non-disqualified racer")
  d$runners[[p$player+1L]]$penalty_ms<-p$milliseconds
 } else if(k=="suspend") {if(phase!="racing" || !is.null(d$end_mode)) .ra_fail("Suspend an unfinished race before the checkered flag");d$phase<-"suspended"
 } else if(k=="resume") {if(phase!="suspended" || !any(vapply(d$runners,function(r) r$status=="running",logical(1)))) .ra_fail("Resume requires an active racer in a suspended race");d$phase<-"racing"
 } else if(k=="end_early") {
  if(phase!="suspended" || p$cutoff_ms>d$elapsed_ms) .ra_fail("Early classification requires suspension and a recorded timing cutoff")
  cutoff<-p$cutoff_ms;d$end_mode<-"early";d$cutoff_ms<-cutoff;d["checkered_ms"]<-list(NULL)
  for(i in 1:8) {d$runners[[i]]$laps<-Filter(function(t) t<=cutoff,d$runners[[i]]$laps);if(d$runners[[i]]$status=="running") d$runners[[i]]$status<-"retired"};d$phase<-"review"
 } else if(k=="cancel_race") {
  if(!(phase %in% c("ready","suspended"))) .ra_fail("Cancel only an unstarted or suspended race")
  d$phase<-"review";d$end_mode<-"early";d$cutoff_ms<-0L;d["checkered_ms"]<-list(NULL)
  for(i in 1:8) {d$runners[[i]]$laps<-list();if(d$runners[[i]]$status=="ready") d$runners[[i]]$status<-"dns" else if(d$runners[[i]]$status=="running") d$runners[[i]]$status<-"retired"}
 } else if(k=="amend_timing") {
  if(phase!="review") .ra_fail("Timing corrections require result review")
  i<-p$player+1L;d$runners[[i]]$laps<-p$laps;d$runners[[i]]$status<-p$status;d$elapsed_ms<-max(c(d$elapsed_ms,unlist(p$laps)))
 } else if(k=="confirm") {if(phase!="review") .ra_fail("Resolve all racers before confirming classification");d$phase<-"finished"};d
}
.ra_circuit_legal<-function(state,kind) {
 d<-state$data;phase<-d$phase;names<-switch(phase,ready=c("configure","start","withdraw","disqualify","cancel_race"),racing=c("cross_line","withdraw","disqualify","time_penalty",if(is.null(d$end_mode)) "suspend"),suspended=c("resume","withdraw","disqualify","time_penalty","end_early","cancel_race"),review=c("amend_timing","disqualify","time_penalty","confirm"),character());s<-.ra_circuit_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Race phase, chronological crossings, distance and participant eligibility must permit the event."),external_observations=list("Official timing-line crossings, penalties, retirements, suspension or classification cutoff."))),constraints=list(phase=phase,target_laps=d$target_laps,completed_laps=lapply(d$runners,function(r) length(r$laps)),checkered_ms=d$checkered_ms,classification_percent=if(kind=="circuit") 90L else 0L,provisionally_classified=as.list(.ra_circuit_classified(d,kind))),completeness=FALSE,reason=paste0("official_",kind,"_sprint_events"))
}
.ra_circuit_outcome<-function(state,pack,kind) {
 d<-state$data;done<-d$phase=="finished";ids<-.ra_circuit_classified(d,kind);keys<-lapply(ids,function(i) .ra_circuit_key(d$runners[[i+1L]]));ids<-ids[order(vapply(keys,function(k) k[[1]],numeric(1)),vapply(keys,function(k) k[[2]],numeric(1)),ids)];best<-if(length(ids)>0) .ra_circuit_key(d$runners[[ids[[1]]+1L]]) else NULL;w<-Filter(function(i) all(.ra_circuit_key(d$runners[[i+1L]])==best),ids)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ids)>0) "ranking" else "no_contest"} else "none",winners=if(done) lapply(w,function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(setdiff(ids,w),function(i) state$players[[i+1L]]$id) else list(),placements=if(done) lapply(ids,function(i) list(player_id=state$players[[i+1L]]$id,place=1L+sum(vapply(ids,function(j) .ra_circuit_less(.ra_circuit_key(d$runners[[j+1L]]),.ra_circuit_key(d$runners[[i+1L]])),logical(1))),laps=length(d$runners[[i+1L]]$laps),adjusted_ms=.ra_circuit_key(d$runners[[i+1L]])[[2]])) else list(),reason=if(done) "classification_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
