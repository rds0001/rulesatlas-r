.ra_trap_next<-function(d) {ix<-which(vapply(d$shooters,function(r) !r$disqualified && length(r$targets)<25L,logical(1)));if(!length(ix)) return(NULL);ix[order(vapply(d$shooters[ix],function(r) length(r$targets),integer(1)),ix)][1]-1L}
.ra_trap_settle<-function(d) {d$phase<-if(is.null(.ra_trap_next(d))) "review" else "ready";d$repeat_second_only<-FALSE;d}
.ra_trap_key<-function(r) c(sum(unlist(r$targets))-r$penalty_targets,as.integer(rev(unlist(r$targets))))
.ra_trap_validate<-function(d) {
 if(d$repeat_second_only && !d$phase %in% c("ready","called")) .ra_fail("Restricted repeat requires an unresolved target",code="INVALID_STATE")
 if(d$phase %in% c("review","finished") && any(vapply(d$shooters,function(r) !r$disqualified && length(r$targets)!=25L,logical(1)))) .ra_fail("Every eligible shooter must resolve twenty-five targets",code="INVALID_STATE")
 if(d$phase %in% c("ready","called") && is.null(.ra_trap_next(d))) .ra_fail("Completed round must enter review",code="INVALID_STATE")
 if(d$phase=="preparation" && any(vapply(d$shooters,function(r) length(r$targets)>0 || r$malfunctions>0 || r$delay_warning,logical(1)))) .ra_fail("Target history exists before the round",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_trap_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;index<-list(type="integer",minimum=0,maximum=5)
 if(k %in% c("start_round","call_target","no_target","malfunction","time_violation","confirm_result")) {
  .ra_fields(p,if(k=="malfunction") list(after_first_miss=list(type="boolean")) else list(),"/params")
  if(k=="start_round") {if(phase!="preparation") .ra_fail("Round already started");d<-.ra_trap_settle(d)} else if(k=="confirm_result") {if(phase!="review") .ra_fail("Resolve all targets and review results first");d$phase<-"finished"} else {
   if(phase!=(if(k %in% c("call_target","time_violation")) "ready" else "called")) .ra_fail("Target phase does not permit this event")
   i<-.ra_trap_next(d)+1L;r<-d$shooters[[i]]
   if(k=="call_target") d$phase<-"called" else if(k=="no_target") d$phase<-"ready" else if(k=="malfunction") {
    if(r$malfunctions<2L) {d$shooters[[i]]$malfunctions<-r$malfunctions+1L;d$phase<-"ready";d$repeat_second_only<-d$repeat_second_only || p$after_first_miss} else {d$shooters[[i]]$targets<-c(r$targets,list(FALSE));d<-.ra_trap_settle(d)}
   } else {if(!r$delay_warning) d$shooters[[i]]$delay_warning<-TRUE else {d$shooters[[i]]$targets<-c(r$targets,list(FALSE));d<-.ra_trap_settle(d)}}
  }
 } else if(k=="record_target") {
  .ra_fields(p,list(hit=list(type="boolean"),shots=list(type="integer",minimum=1,maximum=2)),"/params")
  if(phase!="called") .ra_fail("A valid target must be called first")
  i<-.ra_trap_next(d)+1L;d$shooters[[i]]$targets<-c(d$shooters[[i]]$targets,list(p$hit && (!d$repeat_second_only || p$shots==2L)));d<-.ra_trap_settle(d)
 } else if(k %in% c("correct_target","assess_penalty","correct_penalty","disqualify")) {
  schema<-list(participant=index)
  if(k=="correct_target") {schema$target<-list(type="integer",minimum=0,maximum=24);schema$hit<-list(type="boolean")} else if(k!="disqualify") schema$targets<-list(type="integer",minimum=if(k=="correct_penalty") 0 else 1,maximum=25)
  .ra_fields(p,schema,"/params");i<-p$participant+1L;r<-d$shooters[[i]]
  if(r$disqualified) .ra_fail("Participant is already disqualified")
  if(k=="disqualify") {
   if(phase=="called" && i!=.ra_trap_next(d)+1L) .ra_fail("Resolve the active target before excluding another shooter")
   d$shooters[[i]]$disqualified<-TRUE;if(phase %in% c("ready","called")) d<-.ra_trap_settle(d)
  } else if(k=="correct_target") {
   if(phase!="review" || p$target>=length(r$targets)) .ra_fail("Correct an existing target judgment during review")
   d$shooters[[i]]$targets[[p$target+1L]]<-p$hit
  } else if(k=="correct_penalty") {
   if(phase!="review") .ra_fail("Penalty correction requires review")
   d$shooters[[i]]$penalty_targets<-p$targets
  } else {
   if(!phase %in% c("ready","review") || r$penalty_targets+p$targets>25) .ra_fail("Penalty exceeds the round bound or is unavailable now")
   d$shooters[[i]]$penalty_targets<-r$penalty_targets+p$targets
  }
 } else .ra_fail("Unknown trap action")
 d
}
.ra_trap_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();index<-list(type="integer",minimum=0,maximum=5);empty<-setNames(list(),character())
 if(phase=="preparation") specs$start_round<-empty
 if(phase=="ready") {specs$call_target<-empty;specs$time_violation<-empty}
 if(phase=="called") {specs$record_target<-list(hit=list(type="boolean"),shots=list(type="integer",minimum=1,maximum=2));specs$no_target<-empty;specs$malfunction<-list(after_first_miss=list(type="boolean"))}
 if(phase=="review") {specs$correct_target<-list(participant=index,target=list(type="integer",minimum=0,maximum=24),hit=list(type="boolean"));specs$correct_penalty<-list(participant=index,targets=list(type="integer",minimum=0,maximum=25));specs$confirm_result<-empty}
 if(phase %in% c("ready","review")) specs$assess_penalty<-list(participant=index,targets=list(type="integer",minimum=1,maximum=25))
 if(phase!="finished") specs$disqualify<-list(participant=index)
 i<-.ra_trap_next(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Target phase, next shooter, repetition allowance and review status must permit this event."),external_observations=list("Official hit/lost target, no-target decision, accepted malfunction, preparation-time violation or sanction."))),constraints=list(phase=phase,next_participant=i,repeat_second_only=d$repeat_second_only,shooting_station=if(is.null(i)) NULL else (i+length(d$shooters[[i+1L]]$targets)) %% 5L+1L,preparation_seconds=12L),completeness=FALSE,reason="official_trap_round_events")
}
.ra_trap_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(!vapply(d$shooters,function(r) r$disqualified,logical(1))) else integer();keys<-lapply(eligible,function(i) .ra_trap_key(d$shooters[[i]]))
 places<-vapply(keys,function(k) 1L+sum(vapply(keys,function(other) .ra_ps_compare(other,k)>0,logical(1))),integer(1));o<-order(places,eligible);eligible<-eligible[o];keys<-keys[o];places<-places[o];winners<-lapply(eligible[places==1L],function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) !state$players[[i]]$id %in% unlist(winners),1:6),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=places[j],score=keys[[j]][1])),reason=if(done) "round_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official round confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
