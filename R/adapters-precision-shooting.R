.ra_ps_key<-function(r) {scores<-vapply(r$shots,function(x) x$score,numeric(1));c(sum(scores)-r$penalty_points,sum(vapply(r$shots,function(x) x$inner_ten,logical(1))),vapply(seq(51,1,-10),function(i) sum(scores[i:(i+9L)]),numeric(1)),unlist(lapply(rev(r$shots),function(s) c(s$score,as.integer(s$inner_ten)))))}
.ra_ps_close<-function(d) {for(i in 1:8) if(!d$shooters[[i]]$disqualified) d$shooters[[i]]$shots<-c(d$shooters[[i]]$shots,rep(list(list(score=0L,inner_ten=FALSE)),60L-length(d$shooters[[i]]$shots)));d$phase<-"review";d}
.ra_ps_validate<-function(d) {
 for(r in d$shooters) {
  if(any(vapply(r$shots,function(s) s$inner_ten && s$score!=10L,logical(1)))) .ra_fail("Only a ten may be an inner ten",code="INVALID_STATE")
  if(d$phase %in% c("preparation","sighting") && length(r$shots)) .ra_fail("Competition shots exist before match start",code="INVALID_STATE")
  if(d$phase %in% c("review","finished") && !r$disqualified && length(r$shots)!=60L) .ra_fail("Resolve every required competition shot",code="INVALID_STATE")
 }
 if(d$phase %in% c("preparation","sighting") && d$remaining_seconds!=4500L) .ra_fail("Match clock cannot run before match start",code="INVALID_STATE")
 if(d$phase=="match" && (d$remaining_seconds==0L || all(vapply(d$shooters,function(r) r$disqualified || length(r$shots)==60L,logical(1))))) .ra_fail("Completed match must enter review",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_ps_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;index<-list(type="integer",minimum=0,maximum=7);shot<-list(score=list(type="integer",minimum=0,maximum=10),inner_ten=list(type="boolean"))
 if(k %in% c("begin_sighting","start_match","confirm_results")) {
  .ra_fields(p,list(),"/params");expected<-switch(k,begin_sighting="preparation",start_match="sighting",confirm_results="review")
  if(phase!=expected) .ra_fail("Phase does not permit this command")
  d$phase<-switch(k,begin_sighting="sighting",start_match="match",confirm_results="finished")
  if(k=="start_match" && all(vapply(d$shooters,function(r) r$disqualified,logical(1)))) d<-.ra_ps_close(d)
 } else if(k=="elapse") {
  .ra_fields(p,list(seconds=list(type="integer",minimum=1,maximum=4500)),"/params")
  if(phase!="match" || p$seconds>d$remaining_seconds) .ra_fail("Elapsed time must fit the active match clock")
  d$remaining_seconds<-d$remaining_seconds-p$seconds;if(d$remaining_seconds==0L) d<-.ra_ps_close(d)
 } else if(k %in% c("record_shot","correct_shot")) {
  schema<-c(list(participant=index),shot);if(k=="correct_shot") schema$shot<-list(type="integer",minimum=0,maximum=59)
  .ra_fields(p,schema,"/params");i<-p$participant+1L;r<-d$shooters[[i]]
  if(p$inner_ten && p$score!=10L) .ra_fail("Only a ten may be an inner ten")
  value<-p[c("score","inner_ten")]
  if(k=="record_shot") {
   if(phase!="match" || r$disqualified || length(r$shots)==60L) .ra_fail("No further competition shot is available")
   d$shooters[[i]]$shots<-c(r$shots,list(value))
   if(all(vapply(d$shooters,function(x) x$disqualified || length(x$shots)==60L,logical(1)))) d<-.ra_ps_close(d)
  } else {
   if(phase!="review" || r$disqualified || p$shot>=length(r$shots)) .ra_fail("Correct an eligible recorded shot during review")
   d$shooters[[i]]$shots[[p$shot+1L]]<-value
  }
 } else if(k %in% c("assess_penalty","correct_penalty","disqualify")) {
  schema<-list(participant=index);if(k!="disqualify") schema$points<-list(type="integer",minimum=if(k=="correct_penalty") 0 else 1,maximum=600)
  .ra_fields(p,schema,"/params");i<-p$participant+1L;r<-d$shooters[[i]]
  if(r$disqualified) .ra_fail("Participant is already disqualified")
  if(k=="disqualify") d$shooters[[i]]$disqualified<-TRUE else if(k=="assess_penalty") {
   if(!phase %in% c("sighting","match","review") || r$penalty_points+p$points>600) .ra_fail("Penalty is not permitted in this phase or exceeds the bound")
   d$shooters[[i]]$penalty_points<-r$penalty_points+p$points
  } else {
   if(phase!="review") .ra_fail("Penalty correction requires result review")
   d$shooters[[i]]$penalty_points<-p$points
  }
  if(phase=="match" && all(vapply(d$shooters,function(x) x$disqualified || length(x$shots)==60L,logical(1)))) d<-.ra_ps_close(d)
 } else .ra_fail("Unknown precision-shooting action")
 d
}
.ra_ps_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();index<-list(type="integer",minimum=0,maximum=7);shot<-list(score=list(type="integer",minimum=0,maximum=10),inner_ten=list(type="boolean"));empty<-setNames(list(),character())
 if(phase=="preparation") specs$begin_sighting<-empty
 if(phase=="sighting") specs$start_match<-empty
 if(phase=="match") {specs$record_shot<-c(list(participant=index),shot);specs$elapse<-list(seconds=list(type="integer",minimum=1,maximum=4500))}
 if(phase=="review") {specs$correct_shot<-c(list(participant=index),shot,list(shot=list(type="integer",minimum=0,maximum=59)));specs$correct_penalty<-list(participant=index,points=list(type="integer",minimum=0,maximum=600));specs$confirm_results<-empty}
 if(phase %in% c("sighting","match","review")) specs$assess_penalty<-list(participant=index,points=list(type="integer",minimum=1,maximum=600))
 if(phase!="finished") specs$disqualify<-list(participant=index)
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Phase, remaining shots, timing and eligibility must permit the event."),external_observations=list("Official shot score, inner-ten decision, elapsed range time or assessed penalty."))),constraints=list(phase=phase,remaining_seconds=d$remaining_seconds,shots_remaining=lapply(d$shooters,function(r) if(r$disqualified) 0L else 60L-length(r$shots))),completeness=FALSE,reason="official_precision_shooting_events")
}
.ra_ps_compare<-function(a,b) {ix<-which(a!=b);if(length(ix)) sign(a[ix[1]]-b[ix[1]]) else 0L}
.ra_ps_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";eligible<-if(done) which(!vapply(d$shooters,function(r) r$disqualified,logical(1))) else integer();keys<-lapply(eligible,function(i) .ra_ps_key(d$shooters[[i]]))
 places<-vapply(keys,function(k) 1L+sum(vapply(keys,function(other) .ra_ps_compare(other,k)>0,logical(1))),integer(1));o<-order(places,eligible);eligible<-eligible[o];keys<-keys[o];places<-places[o]
 winners<-lapply(eligible[places==1L],function(i) state$players[[i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(eligible)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) !state$players[[i]]$id %in% unlist(winners),1:8),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(eligible),function(j) list(player_id=state$players[[eligible[j]]]$id,place=places[j],score=keys[[j]][1],inner_tens=keys[[j]][2])),reason=if(done) "qualification_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
