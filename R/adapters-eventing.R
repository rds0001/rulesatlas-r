.ra_eventing_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);p<-i(0,7);e<-setNames(list(),character())
 list(configure=list(xc_fences=i(1,40),jump_efforts=i(1,20),xc_optimum_ms=i(1,3600000),jump_allowed_ms=i(1,600000)),record_dressage=list(player=p,percentage_bp=c(i(0,10000),list(nullable=TRUE)),collective_half_points=i(0,10000),errors=i(0,3)),record_cross_country=list(player=p,completed=i(0,40),refusals=list(type="array",minItems=1,maxItems=40,items=i(0,3)),frangibles=i(0,40),missed_flags=i(0,40),dangerous_riding=i(0,2),time_ms=i(1,7200001)),record_jumping=list(player=p,completed=i(0,20),knockdowns=i(0,20),refusals=i(0,2),time_ms=i(1,1200001)),withdraw=list(player=p),eliminate=list(player=p),disqualify=list(player=p),next_phase=e,confirm=e)
}
.ra_eventing_dress<-function(r) floor((10000L-r$dressage$percentage_bp+5L)/10)
.ra_eventing_xc<-function(d,r) {x<-r$xc;sum(vapply(x$refusals,function(n) if(n==1) 200L else if(n>=2) 600L else 0L,integer(1)))+110L*x$frangibles+150L*x$missed_flags+250L*x$dangerous_riding+4L*max(0,floor((x$time_ms-d$xc_optimum_ms+999L)/1000))}
.ra_eventing_jump<-function(d,r) {x<-r$jump;40L*(x$knockdowns+x$refusals)+4L*max(0,floor((x$time_ms-d$jump_allowed_ms+999L)/1000))}
.ra_eventing_xc_out<-function(d,x) x$completed!=d$xc_fences || sum(unlist(x$refusals))>=3 || x$time_ms>2*d$xc_optimum_ms
.ra_eventing_jump_out<-function(d,x) x$completed!=d$jump_efforts || x$refusals>=2 || x$time_ms>2*d$jump_allowed_ms
.ra_eventing_key<-function(d,r) {xc<-.ra_eventing_xc(d,r);jump<-.ra_eventing_jump(d,r);c(.ra_eventing_dress(r)+xc+jump,xc,abs(r$xc$time_ms-d$xc_optimum_ms),jump,r$jump$time_ms,-r$dressage$collective_half_points)}
.ra_eventing_less<-function(a,b) {for(i in seq_along(a)) {if(a[[i]]<b[[i]]) return(TRUE);if(a[[i]]>b[[i]]) return(FALSE)};FALSE}
.ra_eventing_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");phase<-d$phase
 for(r in d$entrants) {
  dress<-r$dressage;xc<-r$xc;jump<-r$jump
  if(!is.null(dress) && (dress$errors==3)!=is.null(dress$percentage_bp)) bad("Third dressage error eliminates rather than producing a scored percentage")
  if(!is.null(xc) && (is.null(dress) || length(xc$refusals)!=d$xc_fences || xc$completed>d$xc_fences || xc$frangibles>d$xc_fences || xc$missed_flags>d$xc_fences)) bad("Cross-country record requires dressage and the configured course")
  if(!is.null(jump) && (is.null(xc) || jump$completed>d$jump_efforts || jump$knockdowns>jump$completed)) bad("Jumping record requires cross country and valid completed efforts")
  if(phase=="dressage" && (!is.null(xc) || !is.null(jump)) || phase=="cross_country" && !is.null(jump)) bad("Later phase records cannot precede their competition phase")
  if(r$status=="eligible") {
   if(!is.null(dress) && dress$errors>=3 || !is.null(xc) && .ra_eventing_xc_out(d,xc) || !is.null(jump) && .ra_eventing_jump_out(d,jump)) bad("An eliminating component result prevents continuation")
   if(phase!="dressage" && is.null(dress) || phase %in% c("jumping","review","finished") && is.null(xc) || phase %in% c("review","finished") && is.null(jump)) bad("Eligible entrant must complete every preceding phase")
  }
 };invisible(TRUE)
}
.ra_eventing_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_eventing_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown eventing decision")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished") .ra_fail("Competition is finished")
 if(k=="configure") {
  if(phase!="dressage" || any(vapply(d$entrants,function(r) !is.null(r$dressage),logical(1)))) .ra_fail("Course settings are fixed once component scoring starts")
  for(name in names(p)) d[[name]]<-p[[name]]
 } else if(k %in% c("record_dressage","record_cross_country","record_jumping")) {
  expected<-switch(k,record_dressage="dressage",record_cross_country="cross_country",record_jumping="jumping");field<-switch(k,record_dressage="dressage",record_cross_country="xc",record_jumping="jump");i<-p$player+1L;r<-d$entrants[[i]]
  if(phase!=expected || r$status!="eligible" || !is.null(r[[field]])) .ra_fail("Record each eligible combination once in the active phase")
  x<-p;x$player<-NULL
  if(k=="record_dressage") {
   if((x$errors==3)!=is.null(x$percentage_bp)) .ra_fail("Third dressage error must be recorded as elimination without a percentage")
   out<-x$errors==3
  } else if(k=="record_cross_country") {
   if(length(x$refusals)!=d$xc_fences || x$completed>d$xc_fences || x$frangibles>d$xc_fences || x$missed_flags>d$xc_fences) .ra_fail("Cross-country summary must match the configured fences")
   out<-.ra_eventing_xc_out(d,x)
  } else {
   if(x$completed>d$jump_efforts || x$knockdowns>x$completed) .ra_fail("Jumping summary exceeds the completed efforts")
   out<-.ra_eventing_jump_out(d,x)
  }
  r[[field]]<-x;if(out) r$status<-"eliminated";d$entrants[[i]]<-r
 } else if(k %in% c("withdraw","eliminate","disqualify")) {
  i<-p$player+1L;if(d$entrants[[i]]$status!="eligible") .ra_fail("Combination is already excluded")
  d$entrants[[i]]$status<-switch(k,withdraw="withdrawn",eliminate="eliminated",disqualify="dq")
 } else if(k=="next_phase") {
  if(!(phase %in% c("dressage","cross_country","jumping"))) .ra_fail("No component phase can advance")
  field<-switch(phase,dressage="dressage",cross_country="xc",jumping="jump")
  if(any(vapply(d$entrants,function(r) r$status=="eligible" && is.null(r[[field]]),logical(1)))) .ra_fail("Resolve every eligible combination before advancing")
  d$phase<-if(!any(vapply(d$entrants,function(r) r$status=="eligible",logical(1)))) "review" else switch(phase,dressage="cross_country",cross_country="jumping",jumping="review")
 } else if(k=="confirm") {if(phase!="review") .ra_fail("All three phases must reach review before confirmation");d$phase<-"finished"};d
}
.ra_eventing_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,dressage=c("record_dressage","next_phase"),cross_country=c("record_cross_country","next_phase"),jumping=c("record_jumping","next_phase"),review="confirm",character())
 if(phase=="dressage" && all(vapply(d$entrants,function(r) is.null(r$dressage),logical(1)))) names<-c("configure",names)
 if(phase!="finished") names<-c(names,"withdraw","eliminate","disqualify")
 s<-.ra_eventing_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Combination eligibility, component order, score-sheet consistency and elimination limits must permit the event."),external_observations=list("Final dressage percentage after deductions; official course faults, corrected times and exclusion decisions."))),constraints=list(phase=phase,eligible=as.list(which(vapply(d$entrants,function(r) r$status=="eligible",logical(1)))-1L),xc_optimum_ms=d$xc_optimum_ms,xc_time_limit_ms=2*d$xc_optimum_ms,jump_allowed_ms=d$jump_allowed_ms,jump_time_limit_ms=2*d$jump_allowed_ms),completeness=FALSE,reason="official_eventing_component_results")
}
.ra_eventing_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ids<-if(done) which(vapply(d$entrants,function(r) r$status=="eligible",logical(1)))-1L else integer()
 if(length(ids)>0) {keys<-lapply(ids,function(i) .ra_eventing_key(d,d$entrants[[i+1L]]));cols<-lapply(1:6,function(j) vapply(keys,function(k) k[[j]],numeric(1)));ids<-ids[do.call(order,c(cols,list(ids)))]}
 best<-if(length(ids)>0) .ra_eventing_key(d,d$entrants[[ids[[1]]+1L]]) else NULL;w<-Filter(function(i) all(.ra_eventing_key(d,d$entrants[[i+1L]])==best),ids)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ids)>0) "ranking" else "no_contest"} else "none",winners=lapply(w,function(i) state$players[[i+1L]]$id),losers=lapply(setdiff(ids,w),function(i) state$players[[i+1L]]$id),placements=lapply(ids,function(i) list(player_id=state$players[[i+1L]]$id,place=1L+sum(vapply(ids,function(j) .ra_eventing_less(.ra_eventing_key(d,d$entrants[[j+1L]]),.ra_eventing_key(d,d$entrants[[i+1L]])),logical(1))),penalty_tenths=.ra_eventing_key(d,d$entrants[[i+1L]])[[1]])),reason=if(done) "three_phase_classification" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
