.ra_wl_best<-function(a) max(c(0,vapply(Filter(function(x) x$passed,a),function(x) x$weight_kg,numeric(1))))
.ra_wl_candidates<-function(d) which(!vapply(d$declared,is.null,logical(1)))-1L
.ra_wl_next<-function(d) {c<-.ra_wl_candidates(d);if(!length(c)) return(NULL);c[order(vapply(c,function(i) d$declared[[i+1L]],numeric(1)),vapply(c,function(i) length(d$attempts[[d$lift+1L]][[i+1L]]),integer(1)),match(c,unlist(d$lots)))][1]}
.ra_wl_minimum<-function(d,i) {a<-d$attempts[[d$lift+1L]][[i]];max(d$bar_kg,if(length(a)) tail(a,1)[[1]]$weight_kg+as.integer(tail(a,1)[[1]]$passed) else 20)}
.ra_wl_settle<-function(d) {if(!length(.ra_wl_candidates(d))) d$phase<-"review";d}
.ra_wl_validate<-function(d) {
 serials<-numeric()
 for(lift in 1:2) for(a in d$attempts[[lift]]) {
  last<-19
  for(x in a) {if(x$weight_kg<last) .ra_fail("Attempt weight falls below the required next load",code="INVALID_STATE");last<-x$weight_kg+as.integer(x$passed);serials<-c(serials,x$sequence)}
  if(lift>d$lift+1L && length(a)) .ra_fail("Future lift already contains attempts",code="INVALID_STATE")
 }
 if(!identical(as.integer(sort(serials)),seq_along(serials))) .ra_fail("Attempt sequence must be unique and contiguous",code="INVALID_STATE")
 if(!identical(as.integer(sort(unlist(d$lots))),0:7)) .ra_fail("Lot order must be a permutation",code="INVALID_STATE")
 if(d$phase=="setup") {
  if(d$bar_kg!=20 || any(lengths(d$attempts[[d$lift+1L]])>0) || any(!vapply(d$declared,is.null,logical(1))) || any(unlist(d$changes)!=0) || any(unlist(d$withdrawn))) .ra_fail("Lift setup must precede declarations",code="INVALID_STATE")
 } else {
  for(i in 1:8) {
   a<-d$attempts[[d$lift+1L]][[i]]
   if(d$disqualified[[i]] && !d$withdrawn[[i]]) .ra_fail("Disqualified entrants cannot continue",code="INVALID_STATE")
   if(is.null(d$declared[[i]])!=(length(a)==3L || d$withdrawn[[i]])) .ra_fail("Remaining attempts require a declared weight",code="INVALID_STATE")
   if(!is.null(d$declared[[i]]) && d$declared[[i]]<.ra_wl_minimum(d,i)) .ra_fail("Declared weight is below the current minimum",code="INVALID_STATE")
  }
  if((d$phase %in% c("ready","called"))!=(length(.ra_wl_candidates(d))>0)) .ra_fail("Phase and available attempts disagree",code="INVALID_STATE")
 }
 if(d$phase=="finished" && d$lift!=1L) .ra_fail("Both lifts must be completed",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_wl_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;lift<-d$lift+1L;index<-list(type="integer",minimum=0,maximum=7);weight<-list(type="integer",minimum=20,maximum=500)
 if(k=="declare_openers") {
  .ra_fields(p,list(weights_kg=list(type="array",minItems=8,maxItems=8,items=weight)),"/params")
  if(phase!="setup") .ra_fail("Opening declarations are already recorded")
  d$declared<-lapply(1:8,function(i) if(d$disqualified[[i]]) NULL else p$weights_kg[[i]]);d$withdrawn<-d$disqualified;d$phase<-"ready";d<-.ra_wl_settle(d)
 } else if(k=="change_declaration") {
  .ra_fields(p,list(participant=index,weight_kg=weight),"/params");i<-p$participant+1L
  if(!phase %in% c("ready","called") || is.null(d$declared[[i]]) || d$changes[[i]]>=2 || p$weight_kg<.ra_wl_minimum(d,i) || p$weight_kg==d$declared[[i]]) .ra_fail("At most two valid declaration changes are allowed per attempt")
  if(phase=="called" && i!=.ra_wl_next(d)+1L) .ra_fail("During a call, only the called lifter can change the declaration")
  d$declared[[i]]<-p$weight_kg;d$changes[[i]]<-d$changes[[i]]+1L;if(phase=="called") d$phase<-"ready"
 } else if(k=="call_next") {
  .ra_fields(p,list(),"/params");if(phase!="ready") .ra_fail("No lifter can be called now")
  d$bar_kg<-d$declared[[.ra_wl_next(d)+1L]];d$phase<-"called"
 } else if(k %in% c("judge_attempt","time_expired")) {
  .ra_fields(p,if(k=="judge_attempt") list(votes=list(type="array",minItems=3,maxItems=3,items=list(type="boolean"))) else list(),"/params")
  if(phase!="called") .ra_fail("Call the lifter before judging the attempt")
  i<-.ra_wl_next(d)+1L;passed<-k=="judge_attempt" && sum(unlist(p$votes))>=2L
  seq<-1L+sum(vapply(d$attempts,function(g) sum(lengths(g)),integer(1)))
  d$attempts[[lift]][[i]]<-c(d$attempts[[lift]][[i]],list(list(weight_kg=d$declared[[i]],passed=passed,sequence=seq)))
  d$declared[i]<-list(if(length(d$attempts[[lift]][[i]])==3L) NULL else d$declared[[i]]+as.integer(passed))
  d$changes[[i]]<-0L;d$phase<-"ready";d<-.ra_wl_settle(d)
 } else if(k=="withdraw_remaining") {
  .ra_fields(p,list(participant=index),"/params");i<-p$participant+1L
  if(!phase %in% c("ready","called") || is.null(d$declared[[i]])) .ra_fail("No remaining attempts can be withdrawn")
  if(phase=="called" && i!=.ra_wl_next(d)+1L) .ra_fail("Resolve the current called attempt first")
  d$declared[i]<-list(NULL);d$withdrawn[[i]]<-TRUE;d$phase<-"ready";d<-.ra_wl_settle(d)
 } else if(k=="disqualify") {
  .ra_fields(p,list(participant=index),"/params");i<-p$participant+1L
  if(!phase %in% c("ready","review") || d$disqualified[[i]]) .ra_fail("Official exclusion requires a ready platform or result review")
  d$disqualified[[i]]<-TRUE;d$withdrawn[[i]]<-TRUE;d$declared[i]<-list(NULL);if(phase=="ready") d<-.ra_wl_settle(d)
 } else if(k=="confirm_lift") {
  .ra_fields(p,list(),"/params");if(phase!="review") .ra_fail("Resolve every remaining attempt before confirmation")
  if(lift==2L) d$phase<-"finished" else {d$lift<-1L;d$phase<-"setup";d$bar_kg<-20L;d$declared<-rep(list(NULL),8);d$changes<-as.list(rep(0L,8));d$withdrawn<-as.list(rep(FALSE,8))}
 } else .ra_fail("Unknown weightlifting action")
 d
}
.ra_wl_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();index<-list(type="integer",minimum=0,maximum=7);weight<-list(type="integer",minimum=20,maximum=500);empty<-setNames(list(),character())
 if(phase=="setup") specs$declare_openers<-list(weights_kg=list(type="array",minItems=8,maxItems=8,items=weight))
 if(phase %in% c("ready","called")) {specs$change_declaration<-list(participant=index,weight_kg=weight);specs$withdraw_remaining<-list(participant=index)}
 if(phase=="ready") specs$call_next<-empty
 if(phase=="called") {specs$judge_attempt<-list(votes=list(type="array",minItems=3,maxItems=3,items=list(type="boolean")));specs$time_expired<-empty}
 if(phase %in% c("ready","review")) specs$disqualify<-list(participant=index)
 if(phase=="review") specs$confirm_lift<-empty
 i<-.ra_wl_next(d);previous<-NULL;seq<-0
 for(g in d$attempts) for(j in 1:8) for(a in g[[j]]) if(a$sequence>seq) {seq<-a$sequence;previous<-j-1L}
 seconds<-if(!is.null(i) && !is.null(previous) && previous==i && length(d$attempts[[d$lift+1L]][[i+1L]])) 120L else 60L
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Lift phase, ascending load order, remaining attempts and declaration limits must permit the event."),external_observations=list("Official declaration received within its deadline, three referee votes or official expiration of the attempt clock."))),constraints=list(phase=phase,lift=d$lift,next_participant=i,ordinary_attempt_seconds=seconds),completeness=FALSE,reason="official_weightlifting_events")
}
.ra_wl_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 1:8) {
  a<-.ra_wl_best(d$attempts[[1]][[i]]);b<-.ra_wl_best(d$attempts[[2]][[i]])
  if(a>0 && b>0 && !d$disqualified[[i]]) {achieved<-min(vapply(Filter(function(x) x$passed && x$weight_kg==b,d$attempts[[2]][[i]]),function(x) x$sequence,numeric(1)));ranked<-c(ranked,list(list(total=a+b,achieved=achieved,i=i)))}
 }
 if(length(ranked)) ranked<-ranked[order(-vapply(ranked,function(x) x$total,numeric(1)),vapply(ranked,function(x) x$achieved,numeric(1)))]
 winners<-if(length(ranked)) list(state$players[[ranked[[1]]$i]]$id) else list()
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(ranked)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) state$players[[i]]$id!=winners[[1]],1:8),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(ranked),function(j) list(player_id=state$players[[ranked[[j]]$i]]$id,place=j,total_kg=ranked[[j]]$total)),reason=if(done) "total_confirmed" else if(d$phase=="review") "lift_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official lift confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
