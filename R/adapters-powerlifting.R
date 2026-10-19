.ra_pl_best<-function(a) max(c(0,vapply(Filter(function(x) x$passed,a),function(x) x$weight_halfkg,numeric(1))))
.ra_pl_candidates<-function(d) which(!vapply(d$declared,is.null,logical(1)))-1L
.ra_pl_next<-function(d) {c<-.ra_pl_candidates(d);if(!length(c)) return(NULL);c[order(vapply(c,function(i) length(d$attempts[[d$lift+1L]][[i+1L]]),integer(1)),vapply(c,function(i) d$declared[[i+1L]],numeric(1)),match(c,unlist(d$lots)))][1]}
.ra_pl_minimum<-function(d,i) {a<-d$attempts[[d$lift+1L]][[i]];if(length(a)) tail(a,1)[[1]]$weight_halfkg+5L*as.integer(tail(a,1)[[1]]$passed) else 40L}
.ra_pl_settle<-function(d) {if(!length(.ra_pl_candidates(d))) d$phase<-"review";d}
.ra_pl_validate<-function(d) {
 serials<-numeric()
 for(lift in 1:3) for(a in d$attempts[[lift]]) {
  last<-40
  for(x in a) {if(x$weight_halfkg %% 5!=0 || x$weight_halfkg<last) .ra_fail("Attempt weight falls below the required next load",code="INVALID_STATE");last<-x$weight_halfkg+5L*as.integer(x$passed);serials<-c(serials,x$sequence)}
  if(lift>d$lift+1L && length(a)) .ra_fail("Future lift already contains attempts",code="INVALID_STATE")
 }
 if(!identical(as.integer(sort(serials)),seq_along(serials))) .ra_fail("Attempt sequence must be unique and contiguous",code="INVALID_STATE")
 if(!identical(as.integer(sort(unlist(d$lots))),0:7)) .ra_fail("Lot order must be a permutation",code="INVALID_STATE")
 if(d$phase!="setup" && any(vapply(d$bodyweight_g,is.null,logical(1)))) .ra_fail("Official weigh-in is required",code="INVALID_STATE")
 if(d$phase=="setup") {
  if(d$bar_halfkg!=40 || any(lengths(d$attempts[[d$lift+1L]])>0) || any(!vapply(d$declared,is.null,logical(1))) || any(unlist(d$changes)!=0) || any(unlist(d$withdrawn))) .ra_fail("Lift setup must precede declarations",code="INVALID_STATE")
 } else {
  for(i in 1:8) {
   a<-d$attempts[[d$lift+1L]][[i]]
   if(d$disqualified[[i]] && !d$withdrawn[[i]]) .ra_fail("Disqualified entrants cannot continue",code="INVALID_STATE")
   if(is.null(d$declared[[i]])!=(length(a)==3L || d$withdrawn[[i]])) .ra_fail("Remaining attempts require a declared weight",code="INVALID_STATE")
   if(!is.null(d$declared[[i]]) && (d$declared[[i]] %% 5!=0 || d$declared[[i]]<.ra_pl_minimum(d,i))) .ra_fail("Declared weight is below the current minimum",code="INVALID_STATE")
  }
  if((d$phase %in% c("ready","called"))!=(length(.ra_pl_candidates(d))>0)) .ra_fail("Phase and available attempts disagree",code="INVALID_STATE")
 }
 if(d$phase=="finished" && d$lift!=2L) .ra_fail("All three lifts must be completed",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_pl_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;phase<-d$phase;lift<-d$lift+1L;index<-list(type="integer",minimum=0,maximum=7);weight<-list(type="integer",minimum=40,maximum=1000)
 if(k=="record_weigh_in") {
  .ra_fields(p,list(bodyweight_g=list(type="array",minItems=8,maxItems=8,items=list(type="integer",minimum=20000,maximum=300000))),"/params")
  if(phase!="setup" || lift!=1L) .ra_fail("Record official bodyweights before the first lift")
  d$bodyweight_g<-p$bodyweight_g
 } else if(k=="declare_openers") {
  .ra_fields(p,list(weights_halfkg=list(type="array",minItems=8,maxItems=8,items=weight)),"/params")
  if(phase!="setup") .ra_fail("Opening declarations are already recorded")
  if(any(vapply(d$bodyweight_g,is.null,logical(1))) || any(unlist(p$weights_halfkg) %% 5!=0)) .ra_fail("Weigh-in and 2.5-kilogram loading increments are required")
  d$declaration_fixed<-as.list(rep(TRUE,8))
  d$declared<-lapply(1:8,function(i) if(d$disqualified[[i]]) NULL else p$weights_halfkg[[i]]);d$withdrawn<-d$disqualified;d$phase<-"ready";d<-.ra_pl_settle(d)
 } else if(k=="change_declaration") {
  .ra_fields(p,list(participant=index,weight_halfkg=weight),"/params");i<-p$participant+1L
  n<-length(d$attempts[[lift]][[i]]);limit<-if(n==0L) 1L else if(lift==3L && n==2L) 2L else 0L
  if(phase!="ready" || is.null(d$declared[[i]]) || (d$declaration_fixed[[i]] && d$changes[[i]]>=limit) || p$weight_halfkg %% 5!=0 || p$weight_halfkg<.ra_pl_minimum(d,i) || p$weight_halfkg==d$declared[[i]]) .ra_fail("Declaration timing, change limit or loading increment is invalid")
  d$declared[[i]]<-p$weight_halfkg;if(d$declaration_fixed[[i]]) d$changes[[i]]<-d$changes[[i]]+1L;d$declaration_fixed[[i]]<-TRUE
 } else if(k=="call_next") {
  .ra_fields(p,list(),"/params");if(phase!="ready") .ra_fail("No lifter can be called now")
  d$declaration_fixed[[.ra_pl_next(d)+1L]]<-TRUE;d$bar_halfkg<-d$declared[[.ra_pl_next(d)+1L]];d$phase<-"called"
 } else if(k %in% c("judge_attempt","time_expired")) {
  .ra_fields(p,if(k=="judge_attempt") list(votes=list(type="array",minItems=3,maxItems=3,items=list(type="boolean"))) else list(),"/params")
  if(phase!="called") .ra_fail("Call the lifter before judging the attempt")
  i<-.ra_pl_next(d)+1L;passed<-k=="judge_attempt" && sum(unlist(p$votes))>=2L
  seq<-1L+sum(vapply(d$attempts,function(g) sum(lengths(g)),integer(1)))
  d$attempts[[lift]][[i]]<-c(d$attempts[[lift]][[i]],list(list(weight_halfkg=d$declared[[i]],passed=passed,sequence=seq)))
  d$declared[i]<-list(if(length(d$attempts[[lift]][[i]])==3L) NULL else d$declared[[i]]+5L*as.integer(passed))
  d$declaration_fixed[[i]]<-FALSE;d$changes[[i]]<-0L;d$phase<-"ready";d<-.ra_pl_settle(d)
 } else if(k=="withdraw_remaining") {
  .ra_fields(p,list(participant=index),"/params");i<-p$participant+1L
  if(!phase %in% c("ready","called") || is.null(d$declared[[i]])) .ra_fail("No remaining attempts can be withdrawn")
  if(phase=="called" && i!=.ra_pl_next(d)+1L) .ra_fail("Resolve the current called attempt first")
  d$declared[i]<-list(NULL);d$withdrawn[[i]]<-TRUE;d$phase<-"ready";d<-.ra_pl_settle(d)
 } else if(k=="disqualify") {
  .ra_fields(p,list(participant=index),"/params");i<-p$participant+1L
  if(!phase %in% c("ready","review") || d$disqualified[[i]]) .ra_fail("Official exclusion requires a ready platform or result review")
  d$disqualified[[i]]<-TRUE;d$withdrawn[[i]]<-TRUE;d$declared[i]<-list(NULL);if(phase=="ready") d<-.ra_pl_settle(d)
 } else if(k=="confirm_lift") {
  .ra_fields(p,list(),"/params");if(phase!="review") .ra_fail("Resolve every remaining attempt before confirmation")
  if(lift==3L) d$phase<-"finished" else {d$lift<-lift;d$phase<-"setup";d$bar_halfkg<-40L;d$declared<-rep(list(NULL),8);d$changes<-as.list(rep(0L,8));d$withdrawn<-as.list(rep(FALSE,8))}
 } else .ra_fail("Unknown powerlifting action")
 d
}
.ra_pl_legal<-function(state) {
 d<-state$data;phase<-d$phase;specs<-list();index<-list(type="integer",minimum=0,maximum=7);weight<-list(type="integer",minimum=40,maximum=1000);empty<-setNames(list(),character())
 if(phase=="setup" && d$lift==0L) specs$record_weigh_in<-list(bodyweight_g=list(type="array",minItems=8,maxItems=8,items=list(type="integer",minimum=20000,maximum=300000)))
 if(phase=="setup") specs$declare_openers<-list(weights_halfkg=list(type="array",minItems=8,maxItems=8,items=weight))
 if(phase=="ready") {specs$change_declaration<-list(participant=index,weight_halfkg=weight);specs$withdraw_remaining<-list(participant=index)}
 if(phase=="ready") specs$call_next<-empty
 if(phase=="called") specs$withdraw_remaining<-list(participant=index)
 if(phase=="called") {specs$judge_attempt<-list(votes=list(type="array",minItems=3,maxItems=3,items=list(type="boolean")));specs$time_expired<-empty}
 if(phase %in% c("ready","review")) specs$disqualify<-list(participant=index)
 if(phase=="review") specs$confirm_lift<-empty
 i<-.ra_pl_next(d);seconds<-60L
 list(mode="event_types",actions=list(),action_types=lapply(names(specs),function(k) list(type=k,params_schema=specs[[k]],preconditions=list("Lift phase, round and load order, remaining attempts and declaration limits must permit the event."),external_observations=list("Official declaration received within its deadline, three final referee votes or official expiration of the attempt clock."))),constraints=list(phase=phase,lift=d$lift,next_participant=i,ordinary_attempt_seconds=seconds),completeness=FALSE,reason="official_powerlifting_events")
}
.ra_pl_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in 1:8) {
  lifts<-vapply(d$attempts,function(g) .ra_pl_best(g[[i]]),numeric(1))
  if(all(lifts>0) && !d$disqualified[[i]]) {achieved<-min(vapply(Filter(function(x) x$passed && x$weight_halfkg==lifts[3],d$attempts[[3]][[i]]),function(x) x$sequence,numeric(1)));ranked<-c(ranked,list(list(total=sum(lifts),bodyweight=d$bodyweight_g[[i]],achieved=achieved,i=i)))}
 }
 if(length(ranked)) ranked<-ranked[order(-vapply(ranked,function(x) x$total,numeric(1)),vapply(ranked,function(x) x$bodyweight,numeric(1)),vapply(ranked,function(x) x$achieved,numeric(1)))]
 winners<-if(length(ranked)) list(state$players[[ranked[[1]]$i]]$id) else list()
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(length(ranked)) "ranking" else "no_contest" else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(i) state$players[[i]]$id!=winners[[1]],1:8),function(i) state$players[[i]]$id) else list(),placements=lapply(seq_along(ranked),function(j) list(player_id=state$players[[ranked[[j]]$i]]$id,place=j,total_halfkg=ranked[[j]]$total)),reason=if(done) "total_confirmed" else if(d$phase=="review") "lift_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official lift confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
