.ra_field_attempt <- list(kind=list(type="string",enum=list("valid","foul","pass")),distance_mm=list(type="integer",minimum=0,maximum=200000))
.ra_field_key <- function(attempts) {values<-vapply(Filter(function(a) a$kind=="valid",attempts),function(a) a$distance_mm,numeric(1));c(sort(values,decreasing=TRUE),rep(0,3-length(values)))}
.ra_field_auto <- function(d,p) !is.null(d$qualifying_mm) && .ra_field_key(d$attempts[[p]])[1]>=d$qualifying_mm
.ra_field_advance <- function(d) {while(d$current<36 && (d$status[[d$current%%12+1L]]!="active" || .ra_field_auto(d,d$current%%12+1L))) d$current<-d$current+1L;d}
.ra_field_validate <- function(d) {
  if(!d$configured && (d$current!=0 || any(lengths(d$attempts)>0) || !is.null(d$qualifying_mm) || d$places!=8 || any(unlist(d$status)!="active"))) .ra_fail("Unconfigured event contains results",code="INVALID_STATE")
  if(!is.null(d$qualifying_mm) && d$qualifying_mm%%10!=0) .ra_fail("Qualification mark must use ten-millimeter units",code="INVALID_STATE")
  for(p in 1:12) {
    attempts<-d$attempts[[p]];available<-sum((0:2)*12+p-1L<d$current)
    if(length(attempts)>available) .ra_fail("Attempt belongs to a future round",code="INVALID_STATE")
    for(i in seq_along(attempts)) {
      a<-attempts[[i]]
      if((a$kind=="valid" && (a$distance_mm<=0 || a$distance_mm%%10!=0)) || (a$kind!="valid" && a$distance_mm!=0)) .ra_fail("Invalid official mark",code="INVALID_STATE")
      if(!is.null(d$qualifying_mm) && i>1 && any(vapply(attempts[seq_len(i-1L)],function(x) x$distance_mm>=d$qualifying_mm,logical(1)))) .ra_fail("Attempts continued after automatic qualification",code="INVALID_STATE")
    }
    if(d$status[[p]]=="active" && !.ra_field_auto(d,p) && length(attempts)!=available) .ra_fail("Missing scheduled attempt",code="INVALID_STATE")
  }
  if(d$current<36 && (d$status[[d$current%%12+1L]]!="active" || .ra_field_auto(d,d$current%%12+1L))) .ra_fail("Inactive athlete must be skipped",code="INVALID_STATE")
}
.ra_field_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(!d$configured) {
    if(kind!="configure") .ra_fail("Configure qualification first")
    .ra_fields(p,list(places=list(type="integer",minimum=1,maximum=12),qualifying_mm=list(type="integer",minimum=10,maximum=200000,nullable=TRUE)),"/params")
    d$places<-p$places;d["qualifying_mm"]<-list(p$qualifying_mm);d$configured<-TRUE;return(d)
  }
  if(kind %in% c("withdraw","disqualify")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=11)),"/params");t<-p$player+1L
    if(d$status[[t]]=="dq" || (kind=="withdraw" && d$status[[t]]!="active")) .ra_fail("Athlete is already inactive")
    d$status[[t]]<-if(kind=="disqualify") "dq" else "withdrawn";return(.ra_field_advance(d))
  }
  if(kind!="attempt") .ra_fail("Unknown action")
  .ra_fields(p,.ra_field_attempt,"/params");t<-d$current%%12+1L
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  d$attempts[[t]][[length(d$attempts[[t]])+1L]]<-p;d$current<-d$current+1L;.ra_field_advance(d)
}
.ra_field_legal <- function(state) {
  d<-state$data
  if(!d$configured) {
    types<-list(list(type="configure",params_schema=list(places=list(type="integer",minimum=1,maximum=12),qualifying_mm=list(type="integer",minimum=10,maximum=200000,nullable=TRUE)),preconditions=list("The automatic qualification mark is null or a multiple of ten millimeters."),external_observations=list("Published qualifying standard and minimum final field size.")));constraints<-structure(list(),names=character())
  } else {
    types<-list(list(type="attempt",params_schema=.ra_field_attempt,preconditions=list("Valid distances are positive multiples of ten millimeters; foul and pass use zero."),external_observations=list("Official valid mark, foul ruling or voluntary pass.")))
    for(k in c("withdraw","disqualify")) types[[length(types)+1L]]<-list(type=k,params_schema=list(player=list(type="integer",minimum=0,maximum=11)),preconditions=list("Athlete status permits the administrative decision."),external_observations=list("Official withdrawal or disqualification."))
    constraints<-list(player=d$current%%12,round=d$current%/%12+1L)
  }
  list(mode="event_types",actions=list(),action_types=types,constraints=constraints,completeness=FALSE,reason="externally_adjudicated_distance_attempts")
}
.ra_field_greater <- function(a,b) {j<-which(a!=b);length(j)>0 && a[j[1]]>b[j[1]]}
.ra_field_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$configured && d$current==36
  eligible<-if(terminal) Filter(function(p) d$status[[p]]=="active" && .ra_field_key(d$attempts[[p]])[1]>0,1:12) else integer()
  keys<-lapply(eligible,function(p) .ra_field_key(d$attempts[[p]]))
  if(length(eligible)) {order<-order(-vapply(keys,function(k) k[1],numeric(1)),-vapply(keys,function(k) k[2],numeric(1)),-vapply(keys,function(k) k[3],numeric(1)),eligible);eligible<-eligible[order];keys<-keys[order]}
  cutoff<-if(length(keys)) keys[[min(d$places,length(keys))]] else NULL
  placements<-lapply(seq_along(eligible),function(i) {p<-eligible[i];k<-keys[[i]];list(player_id=state$players[[p]]$id,place=1L+sum(vapply(keys,function(other) .ra_field_greater(other,k),logical(1))),best_mm=k[1],qualified=.ra_field_auto(d,p) || !.ra_field_greater(cutoff,k))})
  winners<-if(length(keys)) lapply(eligible[vapply(keys,function(k) all(k==keys[[1]]),logical(1))],function(p) state$players[[p]]$id) else list()
  losers<-if(length(keys)) lapply(eligible[vapply(keys,function(k) any(k!=keys[[1]]),logical(1))],function(p) state$players[[p]]$id) else list()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(length(eligible)) "ranking" else "no_contest"} else "none",winners=winners,losers=losers,placements=placements,reason=if(terminal) "qualification_completed" else if(d$configured) "ongoing" else "awaiting_configuration",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
