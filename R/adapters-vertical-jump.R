.ra_vertical_heights <- list(type="array",minItems=1,maxItems=50,items=list(type="integer",minimum=10,maximum=10000))
.ra_vertical_failures <- function(rows) {count<-0L;for(row in rows) for(v in row) {if(v=="clear") count<-0L else if(v=="fail") count<-count+1L};count}
.ra_vertical_done <- function(d,p,h) {row<-d$attempts[[p]][[h]];d$status[[p]]!="active" || .ra_vertical_failures(d$attempts[[p]])>=3 || (length(row)>0 && tail(unlist(row),1) %in% c("clear","pass"))}
.ra_vertical_advance <- function(d) {
  while(d$height<length(d$heights_mm)) {
    h<-d$height+1L
    for(step in 0:11) {p<-(d$turn+step)%%12+1L;if(!.ra_vertical_done(d,p,h)) {d$turn<-p-1L;return(d)}}
    d$height<-d$height+1L;d$turn<-0L
  }
  d
}
.ra_vertical_rank <- function(d,p) {
  cleared<-which(vapply(d$attempts[[p]],function(row) "clear" %in% unlist(row),logical(1)))
  if(!length(cleared)) return(c(0,0,0))
  h<-tail(cleared,1);c(d$heights_mm[[h]],-length(d$attempts[[p]][[h]]),-sum(unlist(d$attempts[[p]][seq_len(h)])=="fail"))
}
.ra_vertical_validate <- function(d) {
  if(!d$configured) {if(length(d$heights_mm)+sum(lengths(d$attempts))>0 || d$height!=0 || d$turn!=0 || d$places!=8 || any(unlist(d$status)!="active")) .ra_fail("Unconfigured event contains results",code="INVALID_STATE");return(invisible(NULL))}
  heights<-unlist(d$heights_mm);n<-length(heights)
  if(n==0 || any(heights%%10!=0) || any(diff(heights)<=0)) .ra_fail("Heights must increase in ten-millimeter units",code="INVALID_STATE")
  if(d$height>n || any(lengths(d$attempts)!=n)) .ra_fail("Height dimensions mismatch",code="INVALID_STATE")
  for(p in 1:12) {
    rows<-d$attempts[[p]];fails<-0L
    for(h in seq_along(rows)) {
      row<-rows[[h]]
      if(h-1L>d$height && length(row)>0) .ra_fail("Future height contains attempts",code="INVALID_STATE")
      for(j in seq_along(row)) {
        if(fails>=3 || (j>1 && row[[j-1L]] %in% c("clear","pass"))) .ra_fail("Attempt continued after elimination or completed height",code="INVALID_STATE")
        if(row[[j]]=="clear") fails<-0L else if(row[[j]]=="fail") fails<-fails+1L
      }
      if(h-1L<d$height && d$status[[p]]=="active" && fails<3 && (!length(row) || !tail(unlist(row),1) %in% c("clear","pass"))) .ra_fail("Earlier height was not completed",code="INVALID_STATE")
    }
  }
  if(d$height<n && .ra_vertical_done(d,d$turn+1L,d$height+1L)) .ra_fail("Current athlete must be eligible",code="INVALID_STATE")
  if(d$height==n && d$turn!=0) .ra_fail("Completed qualification has no current athlete",code="INVALID_STATE")
}
.ra_vertical_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(!d$configured) {
    if(kind!="configure") .ra_fail("Supply the height schedule first")
    .ra_fields(p,list(heights_mm=.ra_vertical_heights,places=list(type="integer",minimum=1,maximum=12)),"/params")
    d$configured<-TRUE;d$heights_mm<-p$heights_mm;d$places<-p$places;d$attempts<-lapply(1:12,function(i) lapply(p$heights_mm,function(h) list()));return(d)
  }
  if(kind %in% c("withdraw","disqualify")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=11)),"/params");t<-p$player+1L
    if(d$status[[t]]=="dq" || (kind=="withdraw" && d$status[[t]]!="active")) .ra_fail("Athlete is already inactive")
    d$status[[t]]<-if(kind=="disqualify") "dq" else "withdrawn";return(.ra_vertical_advance(d))
  }
  if(kind!="attempt") .ra_fail("Unknown action")
  .ra_fields(p,list(result=list(type="string",enum=list("clear","fail","pass"))),"/params");t<-d$turn+1L;h<-d$height+1L
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  row<-d$attempts[[t]][[h]];row[[length(row)+1L]]<-p$result;d$attempts[[t]][[h]]<-row;d$turn<-t%%12;.ra_vertical_advance(d)
}
.ra_vertical_legal <- function(state) {
  d<-state$data
  if(!d$configured) {schemas<-list(configure=list(heights_mm=.ra_vertical_heights,places=list(type="integer",minimum=1,maximum=12)));constraints<-structure(list(),names=character())} else {
    schemas<-list(attempt=list(result=list(type="string",enum=list("clear","fail","pass"))),withdraw=list(player=list(type="integer",minimum=0,maximum=11)),disqualify=list(player=list(type="integer",minimum=0,maximum=11)))
    constraints<-list(player=d$turn,height_mm=d$heights_mm[[d$height+1L]],consecutive_failures=.ra_vertical_failures(d$attempts[[d$turn+1L]]))
  }
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("The current athlete, height schedule and carried failure count must permit the action."),external_observations=list("Official clearance, failed attempt, voluntary height pass or administrative ruling.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=constraints,completeness=FALSE,reason="externally_adjudicated_vertical_attempts")
}
.ra_vertical_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$configured && d$height==length(d$heights_mm)
  eligible<-if(terminal) Filter(function(p) d$status[[p]]=="active" && .ra_vertical_rank(d,p)[1]>0,1:12) else integer();keys<-lapply(eligible,function(p) .ra_vertical_rank(d,p))
  if(length(eligible)) {order<-order(-vapply(keys,function(k) k[1],numeric(1)),-vapply(keys,function(k) k[2],numeric(1)),-vapply(keys,function(k) k[3],numeric(1)),eligible);eligible<-eligible[order];keys<-keys[order]}
  cutoff<-if(length(keys)) keys[[min(d$places,length(keys))]] else NULL
  placements<-lapply(seq_along(eligible),function(i) {p<-eligible[i];k<-keys[[i]];list(player_id=state$players[[p]]$id,place=1L+sum(vapply(keys,function(other) .ra_field_greater(other,k),logical(1))),best_mm=k[1],attempts_at_best=-k[2],failures_through_best=-k[3],qualified=k[1]==tail(unlist(d$heights_mm),1) || !.ra_field_greater(cutoff,k))})
  winners<-if(length(keys)) lapply(eligible[vapply(keys,function(k) all(k==keys[[1]]),logical(1))],function(p) state$players[[p]]$id) else list()
  losers<-if(length(keys)) lapply(eligible[vapply(keys,function(k) any(k!=keys[[1]]),logical(1))],function(p) state$players[[p]]$id) else list()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(length(eligible)) "ranking" else "no_contest"} else "none",winners=winners,losers=losers,placements=placements,reason=if(terminal) "qualification_completed" else if(d$configured) "ongoing" else "awaiting_configuration",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
