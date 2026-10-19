.ra_board_racing_schemas<-function(kind) {
 s<-.ra_sailing_schemas
 for(k in c("submit_result","amend_result")) {
  p<-s[[k]]$results$items$properties;p$status$values<-c(p$status$values,list("nsc"),if(kind=="kite") list("dct") else list());p$redress_denominator$maximum<-10L;p$redress_denominator$enum<-as.list(c(1L,2L,3L,4L,5L,6L,10L));s[[k]]$results$items$properties<-p
 }
 if(kind=="kite") for(k in c("tangle_penalty","cancel_current_tangle")) s[[k]]<-list(participant=list(type="integer",minimum=0,maximum=7));s
}
.ra_board_racing_normalize<-function(d,row,kind) {
 if(kind=="kite") for(i in 1:8) if(d$current_tangles[[i]]>0) {
  row[[i]]$status<-if(d$tangle_counts[[i]]>=2 || row[[i]]$status=="dne") "dne" else "dct"
  for(k in c("order","redress_numerator","redress_denominator")) row[[i]][k]<-list(NULL)
 };row
}
.ra_board_racing_validate<-function(d,kind) {
 .ra_sailing_validate(d);rows<-c(d$races,if(!is.null(d$pending)) list(d$pending) else list())
 for(row in rows) for(r in row) if(r$status=="redress" && r$redress_numerator>9*r$redress_denominator) .ra_fail("Board-series redress lies between one and nine points",code="INVALID_STATE")
 if(kind=="kite") {
  if(any(unlist(d$tangle_counts)<unlist(d$current_tangles))) .ra_fail("Current-race tangle counts cannot exceed event totals",code="INVALID_STATE")
  if(d$phase %in% c("ready","finished") && any(unlist(d$current_tangles)>0)) .ra_fail("No unresolved race tangle counts outside an active race",code="INVALID_STATE")
  if(!is.null(d$pending)) {
   if(!identical(d$pending,.ra_board_racing_normalize(d,d$pending,kind))) .ra_fail("Race result does not apply its tangle sanction",code="INVALID_STATE")
   if(any(vapply(1:8,function(i) d$pending[[i]]$status=="dct" && d$current_tangles[[i]]==0,logical(1)))) .ra_fail("DCT requires a recorded tangle ruling",code="INVALID_STATE")
  }
  for(i in 1:8) {firsts<-sum(vapply(d$races,function(row) row[[i]]$status=="dct",logical(1)));if(firsts>1 || firsts>d$tangle_counts[[i]]) .ra_fail("Repeated tangle disqualification must be non-excludable",code="INVALID_STATE")}
 };invisible(TRUE)
}
.ra_board_racing_available<-function(d,kind) {
 names<-.ra_sailing_available(d);if(kind=="kite" && d$phase %in% c("racing","review")) names<-c(names,"tangle_penalty","cancel_current_tangle");names
}
.ra_board_racing_apply<-function(state,action,kind) {
 d<-state$data;k<-action$type;p<-action$params
 if(!(k %in% .ra_board_racing_available(d,kind))) .ra_fail("Action is not permitted in this board-race phase")
 .ra_fields(p,.ra_board_racing_schemas(kind)[[k]],"/params")
 if(k=="start_race") d$phase<-"racing" else if(k=="abandon_race") {
  d$phase<-"ready";d["pending"]<-list(NULL);if(kind=="kite") d$current_tangles<-as.list(rep(0L,8))
 } else if(k %in% c("submit_result","amend_result")) {d$pending<-.ra_board_racing_normalize(d,p$results,kind);d$phase<-"review"} else if(k=="confirm_race") {
  d$races[[length(d$races)+1L]]<-d$pending;d["pending"]<-list(NULL);d$phase<-if(length(d$races)==5) "finished" else "ready";if(kind=="kite") d$current_tangles<-as.list(rep(0L,8))
 } else if(k=="tangle_penalty") {
  i<-p$participant+1L;d$tangle_counts[[i]]<-d$tangle_counts[[i]]+1L;d$current_tangles[[i]]<-d$current_tangles[[i]]+1L
  if(!is.null(d$pending)) d$pending<-.ra_board_racing_normalize(d,d$pending,kind)
 } else if(k=="cancel_current_tangle") {
  i<-p$participant+1L;if(d$current_tangles[[i]]==0) .ra_fail("Only a tangle ruling from the unresolved race can be cancelled")
  d$current_tangles[[i]]<-d$current_tangles[[i]]-1L;d$tangle_counts[[i]]<-d$tangle_counts[[i]]-1L;d$phase<-"racing";d["pending"]<-list(NULL)
 };d
}
.ra_board_racing_key<-function(d,i,kind) {
 result<-.ra_sailing_key(d,i)
 if(kind=="windsurf") result$key<-c(result$key[[1L]],if(is.null(result$discard)) 0 else result$scores[[result$discard+1L]],sort(result$scores),rev(result$scores))
 result
}
.ra_board_racing_legal<-function(state,kind) {
 d<-state$data;s<-.ra_board_racing_schemas(kind);constraints<-list(phase=d$phase,confirmed_races=length(d$races),score_unit_denominator=60,provisional_points_units=if(!is.null(d$pending)) .ra_sailing_points(d$pending) else NULL)
 if(kind=="kite") constraints$tangle_counts<-d$tangle_counts
 list(mode="event_types",actions=list(),action_types=lapply(.ra_board_racing_available(d,kind),function(k) list(type=k,params_schema=s[[k]],preconditions=list("A started race and complete reviewed result are required before confirmation."),external_observations=list("Official finishing places, exclusion codes, redress and board-specific rulings."))),constraints=constraints,completeness=FALSE,reason="official_board_series")
}
.ra_board_racing_outcome<-function(state,pack,kind) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) {
  ranked<-lapply(1:8,function(i) c(list(i=i),.ra_board_racing_key(d,i,kind)))
  for(j in 2:8) {a<-j;while(a>1 && .ra_sailing_compare(ranked[[a]]$key,ranked[[a-1L]]$key)<0) {tmp<-ranked[[a]];ranked[[a]]<-ranked[[a-1L]];ranked[[a-1L]]<-tmp;a<-a-1L}}
 }
 winners<-lapply(Filter(function(r) .ra_sailing_compare(r$key,ranked[[1L]]$key)==0,ranked),function(r) state$players[[r$i]]$id)
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) "ranking" else "none",winners=winners,losers=if(done) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_sailing_compare(other$key,r$key)<0,logical(1))),points_units=r$key[1L],score_unit_denominator=60,discarded_race=r$discard)),reason=if(done) "series_confirmed" else if(d$phase=="review") "race_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official race confirmation.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
