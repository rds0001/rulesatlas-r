.ra_rally_config<-list(heats=3L,quantum_us=100000)
.ra_rally_road_penalty<-function(d,i) sum(vapply(d$checkins,function(row) {v<-row[[i]];if(is.null(v)) 0 else if(v>=0) v*10 else -v*60},numeric(1)))
.ra_rally_overdue<-function(d,i,h) {values<-lapply(d$checkins[seq_len(h)],function(row) row[[i]]);v<-values[[h]];!is.null(v) && (v>15 || sum(pmax(0,unlist(values)))>30)}
.ra_rally_validate<-function(d) {
 .ra_th_validate(d,.ra_rally_config)
 for(h in seq_len(3)) for(i in seq_len(8)) {
  v<-d$checkins[[h]][[i]];r<-d$results[[h]][[i]]
  if(h>d$heat+1L && !is.null(v)) .ra_fail("Future road control already recorded",code="INVALID_STATE")
  if(!is.null(v) && r$status=="not_eligible") .ra_fail("Ineligible stage cannot have a road control",code="INVALID_STATE")
  if(r$status=="finished" && is.null(v)) .ra_fail("Stage finish requires its road-control check-in",code="INVALID_STATE")
  if(.ra_rally_overdue(d,i,h) && r$status!="dq") .ra_fail("Maximum lateness requires exclusion",code="INVALID_STATE")
 }
 if(d$phase=="active" && is.null(d$checkins[[d$heat+1L]][[.ra_th_next(d)+1L]])) .ra_fail("A stage cannot start without check-in",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_rally_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;h<-d$heat+1L;i<-.ra_th_next(d);ix<-list(type="integer",minimum=0,maximum=7)
 if(k %in% c("check_in","correct_check_in")) {
  schema<-list(delta_minutes=list(type="integer",minimum=-60,maximum=60));if(k=="correct_check_in") schema$participant<-ix
  .ra_fields(p,schema,"/params")
  if(k=="check_in") {
   if(d$phase!="run" || !is.null(d$checkins[[h]][[i+1L]])) .ra_fail("Check in once before the next stage start")
  } else {i<-p$participant;if(d$phase!="review" || is.null(d$checkins[[h]][[i+1L]])) .ra_fail("Correct an existing current-stage control during review")}
  d$checkins[[h]][[i+1L]]<-p$delta_minutes
  if(.ra_rally_overdue(d,i+1L,h)) {d$results[[h]][[i+1L]]<-list(status="dq",time_us=NULL);if(d$phase=="run") d$phase<-if(is.null(.ra_th_next(d))) "review" else "run"}
  return(d)
 }
 if(k %in% c("assess_penalty","correct_penalty")) {
  .ra_fields(p,list(participant=ix,seconds=list(type="integer",minimum=if(k=="assess_penalty") 1 else 0,maximum=100000)),"/params");i<-p$participant+1L
  if(k=="correct_penalty" && d$phase!="review") .ra_fail("Correct other penalties only during review")
  d$other_penalty_seconds[[i]]<-if(k=="assess_penalty") d$other_penalty_seconds[[i]]+p$seconds else p$seconds
  if(d$other_penalty_seconds[[i]]>100000) .ra_fail("Other penalties exceed the supported bound")
  return(d)
 }
 if(k=="start_next" && d$phase=="run" && is.null(d$checkins[[h]][[i+1L]])) .ra_fail("Check in before starting the stage")
 .ra_th_apply(state,action,.ra_rally_config)
}
.ra_rally_legal<-function(state) {
 result<-.ra_th_legal(state,.ra_rally_config);d<-state$data;schemas<-list();ix<-list(type="integer",minimum=0,maximum=7)
 if(d$phase=="run") schemas$check_in<-list(delta_minutes=list(type="integer",minimum=-60,maximum=60))
 if(d$phase=="review") {schemas$correct_check_in<-list(participant=ix,delta_minutes=list(type="integer",minimum=-60,maximum=60));schemas$correct_penalty<-list(participant=ix,seconds=list(type="integer",minimum=0,maximum=100000))}
 if(d$phase!="finished") schemas$assess_penalty<-list(participant=ix,seconds=list(type="integer",minimum=1,maximum=100000))
 for(k in names(schemas)) result$action_types[[length(result$action_types)+1L]]<-list(type=k,params_schema=schemas[[k]],preconditions=list("Current control, stage eligibility and review phase must permit the event."),external_observations=list("Official signed whole-minute control deviation or additional time sanction."))
 result$constraints$road_penalty_seconds<-lapply(1:8,function(i) .ra_rally_road_penalty(d,i));result$constraints$maximum_single_lateness_minutes<-15L;result$constraints$maximum_total_lateness_minutes<-30L;result
}
.ra_rally_key<-function(d,i) {times<-vapply(d$results,function(row) row[[i]]$time_us,numeric(1));c(sum(times)+1e6*(.ra_rally_road_penalty(d,i)+d$other_penalty_seconds[[i]]),times)}
.ra_rally_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";ranked<-list()
 if(done) for(i in seq_len(8)) if(all(vapply(d$results,function(row) row[[i]]$status=="finished",logical(1)))) ranked[[length(ranked)+1L]]<-list(key=.ra_rally_key(d,i),i=i)
 if(length(ranked)>0) {mat<-do.call(rbind,lapply(ranked,function(r) c(r$key,r$i)));ranked<-ranked[do.call(order,as.data.frame(mat))]}
 winners<-lapply(Filter(function(r) all(r$key==ranked[[1]]$key),ranked),function(r) state$players[[r$i]]$id)
 placements<-lapply(ranked,function(r) list(player_id=state$players[[r$i]]$id,place=1L+sum(vapply(ranked,function(other) .ra_ps_compare(other$key,r$key)<0,logical(1))),total_time_us=r$key[1],road_penalty_seconds=.ra_rally_road_penalty(d,r$i),other_penalty_seconds=d$other_penalty_seconds[[r$i]]))
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(length(ranked)>0) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)>0) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(done) "rally_confirmed" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
