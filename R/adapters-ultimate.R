.ra_ultimate_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);team<-i(0,1);slot<-i(0,27);empty<-setNames(list(),character());vector<-list(type="array",minItems=1,maxItems=7,items=slot)
 s<-list(set_lineup=list(team=team,roster_size=i(7,28),active=list(type="array",minItems=7,maxItems=7,items=slot)),start_game=list(first_receiving=team),start_point=empty,resume_point=empty,begin_second_half=empty,goal=list(team=team,scorer=slot),substitute=list(team=team,out=vector),authorized_replacement=list(team=team,out=slot),matching_replacement=list(team=team,out=slot),timeout=list(team=team),end_timeout=empty,concede=list(team=team));s$substitute[["in"]]<-vector;s$authorized_replacement[["in"]]<-slot;s$matching_replacement[["in"]]<-slot;s
}
.ra_ultimate_active<-function(t) as.list(which(unlist(t$status)=="active")-1L)
.ra_ultimate_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in d$teams) {
  if(length(t$blocked_this_point)!=length(t$status) || length(.ra_ultimate_active(t))!=7) bad("Each team requires seven active players and matching eligibility vectors")
  if(any(vapply(.ra_ultimate_active(t),function(i) t$blocked_this_point[[i+1L]],logical(1)))) bad("An officially replaced player cannot return during the same point")
 }
 scores<-unlist(d$scores)
 if(d$phase=="pre_game" && (any(scores!=0) || d$half!=1 || any(vapply(d$teams,function(t) t$timeouts_used>0,logical(1))))) bad("A new game starts with empty scoring and timeouts")
 if(all(scores==15)) bad("The game ends when the first team reaches fifteen")
 if(d$half==1 && min(scores)>=8) bad("Halftime begins when the first team reaches eight")
 if(d$phase=="halftime" && (d$half!=1 || max(scores)!=8)) bad("Halftime begins at the first eight-point score")
 if(!(d$phase %in% c("halftime","finished")) && d$half==1 && max(scores)>=8) bad("The first half cannot continue beyond eight points")
 if(d$half==2 && max(scores)<8) bad("The second half requires a completed first half")
 if(d$phase=="finished") {
  if(is.null(d$conceded_by) && max(scores)!=15) bad("Victory requires fifteen points or concession")
  if(!is.null(d$timeout_team)) bad("No timeout remains after the game")
 } else if(max(scores)>=15 || !is.null(d$conceded_by)) bad("A decided game must finish")
 if(!is.null(d$timeout_team) && (!(d$phase %in% c("point","between_points")) || d$teams[[d$timeout_team+1L]]$timeouts_used==0)) bad("Timeout must belong to an available playing phase and recorded allowance")
 if(!(d$phase %in% c("point","replacement_break")) && any(vapply(d$teams,function(t) t$matching_allowance>0 || any(unlist(t$blocked_this_point)),logical(1)))) bad("Point-specific replacement rights expire at the point boundary")
 if(d$phase!="replacement_break" && any(vapply(d$teams,function(t) t$matching_allowance>0,logical(1)))) bad("Matching replacements require the current replacement stoppage")
 invisible(TRUE)
}
.ra_ultimate_swap<-function(t,out,inc) {
 if(!(out %in% unlist(.ra_ultimate_active(t))) || inc>=length(t$status) || t$status[[inc+1L]]!="bench" || t$blocked_this_point[[inc+1L]]) .ra_fail("Replacement requires an active outgoing player and an eligible substitute")
 t$status[[out+1L]]<-"bench";t$status[[inc+1L]]<-"active";t
}
.ra_ultimate_clear<-function(d) {for(i in 1:2) {d$teams[[i]]$blocked_this_point<-as.list(rep(FALSE,length(d$teams[[i]]$status)));d$teams[[i]]$matching_allowance<-0L};d}
.ra_ultimate_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_ultimate_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown Ultimate event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished") .ra_fail("The game is finished")
 if(!is.null(d$timeout_team) && !(k %in% c("end_timeout","concede"))) .ra_fail("Complete the timeout before another event")
 if(k=="set_lineup") {
  if(phase!="pre_game") .ra_fail("Set the roster before the game starts")
  n<-p$roster_size;players<-unlist(p$active)
  if(length(unique(players))!=7 || max(players)>=n) .ra_fail("Starting lineup requires seven distinct roster players")
  t<-p$team+1L;d$teams[[t]]$status<-lapply(0:(n-1L),function(i) if(i %in% players) "active" else "bench");d$teams[[t]]$blocked_this_point<-as.list(rep(FALSE,n))
 } else if(k=="start_game") {
  if(phase!="pre_game") .ra_fail("The game has already started")
  d$first_receiving<-p$first_receiving;d$next_receiver<-p$first_receiving;d$phase<-"between_points"
 } else if(k=="start_point") {
  if(phase!="between_points") .ra_fail("A new point is not due")
  d$phase<-"point"
 } else if(k=="resume_point") {
  if(phase!="replacement_break") .ra_fail("No replacement stoppage is active")
  d$phase<-"point";for(i in 1:2) d$teams[[i]]$matching_allowance<-0L
 } else if(k=="begin_second_half") {
  if(phase!="halftime") .ra_fail("Halftime is not due")
  d$half<-2L;d$phase<-"between_points";d$next_receiver<-1L-d$first_receiving;for(i in 1:2) d$teams[[i]]$timeouts_used<-0L
 } else if(k=="goal") {
  if(phase!="point" || !(p$scorer %in% unlist(.ra_ultimate_active(d$teams[[p$team+1L]])))) .ra_fail("An active eligible receiver must score during a point")
  team<-p$team;d$scores[[team+1L]]<-d$scores[[team+1L]]+1L;d$next_receiver<-1L-team;d<-.ra_ultimate_clear(d)
  d$phase<-if(d$scores[[team+1L]]==15) "finished" else if(d$half==1 && d$scores[[team+1L]]==8) "halftime" else "between_points"
 } else if(k=="substitute") {
  if(!(phase %in% c("pre_game","between_points","halftime"))) .ra_fail("Ordinary substitutions require a point boundary")
  outs<-unlist(p$out);ins<-unlist(p[["in"]])
  if(length(intersect(outs,ins))>0 || length(outs)!=length(ins) || anyDuplicated(outs) || anyDuplicated(ins)) .ra_fail("Substitution vectors must pair distinct outgoing and incoming players")
  ti<-p$team+1L;for(i in seq_along(outs)) d$teams[[ti]]<-.ra_ultimate_swap(d$teams[[ti]],outs[[i]],ins[[i]])
 } else if(k %in% c("authorized_replacement","matching_replacement")) {
  if(!(phase %in% c("point","replacement_break"))) .ra_fail("Special replacements require an active point")
  ti<-p$team+1L;t<-d$teams[[ti]]
  if(k=="matching_replacement" && t$matching_allowance==0) .ra_fail("No matching replacement allowance is available")
  t<-.ra_ultimate_swap(t,p$out,p[["in"]])
  if(k=="authorized_replacement") {t$blocked_this_point[[p$out+1L]]<-TRUE;d$teams[[3L-ti]]$matching_allowance<-d$teams[[3L-ti]]$matching_allowance+1L;d$phase<-"replacement_break"} else t$matching_allowance<-t$matching_allowance-1L
  d$teams[[ti]]<-t
 } else if(k=="timeout") {
  ti<-p$team+1L;t<-d$teams[[ti]]
  if(!(phase %in% c("point","between_points")) || t$timeouts_used>=2) .ra_fail("Each team has two timeouts per half")
  d$teams[[ti]]$timeouts_used<-t$timeouts_used+1L;d$timeout_team<-p$team
 } else if(k=="end_timeout") {
  if(is.null(d$timeout_team)) .ra_fail("No timeout is active")
  d["timeout_team"]<-list(NULL)
 } else if(k=="concede") {d$phase<-"finished";d$conceded_by<-p$team;d["timeout_team"]<-list(NULL);d<-.ra_ultimate_clear(d)};d
}
.ra_ultimate_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,pre_game=c("set_lineup","substitute","start_game","concede"),between_points=c("start_point","substitute","timeout","concede"),point=c("goal","authorized_replacement","timeout","concede"),replacement_break=c("resume_point","authorized_replacement","matching_replacement","concede"),halftime=c("begin_second_half","substitute","concede"),character())
 if(!is.null(d$timeout_team)) names<-c("end_timeout","concede")
 s<-.ra_ultimate_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Point phase, active-player eligibility, replacement rights and timeout allowance must permit the event."),external_observations=list("Official goal, completed timeout or authorized replacement; timeout entitlement is adjudicated externally."))),constraints=list(phase=phase,half=d$half,receiving_team=d$next_receiver,pulling_team=1L-d$next_receiver,active=lapply(d$teams,.ra_ultimate_active),timeouts_remaining=lapply(d$teams,function(t) 2L-t$timeouts_used),matching_replacements=lapply(d$teams,function(t) t$matching_allowance)),completeness=FALSE,reason="official_ultimate_events")
}
.ra_ultimate_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-if(done) {if(!is.null(d$conceded_by)) 1L-d$conceded_by else if(d$scores[[1]]>d$scores[[2]]) 0L else 1L} else NULL
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(d$conceded_by)) "concession" else if(done) "fifteen_points" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
