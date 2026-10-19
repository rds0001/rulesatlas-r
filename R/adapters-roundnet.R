.ra_roundnet_schemas<-function() {team<-list(type="integer",minimum=0,maximum=1);empty<-setNames(list(),character());list(set_service_order=list(order=list(type="array",minItems=4,maxItems=4,items=list(type="integer",minimum=0,maximum=3))),start_game=empty,serve=list(result=list(type="string",enum=list("fault","ace","in_play","let"))),rally_result=list(winner=team),concede=list(team=team))}
.ra_roundnet_winner<-function(d) {for(i in 1:2) if(d$scores[[i]]>=21 && d$scores[[i]]-d$scores[[3L-i]]>=2) return(i-1L);NULL}
.ra_roundnet_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");order<-unlist(d$service_order)
 if(!setequal(order,0:3) || any(vapply(1:4,function(i) order[[i]]%/%2==order[[(i%%4)+1L]]%/%2,logical(1)))) bad("Service order must alternate teams and include every player once")
 if(d$phase=="ready" && (any(unlist(d$scores)!=0) || d$service_index!=0 || d$faults!=0)) bad("A new game starts with empty scoring and the first server")
 w<-.ra_roundnet_winner(d)
 if(d$phase=="finished") {
  if(is.null(w) && is.null(d$conceded_by)) bad("A finished game requires a two-point winning margin or concession")
  if(d$faults!=0) bad("No unresolved serve remains after the game")
 } else if(!is.null(w) || !is.null(d$conceded_by)) bad("A decided game must be finished")
 if(!is.null(w) && d$scores[[w+1L]]>21 && d$scores[[w+1L]]-d$scores[[2L-w]]!=2) bad("Extended games end at the first two-point margin")
 invisible(TRUE)
}
.ra_roundnet_point<-function(d,team) {
 serving<-d$service_order[[d$service_index+1L]]%/%2;d$scores[[team+1L]]<-d$scores[[team+1L]]+1L;d$faults<-0L
 if(team!=serving) d$service_index<-(d$service_index+1L)%%4L
 d$phase<-if(is.null(.ra_roundnet_winner(d))) "serve" else "finished";d
}
.ra_roundnet_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_roundnet_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown roundnet event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("The game is finished")
 if(k=="set_service_order") {
  if(d$phase!="ready") .ra_fail("Declare the complete service order before play")
  d$service_order<-p$order
 } else if(k=="start_game") {
  if(d$phase!="ready") .ra_fail("The game has already started")
  d$phase<-"serve"
 } else if(k=="serve") {
  if(d$phase!="serve") .ra_fail("A serve is not due")
  serving<-d$service_order[[d$service_index+1L]]%/%2
  if(p$result=="fault") {if(d$faults==1) d<-.ra_roundnet_point(d,1L-serving) else d$faults<-1L} else if(p$result=="ace") d<-.ra_roundnet_point(d,serving) else if(p$result=="in_play") d$phase<-"rally"
 } else if(k=="rally_result") {
  if(d$phase!="rally") .ra_fail("A valid serve must begin the rally")
  d<-.ra_roundnet_point(d,p$winner)
 } else if(k=="concede") {d$conceded_by<-p$team;d$phase<-"finished";d$faults<-0L};d
}
.ra_roundnet_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,ready=c("set_service_order","start_game","concede"),serve=c("serve","concede"),rally=c("rally_result","concede"),character());s<-.ra_roundnet_schemas();server<-d$service_order[[d$service_index+1L]]
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Game phase and the declared service cycle must permit the event."),external_observations=list("Official serve result, rally winner or concession."))),constraints=list(server=server,serving_team=server%/%2,receiving_team=1L-server%/%2,serve_attempt=d$faults+1L,target=21L,win_by=2L),completeness=FALSE,reason="official_roundnet_rally_judgments")
}
.ra_roundnet_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-if(done) {if(!is.null(d$conceded_by)) 1L-d$conceded_by else .ra_roundnet_winner(d)} else NULL
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) lapply(which((0:3)%/%2==w),function(i) state$players[[i]]$id) else list(),losers=if(done) lapply(which((0:3)%/%2!=w),function(i) state$players[[i]]$id) else list(),placements=list(),reason=if(!is.null(d$conceded_by)) "concession" else if(done) "game_won" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
