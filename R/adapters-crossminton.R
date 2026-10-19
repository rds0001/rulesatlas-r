.ra_crossminton_wins <- function(d) {
  result<-c(0,0)
  for(score in d$games) {
    a<-score[[1]];b<-score[[2]]
    if(max(result)>=3 || max(a,b)<16 || (max(a,b)==16 && min(a,b)>14) || (max(a,b)>16 && abs(a-b)!=2)) .ra_fail("Invalid completed game or continuation after match end",code="INVALID_STATE")
    p<-if(a>b) 1L else 2L;result[p]<-result[p]+1L
  }
  result
}
.ra_crossminton_first <- function(d) {if(!length(d$games)) return(d$first_server);last<-d$games[[length(d$games)]];if(last[[1]]<last[[2]]) 0L else 1L}
.ra_crossminton_server <- function(d) {
  total<-sum(unlist(d$points));block<-if(min(unlist(d$points))>=15) (total-30)%%2 else (total%/%3)%%2
  (.ra_crossminton_first(d)+block)%%2
}
.ra_crossminton_validate <- function(d) {
  w<-.ra_crossminton_wins(d);decision<-d$decision;finished<-max(w)==3 || !is.null(decision)
  if((d$phase=="finished")!=finished) .ra_fail("Match phase and result mismatch",code="INVALID_STATE")
  a<-d$points[[1]];b<-d$points[[2]]
  if(max(a,b)>=16 && abs(a-b)>=2) .ra_fail("Completed current game must be closed",code="INVALID_STATE")
  if(max(w)==3 && (any(unlist(d$points)!=0) || !is.null(decision))) .ra_fail("Finished match has stale points or duplicate decision",code="INVALID_STATE")
  if(!is.null(decision) && decision$kind=="walkover" && (length(d$games)>0 || sum(unlist(d$points))>0)) .ra_fail("Walkover requires pre-play state",code="INVALID_STATE")
}
.ra_crossminton_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="choose_first_server") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params")
    if(length(d$games)>0 || sum(unlist(d$points))>0) .ra_fail("Initial service must be chosen before play")
    d$first_server<-p$player;return(d)
  }
  if(kind=="end_match") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=list("retirement","disqualification","walkover"))),"/params")
    if(p$kind=="walkover" && (length(d$games)>0 || sum(unlist(d$points))>0)) .ra_fail("Walkover requires pre-play state")
    d$decision<-p;d$phase<-"finished";return(d)
  }
  if(kind=="replay") {.ra_fields(p,structure(list(),names=character()),"/params");return(d)}
  if(kind=="service_fault") {.ra_fields(p,structure(list(),names=character()),"/params");winner<-1L-.ra_crossminton_server(d)} else if(kind=="rally") {.ra_fields(p,list(winner=list(type="integer",minimum=0,maximum=1)),"/params");winner<-p$winner} else .ra_fail("Unknown action")
  d$points[[winner+1L]]<-d$points[[winner+1L]]+1L
  if(d$points[[winner+1L]]>=16 && d$points[[winner+1L]]-d$points[[2L-winner]]>=2) {
    d$games<-c(d$games,list(d$points));d$points<-list(0,0)
    if(max(.ra_crossminton_wins(d))==3) d$phase<-"finished"
  }
  d
}
.ra_crossminton_legal <- function(state) {
  d<-state$data;actions<-list(list(type="replay",params=structure(list(),names=character())),list(type="service_fault",params=structure(list(),names=character())));kinds<-list("retirement","disqualification")
  if(!length(d$games) && sum(unlist(d$points))==0) {actions<-c(actions,lapply(0:1,function(p) list(type="choose_first_server",params=list(player=p))));kinds<-c(kinds,list("walkover"))}
  list(mode="event_types",actions=actions,action_types=list(list(type="rally",params_schema=list(winner=list(type="integer",minimum=0,maximum=1)),preconditions=list("Rally is officially awarded to the indicated player."),external_observations=list("Observed rally result or adjudicated fault.")),list(type="end_match",params_schema=list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=kinds)),preconditions=list("Official match-ending decision identifies the losing player."),external_observations=list("Retirement, disqualification or pre-play walkover."))),constraints=list(server=.ra_crossminton_server(d),service_block_size=if(min(unlist(d$points))>=15) 1 else 3,deciding_game_end_changes=if(length(d$games)==4) sum(unlist(d$points))%/%6 else 0),completeness=FALSE,reason="externally_adjudicated_rallies")
}
.ra_crossminton_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";decision<-d$decision;w<-if(!is.null(decision)) 1L-decision$player else if(terminal) which(.ra_crossminton_wins(d)==3)-1L else NULL
  result<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else "win"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) result else "none",winners=if(terminal) list(state$players[[w+1L]]$id) else list(),losers=if(terminal) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(decision)) decision$kind else if(terminal) "three_games_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
