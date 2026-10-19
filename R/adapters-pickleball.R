.ra_pickleball_wins <- function(d) {
  result<-c(0,0)
  for(score in d$games) {
    a<-score[[1]];b<-score[[2]]
    if(max(result)>=2 || max(a,b)<11 || (max(a,b)==11 && min(a,b)>9) || (max(a,b)>11 && abs(a-b)!=2)) .ra_fail("Invalid completed game or play after match end",code="INVALID_STATE")
    p<-if(a>b) 1L else 2L;result[p]<-result[p]+1L
  }
  result
}
.ra_pickleball_validate <- function(d) {
  w<-.ra_pickleball_wins(d);done<-max(w)==2 || !is.null(d$decision)
  if((d$phase=="finished")!=done) .ra_fail("Match phase and result mismatch",code="INVALID_STATE")
  a<-d$points[[1]];b<-d$points[[2]]
  if(max(a,b)>=11 && abs(a-b)>=2) .ra_fail("Completed current game must be closed",code="INVALID_STATE")
  if(sum(unlist(d$games))+a+b>d$rallies) .ra_fail("Scored points exceed completed rallies",code="INVALID_STATE")
  if(d$rallies==0 && (length(d$games)>0 || any(unlist(d$points)!=0) || d$server!=d$first_server)) .ra_fail("Invalid pre-play service state",code="INVALID_STATE")
  if(max(w)==2 && (any(unlist(d$points)!=0) || !is.null(d$decision))) .ra_fail("Completed match has stale points or duplicate decision",code="INVALID_STATE")
  if(!is.null(d$decision) && d$decision$kind=="walkover" && d$rallies!=0) .ra_fail("Walkover requires no completed rally",code="INVALID_STATE")
}
.ra_pickleball_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="choose_first_server") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params")
    if(d$rallies!=0) .ra_fail("Initial service choice is fixed once play begins")
    d$first_server<-p$player;d$server<-p$player;return(d)
  }
  if(kind=="end_match") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=list("retirement","disqualification","walkover"))),"/params")
    if(p$kind=="walkover" && d$rallies!=0) .ra_fail("Walkover requires no completed rally")
    d$decision<-p;d$phase<-"finished";return(d)
  }
  if(kind=="replay") {.ra_fields(p,structure(list(),names=character()),"/params");return(d)}
  if(kind=="service_fault") {.ra_fields(p,structure(list(),names=character()),"/params");winner<-1L-d$server} else if(kind=="rally") {.ra_fields(p,list(winner=list(type="integer",minimum=0,maximum=1)),"/params");winner<-p$winner} else .ra_fail("Unknown action")
  d$rallies<-d$rallies+1L
  if(winner!=d$server) {d$server<-winner;return(d)}
  d$points[[winner+1L]]<-d$points[[winner+1L]]+1L
  if(d$points[[winner+1L]]>=11 && d$points[[winner+1L]]-d$points[[2L-winner]]>=2) {
    d$games<-c(d$games,list(d$points));d$points<-list(0,0);d$server<-(d$first_server+length(d$games))%%2
    if(max(.ra_pickleball_wins(d))==2) d$phase<-"finished"
  }
  d
}
.ra_pickleball_legal <- function(state) {
  d<-state$data;kinds<-list("retirement","disqualification");actions<-list(list(type="replay",params=structure(list(),names=character())),list(type="service_fault",params=structure(list(),names=character())))
  if(d$rallies==0) {kinds<-c(kinds,list("walkover"));actions<-c(actions,lapply(0:1,function(p) list(type="choose_first_server",params=list(player=p))))}
  list(mode="event_types",actions=actions,action_types=list(list(type="rally",params_schema=list(winner=list(type="integer",minimum=0,maximum=1)),preconditions=list("Adjudicated rally winner; only the server scores, otherwise service changes."),external_observations=list("Official rally result, including faults under the two-bounce, non-volley-zone and service rules.")),list(type="end_match",params_schema=list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=kinds)),preconditions=list("Official match-ending decision names the losing player."),external_observations=list("Retirement, disqualification or pre-play walkover."))),constraints=list(server=d$server,service_court=if(d$points[[d$server+1L]]%%2==0) "right" else "left",points=d$points),completeness=FALSE,reason="externally_adjudicated_rallies")
}
.ra_pickleball_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";decision<-d$decision;w<-if(!is.null(decision)) 1L-decision$player else if(terminal) which(.ra_pickleball_wins(d)==2)-1L else NULL
  result<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else "win"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) result else "none",winners=if(terminal) list(state$players[[w+1L]]$id) else list(),losers=if(terminal) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(decision)) decision$kind else if(terminal) "two_games_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
