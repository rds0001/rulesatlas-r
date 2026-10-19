.ra_carom_validate <- function(d) {
  a<-d$innings[[1]];b<-d$innings[[2]];p<-d$points[[1]];q<-d$points[[2]];done<-d$phase=="finished"
  if(a<1 || b>a || a-b>1 || (!done && ((d$turn==0 && a!=b+1) || (d$turn==1 && a!=b)))) .ra_fail("Inning counts and current player disagree",code="INVALID_STATE")
  if(d$equalizing!=(p==40 && q<=40 && d$turn==1)) .ra_fail("Equalizing inning requires the first player to have reached forty",code="INVALID_STATE")
  if(sum(unlist(d$points))>d$shots || a+b-1>d$shots) .ra_fail("Points or innings exceed recorded shots",code="INVALID_STATE")
  if(d$shots==0 && (any(unlist(d$points)!=0) || a!=1 || b!=0 || d$turn!=0 || !d$opening_required)) .ra_fail("Invalid opening position",code="INVALID_STATE")
  if(!done && p==40 && !d$equalizing) .ra_fail("First player reaching forty requires the equalizing inning",code="INVALID_STATE")
  if(done && is.null(d$decision) && (d$turn!=1 || a!=b || d$opening_required)) .ra_fail("A normal finish must complete equal innings",code="INVALID_STATE")
  if(!done && (q==40 || !is.null(d$decision))) .ra_fail("A completed game must be terminal",code="INVALID_STATE")
  if(done && is.null(d$decision) && p<40 && q<40) .ra_fail("A normal finish requires a player to reach forty",code="INVALID_STATE")
  if(done && !is.null(d$decision) && d$decision$kind=="walkover" && d$shots!=0) .ra_fail("Walkover requires no recorded shot",code="INVALID_STATE")
  if(d$opening_required && d$shots>0 && !d$equalizing) .ra_fail("A new opening position is used only for the equalizing inning",code="INVALID_STATE")
}
.ra_carom_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn
  if(k=="end_match") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=list("retirement","disqualification","walkover"))),"/params")
    if(p$kind=="walkover" && d$shots>0) .ra_fail("Walkover requires no recorded shot")
    d$decision<-p;d$phase<-"finished";return(d)
  }
  if(k!="shot") .ra_fail("Unknown action")
  .ra_fields(p,list(contacts=list(type="array",minItems=0,maxItems=2,items=list(type="string",enum=list("red","opponent"))),cushions_before_second=list(type="integer",minimum=0,maximum=100),foul=list(type="boolean")),"/params")
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(anyDuplicated(unlist(p$contacts))) .ra_fail("Contacts list records the first contact with each distinct object ball")
  good<-length(p$contacts)==2 && p$cushions_before_second>=3 && !p$foul && (!d$opening_required || p$contacts[[1]]=="red")
  d$shots<-d$shots+1L;d$opening_required<-FALSE
  if(good) {
    d$points[[t+1L]]<-d$points[[t+1L]]+1L
    if(d$points[[t+1L]]==40) {
      if(t==1) d$phase<-"finished" else {d$turn<-1L;d$innings[[2]]<-d$innings[[2]]+1L;d$equalizing<-TRUE;d$opening_required<-TRUE}
    }
  } else if(t==0) {d$turn<-1L;d$innings[[2]]<-d$innings[[2]]+1L}
  else if(d$equalizing) d$phase<-"finished"
  else {d$turn<-0L;d$innings[[1]]<-d$innings[[1]]+1L}
  d
}
.ra_carom_legal <- function(state) {
  d<-state$data;kinds<-list("retirement","disqualification")
  if(d$shots==0) kinds<-c(kinds,list("walkover"))
  list(mode="event_types",actions=list(),action_types=list(list(type="shot",params_schema=list(contacts=list(type="array",minItems=0,maxItems=2,items=list(type="string",enum=list("red","opponent"))),cushions_before_second=list(type="integer",minimum=0,maximum=100),foul=list(type="boolean")),preconditions=list("Record each distinct object ball once in first-contact order; cushion count is before contacting the second distinct object ball. Opening requires red first."),external_observations=list("Official object contacts, cushion count and any foul.")),list(type="end_match",params_schema=list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=kinds)),preconditions=list("Official decision identifies the losing player."),external_observations=list("Retirement, disqualification or pre-play walkover."))),constraints=list(turn=d$turn,target=40,equalizing=d$equalizing,opening_required=d$opening_required),completeness=FALSE,reason="adjudicated_shot_facts_without_ball_motion")
}
.ra_carom_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";decision<-d$decision;points<-unlist(d$points);wins<-if(done) if(!is.null(decision)) 2L-decision$player else which(points==max(points)) else integer()
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else if(length(wins)==2) "draw" else "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(done) lapply(setdiff(1:2,wins),function(p) state$players[[p]]$id) else list(),placements=if(done) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=if(p %in% wins) 1L else 2L,points=d$points[[p]],innings=d$innings[[p]])) else list(),reason=if(!is.null(decision)) decision$kind else if(done) "target_with_equal_innings" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
