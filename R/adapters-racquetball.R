.ra_rb_wins <- function(d) {
  w<-c(0L,0L)
  for(i in seq_along(d$games)) {
    g<-unlist(d$games[[i]]);target<-if(i==3) 11L else 15L
    if(max(w)>=2 || max(g)!=target || min(g)>=target) .ra_fail("Invalid completed game",code="INVALID_STATE")
    p<-which.max(g);w[p]<-w[p]+1L
  }
  w
}
.ra_rb_server <- function(d) {
  n<-length(d$games)
  if(n<2) return((d$first_server+n)%%2L)
  totals<-unlist(d$games[[1]])+unlist(d$games[[2]])
  if(totals[1]==totals[2]) NULL else which.max(totals)-1L
}
.ra_rb_validate <- function(d) {
  w<-.ra_rb_wins(d);done<-max(w)==2 || !is.null(d$decision)
  if((d$phase=="finished")!=done) .ra_fail("Match phase and outcome mismatch",code="INVALID_STATE")
  target<-if(length(d$games)==2) 11L else 15L
  if(max(unlist(d$points))>=target) .ra_fail("Completed game must be closed",code="INVALID_STATE")
  if(sum(unlist(d$games))+sum(unlist(d$points))>d$rallies) .ra_fail("Points exceed completed rallies",code="INVALID_STATE")
  if(!d$started && (d$rallies>0 || d$faults>0 || length(d$games) || d$server!=d$first_server)) .ra_fail("Invalid pre-play state",code="INVALID_STATE")
  if(d$phase=="choose_server" && (length(d$games)!=2 || !is.null(.ra_rb_server(d)) || any(unlist(d$points)!=0) || d$faults>0)) .ra_fail("Service choice is only for a tied aggregate before game three",code="INVALID_STATE")
  if(max(w)==2 && (any(unlist(d$points)!=0) || d$faults>0 || !is.null(d$decision))) .ra_fail("Completed match cannot retain current points, faults or a second decision",code="INVALID_STATE")
  if(!is.null(d$decision) && d$decision$kind=="walkover" && d$started) .ra_fail("Walkover requires no play",code="INVALID_STATE")
}
.ra_rb_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;ps<-list(type="integer",minimum=0,maximum=1);empty<-structure(list(),names=character())
  if(k %in% c("choose_first_server","choose_tiebreak_server")) {
    .ra_fields(p,list(player=ps),"/params")
    if(k=="choose_first_server") {
      if(d$started) .ra_fail("Choose the initial server before play")
      d$first_server<-p$player
    } else if(d$phase!="choose_server") .ra_fail("Tiebreak server choice requires equal aggregate points")
    d$server<-p$player;d$phase<-"playing";return(d)
  }
  if(k=="end_match") {
    .ra_fields(p,list(player=ps,kind=list(type="string",enum=list("retirement","disqualification","walkover"))),"/params")
    if(p$kind=="walkover" && d$started) .ra_fail("Walkover requires no play")
    d$decision<-p;d$phase<-"finished";return(d)
  }
  if(d$phase!="playing") .ra_fail("Choose the tiebreak server before play")
  if(k=="replay") {.ra_fields(p,empty,"/params");d$faults<-0L;d$started<-TRUE;return(d)}
  if(k=="service_fault") {
    .ra_fields(p,empty,"/params");d$started<-TRUE
    if(d$faults==0) {d$faults<-1L;return(d)}
    winner<-1L-d$server
  } else if(k=="out_serve") {.ra_fields(p,empty,"/params");winner<-1L-d$server}
  else if(k=="rally") {.ra_fields(p,list(winner=ps),"/params");winner<-p$winner}
  else .ra_fail("Unknown action")
  d$started<-TRUE;d$faults<-0L;d$rallies<-d$rallies+1L
  if(winner!=d$server) {d$server<-winner;return(d)}
  d$points[[winner+1L]]<-d$points[[winner+1L]]+1L;target<-if(length(d$games)==2) 11L else 15L
  if(d$points[[winner+1L]]==target) {
    d$games[[length(d$games)+1L]]<-d$points;d$points<-list(0L,0L)
    if(max(.ra_rb_wins(d))==2) d$phase<-"finished" else {
      server<-.ra_rb_server(d)
      if(is.null(server)) d$phase<-"choose_server" else d$server<-server
    }
  }
  d
}
.ra_rb_legal <- function(state) {
  d<-state$data;actions<-list();types<-list();kinds<-list("retirement","disqualification");ps<-list(type="integer",minimum=0,maximum=1);empty<-structure(list(),names=character())
  if(!d$started) {
    kinds<-c(kinds,list("walkover"));actions<-lapply(0:1,function(p) list(type="choose_first_server",params=list(player=p)))
  }
  if(d$phase=="choose_server") actions<-c(actions,lapply(0:1,function(p) list(type="choose_tiebreak_server",params=list(player=p)))) else {
    actions<-c(actions,lapply(c("service_fault","out_serve","replay"),function(k) list(type=k,params=empty)))
    types<-list(list(type="rally",params_schema=list(winner=ps),preconditions=list("Official rally winner; only the server scores."),external_observations=list("Adjudicated rally, including court, contact and avoidable-hinder decisions.")))
  }
  types<-c(types,list(list(type="end_match",params_schema=list(player=ps,kind=list(type="string",enum=kinds)),preconditions=list("Official match-ending decision identifies the losing player."),external_observations=list("Retirement, disqualification or pre-play walkover."))))
  list(mode="event_types",actions=actions,action_types=types,constraints=list(server=d$server,serve_attempt=1L+d$faults,target=if(length(d$games)==2) 11L else 15L),completeness=FALSE,reason="externally_adjudicated_rallies_and_serve_faults")
}
.ra_rb_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";decision<-d$decision;w<-if(!is.null(decision)) 1L-decision$player else if(done) which(.ra_rb_wins(d)==2)[1]-1L else NULL
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(decision)) decision$kind else if(done) "two_games_won" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
