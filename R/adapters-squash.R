.ra_sq_player <- list(type="integer",minimum=0,maximum=1)
.ra_sq_box <- list(type="enum",values=list("left","right"))
.ra_sq_validate <- function(d) {
  points<-unlist(d$points);games<-unlist(d$games_won);finished<-!is.null(d$decision) || 3 %in% games
  if(all(games==3)) .ra_fail("Both players cannot win the match",code="INVALID_STATE")
  if(max(points)>=11 && abs(points[1]-points[2])>=2) .ra_fail("Completed game must be closed",code="INVALID_STATE")
  if(d$finished!=finished) .ra_fail("Match phase mismatch",code="INVALID_STATE")
  if(3 %in% games && (any(points!=0) || !is.null(d$decision))) .ra_fail("Invalid completed match",code="INVALID_STATE")
  if(!d$started && (any(points!=0) || any(games!=0) || d$serve_box!="choose")) .ra_fail("Invalid pre-play state",code="INVALID_STATE")
  if(d$serve_box=="choose" && sum(points)>0 && points[d$server+1L]==0) .ra_fail("Receiver has not won service",code="INVALID_STATE")
}
.ra_sq_close <- function(d,w) {
  d$games_won[[w+1L]]<-d$games_won[[w+1L]]+1L;d$points<-list(0L,0L);d$server<-w;d$serve_box<-"choose";d$started<-TRUE;d$finished<-d$games_won[[w+1L]]==3;d
}
.ra_sq_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="choose_first_server") {
    .ra_fields(p,list(player=.ra_sq_player),"/params")
    if(d$started) .ra_fail("First server is already fixed")
    d$server<-p$player
  } else if(kind=="choose_box") {
    .ra_fields(p,list(box=.ra_sq_box),"/params")
    if(d$serve_box!="choose") .ra_fail("Serve box is already determined")
    if(!is.null(action$actor) && action$actor!=state$players[[d$server+1L]]$id) .ra_fail("Only the server chooses a box")
    d$serve_box<-p$box;d$started<-TRUE
  } else if(kind=="let") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(d$serve_box=="choose") .ra_fail("Choose a serve box first")
  } else if(kind=="award_rally") {
    .ra_fields(p,list(player=.ra_sq_player),"/params")
    if(d$serve_box=="choose") .ra_fail("Choose a serve box first")
    w<-p$player;previous<-d$server;d$points[[w+1L]]<-d$points[[w+1L]]+1L
    if(max(unlist(d$points))>=11 && abs(d$points[[1]]-d$points[[2]])>=2) d<-.ra_sq_close(d,w) else {
      d$server<-w;d$serve_box<-if(w==previous) {if(d$serve_box=="left") "right" else "left"} else "choose"
    }
  } else if(kind=="award_game") {
    .ra_fields(p,list(player=.ra_sq_player),"/params");d<-.ra_sq_close(d,p$player)
  } else if(kind=="end_match") {
    .ra_fields(p,list(player=.ra_sq_player,kind=list(type="enum",values=list("retirement","disqualification","conduct_match","walkover"))),"/params")
    if(p$kind=="walkover" && d$started) .ra_fail("Walkover requires pre-play state")
    d$decision<-p;d$finished<-TRUE
  } else .ra_fail("Unknown action")
  d
}
.ra_sq_legal <- function(state) {
  d<-state$data;schemas<-list()
  if(!d$started) schemas$choose_first_server<-list(player=.ra_sq_player)
  if(d$serve_box=="choose") schemas$choose_box<-list(box=.ra_sq_box) else {schemas$award_rally<-list(player=.ra_sq_player);schemas$let<-structure(list(),names=character())}
  schemas$award_game<-list(player=.ra_sq_player)
  kinds<-c("retirement","disqualification","conduct_match",if(!d$started) "walkover" else character())
  schemas$end_match<-list(player=.ra_sq_player,kind=list(type="enum",values=as.list(kinds)))
  list(mode="event_types",actions=list(),action_types=lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("The game phase and service state must permit the event."),external_observations=list("An adjudicated rally, let or formal conduct/game/match decision."))),constraints=list(server=d$server,serve_box=d$serve_box),completeness=FALSE,reason="externally_adjudicated_events")
}
.ra_sq_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$finished;decision<-d$decision;w<-if(!is.null(decision)) 1L-decision$player else if(terminal) which(unlist(d$games_won)==3)[1]-1L else NULL
  kind<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",conduct_match="technical",walkover="forfeit") else if(terminal) "win" else "none"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=kind,winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(!is.null(decision)) decision$kind else if(terminal) "three_games_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
