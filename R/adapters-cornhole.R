.ra_cornhole_validate <- function(d) {
  if(d$fouls[[d$first+1L]]>(d$pitched+1)%/%2 || d$fouls[[2L-d$first]]>d$pitched%/%2) .ra_fail("Foul count exceeds pitched bags",code="INVALID_STATE")
  scores<-unlist(d$scores)
  if(sum(scores>=21)>1 || (max(scores)>=21 && d$pitched!=0)) .ra_fail("Invalid completed-game state",code="INVALID_STATE")
  if(max(scores)>=21 && d$scores[[d$first+1L]]<21) .ra_fail("Winner must lead the next inning",code="INVALID_STATE")
  if(d$innings==0 && any(scores!=0)) .ra_fail("Scores require a completed inning",code="INVALID_STATE")
  if(any(scores>d$innings*12)) .ra_fail("Score exceeds completed inning capacity",code="INVALID_STATE")
}
.ra_cornhole_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="choose_first") {
    if(d$innings!=0 || d$pitched!=0) .ra_fail("Opening player is already fixed")
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");d$first<-p$player;return(d)
  }
  if(kind=="pitch") {
    if(d$pitched==8) .ra_fail("Score the completed inning first")
    .ra_fields(p,list(foul=list(type="boolean")),"/params");t<-(d$first+d$pitched)%%2+1L
    if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    d$pitched<-d$pitched+1L;d$fouls[[t]]<-d$fouls[[t]]+as.integer(p$foul);return(d)
  }
  if(kind!="score_inning") .ra_fail("Unknown action")
  if(d$pitched!=8) .ra_fail("Both players must pitch all four bags")
  vector<-list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=0,maximum=4))
  .ra_fields(p,list(board=vector,hole=vector),"/params")
  for(t in 1:2) if(p$board[[t]]+p$hole[[t]]>4-d$fouls[[t]]) .ra_fail("Scoring bags exceed eligible pitched bags")
  points<-unlist(p$board)+3*unlist(p$hole);delta<-points[1]-points[2]
  if(delta!=0) {d$first<-if(delta>0) 0L else 1L;d$scores[[d$first+1L]]<-d$scores[[d$first+1L]]+abs(delta)}
  d$innings<-d$innings+1L;d$pitched<-0L;d$fouls<-list(0L,0L);d
}
.ra_cornhole_legal <- function(state) {
  d<-state$data;types<-list();vector<-list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=0,maximum=4))
  if(d$pitched<8) types[[1]]<-list(type="pitch",params_schema=list(foul=list(type="boolean")),preconditions=list("The next player pitches exactly one bag."),external_observations=list("Official foul ruling.")) else types[[1]]<-list(type="score_inning",params_schema=list(board=vector,hole=vector),preconditions=list("Final scoring bags exclude foul bags and total at most four minus fouls per player."),external_observations=list("Final board and hole counts after all eight pitches; displaced bags are judged in their final position."))
  if(d$innings==0 && d$pitched==0) types[[length(types)+1L]]<-list(type="choose_first",params_schema=list(player=list(type="integer",minimum=0,maximum=1)),preconditions=list("No pitch has been made."),external_observations=list("Opening player determined externally."))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(player=if(d$pitched<8) (d$first+d$pitched)%%2 else NULL),completeness=FALSE,reason="externally_adjudicated_bag_results")
}
.ra_cornhole_outcome <- function(state,pack) {
  d<-state$data;wins<-which(unlist(d$scores)>=21);w<-if(length(wins)) wins[1] else NULL
  list(status=if(length(wins)) "terminal" else "ongoing",result_kind=if(length(wins)) "win" else "none",winners=if(is.null(w)) list() else list(state$players[[w]]$id),losers=if(is.null(w)) list() else list(state$players[[3L-w]]$id),placements=list(),reason=if(length(wins)) "twenty_one_reached" else "ongoing",evidence_required=list(),rule_id=if(length(wins)) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
