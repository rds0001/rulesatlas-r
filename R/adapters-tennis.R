.ra_tennis_set_winner <- function(score) {
  v<-unlist(score)
  if((max(v)==6 && min(v)<=4) || (max(v)==7 && min(v) %in% c(5,6))) return(if(v[1]>v[2]) 0L else 1L)
  .ra_fail("Invalid completed set score",code="INVALID_STATE")
}
.ra_tennis_counts <- function(data) {
  wins<-c(0L,0L)
  for(score in data$sets) {
    if(max(wins)==2L) .ra_fail("Sets continued after match end",code="INVALID_STATE")
    w<-.ra_tennis_set_winner(score)+1L; wins[w]<-wins[w]+1L
  }
  wins
}
.ra_tennis_server <- function(data) {
  first<-(sum(unlist(data$sets))+sum(unlist(data$games)))%%2
  if(identical(as.numeric(unlist(data$games)),c(6,6))) {
    total<-sum(unlist(data$points))
    return((first+if(total==0) 0 else floor((total+1)/2)%%2)%%2)
  }
  first
}
.ra_tennis_validate <- function(data) {
  wins<-.ra_tennis_counts(data); games<-unlist(data$games); points<-unlist(data$points); decision<-data$decision
  finished<-max(wins)==2L; tie<-all(games==6)
  expected<-if(finished || !is.null(decision)) "finished" else if(tie) "tiebreak" else "game"
  if(data$phase!=expected) .ra_fail("Scoring phase mismatch",code="INVALID_STATE")
  if(!tie && max(games)>=6 && abs(games[1]-games[2])>=2) .ra_fail("Unclosed completed set",code="INVALID_STATE")
  if(tie) {
    if(max(points)>=7 && abs(points[1]-points[2])>=2) .ra_fail("Unclosed completed tiebreak",code="INVALID_STATE")
  } else if(max(points)>4 || (max(points)==4 && !all(sort(points)==c(3,4)))) .ra_fail("Invalid advantage score",code="INVALID_STATE")
  if(data$server!=.ra_tennis_server(data)) .ra_fail("Incorrect service order",code="INVALID_STATE")
  if(finished && (!is.null(decision) || any(games!=0) || any(points!=0))) .ra_fail("Invalid completed match",code="INVALID_STATE")
  if(!is.null(decision) && decision$kind=="walkover" && (length(data$sets)>0 || any(games!=0) || any(points!=0))) .ra_fail("Walkover requires pre-play state",code="INVALID_STATE")
}
.ra_tennis_outcome <- function(state,pack) {
  data<-state$data; wins<-.ra_tennis_counts(data); decision<-data$decision; terminal<-data$phase=="finished"
  w<-if(!is.null(decision)) 1-decision$player else if(terminal) which(wins==2L)[1]-1L else NULL
  kind<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else if(terminal) "win" else "none"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=kind,winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(!is.null(decision)) decision$kind else if(terminal) "two_sets_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
.ra_tennis_apply <- function(state,action) {
  data<-state$data; params<-action$params
  if(identical(action$type,"end_match")) {
    .ra_fields(params,list(kind=list(type="enum",values=list("retirement","disqualification","walkover")),player=list(type="integer",minimum=0,maximum=1)),"/params")
    data$decision<-params; data$phase<-"finished"; .ra_tennis_validate(data); return(data)
  }
  if(!identical(action$type,"award_point")) .ra_fail("Unknown action")
  .ra_fields(params,list(player=list(type="integer",minimum=0,maximum=1)),"/params")
  player<-params$player+1L; other<-3L-player; points<-unlist(data$points); points[player]<-points[player]+1L
  data$points<-as.list(points)
  if(data$phase=="tiebreak") {
    if(points[player]>=7 && points[player]-points[other]>=2) {
      score<-list(6L,6L); score[[player]]<-7L; data$sets[[length(data$sets)+1L]]<-score
      data$games<-list(0L,0L); data$points<-list(0L,0L); data$phase<-"game"
    }
  } else if(points[player]>=4 && points[player]-points[other]>=2) {
    data$games[[player]]<-data$games[[player]]+1L; data$points<-list(0L,0L); games<-unlist(data$games)
    if(games[player]>=6 && games[player]-games[other]>=2) {
      data$sets[[length(data$sets)+1L]]<-data$games; data$games<-list(0L,0L)
    } else if(all(games==6)) data$phase<-"tiebreak"
  } else if(all(points==4)) data$points<-list(3L,3L)
  if(max(.ra_tennis_counts(data))==2L) data$phase<-"finished"
  data$server<-.ra_tennis_server(data); data
}
.ra_tennis_legal <- function(state) {
  data<-state$data; kinds<-list("retirement","disqualification")
  if(!length(data$sets) && all(unlist(data$games)==0) && all(unlist(data$points)==0)) kinds<-c(kinds,list("walkover"))
  list(mode="event_types",actions=list(),action_types=list(
    list(type="award_point",params_schema=list(player=list(type="integer",minimum=0,maximum=1)),preconditions=list("Match is ongoing."),external_observations=list("An adjudicator has awarded this point to the specified participant.")),
    list(type="end_match",params_schema=list(kind=list(type="enum",values=kinds),player=list(type="integer",minimum=0,maximum=1)),preconditions=list("Match is ongoing; walkover is permitted only before play."),external_observations=list("An official match-ending decision identifies the losing participant."))),constraints=list(server=data$server,phase=data$phase),completeness=FALSE,reason="externally_adjudicated_events")
}
