.ra_archery_points <- function(d) {
  scores<-c(0,0)
  for(end in d$ends) {
    if(max(scores)>=6) .ra_fail("Sets continued after match end",code="INVALID_STATE")
    if(end[[1]]==end[[2]]) scores<-scores+1L else {p<-which.max(unlist(end));scores[p]<-scores[p]+2L}
  }
  scores
}
.ra_archery_validate <- function(d) {
  scores<-.ra_archery_points(d);decision<-d$decision;phase<-d$phase;sizes<-lengths(d$arrows)
  if(!is.null(decision)) {
    if((length(d$ends)<5 && all(sizes==3)) || (length(d$ends)==5 && (max(sizes)>1 || (all(sizes==1) && !identical(d$arrows[[1]],d$arrows[[2]]))))) .ra_fail("Official decision follows an already completed end or shoot-off",code="INVALID_STATE")
    if(phase!="finished") .ra_fail("Official decision must finish the match",code="INVALID_STATE")
    if(decision$kind=="walkover" && (length(d$ends)>0 || sum(sizes)>0)) .ra_fail("Walkover requires pre-play state",code="INVALID_STATE")
    if(max(scores)>=6 || !is.null(d$winner)) .ra_fail("Duplicate match-ending decisions",code="INVALID_STATE")
  } else if(max(scores)>=6) {
    if(phase!="finished" || sum(sizes)>0 || is.null(d$winner) || d$winner!=which.max(scores)-1L) .ra_fail("Completed set match has invalid outcome",code="INVALID_STATE")
  } else if(length(d$ends)==5) {
    if(any(scores!=5) || !phase %in% c("shoot_off","judge","finished") || max(sizes)>1) .ra_fail("Shoot-off requires five tied set points each",code="INVALID_STATE")
    if(phase=="shoot_off" && (all(sizes==1) || !is.null(d$winner))) .ra_fail("Completed shoot-off arrows require a decision",code="INVALID_STATE")
    if(phase=="judge" && (!all(sizes==1) || !identical(d$arrows[[1]],d$arrows[[2]]) || !is.null(d$winner))) .ra_fail("Closest-arrow judgment requires equal arrow scores",code="INVALID_STATE")
    if(phase=="finished") {
      if(!all(sizes==1) || is.null(d$winner)) .ra_fail("Shoot-off result is missing",code="INVALID_STATE")
      a<-d$arrows[[1]][[1]];b<-d$arrows[[2]][[1]]
      if(a!=b && d$winner!=if(a>b) 0 else 1) .ra_fail("Higher shoot-off arrow must win",code="INVALID_STATE")
    }
  } else if(phase!="set" || all(sizes==3) || !is.null(d$winner)) .ra_fail("Invalid ongoing set state",code="INVALID_STATE")
}
.ra_archery_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="end_match") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=list("retirement","disqualification","walkover"))),"/params")
    if(p$kind=="walkover" && (length(d$ends)>0 || sum(lengths(d$arrows))>0)) .ra_fail("Walkover requires pre-play state")
    d$decision<-p;d$phase<-"finished";return(d)
  }
  if(kind=="closest") {
    .ra_fields(p,list(winner=list(type="integer",minimum=0,maximum=1,nullable=TRUE)),"/params")
    if(d$phase!="judge") .ra_fail("Closest-arrow decision is only needed after tied shoot-off arrows")
    if(is.null(p$winner)) {d$arrows<-list(list(),list());d$phase<-"shoot_off"} else {d$winner<-p$winner;d$phase<-"finished"};return(d)
  }
  if(kind!="arrow") .ra_fail("Unknown action")
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),score=list(type="integer",minimum=0,maximum=10)),"/params");t<-p$player
  if(!d$phase %in% c("set","shoot_off")) .ra_fail("Arrow scoring is suspended")
  limit<-if(d$phase=="set") 3 else 1
  if(length(d$arrows[[t+1L]])>=limit) .ra_fail("This player has already completed the current end")
  d$arrows[[t+1L]]<-c(d$arrows[[t+1L]],list(p$score))
  if(all(lengths(d$arrows)==limit)) {
    if(d$phase=="set") {
      d$ends[[length(d$ends)+1L]]<-lapply(d$arrows,function(a) sum(unlist(a)));d$arrows<-list(list(),list());scores<-.ra_archery_points(d)
      if(max(scores)>=6) {d$winner<-which.max(scores)-1L;d$phase<-"finished"} else if(length(d$ends)==5) d$phase<-"shoot_off"
    } else {
      a<-d$arrows[[1]][[1]];b<-d$arrows[[2]][[1]]
      if(a==b) d$phase<-"judge" else {d$winner<-if(a>b) 0 else 1;d$phase<-"finished"}
    }
  }
  d
}
.ra_archery_legal <- function(state) {
  d<-state$data
  if(d$phase %in% c("set","shoot_off")) {
    limit<-if(d$phase=="set") 3 else 1
    types<-list(list(type="arrow",params_schema=list(player=list(type="integer",enum=as.list(which(lengths(d$arrows)<limit)-1L)),score=list(type="integer",minimum=0,maximum=10)),preconditions=list("Record one officially credited arrow, including zero for a miss or invalidated arrow."),external_observations=list("Adjudicated arrow value after applicable shooting violations.")))
  } else types<-list(list(type="closest",params_schema=list(winner=list(type="integer",minimum=0,maximum=1,nullable=TRUE)),preconditions=list("Equal shoot-off arrow values; null means equally close and repeats the shoot-off."),external_observations=list("Judge identifies the arrow closer to the center.")))
  kinds<-list("retirement","disqualification");if(!length(d$ends) && sum(lengths(d$arrows))==0) kinds<-c(kinds,list("walkover"))
  types[[length(types)+1L]]<-list(type="end_match",params_schema=list(player=list(type="integer",minimum=0,maximum=1),kind=list(type="string",enum=kinds)),preconditions=list("Official match-ending decision names the losing player."),external_observations=list("Official retirement, disqualification or pre-play walkover."))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(set_points=as.list(.ra_archery_points(d)),phase=d$phase),completeness=FALSE,reason="externally_adjudicated_arrows")
}
.ra_archery_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";decision<-d$decision;w<-if(!is.null(decision)) 1L-decision$player else d$winner
  result<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else "win"
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) result else "none",winners=if(terminal) list(state$players[[w+1L]]$id) else list(),losers=if(terminal) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(terminal) if(!is.null(decision)) decision$kind else "set_match_or_shoot_off_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
