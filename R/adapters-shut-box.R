.ra_box_choices <- function(d) {
  if(is.null(d$roll_sum)) return(list())
  opened<-which(unlist(d$open));out<-list()
  for(n in seq_along(opened)) {
    # combn's scalar special case is avoided by passing indices as a list.
    cs<-utils::combn(as.list(opened),n,simplify=FALSE)
    for(c in cs) if(sum(unlist(c))==d$roll_sum) out[[length(out)+1L]]<-as.list(unlist(c))
  }
  out
}
.ra_box_validate <- function(d) {
  t<-d$turn+1L;scores<-d$scores;phase<-d$phase
  if((t==1 && !is.null(scores[[1]])) || (t==2 && is.null(scores[[1]]))) .ra_fail("Round order mismatch",code="INVALID_STATE")
  if(phase=="finished") {
    if(t!=2 || is.null(scores[[2]]) || scores[[2]]!=sum(which(unlist(d$open))) || !is.null(d$roll_sum)) .ra_fail("Final round score mismatch",code="INVALID_STATE")
  } else if(!is.null(scores[[t]]) || !any(unlist(d$open))) .ra_fail("Round must be closed",code="INVALID_STATE")
  if((phase=="close")!=!is.null(d$roll_sum)) .ra_fail("Roll phase mismatch",code="INVALID_STATE")
  if(phase=="close") {
    if(d$roll_sum==1 && any(unlist(d$open[7:9]))) .ra_fail("One die is not available",code="INVALID_STATE")
    if(!length(.ra_box_choices(d))) .ra_fail("A blocked roll must end the round",code="INVALID_STATE")
  }
}
.ra_box_end <- function(d) {
  d$scores[[d$turn+1L]]<-sum(which(unlist(d$open)));d["roll_sum"]<-list(NULL)
  if(d$turn==0) {d$turn<-1L;d$open<-as.list(rep(TRUE,9));d$phase<-"roll"} else d$phase<-"finished"
  d
}
.ra_box_apply <- function(state,action) {
  d<-state$data;p<-action$params;t<-d$turn+1L
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  if(d$phase=="roll") {
    if(action$type!="roll") .ra_fail("A dice result is required")
    .ra_fields(p,list(dice=list(type="array",items=list(type="integer",minimum=1,maximum=6),minItems=1,maxItems=2)),"/params")
    if(length(p$dice)==1 && any(unlist(d$open[7:9]))) .ra_fail("One die is unavailable while 7, 8 or 9 is open")
    d$roll_sum<-sum(unlist(p$dice));d$phase<-"close"
    if(!length(.ra_box_choices(d))) d<-.ra_box_end(d)
  } else {
    if(action$type!="close") .ra_fail("Choose tiles matching the roll")
    .ra_fields(p,list(tiles=list(type="array",items=list(type="integer",minimum=1,maximum=9),minItems=1,maxItems=9)),"/params");tiles<-sort(unlist(p$tiles))
    if(!any(vapply(.ra_box_choices(d),function(c) length(c)==length(tiles) && all(unlist(c)==tiles),logical(1)))) .ra_fail("Tiles must be distinct, open and sum to the roll")
    for(tile in tiles) d$open[[tile]]<-FALSE
    d["roll_sum"]<-list(NULL);d$phase<-"roll"
    if(!any(unlist(d$open))) d<-.ra_box_end(d)
  }
  d
}
.ra_box_legal <- function(state) {
  d<-state$data
  if(d$phase=="close") return(lapply(.ra_box_choices(d),function(tiles) list(type="close",params=list(tiles=tiles))))
  list(mode="event_types",actions=list(),action_types=list(list(type="roll",params_schema=list(dice=list(type="array",items=list(type="integer",minimum=1,maximum=6),minItems=if(!any(unlist(d$open[7:9]))) 1L else 2L,maxItems=2)),preconditions=list("One die is optional only when tiles 7, 8 and 9 are all closed."),external_observations=list("Observed dice result; no dice are generated."))),constraints=list(actor=state$players[[d$turn+1L]]$id),completeness=FALSE,reason="external_dice_result")
}
.ra_box_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";w<-if(!terminal || d$scores[[1]]==d$scores[[2]]) NULL else if(d$scores[[1]]<d$scores[[2]]) 1L else 2L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(is.null(w)) "draw" else "win"} else "none",winners=if(is.null(w)) list() else list(state$players[[w]]$id),losers=if(is.null(w)) list() else list(state$players[[3L-w]]$id),placements=list(),reason=if(terminal) "both_rounds_completed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
