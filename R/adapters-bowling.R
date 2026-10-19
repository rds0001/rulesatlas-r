.ra_bowling_finished <- function(frame,index) {
  if(index<9) return(length(frame)==2 || (length(frame)==1 && frame[[1]]==10))
  length(frame)==3 || (length(frame)==2 && sum(unlist(frame))<10)
}
.ra_bowling_maximum <- function(frame,index) {
  if(!length(frame)) return(10L)
  if(index<9 || length(frame)==1) return(if(frame[[1]]!=10) 10L-frame[[1]] else 10L)
  if(frame[[1]]==10 && frame[[2]]!=10) 10L-frame[[2]] else 10L
}
.ra_bowling_scores <- function(frames) {
  result<-list()
  for(i in seq_along(frames)) {
    frame<-frames[[i]];score<-NULL
    if(.ra_bowling_finished(frame,i-1L)) {
      later<-if(i<10) unlist(frames[(i+1L):10]) else numeric()
      if(i==10) score<-sum(unlist(frame)) else if(frame[[1]]==10) {if(length(later)>=2) score<-10+sum(later[1:2])} else if(sum(unlist(frame))==10) {if(length(later)) score<-10+later[1]} else score<-sum(unlist(frame))
    }
    result[i]<-list(score)
  }
  result
}
.ra_bowling_validate <- function(d) {
  for(p in 1:2) for(i in 1:10) {
    frame<-d$frames[[p]][[i]];index<-(i-1L)*2+p-1L
    if(index>d$current && length(frame)) .ra_fail("Future frame contains deliveries",code="INVALID_STATE")
    for(j in seq_along(frame)) {prefix<-head(frame,j-1L);if(.ra_bowling_finished(prefix,i-1L) || frame[[j]]>.ra_bowling_maximum(prefix,i-1L)) .ra_fail("Delivery exceeds standing pins or frame limit",code="INVALID_STATE")}
    if(.ra_bowling_finished(frame,i-1L)!=(index<d$current)) .ra_fail("Frame completion and turn mismatch",code="INVALID_STATE")
  }
}
.ra_bowling_apply <- function(state,action) {
  if(action$type!="roll") .ra_fail("Unknown action")
  .ra_fields(action$params,list(pins=list(type="integer",minimum=0,maximum=10),foul=list(type="boolean")),"/params")
  d<-state$data;p<-d$current%%2+1L;i<-d$current%/%2+1L;frame<-d$frames[[p]][[i]];v<-action$params$pins
  if(!is.null(action$actor) && action$actor!=state$players[[p]]$id) .ra_fail("Wrong actor")
  if(action$params$foul && v!=0) .ra_fail("A foul must score zero pins")
  if(v>.ra_bowling_maximum(frame,i-1L)) .ra_fail("Delivery exceeds standing pins")
  frame[[length(frame)+1L]]<-v;d$frames[[p]][[i]]<-frame
  if(.ra_bowling_finished(frame,i-1L)) d$current<-d$current+1L
  d
}
.ra_bowling_legal <- function(state) {
  d<-state$data;p<-d$current%%2+1L;i<-d$current%/%2+1L;limit<-.ra_bowling_maximum(d$frames[[p]][[i]],i-1L)
  list(mode="event_types",actions=list(),action_types=list(list(type="roll",params_schema=list(pins=list(type="integer",minimum=0,maximum=limit),foul=list(type="boolean")),preconditions=list("A foul scores zero; the credited pinfall cannot exceed standing pins."),external_observations=list("Official credited pinfall and foul ruling; a dead ball is not a delivery."))),constraints=list(player=p-1L,frame=i,standing_pins=limit),completeness=FALSE,reason="externally_observed_delivery")
}
.ra_bowling_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$current==20;totals<-vapply(d$frames,function(f) sum(unlist(.ra_bowling_scores(f))),numeric(1))
  winners<-if(terminal) lapply(which(totals==max(totals)),function(p) state$players[[p]]$id) else list()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(totals[1]==totals[2]) "draw" else "win"} else "none",winners=winners,losers=if(terminal) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=if(terminal) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=1L+sum(totals>totals[p]),score=totals[p])) else list(),reason=if(terminal) "ten_frames_completed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
