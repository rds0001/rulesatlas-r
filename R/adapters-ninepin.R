.ra_np_totals <- function(d) vapply(d$scores,function(s) sum(unlist(s)),numeric(1))
.ra_np_complete <- function(d) all(vapply(d$scores,function(s) length(s)==4 && all(lengths(s)==30),logical(1)))
.ra_np_validate <- function(d) {
 if(anyDuplicated(unlist(d$standing))) .ra_fail("Standing pins must be distinct",code="INVALID_STATE")
 for(p in 1:2) {s<-d$scores[[p]];if(length(s)>4 || (length(s)>1 && any(lengths(s[-length(s)])!=30))) .ra_fail("Only the current lane may have fewer than thirty deliveries",code="INVALID_STATE")}
 if(d$phase=="regulation") {
  a<-d$scores[[d$turn+1L]];b<-d$scores[[2L-d$turn]]
  if(length(a)!=d$lane+1 || length(a[[length(a)]])>=30 || !(length(b) %in% c(d$lane,d$lane+1)) || (length(b)==d$lane+1 && length(b[[length(b)]])!=30)) .ra_fail("Current lane and delivery order disagree with scorecards",code="INVALID_STATE")
  if(length(a[[length(a)]])<15 && !identical(sort(as.integer(unlist(d$standing))),0:8)) .ra_fail("Full-frame deliveries require all nine standing pins",code="INVALID_STATE")
  if(d$rolloff_round!=0 || any(lengths(d$rolloff)>0)) .ra_fail("Roll-off data requires tied completed regulation",code="INVALID_STATE")
 }
 if(d$phase=="rolloff") {
  ts<-.ra_np_totals(d)
  if(!.ra_np_complete(d) || ts[1]!=ts[2]) .ra_fail("Roll-offs require tied completed regulation",code="INVALID_STATE")
  if(length(d$rolloff[[d$turn+1L]])>=3 || !(length(d$rolloff[[2L-d$turn]]) %in% c(0,3))) .ra_fail("Each roll-off participant takes three deliveries",code="INVALID_STATE")
  if(!identical(sort(as.integer(unlist(d$standing))),0:8)) .ra_fail("Roll-off deliveries are into a full frame",code="INVALID_STATE")
 }
 if(d$phase!="finished") {if(!is.null(d$winner) || !is.null(d$withdrawn)) .ra_fail("Ongoing contest cannot have a winner or withdrawal",code="INVALID_STATE")} else {
  w<-d$winner;if(is.null(w)) .ra_fail("A finished contest requires a winner",code="INVALID_STATE")
  if(!is.null(d$withdrawn)) {if(w==d$withdrawn) .ra_fail("Withdrawn participant cannot win",code="INVALID_STATE")} else {
   if(!.ra_np_complete(d)) .ra_fail("Complete all four lanes before declaring a total winner",code="INVALID_STATE")
   ts<-.ra_np_totals(d)
   if(ts[1]==ts[2]) {if(any(lengths(d$rolloff)!=3) || sum(unlist(d$rolloff[[w+1L]]))<=sum(unlist(d$rolloff[[2L-w]]))) .ra_fail("Tied regulation requires a winning roll-off",code="INVALID_STATE")} else if(ts[w+1L]<=ts[2L-w]) .ra_fail("Highest total must win",code="INVALID_STATE")
  }
 }
 invisible(TRUE)
}
.ra_np_apply <- function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;t<-d$turn
 if(k=="withdraw") {
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
  if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
  d$withdrawn<-q;d$winner<-1L-q;d$phase<-"finished";return(d)
 }
 if(k!="delivery") .ra_fail("Unknown action")
 .ra_fields(p,list(knocked=list(type="array",minItems=0,maxItems=9,items=list(type="integer",minimum=0,maximum=8)),foul=list(type="boolean")),"/params")
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 pins<-unlist(p$knocked)
 if(anyDuplicated(pins) || any(!(pins %in% unlist(d$standing)))) .ra_fail("Only distinct standing pins can be knocked down")
 score<-if(p$foul) 0L else length(pins)
 if(d$phase=="rolloff") {
  d$rolloff[[t+1L]]<-c(d$rolloff[[t+1L]],list(score))
  if(length(d$rolloff[[t+1L]])==3) {
   if(!length(d$rolloff[[2L-t]])) d$turn<-1L-t else if(sum(unlist(d$rolloff[[1]]))!=sum(unlist(d$rolloff[[2]]))) {d$winner<-if(sum(unlist(d$rolloff[[1]]))>sum(unlist(d$rolloff[[2]]))) 0L else 1L;d$phase<-"finished"} else {d$rolloff_round<-d$rolloff_round+1L;d$rolloff<-list(list(),list());d$turn<-d$rolloff_round%%2L}
  }
  return(d)
 }
 n<-length(d$scores[[t+1L]]);lane<-d$scores[[t+1L]][[n]];clearing<-length(lane)>=15;lane<-c(lane,list(score));d$scores[[t+1L]][[n]]<-lane
 if(clearing && !p$foul) d$standing<-as.list(setdiff(unlist(d$standing),pins))
 if(!length(d$standing) || length(lane)<=15) d$standing<-as.list(0:8)
 if(length(lane)==30) {
  d$standing<-as.list(0:8)
  if(length(d$scores[[2L-t]])==d$lane) {d$turn<-1L-t;d$scores[[2L-t]]<-c(d$scores[[2L-t]],list(list()))} else if(d$lane<3) {d$lane<-d$lane+1L;d$turn<-d$lane%%2L;d$scores[[d$turn+1L]]<-c(d$scores[[d$turn+1L]],list(list()))} else {
   total<-.ra_np_totals(d)
   if(total[1]==total[2]) {d$phase<-"rolloff";d$turn<-0L} else {d$phase<-"finished";d$winner<-if(total[1]>total[2]) 0L else 1L}
  }
 }
 d
}
.ra_np_legal <- function(state) {
 d<-state$data;s<-d$scores[[d$turn+1L]]
 list(mode="event_types",actions=lapply(0:1,function(p) list(type="withdraw",params=list(player=p))),action_types=list(list(type="delivery",params_schema=list(knocked=list(type="array",minItems=0,maxItems=9,items=list(type="integer",minimum=0,maximum=8)),foul=list(type="boolean")),preconditions=list("Distinct knocked pins must be a subset of the standing frame. A foul consumes a delivery, scores zero and restores its previous frame."),external_observations=list("Adjudicated knocked-pin identities and foul flag."))),constraints=list(turn=d$turn,lane=d$lane+1L,standing=d$standing,mode=if(d$phase=="rolloff" || length(s[[length(s)]])<15) "full" else "clearing"),completeness=FALSE,reason="externally_observed_pinfall")
}
.ra_np_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;ts<-.ra_np_totals(d)
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,pins=ts[p+1L],rolloff_pins=sum(unlist(d$rolloff[[p+1L]])))) else list(),reason=if(done) if(!is.null(d$withdrawn)) "withdrawal" else if(ts[1]==ts[2]) "rolloff" else "highest_total" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
