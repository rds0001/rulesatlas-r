.ra_pet_remaining <- function(d,p) 3L-sum(unlist(d$thrown)[(p*3L+1L):(p*3L+3L)])
.ra_pet_nearest <- function(d,p) {x<-unlist(d$distances_mm[(p*3L+1L):(p*3L+3L)]);if(length(x)) min(x) else NULL}
.ra_pet_tied <- function(d) identical(.ra_pet_nearest(d,0),.ra_pet_nearest(d,1))
.ra_pet_points <- function(d,dead=FALSE) {
  if(dead) {rem<-vapply(0:1,function(p) .ra_pet_remaining(d,p),numeric(1));return(if((rem[1]==0)!=(rem[2]==0)) as.list(rem) else list(0L,0L))}
  a<-.ra_pet_nearest(d,0);b<-.ra_pet_nearest(d,1)
  if(identical(a,b)) return(list(0L,0L))
  winner<-if(is.null(a)) 1L else if(is.null(b) || a<b) 0L else 1L;limit<-.ra_pet_nearest(d,1L-winner);values<-unlist(d$distances_mm[(winner*3L+1L):(winner*3L+3L)]);points<-if(is.null(limit)) length(values) else sum(values<limit);out<-list(0L,0L);out[[winner+1L]]<-points;out
}
.ra_pet_finish <- function(d,points) {
  d$scores<-as.list(unlist(d$scores)+unlist(points));d$last_end<-points
  if(max(unlist(d$scores))>=13) {d$phase<-"finished";return(d)}
  if(points[[1]]!=points[[2]]) d$jack_player<-if(points[[1]]>points[[2]]) 0L else 1L
  d$turn<-d$jack_player;d$phase<-"jack_throw";d$thrown<-as.list(rep(FALSE,6));d$distances_mm<-rep(list(NULL),6);d$end_number<-d$end_number+1L;d$tie_active<-FALSE;d
}
.ra_pet_validate <- function(d) {
  for(i in 1:6) if(!is.null(d$distances_mm[[i]]) && !d$thrown[[i]]) .ra_fail("Only thrown boules may have measured distances",code="INVALID_STATE")
  if(d$phase %in% c("jack_throw","jack_place")) {
    if(any(unlist(d$thrown)) || length(unlist(d$distances_mm))) .ra_fail("A new jack requires cleared boule state",code="INVALID_STATE")
    expected<-if(d$phase=="jack_throw") d$jack_player else 1L-d$jack_player
    if(d$turn!=expected) .ra_fail("Jack throw or placement belongs to the designated player",code="INVALID_STATE")
  }
  if(d$tie_active && (!d$phase %in% c("boules","finished") || !.ra_pet_tied(d) || !any(unlist(d$thrown)))) .ra_fail("Tie alternation requires an unresolved distance tie after play",code="INVALID_STATE")
  if(d$phase=="boules") {
    if(.ra_pet_remaining(d,d$turn)==0) .ra_fail("Next player must have a boule remaining",code="INVALID_STATE")
    if(!any(unlist(d$thrown)) && d$turn!=d$jack_player) .ra_fail("The jack thrower plays the first boule even after an opponent places the jack",code="INVALID_STATE")
    a<-.ra_pet_nearest(d,0);b<-.ra_pet_nearest(d,1)
    if(!identical(a,b)) {
      trailing<-if(is.null(a)) 0L else if(is.null(b) || a<b) 1L else 0L
      if(.ra_pet_remaining(d,trailing)>0 && d$turn!=trailing) .ra_fail("The player without the nearest boule must play while having boules",code="INVALID_STATE")
    }
  }
  if((d$phase=="finished")!=(max(unlist(d$scores))>=13)) .ra_fail("First to thirteen ends the game",code="INVALID_STATE")
  if(min(unlist(d$scores))>=13) .ra_fail("Both players cannot reach thirteen in one end",code="INVALID_STATE")
  if(sum(unlist(d$last_end))>3 || all(unlist(d$last_end)>0) || any(unlist(d$last_end)>unlist(d$scores))) .ra_fail("Invalid previous end award",code="INVALID_STATE")
}
.ra_pet_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k %in% c("throw_jack","place_jack")) {
    .ra_fields(p,list(distance_mm=list(type="integer",minimum=0,maximum=100000),clearances_valid=list(type="boolean")),"/params")
    if(d$phase!=(if(k=="throw_jack") "jack_throw" else "jack_place")) .ra_fail("Jack action is not available in this phase")
    valid<-p$distance_mm>=6000 && p$distance_mm<=10000 && p$clearances_valid
    if(k=="place_jack" && !valid) .ra_fail("The placed jack must satisfy distance and official clearances")
    if(valid) {d$phase<-"boules";d$turn<-d$jack_player} else {d$phase<-"jack_place";d$turn<-1L-d$jack_player}
    return(d)
  }
  if(k!="throw") .ra_fail("Unknown action")
  .ra_fields(p,list(boule=list(type="integer",minimum=0,maximum=5),distances_mm=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=100000,nullable=TRUE)),jack_dead=list(type="boolean")),"/params");i<-p$boule+1L
  if(d$phase!="boules" || p$boule%/%3!=t || d$thrown[[i]]) .ra_fail("Throw one unused boule belonging to the current player")
  oldtie<-d$tie_active;d$thrown[[i]]<-TRUE
  for(j in 1:6) if(!is.null(p$distances_mm[[j]]) && (!d$thrown[[j]] || (j!=i && is.null(d$distances_mm[[j]])))) .ra_fail("Unthrown or already dead boules cannot acquire a distance")
  d$distances_mm<-p$distances_mm;d$tie_active<-.ra_pet_tied(d)
  if(p$jack_dead) {
    if(length(unlist(d$distances_mm))) .ra_fail("A dead jack has no active distance measurements")
    return(.ra_pet_finish(d,.ra_pet_points(d,TRUE)))
  }
  if(.ra_pet_remaining(d,0)==0 && .ra_pet_remaining(d,1)==0) return(.ra_pet_finish(d,.ra_pet_points(d)))
  if(.ra_pet_remaining(d,0)==0) d$turn<-1L else if(.ra_pet_remaining(d,1)==0) d$turn<-0L
  else if(.ra_pet_tied(d)) d$turn<-if(oldtie) 1L-t else t
  else {a<-.ra_pet_nearest(d,0);b<-.ra_pet_nearest(d,1);d$turn<-if(is.null(a)) 0L else if(is.null(b) || a<b) 1L else 0L}
  d
}
.ra_pet_legal <- function(state) {
  d<-state$data
  if(d$phase %in% c("jack_throw","jack_place")) types<-list(list(type=if(d$phase=="jack_throw") "throw_jack" else "place_jack",params_schema=list(distance_mm=list(type="integer",minimum=0,maximum=100000),clearances_valid=list(type="boolean")),preconditions=list("A valid jack is six through ten metres from the circle and satisfies official clearances. Only one thrown attempt; opponent places after failure."),external_observations=list("Measured jack distance and official clearance judgment."))) else types<-list(list(type="throw",params_schema=list(boule=list(type="integer",minimum=0,maximum=5),distances_mm=list(type="array",minItems=6,maxItems=6,items=list(type="integer",minimum=0,maximum=100000,nullable=TRUE)),jack_dead=list(type="boolean")),preconditions=list("Unused owned boule; supply the official new distance of every live thrown boule. Null marks unthrown or dead boules. A dead jack requires all-null distances."),external_observations=list("Official post-throw distances, dead-boule and dead-jack decisions.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(turn=d$turn,remaining=lapply(0:1,function(p) .ra_pet_remaining(d,p)),end_number=d$end_number),completeness=FALSE,reason="official_distances_without_throw_simulation")
}
.ra_pet_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";w<-if(d$scores[[1]]>=13) 0L else 1L
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,score=d$scores[[p+1L]])) else list(),reason=if(done) "thirteen_points" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
