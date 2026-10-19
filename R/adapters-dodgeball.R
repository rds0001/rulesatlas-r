.ra_dodgeball_schemas<-function() {
 team<-list(type="integer",minimum=0,maximum=1);player<-list(type="integer",minimum=0,maximum=5);thrower<-player;thrower$nullable<-TRUE;empty<-setNames(list(),character());event<-list(type="object",properties=list(kind=list(type="string",enum=list("hit","boundary","catch")),team=team,player=player,thrower=thrower),required=list("kind","team","player","thrower"))
 list(start_set=empty,resolve_exchange=list(events=list(type="array",minItems=1,maxItems=24,items=event)),disqualify_player=list(team=team,player=player),confirm_result=empty,restart_drawn_set=empty,concede=list(team=team))
}
.ra_dodgeball_active<-function(d,t) as.list(which(unlist(d$status[[t+1L]])=="active")-1L)
.ra_dodgeball_settle<-function(d) {if(any(vapply(0:1,function(t) length(.ra_dodgeball_active(d,t))==0,logical(1)))) d$phase<-"review";d}
.ra_dodgeball_eliminate<-function(d,t,i) {
 if(d$status[[t+1L]][[i+1L]]=="active") {d$status[[t+1L]][[i+1L]]<-"out";d$out_order[[t+1L]][[length(d$out_order[[t+1L]])+1L]]<-i};d
}
.ra_dodgeball_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) if(anyDuplicated(unlist(d$out_order[[t]])) || !setequal(unlist(d$out_order[[t]]),which(unlist(d$status[[t]])=="out")-1L)) bad("The re-entry queue must contain each temporarily eliminated player exactly once")
 empty<-any(vapply(0:1,function(t) length(.ra_dodgeball_active(d,t))==0,logical(1)))
 if(d$phase=="ready" && (any(unlist(d$status)!="active") || !is.null(d$conceded_by))) bad("A new set starts with six active players per team")
 if(d$phase=="playing" && empty) bad("A fully eliminated team requires result review")
 if(d$phase=="review" && !empty) bad("Result review requires a fully eliminated team")
 if(d$phase=="finished" && !empty && is.null(d$conceded_by)) bad("A finished set requires elimination or concession")
 if(d$phase!="finished" && !is.null(d$conceded_by)) bad("A conceded set is finished")
 invisible(TRUE)
}
.ra_dodgeball_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_dodgeball_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown dodgeball event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("The set is finished")
 if(k=="start_set") {
  if(d$phase!="ready") .ra_fail("The set has already started")
  d$phase<-"playing"
 } else if(k=="resolve_exchange") {
  if(d$phase!="playing") .ra_fail("An exchange requires an active set")
  for(e in p$events) {
   t<-e$team;i<-e$player;kind<-e$kind
   if(d$status[[t+1L]][[i+1L]]!="active") .ra_fail("The affected player must be active at this adjudicated event")
   if(kind %in% c("hit","boundary")) {
    if(!is.null(e$thrower)) .ra_fail("An out event does not identify a catch thrower")
    d<-.ra_dodgeball_eliminate(d,t,i)
   } else {
    if(is.null(e$thrower)) .ra_fail("A catch identifies the opposing thrower")
    d<-.ra_dodgeball_eliminate(d,1L-t,e$thrower)
    if(length(d$out_order[[t+1L]])>0) {revived<-d$out_order[[t+1L]][[1L]];d$out_order[[t+1L]]<-d$out_order[[t+1L]][-1L];d$status[[t+1L]][[revived+1L]]<-"active"}
   }
  };d<-.ra_dodgeball_settle(d)
 } else if(k=="disqualify_player") {
  if(!(d$phase %in% c("playing","review"))) .ra_fail("Player exclusion requires a started set")
  t<-p$team+1L;i<-p$player+1L
  if(d$status[[t]][[i]]=="dismissed") .ra_fail("Player is already dismissed")
  d$status[[t]][[i]]<-"dismissed";d$out_order[[t]]<-Filter(function(x) x!=p$player,d$out_order[[t]]);d<-.ra_dodgeball_settle(d)
 } else if(k=="confirm_result") {
  if(d$phase!="review") .ra_fail("Resolve an elimination before confirming the result")
  d$phase<-"finished"
 } else if(k=="restart_drawn_set") {
  if(d$phase!="review" || length(.ra_dodgeball_active(d,0L))>0 || length(.ra_dodgeball_active(d,1L))>0) .ra_fail("Only simultaneous complete elimination can be replayed")
  for(t in 1:2) {d$status[[t]]<-lapply(d$status[[t]],function(s) if(s=="dismissed") "dismissed" else "active");d$out_order[[t]]<-list()}
  if(any(vapply(0:1,function(t) length(.ra_dodgeball_active(d,t))==0,logical(1)))) .ra_fail("A dismissed entire team cannot replay")
  d$phase<-"playing"
 } else if(k=="concede") {d$conceded_by<-p$team;d$phase<-"finished"};d
}
.ra_dodgeball_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,ready=c("start_set","concede"),playing=c("resolve_exchange","disqualify_player","concede"),review=c("confirm_result","disqualify_player","concede"),character())
 if(d$phase=="review" && length(.ra_dodgeball_active(d,0L))==0 && length(.ra_dodgeball_active(d,1L))==0) names<-c(names,"restart_drawn_set")
 s<-.ra_dodgeball_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Player eligibility, re-entry order and set phase must permit the event."),external_observations=list("Official ordered valid hits, boundary outs, catches and exclusions for one resolved exchange."))),constraints=list(active=lapply(0:1,function(t) .ra_dodgeball_active(d,t)),next_reentry=lapply(d$out_order,function(q) if(length(q)>0) q[[1L]] else NULL)),completeness=FALSE,reason="official_dodgeball_exchange_rulings")
}
.ra_dodgeball_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-NULL
 if(done) {if(!is.null(d$conceded_by)) w<-1L-d$conceded_by else if((length(.ra_dodgeball_active(d,0L))>0)!=(length(.ra_dodgeball_active(d,1L))>0)) w<-if(length(.ra_dodgeball_active(d,0L))>0) 0L else 1L}
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "draw" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(d$conceded_by)) "concession" else if(done) "set_finished" else if(d$phase=="review") "exchange_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official resolution of the complete exchange.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
