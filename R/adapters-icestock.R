.ra_icestock_empty<-function() lapply(1:8,function(i) list(status="unplayed",rank=NULL))
.ra_icestock_schemas<-function() {
 team<-list(type="integer",minimum=0,maximum=1);player<-list(type="integer",minimum=0,maximum=3);empty<-setNames(list(),character());stock<-list(type="object",properties=list(status=list(type="string",enum=list("unplayed","live","dead")),rank=list(type="integer",minimum=1,maximum=8,nullable=TRUE)),required=list("status","rank"));board<-list(type="array",minItems=8,maxItems=8,items=stock)
 list(start_match=list(first_team=team),deliver_stock=list(team=team,player=player,board=board),set_available=list(team=team,player=player,available=list(type="boolean")),disqualify_player=list(team=team,player=player),assess_penalty=list(team=team,points=list(type="integer",minimum=1,maximum=100)),agree_end=empty,repeat_end=empty,measure_stocks=list(board=board),confirm_end=empty,confirm_match=empty,abandon_match=empty)
}
.ra_icestock_remaining<-function(d,t) as.list(Filter(function(i) d$available[[t+1L]][[i+1L]] && d$board[[t*4L+i+1L]]$status=="unplayed",0:3))
.ra_icestock_nearest<-function(d,t) {r<-vapply(Filter(function(s) s$status=="live",d$board[(t*4L+1L):(t*4L+4L)]),function(s) s$rank,numeric(1));if(length(r)>0) min(r) else 9}
.ra_icestock_next<-function(d,last) {
 a<-.ra_icestock_nearest(d,0L);b<-.ra_icestock_nearest(d,1L);team<-if(a==b) last else if(a>b) 0L else 1L
 if(length(.ra_icestock_remaining(d,team))>0) team else if(length(.ra_icestock_remaining(d,1L-team))>0) 1L-team else NULL
}
.ra_icestock_points<-function(d) {
 a<-.ra_icestock_nearest(d,0L);b<-.ra_icestock_nearest(d,1L);r<-list(0L,0L)
 if(a!=b) {t<-if(a<b) 0L else 1L;r[[t+1L]]<-sum(vapply(d$board[(t*4L+1L):(t*4L+4L)],function(s) s$status=="live" && s$rank<.ra_icestock_nearest(d,1L-t),logical(1)))};r
}
.ra_icestock_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 totals<-vapply(1:2,function(t) sum(vapply(d$end_scores,function(r) r[[t]],numeric(1))),numeric(1))
 if(any(unlist(d$scores)!=totals)) bad("Score totals must equal confirmed ends")
 if(any(vapply(d$end_scores,function(r) r[[1]]>0 && r[[2]]>0,logical(1)))) bad("Only one team scores positive stock points in an end")
 for(s in d$board) if((s$status=="live")!=!is.null(s$rank)) bad("Only valid live stocks have proximity ranks")
 for(t in 1:2) {
  if(any(unlist(d$available[[t]]) & unlist(d$dismissed[[t]]))) bad("Dismissed players cannot be available")
  if(d$phase %in% c("playing","end_review","match_review") && sum(unlist(d$available[[t]]))<3) bad("An incomplete team requires competition adjudication")
 }
 if(d$phase=="setup" && (length(d$end_scores)>0 || any(vapply(d$board,function(s) s$status!="unplayed",logical(1))) || d$early_end)) bad("Unstarted match must have an empty board")
 if(d$phase=="playing") {
  if(is.null(d$turn) || length(.ra_icestock_remaining(d,d$turn))==0 || d$early_end) bad("The next team needs an available unused delivery")
  if(any(vapply(d$board,function(s) s$status!="unplayed",logical(1))) && !is.null(d$last_team) && !identical(as.numeric(d$turn),as.numeric(.ra_icestock_next(d,d$last_team)))) bad("Turn contradicts the current best position")
 }
 if(d$phase=="end_review" && !d$early_end && (length(.ra_icestock_remaining(d,0L))>0 || length(.ra_icestock_remaining(d,1L))>0)) bad("Unplayed stocks require mutual early-end agreement")
 if(d$phase %in% c("match_review","finished") && length(d$end_scores)!=6) bad("Six confirmed ends are required")
 if(d$phase %in% c("playing","end_review") && length(d$end_scores)>=6) bad("The six-end match is complete")
 if(!(d$phase %in% c("setup","playing")) && !is.null(d$turn)) bad("No delivery is due outside active play")
 invisible(TRUE)
}
.ra_icestock_settle<-function(d,last) {d$last_team<-last;d["turn"]<-list(.ra_icestock_next(d,last));if(is.null(d$turn)) d$phase<-"end_review";d}
.ra_icestock_restart<-function(d) {d$board<-.ra_icestock_empty();d$early_end<-FALSE;d["last_team"]<-list(NULL);d$turn<-(d$first_team+length(d$end_scores))%%2L;d$phase<-"playing";d}
.ra_icestock_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_icestock_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown icestock event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase %in% c("finished","abandoned")) .ra_fail("Match is not open")
 if(k=="start_match") {
  if(d$phase!="setup" || any(vapply(d$available,function(a) sum(unlist(a))<3,logical(1)))) .ra_fail("Each team needs at least three eligible players")
  d$first_team<-p$first_team;d<-.ra_icestock_restart(d)
 } else if(k=="deliver_stock") {
  t<-p$team;i<-t*4L+p$player+1L
  if(d$phase!="playing" || t!=d$turn || !(p$player %in% unlist(.ra_icestock_remaining(d,t)))) .ra_fail("Wrong team or already used or unavailable player")
  new<-p$board
  for(j in 1:8) {
   before<-d$board[[j]];after<-new[[j]]
   if(j==i) {if(after$status=="unplayed") .ra_fail("A delivery must resolve live or dead")} else if(before$status %in% c("unplayed","dead") && !identical(after,before)) .ra_fail("Unused or removed stocks cannot be introduced by another delivery") else if(before$status=="live" && after$status=="unplayed") .ra_fail("A played stock cannot become unplayed")
  };d$board<-new;d<-.ra_icestock_settle(d,t)
 } else if(k %in% c("set_available","disqualify_player")) {
  t<-p$team+1L;i<-p$player+1L
  if(k=="set_available") {
   if(d$dismissed[[t]][[i]] && p$available) .ra_fail("A dismissed player cannot return")
   d$available[[t]][[i]]<-p$available
  } else {d$dismissed[[t]][[i]]<-TRUE;d$available[[t]][[i]]<-FALSE}
  if(d$phase!="setup" && sum(unlist(d$available[[t]]))<3) {d$phase<-"abandoned";d["turn"]<-list(NULL)} else if(d$phase=="playing") d<-.ra_icestock_settle(d,if(!is.null(d$last_team)) d$last_team else d$turn)
 } else if(k=="assess_penalty") d$penalties[[p$team+1L]]<-d$penalties[[p$team+1L]]+p$points else if(k=="agree_end") {
  if(d$phase!="playing") .ra_fail("Only an active end can be ended early by agreement")
  d$early_end<-TRUE;d["turn"]<-list(NULL);d$phase<-"end_review"
 } else if(k=="repeat_end") {
  if(!(d$phase %in% c("playing","end_review"))) .ra_fail("Only the unresolved end may be repeated")
  d<-.ra_icestock_restart(d)
 } else if(k=="measure_stocks") {
  if(d$phase!="end_review") .ra_fail("Measurement correction requires end review")
  if(any(vapply(1:8,function(i) d$board[[i]]$status!=p$board[[i]]$status,logical(1)))) .ra_fail("Measurement changes proximity, not stock inventory")
  d$board<-p$board
 } else if(k=="confirm_end") {
  if(d$phase!="end_review") .ra_fail("Resolve the end before confirming its score")
  r<-.ra_icestock_points(d);d$end_scores[[length(d$end_scores)+1L]]<-r;d$scores<-lapply(1:2,function(t) d$scores[[t]]+r[[t]])
  if(length(d$end_scores)==6) {d$phase<-"match_review";d["turn"]<-list(NULL)} else d<-.ra_icestock_restart(d)
 } else if(k=="confirm_match") {
  if(d$phase!="match_review") .ra_fail("Six confirmed ends are required")
  d$phase<-"finished"
 } else if(k=="abandon_match") {d$phase<-"abandoned";d["turn"]<-list(NULL)};d
}
.ra_icestock_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,setup="start_match",playing=c("deliver_stock","agree_end","repeat_end"),end_review=c("measure_stocks","confirm_end","repeat_end"),match_review="confirm_match",character())
 if(!(d$phase %in% c("finished","abandoned"))) names<-c(names,"set_available","disqualify_player","assess_penalty","abandon_match")
 s<-.ra_icestock_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Team eligibility, available deliveries and end phase must permit the event."),external_observations=list("Official final stock proximities, penalty, availability or agreed-end ruling."))),constraints=list(phase=d$phase,next_team=d$turn,available_players=if(!is.null(d$turn)) .ra_icestock_remaining(d,d$turn) else list(),provisional_end_score=if(d$phase=="end_review") .ra_icestock_points(d) else NULL),completeness=FALSE,reason="official_icestock_rulings")
}
.ra_icestock_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";scores<-unlist(d$scores)-unlist(d$penalties);w<-if(scores[[1]]==scores[[2]]) NULL else if(scores[[1]]>scores[[2]]) 0L else 1L
 list(status=if(done) "terminal" else if(d$phase %in% c("end_review","match_review","abandoned")) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "draw" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) "match_finished" else if(d$phase=="abandoned") "competition_decision_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official competition decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
