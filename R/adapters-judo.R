.ra_ju_player<-list(type="integer",minimum=0,maximum=1)
.ra_ju_finish<-function(d,w,reason) {d$winner<-w;d$reason<-reason;d$phase<-"finished";d$running<-FALSE;d["hold"]<-list(NULL);d}
.ra_ju_time_end<-function(d) {
 if(d$phase=="regulation" && d$remaining_ms==0 && is.null(d$hold)) {
  if(d$waza[[1]]!=d$waza[[2]]) return(.ra_ju_finish(d,if(d$waza[[1]]>d$waza[[2]]) 0L else 1L,"regulation_score"))
  d$phase<-"golden";d$running<-FALSE
 }
 d
}
.ra_ju_award<-function(d,p) {
 d$waza[[p+1L]]<-d$waza[[p+1L]]+1L
 if(d$waza[[p+1L]]==2) return(.ra_ju_finish(d,p,"two_waza_ari"))
 if(d$phase=="golden") return(.ra_ju_finish(d,p,"golden_score"))
 d
}
.ra_ju_validate<-function(d) {
 invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(d$phase=="ready") {
  if(d$remaining_ms!=240000 || d$golden_ms!=0 || any(unlist(d$waza)!=0) || any(unlist(d$shido)!=0) || !is.null(d$hold) || d$running || !is.null(d$winner) || !is.null(d$reason)) invalid("Ready contest must contain no elapsed play or scores")
  return(invisible(TRUE))
 }
 if(d$phase=="regulation" && (d$golden_ms!=0 || (d$remaining_ms==0 && is.null(d$hold)))) invalid("Expired regulation must resolve or enter golden score")
 if(d$phase=="golden" && (d$remaining_ms!=0 || d$waza[[1]]!=d$waza[[2]])) invalid("Golden score requires tied regulation and zero remaining time")
 if(!is.null(d$hold)) {
  h<-d$hold
  if(!d$running || h$elapsed_ms>=20000 || h$awarded!=(h$elapsed_ms>=10000) || (h$awarded && d$waza[[h$player+1L]]==0)) invalid("Active hold requires running time and consistent ten-second credit")
 }
 if(d$phase=="golden" && !is.null(d$hold) && d$hold$awarded) invalid("A credited golden-score hold must finish the contest")
 if(d$phase=="finished") {
  if(is.null(d$winner) || is.null(d$reason) || d$running || !is.null(d$hold)) invalid("Finished contest requires a winner and stopped clocks")
  w<-d$winner;reason<-d$reason
  if(reason=="two_waza_ari" && d$waza[[w+1L]]!=2) invalid("Two-waza-ari finish requires two awards")
  if(reason=="third_shido" && d$shido[[2L-w]]!=3) invalid("Third-shido finish requires three penalties against the loser")
  if(reason=="regulation_score" && (d$remaining_ms!=0 || d$waza[[w+1L]]<=d$waza[[2L-w]])) invalid("Regulation winner must lead at time expiry")
  if(reason=="golden_score" && (d$remaining_ms!=0 || d$waza[[w+1L]]<=d$waza[[2L-w]])) invalid("Golden-score winner must have the new scoring lead")
  if(reason=="walkover" && (d$remaining_ms!=240000 || d$golden_ms!=0 || any(unlist(d$waza)!=0) || any(unlist(d$shido)!=0))) invalid("Walkover requires no elapsed contest or scores")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(d$waza))>=2 || max(unlist(d$shido))>=3) invalid("Winning score or third shido must finish the contest")
 invisible(TRUE)
}
.ra_ju_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(k=="end_contest") {
  .ra_fields(p,list(loser=.ra_ju_player,reason=list(type="string",enum=list("direct_disqualification","retirement","walkover"))),"/params")
  if(p$reason=="walkover" && d$phase!="ready") .ra_fail("Walkover only before the contest starts")
  return(.ra_ju_finish(d,1L-p$loser,p$reason))
 }
 if(k=="resume") {.ra_fields(p,list(),"/params");if(d$running) .ra_fail("Clock already running");if(d$phase=="ready") d$phase<-"regulation";d$running<-TRUE;return(d)}
 if(d$phase=="ready") .ra_fail("Start the contest before recording play events")
 if(k=="pause") {.ra_fields(p,list(),"/params");if(!d$running) .ra_fail("Clock already stopped");d$running<-FALSE;d["hold"]<-list(NULL);return(.ra_ju_time_end(d))}
 if(k=="advance_clock") {
  .ra_fields(p,list(elapsed_ms=list(type="integer",minimum=1,maximum=1000000000)),"/params");n<-p$elapsed_ms
  if(!d$running) .ra_fail("Only running contest time may advance")
  h<-d$hold
  if(is.null(h) && d$phase=="regulation" && n>d$remaining_ms) .ra_fail("Regulation cannot advance beyond time expiry without an active hold")
  if(!is.null(h)) {winning_at<-if(!h$awarded && (d$waza[[h$player+1L]]==1 || d$phase=="golden")) 10000L else 20000L;if(h$elapsed_ms+n>winning_at) .ra_fail("Clock update cannot pass the instant a hold wins the contest")}
  if(d$phase=="regulation") d$remaining_ms<-max(0L,d$remaining_ms-n) else d$golden_ms<-d$golden_ms+n
  if(!is.null(h)) {
   h$elapsed_ms<-h$elapsed_ms+n
   if(h$elapsed_ms>=10000 && !h$awarded) {h$awarded<-TRUE;d$hold<-h;d<-.ra_ju_award(d,h$player);if(d$phase=="finished") return(d)}
   d$hold<-h
   if(h$elapsed_ms==20000) return(.ra_ju_finish(d,h$player,"hold_ippon"))
  }
  return(.ra_ju_time_end(d))
 }
 if(k=="start_hold") {.ra_fields(p,list(player=.ra_ju_player),"/params");if(!d$running || !is.null(d$hold)) .ra_fail("Start one adjudicated hold while the contest clock runs");d$hold<-list(player=p$player,elapsed_ms=0L,awarded=FALSE);return(d)}
 if(k=="end_hold") {.ra_fields(p,list(),"/params");if(is.null(d$hold)) .ra_fail("No active hold");d["hold"]<-list(NULL);return(.ra_ju_time_end(d))}
 if(k=="award_score") {
  .ra_fields(p,list(player=.ra_ju_player,score=list(type="string",enum=list("waza_ari","ippon"))),"/params")
  if(!is.null(d$hold)) .ra_fail("Hold scores come from its clock; use submission for a separate submission decision")
  if(p$score=="ippon") return(.ra_ju_finish(d,p$player,"ippon"))
  return(.ra_ju_award(d,p$player))
 }
 if(k=="submission") {.ra_fields(p,list(loser=.ra_ju_player),"/params");return(.ra_ju_finish(d,1L-p$loser,"submission"))}
 if(k=="shido") {.ra_fields(p,list(player=.ra_ju_player),"/params");q<-p$player;d$shido[[q+1L]]<-d$shido[[q+1L]]+1L;return(if(d$shido[[q+1L]]==3) .ra_ju_finish(d,1L-q,"third_shido") else d)}
 .ra_fail("Unknown action")
}
.ra_ju_legal<-function(state) {
 d<-state$data;actions<-list(list(type=if(d$running) "pause" else "resume",params=structure(list(),names=character())));types<-list()
 event<-function(k,params,condition,obs) list(type=k,params_schema=params,preconditions=list(condition),external_observations=list(obs))
 types<-list(event("end_contest",list(loser=.ra_ju_player,reason=list(type="string",enum=as.list(c("direct_disqualification","retirement",if(d$phase=="ready") "walkover")))),"Walkover only before starting.","Official exclusion, withdrawal or pre-contest forfeit."))
 if(d$phase!="ready") {
  types<-c(types,list(event("shido",list(player=.ra_ju_player),"Third shido ends the contest; smaller penalty differences do not decide a tied contest.","Official minor-penalty award."),event("submission",list(loser=.ra_ju_player),"Contest is ongoing.","Official submission decision.")))
  if(is.null(d$hold)) types<-c(types,list(event("award_score",list(player=.ra_ju_player,score=list(type="string",enum=list("waza_ari","ippon"))),"Two waza-ari or one golden-score award wins.","Adjudicated non-hold score.")))
  if(d$running) {
   h<-d$hold;limit<-if(d$phase=="regulation" && is.null(h)) d$remaining_ms else if(!is.null(h)) (if(!h$awarded && (d$waza[[h$player+1L]]==1 || d$phase=="golden")) 10000L else 20000L)-h$elapsed_ms else 1000000000L
   types<-c(types,list(event("advance_clock",list(elapsed_ms=list(type="integer",minimum=1,maximum=limit)),"Do not pass a winning hold threshold or unheld regulation expiry.","Official elapsed active contest milliseconds.")))
   if(is.null(h)) types<-c(types,list(event("start_hold",list(player=.ra_ju_player),"Only one active hold.","Referee announces a valid hold."))) else actions<-c(actions,list(list(type="end_hold",params=structure(list(),names=character()))))
  }
 }
 list(mode="event_types",actions=actions,action_types=types,constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,running=d$running),completeness=FALSE,reason="official_scores_penalties_and_clock_events")
}
.ra_ju_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
