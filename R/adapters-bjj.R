.ra_bjj_player<-list(type="integer",minimum=0,maximum=1)
.ra_bjj_players<-list(type="array",minItems=1,maxItems=2,items=.ra_bjj_player)
.ra_bjj_points<-c(takedown=2L,sweep=2L,knee_on_belly=2L,guard_pass=3L,mount=4L,back_control=4L)
.ra_bjj_leaders<-function(d) {
 a<-c(d$points[[1]],d$advantages[[1]],-d$penalties[[1]]);b<-c(d$points[[2]],d$advantages[[2]],-d$penalties[[2]])
 for(i in 1:3) if(a[i]!=b[i]) return(if(a[i]>b[i]) 0L else 1L)
 0:1
}
.ra_bjj_finish<-function(d,w,reason,excluded=list()) {d$phase<-"finished";d$running<-FALSE;d["winner"]<-list(w);d$reason<-reason;d$excluded<-excluded;d}
.ra_bjj_validate<-function(d) {
 invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(anyDuplicated(unlist(d$excluded))) invalid("Excluded players must be distinct")
 if(d$phase=="ready") {
  if(d$remaining_ms!=600000 || any(unlist(c(d$points,d$advantages,d$penalties))!=0) || d$running || !is.null(d$winner) || !is.null(d$reason) || length(d$excluded)>0) invalid("Ready contest must contain no play or awards")
  return(invisible(TRUE))
 }
 for(p in 0:1) {n<-d$penalties[[p+1L]];if((n>=2 && d$advantages[[2L-p]]<1) || (n>=3 && d$points[[2L-p]]<2)) invalid("Penalty ladder must include its opposing advantage and point awards")}
 if(d$phase=="fighting" && d$remaining_ms==0) invalid("Expired contest requires result review")
 if(d$phase=="review" && (d$remaining_ms!=0 || d$running)) invalid("Result review requires an expired stopped clock")
 if(d$phase=="finished") {
  if(d$running || is.null(d$reason)) invalid("Finished contest requires a reason and stopped clock")
  if(d$reason=="disqualification") {
   if(!length(d$excluded) || (length(d$excluded)==2 && !is.null(d$winner)) || (length(d$excluded)==1 && (is.null(d$winner) || d$winner!=1L-d$excluded[[1]]))) invalid("Disqualification outcome must exclude the losing participants")
  } else if(is.null(d$winner) || length(d$excluded)>0) invalid("Ordinary result requires one winner and no disqualification list")
  if(d$reason=="walkover" && (d$remaining_ms!=600000 || any(unlist(c(d$points,d$advantages,d$penalties))!=0))) invalid("Walkover requires no elapsed play or awards")
  if(d$reason=="score" && (d$remaining_ms!=0 || !identical(as.integer(.ra_bjj_leaders(d)),as.integer(d$winner)))) invalid("Score result must follow expired time and the ranking criteria")
  if(d$reason=="referee_decision" && (d$remaining_ms!=0 || length(.ra_bjj_leaders(d))!=2)) invalid("Referee tie decision requires equal points, advantages and penalties at time")
  for(p in 0:1) if(d$penalties[[p+1L]]==4 && !(p %in% unlist(d$excluded))) invalid("Fourth penalty requires disqualification")
 } else if(!is.null(d$winner) || !is.null(d$reason) || length(d$excluded)>0 || max(unlist(d$penalties))==4) invalid("Unresolved contest cannot retain a final decision")
 invisible(TRUE)
}
.ra_bjj_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(k=="end_contest") {
  .ra_fields(p,list(loser=.ra_bjj_player,reason=list(type="string",enum=list("submission","retirement","walkover"))),"/params")
  if((p$reason=="walkover" && d$phase!="ready") || (p$reason=="submission" && d$phase=="ready")) .ra_fail("Walkover precedes play; submission requires a started contest")
  return(.ra_bjj_finish(d,1L-p$loser,p$reason))
 }
 if(k=="disqualify") {
  .ra_fields(p,list(players=.ra_bjj_players),"/params")
  if(anyDuplicated(unlist(p$players))) .ra_fail("Disqualify distinct players")
  ex<-sort(unlist(p$players));return(.ra_bjj_finish(d,if(length(ex)==2) NULL else 1L-ex[1],"disqualification",as.list(ex)))
 }
 if(k=="resume") {.ra_fields(p,list(),"/params");if(d$running || !(d$phase %in% c("ready","fighting"))) .ra_fail("Resume only an unexpired stopped contest");d$phase<-"fighting";d$running<-TRUE;return(d)}
 if(d$phase=="ready") .ra_fail("Start the contest before recording play events")
 if(k=="pause") {.ra_fields(p,list(),"/params");if(!d$running) .ra_fail("Clock already stopped");d$running<-FALSE;return(d)}
 if(k=="advance_clock") {
  .ra_fields(p,list(elapsed_ms=list(type="integer",minimum=1,maximum=600000)),"/params")
  if(!d$running || p$elapsed_ms>d$remaining_ms) .ra_fail("Advance only active contest time up to expiry")
  d$remaining_ms<-d$remaining_ms-p$elapsed_ms
  if(d$remaining_ms==0) {d$running<-FALSE;d$phase<-"review"}
  return(d)
 }
 if(k=="award_points") {.ra_fields(p,list(player=.ra_bjj_player,technique=list(type="string",enum=as.list(names(.ra_bjj_points)))),"/params");d$points[[p$player+1L]]<-d$points[[p$player+1L]]+.ra_bjj_points[[p$technique]];return(d)}
 if(k=="award_advantage") {.ra_fields(p,list(player=.ra_bjj_player),"/params");d$advantages[[p$player+1L]]<-d$advantages[[p$player+1L]]+1L;return(d)}
 if(k=="penalize") {
  .ra_fields(p,list(players=.ra_bjj_players),"/params")
  if(anyDuplicated(unlist(p$players))) .ra_fail("Penalize distinct players once each")
  excluded<-integer()
  for(q in unlist(p$players)) {
   d$penalties[[q+1L]]<-d$penalties[[q+1L]]+1L
   if(d$penalties[[q+1L]]==2) d$advantages[[2L-q]]<-d$advantages[[2L-q]]+1L else if(d$penalties[[q+1L]]==3) d$points[[2L-q]]<-d$points[[2L-q]]+2L else if(d$penalties[[q+1L]]==4) excluded<-c(excluded,q)
  }
  if(length(excluded)) {excluded<-sort(excluded);return(.ra_bjj_finish(d,if(length(excluded)==2) NULL else 1L-excluded[1],"disqualification",as.list(excluded)))}
  return(d)
 }
 if(k=="confirm_result") {.ra_fields(p,list(),"/params");if(d$phase!="review" || length(.ra_bjj_leaders(d))!=1) .ra_fail("Confirm an expired contest with a winner by points, advantages or fewer penalties");return(.ra_bjj_finish(d,.ra_bjj_leaders(d)[1],"score"))}
 if(k=="referee_decision") {.ra_fields(p,list(winner=.ra_bjj_player),"/params");if(d$phase!="review" || length(.ra_bjj_leaders(d))!=2) .ra_fail("Referee decides only an expired complete statistical tie");return(.ra_bjj_finish(d,p$winner,"referee_decision"))}
 .ra_fail("Unknown action")
}
.ra_bjj_legal<-function(state) {
 d<-state$data;actions<-list();types<-list();empty<-structure(list(),names=character())
 event<-function(k,params,cond,obs) list(type=k,params_schema=params,preconditions=list(cond),external_observations=list(obs))
 if(d$phase!="review") actions<-list(list(type=if(d$running) "pause" else "resume",params=empty))
 types<-list(event("end_contest",list(loser=.ra_bjj_player,reason=list(type="string",enum=as.list(c("retirement",if(d$phase=="ready") "walkover" else "submission")))),"Walkover only before play.","Official submission, withdrawal or pre-start forfeit."),event("disqualify",list(players=.ra_bjj_players),"Name distinct participants; simultaneous double exclusion has no winner.","Official serious-foul disqualification."))
 if(d$phase!="ready") {
  types<-c(types,list(event("award_points",list(player=.ra_bjj_player,technique=list(type="string",enum=as.list(names(.ra_bjj_points)))),"Use finalized technical awards, including any late award during review.","Referee has established the technique and required control or stabilization."),event("award_advantage",list(player=.ra_bjj_player),"Contest or its result review is open.","Referee awards an advantage."),event("penalize",list(players=.ra_bjj_players),"Second penalty awards opposing advantage; third two points; fourth disqualifies.","Official escalating penalty, including simultaneous penalties.")))
  if(d$running) types<-c(types,list(event("advance_clock",list(elapsed_ms=list(type="integer",minimum=1,maximum=d$remaining_ms)),"Only active time up to the limit.","Official active elapsed milliseconds.")))
  if(d$phase=="review") {if(length(.ra_bjj_leaders(d))==1) actions<-c(actions,list(list(type="confirm_result",params=empty))) else types<-c(types,list(event("referee_decision",list(winner=.ra_bjj_player),"Points, advantages and penalties are tied after time.","Official referee selection of the winner.")))}
 }
 list(mode="event_types",actions=actions,action_types=types,constraints=list(phase=d$phase,remaining_ms=d$remaining_ms,running=d$running),completeness=FALSE,reason="official_technical_awards_and_result_review")
}
.ra_bjj_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;dq<-identical(d$reason,"disqualification")
 list(status=if(done) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(done) if(dq) "disqualification" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done) lapply(setdiff(0:1,w),function(p) state$players[[p+1L]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="review") "review_required" else "ongoing",evidence_required=if(d$phase=="review") list("Result confirmation or referee tie decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
