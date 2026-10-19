.ra_jc_rest<-function(config) if(is.null(config$rest_ms)) 60000L else config$rest_ms
.ra_jc_player<-list(type="integer",minimum=0,maximum=1)
.ra_jc_totals<-function(d) lapply(1:3,function(j) lapply(1:2,function(p) sum(vapply(seq_along(d$cards),function(r) d$cards[[r]][[j]][[p]]-d$deductions[[r]][[p]],numeric(1)))))
.ra_jc_decision<-function(d) {
 votes<-vapply(.ra_jc_totals(d),function(s) sign(s[[1]]-s[[2]]),numeric(1));winner<-if(sum(votes==1)>=2) 0L else if(sum(votes== -1)>=2) 1L else NULL
 if(is.null(winner)) kind<-if(sum(votes==0)==3) "unanimous_draw" else if(sum(votes==0)==2) "majority_draw" else "split_draw" else kind<-if(sum(votes==(if(winner==0) 1 else -1))==3) "unanimous_decision" else if(any(votes==0)) "majority_decision" else "split_decision"
 list(winner=winner,reason=paste0(if(d$technical) "technical_" else "",kind))
}
.ra_jc_finish<-function(d,w,reason) {d$phase<-"finished";d$running<-FALSE;d["winner"]<-list(w);d$reason<-reason;d$break_ms<-0L;d["count"]<-list(NULL);d}
.ra_jc_validate<-function(d,config) {
 c<-config;r<-d$round;n<-length(d$cards);invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(n>r+1L) invalid("Scorecards cannot extend beyond the current round")
 for(card in d$cards) if(any(vapply(card,function(s) max(unlist(s))!=10 || min(unlist(s))<c$minimum_score,logical(1)))) invalid("Each unadjusted judge score must obey the configured ten-point-must scale")
 if(r+1<c$rounds && any(unlist(d$deductions[seq(r+2,c$rounds)])!=0)) invalid("Future rounds cannot contain deductions")
 if(!d$started && (r!=0 || n!=0 || d$remaining_ms!=c$duration_ms || any(unlist(d$deductions)!=0) || !is.null(d$count))) invalid("Unstarted bout must contain no round activity")
 if(d$phase!="finished") {
  if(!is.null(d$winner) || !is.null(d$reason)) invalid("Ongoing bout cannot retain a final decision")
  if(n!=(if(d$phase=="break") r+1L else r)) invalid("Completed scorecards must agree with the current round")
  if(d$phase=="break") {if(r==c$rounds-1L || d$remaining_ms!=0 || !d$running || d$break_ms==0) invalid("Inter-round break requires a scored nonfinal round")} else if(d$break_ms!=0) invalid("Break time outside an inter-round break")
  if(d$phase=="ready" && (d$remaining_ms!=c$duration_ms || d$running)) invalid("Ready round requires its full stopped clock")
  if(d$phase=="fighting" && (!d$started || d$remaining_ms==0)) invalid("Active round must be started and unexpired")
  if(d$phase=="judging" && (d$running || (d$remaining_ms!=0 && !d$technical))) invalid("Judge submission requires an expired or technically stopped round")
 } else {
  if(d$running || d$break_ms!=0 || !is.null(d$count) || is.null(d$reason)) invalid("Finished bout must have stopped clocks and a reason")
  if(grepl("decision|draw",d$reason) && d$reason!="technical_draw") {
   dec<-.ra_jc_decision(d)
   if(n!=r+1L || (!d$technical && (n!=c$rounds || d$remaining_ms!=0)) || !identical(as.integer(d$winner),as.integer(dec$winner)) || d$reason!=dec$reason) invalid("Judged result must agree with complete submitted cards")
  } else if(d$reason %in% c("no_contest","double_disqualification","technical_draw")) {if(!is.null(d$winner)) invalid("No-contest, technical draw or double exclusion has no winner")} else if(is.null(d$winner)) invalid("A decisive stoppage requires its winning participant")
  if(d$reason=="forfeit" && d$started) invalid("Pre-start forfeit cannot follow live action")
 }
 if(d$technical && (r<c$technical_after || !(d$phase %in% c("judging","finished")))) invalid("Technical score decision requires enough completed rounds and result submission")
 if(d$phase=="count") {if(!c$counts || is.null(d$count) || !d$started || d$running!=(d$remaining_ms>0)) invalid("Knockdown count requires the configured count format and clock state")} else if(!is.null(d$count)) invalid("Knockdown-count data outside its phase")
 invisible(TRUE)
}
.ra_jc_apply<-function(state,action,config) {
 d<-state$data;p<-action$params;k<-action$type;c<-config
 if(k=="stop") {
  .ra_fields(p,list(method=list(type="string",enum=as.list(c("knockout","tko","submission","disqualification","double_disqualification","no_contest","forfeit","technical_draw"))),winner=c(.ra_jc_player,list(nullable=TRUE))),"/params");method<-p$method
  if(is.null(p$winner)!=(method %in% c("no_contest","double_disqualification","technical_draw"))) .ra_fail("Decisive stoppages require a winner; nondecisive results must not name one")
  if((method=="forfeit" && d$started) || (!(method %in% c("forfeit","no_contest","disqualification","double_disqualification")) && !d$started)) .ra_fail("Stoppage method disagrees with whether the bout has started")
  if(c$counts && method %in% c("knockout","submission")) .ra_fail("Count-format knockouts require the tenth count; submissions are outside this format")
  return(.ra_jc_finish(d,p$winner,method))
 }
 if(k=="resume") {.ra_fields(p,list(),"/params");if(!(d$phase %in% c("ready","fighting")) || d$running) .ra_fail("Resume only a stopped ready or active round");d$phase<-"fighting";d$running<-TRUE;d$started<-TRUE;return(d)}
 if(k=="pause") {.ra_fields(p,list(),"/params");if(d$phase!="fighting" || !d$running) .ra_fail("Pause only an actively running round");d$running<-FALSE;return(d)}
 if(k=="advance_clock") {
  .ra_fields(p,list(elapsed_ms=list(type="integer",minimum=1,maximum=max(c$duration_ms,.ra_jc_rest(c)))),"/params");n<-p$elapsed_ms
  if(!d$running || !(d$phase %in% c("fighting","count","break"))) .ra_fail("Only official running time may advance")
  field<-if(d$phase=="break") "break_ms" else "remaining_ms"
  if(n>d[[field]]) .ra_fail("Clock event cannot pass the current interval limit")
  d[[field]]<-d[[field]]-n
  if(d[[field]]==0) {
   d$running<-FALSE
   if(d$phase=="break") {d$round<-d$round+1L;d$remaining_ms<-c$duration_ms;d$phase<-"ready"} else if(d$phase!="count") d$phase<-"judging"
  }
  return(d)
 }
 if(k=="deduct_points") {
  .ra_fields(p,list(player=.ra_jc_player,points=list(type="integer",minimum=1,maximum=2)),"/params")
  if(!(d$phase %in% c("fighting","count","judging")) || d$deductions[[d$round+1L]][[p$player+1L]]+p$points>10) .ra_fail("Record referee deductions in the current unscored round")
  d$deductions[[d$round+1L]][[p$player+1L]]<-d$deductions[[d$round+1L]][[p$player+1L]]+p$points;return(d)
 }
 if(k=="technical_stop") {.ra_fields(p,list(),"/params");if(d$phase!="fighting" || d$round<c$technical_after) .ra_fail("Technical scoring requires enough completed rounds and an active partial round");d$technical<-TRUE;d$phase<-"judging";d$running<-FALSE;return(d)}
 if(k=="submit_scores") {
  .ra_fields(p,list(scores=list(type="array",minItems=3,maxItems=3,items=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=c$minimum_score,maximum=10)))),"/params")
  if(d$phase!="judging" || any(vapply(p$scores,function(s) max(unlist(s))!=10,logical(1)))) .ra_fail("Submit three unadjusted ten-point-must scores for the current round")
  d$cards<-c(d$cards,list(p$scores))
  if(d$technical || d$round==c$rounds-1L) {dec<-.ra_jc_decision(d);return(.ra_jc_finish(d,dec$winner,dec$reason))}
  d$phase<-"break";d$break_ms<-.ra_jc_rest(c);d$running<-TRUE;return(d)
 }
 if(k=="knockdown") {.ra_fields(p,list(player=.ra_jc_player),"/params");if(!c$counts || d$phase!="fighting" || !d$running) .ra_fail("Record a knockdown only during active count-format fighting");d$count<-list(player=p$player,number=0L);d$phase<-"count";return(d)}
 if(k=="advance_count") {.ra_fields(p,list(number=list(type="integer",minimum=1,maximum=10)),"/params");if(d$phase!="count" || p$number!=d$count$number+1L) .ra_fail("Referee knockdown counts advance one number at a time");if(p$number==10) return(.ra_jc_finish(d,1L-d$count$player,"knockout"));d$count$number<-p$number;return(d)}
 if(k=="fighter_ready") {.ra_fields(p,list(),"/params");if(d$phase!="count" || d$count$number<8) .ra_fail("A ready boxer may resume only after the mandatory eight count");d["count"]<-list(NULL);d$phase<-if(d$remaining_ms>0) "fighting" else "judging";return(d)}
 .ra_fail("Unknown action")
}
.ra_jc_legal<-function(state,config) {
 d<-state$data;c<-config;actions<-list();types<-list();empty<-structure(list(),names=character())
 event<-function(k,params,cond,obs) list(type=k,params_schema=params,preconditions=list(cond),external_observations=list(obs))
 methods<-if(d$started) c("tko","disqualification","double_disqualification","no_contest","technical_draw",if(!c$counts) c("knockout","submission")) else c("forfeit","disqualification","double_disqualification","no_contest")
 types<-list(event("stop",list(method=list(type="string",enum=as.list(methods)),winner=c(.ra_jc_player,list(nullable=TRUE))),"Winning seat is required except no-contest, technical draw and double disqualification.","Officially adjudicated bout stoppage method and winner."))
 if(d$phase %in% c("ready","fighting")) actions<-list(list(type=if(d$running) "pause" else "resume",params=empty))
 if(d$running && d$phase %in% c("fighting","count","break")) types<-c(types,list(event("advance_clock",list(elapsed_ms=list(type="integer",minimum=1,maximum=if(d$phase=="break") d$break_ms else d$remaining_ms)),"Do not exceed the current interval.","Official elapsed running milliseconds.")))
 if(d$phase %in% c("fighting","count","judging")) types<-c(types,list(event("deduct_points",list(player=.ra_jc_player,points=list(type="integer",minimum=1,maximum=2)),"Deduction applies equally to all judges in the current unscored round.","Official referee point deduction, separate from raw judge scores.")))
 if(d$phase=="fighting" && d$round>=c$technical_after) types<-c(types,list(event("technical_stop",empty,"Score the partial round after the configured minimum completed rounds.","Official decision that a technical score result applies.")))
 if(d$phase=="judging") types<-c(types,list(event("submit_scores",list(scores=list(type="array",minItems=3,maxItems=3,items=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=c$minimum_score,maximum=10)))),"Each judge gives at least one athlete ten before referee deductions.","Three finalized judge score pairs, excluding separately recorded deductions.")))
 if(c$counts && d$phase=="fighting" && d$running) types<-c(types,list(event("knockdown",list(player=.ra_jc_player),"A physical knockdown has been adjudicated; no standing count.","Referee identifies the downed boxer.")))
 if(d$phase=="count") {actions<-c(actions,list(list(type="advance_count",params=list(number=d$count$number+1L))));if(d$count$number>=8) actions<-c(actions,list(list(type="fighter_ready",params=empty)))}
 list(mode="event_types",actions=actions,action_types=types,constraints=list(phase=d$phase,round=d$round+1L,remaining_ms=d$remaining_ms,break_ms=d$break_ms),completeness=FALSE,reason="official_clocks_referee_decisions_and_judge_cards")
}
.ra_jc_outcome<-function(state,pack,config) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;reason<-d$reason;kind<-"none"
 if(done) kind<-if(reason=="no_contest") "no_contest" else if(reason %in% c("disqualification","double_disqualification")) "disqualification" else if(reason=="forfeit") "forfeit" else if(is.null(w)) "draw" else if(d$technical) "technical" else "win"
 list(status=if(done) "terminal" else if(d$phase=="judging") "awaiting_adjudication" else "ongoing",result_kind=kind,winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && (!is.null(w) || reason=="double_disqualification")) lapply(setdiff(0:1,w),function(p) state$players[[p+1L]]$id) else list(),placements=if(done && grepl("decision|draw",reason) && reason!="technical_draw") lapply(1:3,function(j) list(judge=j-1L,scores=.ra_jc_totals(d)[[j]])) else list(),reason=if(done) reason else if(d$phase=="judging") "round_scores_required" else "ongoing",evidence_required=if(d$phase=="judging") list("Three finalized round score pairs.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
