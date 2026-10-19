.ra_mt_config<-list(rounds=5L,duration_ms=180000L,minimum_score=7L,counts=TRUE,technical_after=5L,rest_ms=120000L)
.ra_mt_players<-list(type="array",minItems=1,maxItems=2,items=list(type="integer",minimum=0,maximum=1))
.ra_mt_draws<-c("double_count_out","simultaneous_count_limit","mutual_stoppage_draw")
.ra_mt_clean<-function(d) {if(d$phase=="finished") {d$down<-list();d["count_kind"]<-list(NULL);d$count_paused<-FALSE;d$recent_counts<-list(NULL,NULL);d["recovery"]<-list(NULL);d["warning_origin"]<-list(NULL)};d}
.ra_mt_finish<-function(d,w,reason) .ra_mt_clean(.ra_jc_finish(d,w,reason))
.ra_mt_end_count<-function(d) {
 d["count"]<-list(NULL);d$down<-list();d["count_kind"]<-list(NULL);d$count_paused<-FALSE;d$phase<-if(d$remaining_ms>0) "fighting" else "judging";d$running<-d$remaining_ms>0
 if(d$phase=="judging") d$recent_counts<-list(NULL,NULL)
 d
}
.ra_mt_limits<-function(d) (0:1)[vapply(1:2,function(p) any(vapply(d$knockdowns,function(r) r[[p]]>=3,logical(1))) || sum(vapply(d$knockdowns,function(r) r[[p]],numeric(1)))>=5,logical(1))]
.ra_mt_warn<-function(d,p) {
 if(!(d$phase %in% c("fighting","judging","count")) || d$running || (d$phase=="count" && !d$count_paused)) .ra_fail("Formal warnings require stopped fighting or a paused count")
 d$warnings[[d$round+1L]][[p+1L]]<-d$warnings[[d$round+1L]][[p+1L]]+1L;d$deductions[[d$round+1L]][[p+1L]]<-d$deductions[[d$round+1L]][[p+1L]]+1L
 if(sum(vapply(d$warnings,function(r) r[[p+1L]],numeric(1)))==3) {d$warning_origin<-d$phase;d$phase<-"warning_decision"}
 d
}
.ra_mt_validate<-function(d) {
 x<-d;invalid<-function(msg) .ra_fail(msg,code="INVALID_STATE")
 if(d$phase=="warning_decision" && (is.null(d$warning_origin) || !(d$warning_origin %in% c("fighting","judging","count")))) invalid("Warning decision requires a valid preceding phase")
 if(d$phase=="recovery") x$phase<-"fighting"
 if(d$phase=="warning_decision") x$phase<-d$warning_origin
 if(x$phase=="count") x$running<-x$remaining_ms>0
 if(!is.null(d$reason) && d$reason %in% .ra_mt_draws) x$reason<-"technical_draw"
 if(!is.null(d$reason) && d$reason %in% c("no_decision","cancelled")) x$reason<-"no_contest"
 .ra_jc_validate(x,.ra_mt_config)
 if(!d$started && (any(unlist(c(d$knockdowns,d$cautions))!=0) || any(!vapply(d$recent_counts,is.null,logical(1))))) invalid("Count and caution history requires a started bout")
 if(d$technical) invalid("This WMC variant does not use partial technical scorecards")
 if(!identical(as.integer(unlist(d$warnings)),as.integer(unlist(d$deductions)))) invalid("Every formal warning deducts exactly one point")
 if(d$round<4 && any(unlist(c(d$knockdowns[seq(d$round+2L,5L)],d$warnings[seq(d$round+2L,5L)]))!=0)) invalid("Future rounds cannot contain counts or warnings")
 for(r in seq_along(d$cards)) for(scores in d$cards[[r]]) if(scores[[1]]!=scores[[2]]) {
  loser<-if(scores[[1]]<scores[[2]]) 1L else 2L;kds<-d$knockdowns[[r]][[loser]]
  if((kds==1 && scores[[loser]]>8) || (kds==2 && scores[[loser]]!=7)) invalid("Losing-round scores must reflect one or two counts before warning deductions")
 }
 limited<-.ra_mt_limits(d)
 if(length(limited)>0) {
  expected<-if(length(limited)==2) "simultaneous_count_limit" else "count_limit_tko";w<-if(length(limited)==2) NULL else 1L-limited[1]
  if(d$phase!="finished" || is.null(d$reason) || d$reason!=expected || !identical(as.integer(d$winner),as.integer(w))) invalid("Three round counts or five bout counts immediately stop the bout")
 }
 totals<-vapply(1:2,function(p) sum(vapply(d$warnings,function(r) r[[p]],numeric(1))),numeric(1));warned<-which(totals>=3)-1L
 if(length(warned)>1 || any(totals>3)) invalid("A third formal warning requires immediate resolution")
 if(length(warned)>0 && !(d$phase=="warning_decision" || (d$phase=="finished" && (identical(d$reason,"no_decision") || (identical(d$reason,"disqualification") && !is.null(d$winner) && d$winner==1L-warned[1]))))) invalid("Third warning must resolve as exclusion or no decision")
 if(d$phase=="warning_decision") {if(!length(warned) || is.null(d$warning_origin) || !(d$warning_origin %in% c("fighting","judging","count")) || d$running) invalid("Warning decision requires a stopped third-warning event")} else if(!is.null(d$warning_origin)) invalid("Warning origin outside its decision phase")
 count_phase<-d$phase=="count" || (d$phase=="warning_decision" && identical(d$warning_origin,"count"))
 if(count_phase) {
  if(!length(d$down) || anyDuplicated(unlist(d$down)) || d$count$player!=d$down[[1]] || is.null(d$count_kind) || d$count$number>=(if(d$count_kind=="ring_out") 20 else 10)) invalid("Active count requires its distinct counted players, kind and unfinished number")
 } else if(length(d$down)>0 || !is.null(d$count_kind) || d$count_paused) invalid("Count metadata outside an active count")
 if(d$phase=="recovery") {if(is.null(d$recovery) || d$running || d$remaining_ms==0) invalid("Recovery interval requires a stopped unexpired contest")} else if(!is.null(d$recovery)) invalid("Recovery timer outside its phase")
 invisible(TRUE)
}
.ra_mt_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;player<-.ra_jc_player
 if(d$phase=="warning_decision") {
  if(k!="resolve_warning") .ra_fail("Resolve the third warning before continuing")
  .ra_fields(p,list(result=list(type="string",enum=list("disqualification","no_decision"))),"/params");loser<-which(vapply(1:2,function(q) sum(vapply(d$warnings,function(r) r[[q]],numeric(1)))==3,logical(1)))[1]-1L
  return(.ra_mt_finish(d,if(p$result=="disqualification") 1L-loser else NULL,p$result))
 }
 if(k=="official_result") {
  .ra_fields(p,list(result=list(type="string",enum=list("no_decision","cancelled","mutual_stoppage_draw"))),"/params")
  if(!d$started && p$result=="mutual_stoppage_draw") .ra_fail("Mutual stoppage requires a started contest")
  return(.ra_mt_finish(d,NULL,p$result))
 }
 if(k=="warn") {.ra_fields(p,list(player=player),"/params");return(.ra_mt_warn(d,p$player))}
 if(k=="caution") {
  .ra_fields(p,list(player=player,kind=list(type="string",enum=list("leg_push","fake_fall"))),"/params");q<-p$player;i<-if(p$kind=="leg_push") 1L else 2L
  if(d$phase!="fighting" || d$running) .ra_fail("Record cautions during a referee stoppage")
  if(d$cautions[[q+1L]][[i]]==2) return(.ra_mt_warn(d,q))
  d$cautions[[q+1L]][[i]]<-d$cautions[[q+1L]][[i]]+1L;return(d)
 }
 if(k %in% c("deduct_points","technical_stop","knockdown","fighter_ready")) .ra_fail("Use the WMC warning and count events for this profile")
 if(k=="start_count") {
  .ra_fields(p,list(players=.ra_mt_players,kind=list(type="string",enum=list("knockdown","ring_out"))),"/params")
  if(!d$started || !(d$phase %in% c("fighting","ready")) || (d$phase=="fighting" && !d$running) || anyDuplicated(unlist(p$players))) .ra_fail("Begin a count for distinct participants during active fighting or after a completed break")
  down<-sort(unlist(p$players));d$recent_counts<-list(NULL,NULL)
  if(p$kind=="knockdown") {
   for(q in down) d$knockdowns[[d$round+1L]][[q+1L]]<-d$knockdowns[[d$round+1L]][[q+1L]]+1L
   limited<-.ra_mt_limits(d)
   if(length(limited)>0) return(.ra_mt_finish(d,if(length(limited)==2) NULL else 1L-limited[1],if(length(limited)==2) "simultaneous_count_limit" else "count_limit_tko"))
  }
  d$phase<-"count";d$running<-TRUE;d$count<-list(player=down[1],number=0L);d$down<-as.list(down);d$count_kind<-p$kind;d$count_paused<-FALSE;return(d)
 }
 if(k %in% c("pause_count","resume_count")) {.ra_fields(p,list(),"/params");if(d$phase!="count" || d$count_paused==(k=="pause_count")) .ra_fail("Count pause state does not permit this event");d$count_paused<-k=="pause_count";return(d)}
 if(k=="pause" && d$phase=="count") {.ra_fields(p,list(),"/params");if(!d$running) .ra_fail("Clock already stopped");d$running<-FALSE;d$count_paused<-TRUE;return(d)}
 if(k=="resume" && d$phase=="count") {.ra_fields(p,list(),"/params");if(d$running || d$remaining_ms==0) .ra_fail("Resume only a stopped unexpired count clock");d$running<-TRUE;return(d)}
 if(k=="advance_count") {
  .ra_fields(p,list(number=list(type="integer",minimum=1,maximum=20)),"/params")
  if(d$phase!="count" || d$count_paused || p$number!=d$count$number+1L) .ra_fail("An unpaused referee count advances one number at a time")
  limit<-if(d$count_kind=="ring_out") 20L else 10L
  if(p$number==limit) return(.ra_mt_finish(d,if(length(d$down)==2) NULL else 1L-d$down[[1]],if(length(d$down)==2) "double_count_out" else if(limit==20) "tko" else "knockout"))
  d$count$number<-p$number;return(d)
 }
 if(k=="counted_fighter_ready") {
  .ra_fields(p,list(player=player),"/params");q<-p$player
  if(d$phase!="count" || d$count_kind!="knockdown" || d$count_paused || !(q %in% unlist(d$down)) || d$count$number<8) .ra_fail("A counted boxer must be ready at or after eight before ten")
  d$recent_counts[[q+1L]]<-d$count$number;d$down<-as.list(setdiff(unlist(d$down),q))
  if(!length(d$down)) return(.ra_mt_end_count(d))
  d$count$player<-d$down[[1]];return(d)
 }
 if(k=="return_to_ring") {
  .ra_fields(p,list(players=.ra_mt_players),"/params")
  if(d$phase!="count" || d$count_kind!="ring_out" || d$count_paused || anyDuplicated(unlist(p$players)) || any(!(unlist(p$players) %in% unlist(d$down)))) .ra_fail("Return distinct currently counted boxers before twenty")
  if(length(d$down)==2 && length(p$players)==1) return(.ra_mt_finish(d,p$players[[1]],"ring_return_win"))
  return(.ra_mt_end_count(d))
 }
 if(k=="recollapse") {
  .ra_fields(p,list(player=player),"/params");q<-p$player
  if(d$phase!="fighting" || !d$running || is.null(d$recent_counts[[q+1L]])) .ra_fail("Resume a prior count only after a recorded return and without a new blow")
  d$phase<-"count";d$count<-list(player=q,number=d$recent_counts[[q+1L]]);d$down<-list(q);d$count_kind<-"knockdown";d$count_paused<-FALSE;return(d)
 }
 if(k=="new_exchange") {.ra_fields(p,list(),"/params");if(d$phase!="fighting" || !d$running) .ra_fail("New exchange requires active fighting");d$recent_counts<-list(NULL,NULL);return(d)}
 if(k=="start_recovery") {.ra_fields(p,list(player=player),"/params");if(d$phase!="fighting") .ra_fail("A recovery interval requires an ongoing round");d$phase<-"recovery";d$running<-FALSE;d$recovery<-list(player=p$player,remaining_ms=300000L);return(d)}
 if(d$phase=="recovery") {
  if(k=="advance_recovery") {.ra_fields(p,list(elapsed_ms=list(type="integer",minimum=1,maximum=300000)),"/params");if(p$elapsed_ms>d$recovery$remaining_ms) .ra_fail("Recovery cannot exceed five minutes");d$recovery$remaining_ms<-d$recovery$remaining_ms-p$elapsed_ms;return(d)}
  if(k=="recovered") {.ra_fields(p,list(),"/params");d$phase<-"fighting";d$running<-FALSE;d["recovery"]<-list(NULL);return(d)}
  if(k=="recovery_failure") {.ra_fields(p,list(result=list(type="string",enum=list("loss","no_decision"))),"/params");q<-d$recovery$player;return(.ra_mt_finish(d,if(p$result=="loss") 1L-q else NULL,if(p$result=="loss") "tko" else "no_decision"))}
  if(k!="stop") .ra_fail("Resolve or time the recovery interval before continuing")
 }
 oldround<-d$round;state$data<-d;result<-.ra_jc_apply(state,action,.ra_mt_config)
 if(result$round!=oldround || result$phase %in% c("break","judging")) result$recent_counts<-list(NULL,NULL)
 .ra_mt_clean(result)
}
.ra_mt_legal<-function(state) {
 d<-state$data;types<-list();actions<-list();empty<-structure(list(),names=character());player<-.ra_jc_player
 event<-function(k,params,cond,obs) list(type=k,params_schema=params,preconditions=list(cond),external_observations=list(obs))
 if(d$phase=="warning_decision") types<-list(event("resolve_warning",list(result=list(type="string",enum=list("disqualification","no_decision"))),"Third warning requires a final disposition.","Referee chooses disqualification or no decision.")) else {
  base<-.ra_jc_legal(state,.ra_mt_config);types<-Filter(function(x) !(x$type %in% c("deduct_points","technical_stop","knockdown")),base$action_types);actions<-base$actions
  types<-c(types,list(event("official_result",list(result=list(type="string",enum=as.list(c("no_decision","cancelled",if(d$started) "mutual_stoppage_draw")))),"Use the stated official disposition.","Chairman or referee declares no decision, cancellation or mutual inability to continue.")))
  if(d$phase %in% c("fighting","judging","count") && !d$running && (d$phase!="count" || d$count_paused)) types<-c(types,list(event("warn",list(player=player),"Deduct one point; the third warning requires a final decision.","Formal referee warning, not a mere caution.")))
  if(d$phase=="fighting") {
   types<-c(types,list(event("start_recovery",list(player=player),"At most five minutes; contest time is stopped.","Referee grants the specific foul-recovery interval.")))
   if(!d$running) types<-c(types,list(event("caution",list(player=player,kind=list(type="string",enum=list("leg_push","fake_fall"))),"Two recorded cautions of the named category precede subsequent formal warnings.","Referee identifies the repeatable caution category."))) else if(any(!vapply(d$recent_counts,is.null,logical(1)))) {
    actions<-c(actions,list(list(type="new_exchange",params=empty)))
    for(p in 0:1) if(!is.null(d$recent_counts[[p+1L]])) actions<-c(actions,list(list(type="recollapse",params=list(player=p))))
   }
  }
  if(d$started && (d$phase=="ready" || (d$phase=="fighting" && d$running))) types<-c(types,list(event("start_count",list(players=.ra_mt_players,kind=list(type="string",enum=list("knockdown","ring_out"))),"Count distinct players; knockdown limits are three per round or five in the bout.","Referee identifies the counted player or simultaneous counted players and count kind.")))
  if(d$phase=="count") {
   actions<-list(list(type=if(d$count_paused) "resume_count" else "pause_count",params=empty))
   if(d$remaining_ms>0) actions<-c(actions,list(list(type=if(d$running) "pause" else "resume",params=empty)))
   if(!d$count_paused) {
    actions<-c(actions,list(list(type="advance_count",params=list(number=d$count$number+1L))))
    if(d$count_kind=="knockdown" && d$count$number>=8) actions<-c(actions,lapply(d$down,function(p) list(type="counted_fighter_ready",params=list(player=p))))
    if(d$count_kind=="ring_out") types<-c(types,list(event("return_to_ring",list(players=.ra_mt_players),"Return counted players before twenty.","Referee reports the returning player or both simultaneous returnees.")))
   }
  }
  if(d$phase=="recovery") {
   actions<-list(list(type="recovered",params=empty))
   if(d$recovery$remaining_ms>0) types<-c(types,list(event("advance_recovery",list(elapsed_ms=list(type="integer",minimum=1,maximum=d$recovery$remaining_ms)),"No more than the remaining recovery interval.","Official elapsed recovery milliseconds.")))
   types<-c(types,list(event("recovery_failure",list(result=list(type="string",enum=list("loss","no_decision"))),"Referee resolves refusal or inability to continue.","Official loss or no-decision ruling.")))
  }
 }
 list(mode="event_types",actions=actions,action_types=types,constraints=list(phase=d$phase,round=d$round+1L,remaining_ms=d$remaining_ms,break_ms=d$break_ms,count_paused=d$count_paused),completeness=FALSE,reason="wmc_counts_warnings_clocks_and_judged_rounds")
}
.ra_mt_outcome<-function(state,pack) {
 result<-.ra_jc_outcome(state,pack,.ra_mt_config);d<-state$data
 if(d$phase=="warning_decision") {result$status<-"awaiting_adjudication";result$reason<-"third_warning_disposition";result$evidence_required<-list("Disqualification or no-decision ruling.")}
 if(identical(d$reason,"no_decision")) result$result_kind<-"other"
 if(identical(d$reason,"cancelled")) result$result_kind<-"no_contest"
 if(!is.null(d$reason) && d$reason %in% c(.ra_mt_draws,"no_decision","cancelled")) result$placements<-list()
 result
}
