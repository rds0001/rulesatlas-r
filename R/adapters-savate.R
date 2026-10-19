.ra_savate_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_savate_cardvalid<-function(card) is.null(card) || all(vapply(card,function(pair) paste(unlist(pair),collapse=",") %in% c("2,2","3,2","2,3","3,1","1,3"),logical(1)))
.ra_savate_totals<-function(d) lapply(1:3,function(j) lapply(1:2,function(t) sum(vapply(d$cards,function(card) if(is.null(card[[j]])) 0 else sum(vapply(1:2,function(k) card[[j]][[k]][[t]],numeric(1))),numeric(1)))-2L*d$penalties[[t]]))
.ra_savate_votes<-function(d) {
 s<-.ra_savate_totals(d)
 lapply(1:3,function(j) if(all(vapply(d$cards,function(card) is.null(card[[j]]),logical(1)))) "non_decision" else if(s[[j]][[1]]==s[[j]][[2]]) "draw" else as.character(as.integer(s[[j]][[2]]>s[[j]][[1]])))
}
.ra_savate_majority<-function(d) {vv<-unlist(.ra_savate_votes(d));for(v in c("0","1","draw","non_decision")) if(sum(vv==v)>=2) return(v);NULL}
.ra_savate_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$break_ms<-0L;d}
.ra_savate_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(row in d$cards) for(card in row) if(!.ra_savate_cardvalid(card)) bad("Each category must be tied, dominated or markedly dominated")
 n<-length(d$cards);r<-d$round
 if(n>r || n<r-1L) bad("Cards must agree with the current round")
 if(d$phase %in% c("ready","fighting","paused","judging") && n!=r-1L) bad("Current round cannot already be scored")
 if(d$phase %in% c("break","decision","delegate") && n!=r) bad("Completed-round phase requires its three cards")
 if(d$phase=="ready" && d$remaining_ms!=120000) bad("Ready round requires its full clock")
 if(d$phase %in% c("fighting","paused") && d$remaining_ms==0) bad("Expired round requires judging")
 if(d$phase=="judging" && d$remaining_ms!=0 && !d$technical_stop) bad("Partial-round judging requires official stoppage")
 if(d$phase=="break" && (r==3 || d$break_ms==0 || d$remaining_ms!=0 || d$technical_stop)) bad("Rest follows a completed nonfinal round")
 if(d$phase!="break" && d$break_ms!=0) bad("Rest clock outside an interval")
 if(d$phase %in% c("decision","delegate") && !(r==3 || d$technical_stop)) bad("Final decision requires all rounds or official stoppage")
 if(d$phase=="finished") {
  if(is.null(d$reason) || is.null(d$winner)!=d$reason %in% c("draw","non_decision","double_disqualification")) bad("Result must agree with winner presence")
  if(d$reason=="judges" && !identical(.ra_savate_majority(d),as.character(d$winner))) bad("Judge victory must agree with adjusted cards")
  if(d$reason=="draw" && (!d$allow_draw || !identical(.ra_savate_majority(d),"draw"))) bad("Draw requires a majority and permitted format")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(d$penalties))>=3) bad("A decided bout cannot continue")
}
.ra_savate_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_savate_i;p<-i(0,1);b<-list(type="boolean");empty<-setNames(list(),character());pair<-list(type="array",minItems=2,maxItems=2,items=i(1,3));card<-list(type="array",minItems=2,maxItems=2,items=pair,nullable=TRUE);cards<-list(type="array",minItems=3,maxItems=3,items=card);approval<-list(type="array",minItems=3,maxItems=3,items=b)
 if(ph=="finished") return(s)
 if(ph=="ready") {s$start_round<-empty;if(d$round==1) s$configure<-list(allow_draw=b)}
 if(ph %in% c("fighting","break")) s$elapse<-list(milliseconds=i(1,120000))
 if(ph=="fighting") s$pause<-empty
 if(ph=="paused") s$resume<-empty
 if(ph %in% c("fighting","paused")) s$medical_score_stop<-empty
 if(ph=="judging") s$submit_round<-list(cards=cards)
 if(ph=="decision") {s$confirm_result<-empty;s$correct_round<-list(round=i(1,length(d$cards)),cards=cards)}
 if(ph=="delegate") s$delegate_decision<-list(winner=p)
 if(!ph %in% c("decision","delegate")) s$penalty_vote<-list(player=p,approvals=approval)
 c(s,list(disqualify_vote=list(player=p,approvals=approval),double_disqualification=list(approvals=list(type="array",minItems=2,maxItems=2,items=approval)),withdraw=list(player=p),referee_stop=list(winner=p),non_decision=empty))
}
.ra_savate_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_savate_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="configure") d$allow_draw<-p$allow_draw else if(k %in% c("start_round","resume")) d$phase<-"fighting" else if(k=="pause") d$phase<-"paused" else if(k=="elapse") {
  f<-if(d$phase=="break") "break_ms" else "remaining_ms"
  if(p$milliseconds>d[[f]]) .ra_fail("Elapsed time exceeds the current interval")
  d[[f]]<-d[[f]]-p$milliseconds
  if(d[[f]]==0) {
   if(f=="break_ms") {d$phase<-"ready";d$round<-d$round+1L;d$remaining_ms<-120000L} else d$phase<-"judging"
  }
 } else if(k=="medical_score_stop") {d$phase<-"judging";d$technical_stop<-TRUE} else if(k %in% c("submit_round","correct_round")) {
  if(!all(vapply(p$cards,.ra_savate_cardvalid,logical(1)))) .ra_fail("Each category must use an allowed scoring pair")
  if(k=="correct_round") d$cards[[p$round]]<-p$cards else {
   d$cards<-c(d$cards,list(p$cards))
   if(d$round==3 || d$technical_stop) d$phase<-"decision" else {d$phase<-"break";d$break_ms<-60000L}
  }
 } else if(k=="penalty_vote") {
  if(sum(unlist(p$approvals))>=2) {
   d$penalties[[p$player+1L]]<-d$penalties[[p$player+1L]]+1L
   if(d$penalties[[p$player+1L]]==3) d<-.ra_savate_finish(d,1L-p$player,"disqualification")
  }
  if(d$phase=="fighting") d$phase<-"paused"
 } else if(k=="disqualify_vote") {
  if(sum(unlist(p$approvals))>=2) d<-.ra_savate_finish(d,1L-p$player,"disqualification") else if(d$phase=="fighting") d$phase<-"paused"
 } else if(k=="double_disqualification") {
  if(any(vapply(p$approvals,function(v) sum(unlist(v))<2,logical(1)))) .ra_fail("Both exclusions require a separate judge majority")
  d<-.ra_savate_finish(d,NULL,"double_disqualification")
 } else if(k=="confirm_result") {
  m<-.ra_savate_majority(d)
  if(!is.null(m) && m %in% c("0","1")) d<-.ra_savate_finish(d,as.integer(m),"judges") else if(identical(m,"non_decision")) d<-.ra_savate_finish(d,NULL,"non_decision") else if(identical(m,"draw") && d$allow_draw) d<-.ra_savate_finish(d,NULL,"draw") else d$phase<-"delegate"
 } else if(k=="delegate_decision") d<-.ra_savate_finish(d,p$winner,"delegate") else if(k=="withdraw") d<-.ra_savate_finish(d,1L-p$player,"withdrawal") else if(k=="referee_stop") d<-.ra_savate_finish(d,p$winner,"referee_stop") else if(k=="non_decision") d<-.ra_savate_finish(d,NULL,"non_decision")
 d
}
.ra_savate_legal<-function(state) {
 d<-state$data;s<-.ra_savate_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Round, judge approval, scorecard and penalty conditions must permit the event."),external_observations=list("Official paired category marks, accepted penalty votes and stoppage rulings."))),constraints=list(phase=d$phase,round=d$round,remaining_ms=d$remaining_ms,break_ms=d$break_ms,penalties=d$penalties,judge_totals=.ra_savate_totals(d),judge_votes=.ra_savate_votes(d)),completeness=FALSE,reason="official_assaut_double_category_cards")
}
.ra_savate_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;review<-d$phase %in% c("judging","decision","delegate")
 list(status=if(done) "terminal" else if(review) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$reason=="non_decision") "no_contest" else if(d$reason=="draw") "draw" else if(d$reason=="double_disqualification") "disqualification" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else if(identical(d$reason,"double_disqualification")) lapply(state$players,function(p) p$id) else list(),placements=list(),reason=if(done) d$reason else if(review) "official_judging_required" else "ongoing",evidence_required=if(review) list("Official judge cards or delegate decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
