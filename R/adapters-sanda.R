.ra_sanda_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_sanda_wins<-function(d) vapply(0:1,function(t) sum(vapply(d$rounds,function(w) !is.null(w) && w==t,logical(1))),numeric(1))
.ra_sanda_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d$break_ms<-0L;d}
.ra_sanda_award<-function(d,w) {
 d$rounds<-c(d$rounds,list(w));ww<-.ra_sanda_wins(d)
 if(max(ww)>=2) d<-.ra_sanda_finish(d,as.integer(ww[2]>ww[1]),"two_rounds") else if(length(d$rounds)>=3) {
  if(ww[1]!=ww[2]) d<-.ra_sanda_finish(d,as.integer(ww[2]>ww[1]),"rounds") else if(d$round_robin) d<-.ra_sanda_finish(d,NULL,"draw") else if(d$warnings[[1]]!=d$warnings[[2]]) d<-.ra_sanda_finish(d,as.integer(d$warnings[[2]]<d$warnings[[1]]),"fewer_warnings") else if(d$admonitions[[1]]!=d$admonitions[[2]]) d<-.ra_sanda_finish(d,as.integer(d$admonitions[[2]]<d$admonitions[[1]]),"fewer_admonitions") else {d$phase<-"break";d$break_ms<-60000L}
 } else {d$phase<-"break";d$break_ms<-60000L}
 d
}
.ra_sanda_gap<-function(d) {for(t in 0:1) if(all(vapply(d$scores,function(s) s[[t+1L]]-s[[2L-t]]>=12,logical(1)))) return(.ra_sanda_finish(d,t,"twelve_point_gap"));d}
.ra_sanda_add<-function(d,t,n) {d$scores<-lapply(d$scores,function(s) {s[[t+1L]]<-s[[t+1L]]+n;s});.ra_sanda_gap(d)}
.ra_sanda_votes<-function(d) lapply(d$scores,function(s) {
 pairs<-list(s,d$round_warnings,d$round_admonitions,d$weights_grams)
 for(j in 1:4) {pair<-unlist(pairs[[j]]);if(pair[1]!=pair[2]) return(if(j==1) as.integer(pair[2]>pair[1]) else as.integer(pair[2]<pair[1]))}
 NULL
})
.ra_sanda_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) if(d$round_warnings[[t]]>d$warnings[[t]] || d$round_admonitions[[t]]>d$admonitions[[t]] || d$round_counts[[t]]>d$total_counts[[t]]) bad("Round counters cannot exceed bout totals")
 if(d$phase %in% c("judging","split_review") && d$remaining_ms!=0) bad("Score review follows the round clock")
 if(d$phase=="break" && (length(d$rounds)==0 || d$break_ms==0)) bad("Rest follows a completed round")
 if(d$phase!="break" && d$break_ms!=0) bad("Rest clock outside a rest")
 if(d$phase=="ready" && d$remaining_ms!=120000) bad("Ready round has its full duration")
 if(d$phase %in% c("fighting","paused") && d$remaining_ms==0) bad("Expired rounds require judging")
 if(!d$phase %in% c("finished","break") && (max(unlist(d$off_platform))>=2 || max(unlist(d$round_counts))>=2)) bad("Round-ending event must leave active play")
 for(j in seq_along(d$rounds)) if(any(vapply(0:1,function(t) sum(vapply(head(d$rounds,j-1L),function(w) !is.null(w) && w==t,logical(1)))>=2,logical(1)))) bad("No round follows two round wins")
 if(d$phase=="finished") {
  if(is.null(d$reason) || is.null(d$winner)!=d$reason %in% c("draw","no_contest")) bad("Finished result must agree with winner presence")
  if(d$reason=="two_rounds" && .ra_sanda_wins(d)[d$winner+1L]!=2) bad("Two-round victory requires two wins")
  if(d$reason=="draw" && (!d$round_robin || length(d$rounds)<3 || .ra_sanda_wins(d)[1]!=.ra_sanda_wins(d)[2])) bad("Draw requires tied round-robin bout")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(.ra_sanda_wins(d))>=2 || max(unlist(d$warnings))>=3 || max(unlist(d$total_counts))>=3) bad("A decided bout must finish")
 if(d$phase!="finished" && .ra_sanda_gap(d)$phase=="finished") bad("Twelve-point consensus ends the bout")
}
.ra_sanda_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_sanda_i;p<-i(0,1);b<-list(type="boolean");empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="ready") {s$start_round<-empty;if(length(d$rounds)==0) s$configure<-list(round_robin=b,weights_grams=list(type="array",minItems=2,maxItems=2,items=i(1,200000)))}
 if(ph %in% c("fighting","break")) s$elapse<-list(milliseconds=i(1,120000))
 if(ph=="fighting") s$pause<-empty
 if(ph=="paused") s$resume<-empty
 if(ph %in% c("fighting","paused","judging")) {
  s$technical_score<-list(player=p,points=list(type="enum",values=list(1L,2L)),judges=list(type="array",minItems=5,maxItems=5,items=b));s$sanction<-list(player=p,kind=list(type="enum",values=list("admonition","warning")))
 }
 if(ph %in% c("fighting","paused")) {s$off_platform<-list(player=p);s$forcible_count<-list(player=p,continued=b)}
 if(ph=="judging") s$close_round<-empty
 if(ph=="split_review") s$head_judge_decision<-list(winner=c(p,list(nullable=TRUE)))
 c(s,list(referee_stop=list(winner=p),withdraw=list(player=p),disqualify=list(player=p),no_contest=empty))
}
.ra_sanda_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_sanda_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="configure") {d$round_robin<-p$round_robin;d$weights_grams<-p$weights_grams} else if(k %in% c("start_round","resume")) d$phase<-"fighting" else if(k=="pause") d$phase<-"paused" else if(k=="elapse") {
  field<-if(d$phase=="break") "break_ms" else "remaining_ms"
  if(p$milliseconds>d[[field]]) .ra_fail("Elapsed time exceeds the current interval")
  d[[field]]<-d[[field]]-p$milliseconds
  if(d[[field]]==0) {
   if(field=="break_ms") {d$phase<-"ready";d$remaining_ms<-120000L;d$scores<-rep(list(list(0L,0L)),5);d$round_warnings<-list(0L,0L);d$round_admonitions<-list(0L,0L);d$round_counts<-list(0L,0L);d$off_platform<-list(0L,0L)} else d$phase<-"judging"
  }
 } else if(k=="technical_score") {
  if(!any(unlist(p$judges))) .ra_fail("At least one judge must award the technical score")
  for(j in 1:5) if(p$judges[[j]]) d$scores[[j]][[p$player+1L]]<-d$scores[[j]][[p$player+1L]]+p$points
  d<-.ra_sanda_gap(d)
 } else if(k=="sanction") {
  t<-p$player;kind<-p$kind;name<-if(kind=="warning") "warnings" else "admonitions";rname<-paste0("round_",name);d[[name]][[t+1L]]<-d[[name]][[t+1L]]+1L;d[[rname]][[t+1L]]<-d[[rname]][[t+1L]]+1L
  d<-.ra_sanda_add(d,1L-t,if(kind=="warning") 2L else 1L)
  if(d$warnings[[t+1L]]==3) d<-.ra_sanda_finish(d,1L-t,"three_warnings")
 } else if(k=="off_platform") {
  t<-p$player;d$off_platform[[t+1L]]<-d$off_platform[[t+1L]]+1L;d<-.ra_sanda_add(d,1L-t,2L)
  if(d$phase!="finished" && d$off_platform[[t+1L]]==2) d<-.ra_sanda_award(d,1L-t)
  if(d$phase=="fighting") d$phase<-"paused"
 } else if(k=="forcible_count") {
  t<-p$player;d$round_counts[[t+1L]]<-d$round_counts[[t+1L]]+1L;d$total_counts[[t+1L]]<-d$total_counts[[t+1L]]+1L;d<-.ra_sanda_add(d,1L-t,2L)
  if(!p$continued) d<-.ra_sanda_finish(d,1L-t,"knockout") else if(d$total_counts[[t+1L]]==3) d<-.ra_sanda_finish(d,1L-t,"three_counts") else if(d$phase!="finished" && d$round_counts[[t+1L]]==2) d<-.ra_sanda_award(d,1L-t)
  if(d$phase=="fighting") d$phase<-"paused"
 } else if(k=="close_round") {
  vv<-.ra_sanda_votes(d);found<-FALSE
  for(w in list(0L,1L,NULL)) if(sum(vapply(vv,function(v) identical(v,w),logical(1)))>=3) {d<-.ra_sanda_award(d,w);found<-TRUE;break}
  if(!found) d$phase<-"split_review"
 } else if(k=="head_judge_decision") d<-.ra_sanda_award(d,p$winner) else if(k=="referee_stop") d<-.ra_sanda_finish(d,p$winner,"referee_stop") else if(k %in% c("withdraw","disqualify")) d<-.ra_sanda_finish(d,1L-p$player,if(k=="withdraw") "withdrawal" else "disqualification") else if(k=="no_contest") d<-.ra_sanda_finish(d,NULL,"no_contest")
 d
}
.ra_sanda_legal<-function(state) {
 d<-state$data;s<-.ra_sanda_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Round, referee counters and scoring conditions must permit the event."),external_observations=list("Judge-approved technical points, official completed counts, platform exits and sanctions."))),constraints=list(phase=d$phase,round=length(d$rounds)+(if(d$phase %in% c("break","finished")) 0L else 1L),remaining_ms=d$remaining_ms,break_ms=d$break_ms,rounds_won=as.list(.ra_sanda_wins(d)),scores=d$scores,warnings=d$warnings,admonitions=d$admonitions,round_counts=d$round_counts,total_counts=d$total_counts,off_platform=d$off_platform),completeness=FALSE,reason="official_2017_sanda_events")
}
.ra_sanda_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;review<-d$phase %in% c("judging","split_review")
 list(status=if(done) "terminal" else if(review) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$reason=="no_contest") "no_contest" else if(is.null(w)) "draw" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(review) "round_review_required" else "ongoing",evidence_required=if(review) list("Official score-sheet review or head judge ruling.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
