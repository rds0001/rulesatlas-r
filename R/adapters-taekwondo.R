.ra_taekwondo_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_taekwondo_values<-c(punch=1,body=2,turn_body=4,head=3,turn_head=5)
.ra_taekwondo_roundscores<-function(d,r) vapply(0:1,function(t) sum(vapply(names(.ra_taekwondo_values),function(k) d$techniques[[r+1L]][[t+1L]][[k]]*.ra_taekwondo_values[[k]],numeric(1)))+d$gam_jeom[[r+1L]][[2L-t]],numeric(1))
.ra_taekwondo_scores<-function(d) if(d$round==4) .ra_taekwondo_roundscores(d,3L) else vapply(0:1,function(t) sum(vapply(0:2,function(r) .ra_taekwondo_roundscores(d,r)[t+1L],numeric(1))),numeric(1))
.ra_taekwondo_penalties<-function(d,all_rounds=FALSE) vapply(0:1,function(t) sum(vapply(seq_len(if(all_rounds) 4L else 3L),function(r) d$gam_jeom[[r]][[t+1L]],numeric(1))),numeric(1))
.ra_taekwondo_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d["count"]<-list(NULL);d$break_ms<-0L;d}
.ra_taekwondo_evaluate<-function(d) {
 s<-.ra_taekwondo_scores(d)
 if(d$round==4) {
  for(t in 0:1) {
   if(d$gam_jeom[[4]][[t+1L]]>=2) return(.ra_taekwondo_finish(d,1L-t,"golden_penalties"))
  }
  for(t in 0:1) {
   if(s[t+1L]>=2) return(.ra_taekwondo_finish(d,t,"golden_points"))
  }
 } else {
  for(t in 0:1) if(.ra_taekwondo_penalties(d)[t+1L]>=10) return(.ra_taekwondo_finish(d,1L-t,"punitive"))
  if(d$point_gap && (d$round==3 || (d$round==2 && d$phase=="round_review")) && abs(s[1]-s[2])>=20) return(.ra_taekwondo_finish(d,as.integer(s[2]>s[1]),"point_gap"))
 }
 d
}
.ra_taekwondo_superiority<-function(d) {
 values<-list(vapply(0:1,function(t) d$techniques[[4]][[t+1L]]$punch,numeric(1)),unlist(d$pss_hits),vapply(0:1,function(t) sum(vapply(0:2,function(r) {s<-.ra_taekwondo_roundscores(d,r);s[t+1L]>s[2L-t]},logical(1))),numeric(1)),-.ra_taekwondo_penalties(d,TRUE))
 for(pair in values) if(pair[1]!=pair[2]) return(as.integer(pair[2]>pair[1]))
 NULL
}
.ra_taekwondo_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");r<-d$round
 if(r<4) for(j in seq.int(r+1L,4L)) if(any(unlist(d$techniques[[j]])!=0) || any(unlist(d$gam_jeom[[j]])!=0)) bad("Future rounds cannot contain scores or penalties")
 if(d$phase=="ready" && (d$remaining_ms!=(if(r==4) 60000 else 120000) || d$break_ms!=0)) bad("Ready round requires its full duration")
 if(d$phase=="break" && (r==1 || d$break_ms==0 || d$remaining_ms!=(if(r==4) 60000 else 120000))) bad("Inter-round rest precedes the next full round")
 if(d$phase!="break" && d$break_ms!=0) bad("Rest clock outside the inter-round break")
 if(d$phase %in% c("fighting","paused") && d$remaining_ms==0) bad("Expired round requires review")
 if(d$phase %in% c("round_review","superiority") && d$remaining_ms!=0) bad("Round review requires an expired clock")
 if(d$phase=="superiority" && (r!=4 || is.null(d$pss_hits))) bad("Superiority follows the golden round and recorded PSS hits")
 if(r==4 && d$remaining_ms>60000) bad("Golden round lasts one minute")
 if(r==4 && sum(vapply(0:2,function(j) {s<-.ra_taekwondo_roundscores(d,j);s[1]-s[2]},numeric(1)))!=0) bad("Golden round requires tied regulation scores")
 if(r!=4 && !is.null(d$pss_hits)) bad("PSS tie-break totals belong to the golden round")
 if(d$phase=="count") {
  if(is.null(d$count) || d$remaining_ms==0) bad("Count requires an unexpired stopped round")
  if(anyDuplicated(unlist(d$count$players))) bad("Counted athletes must be distinct")
  if(any(vapply(0:1,function(t) d$count$ready[[t+1L]] && !t %in% unlist(d$count$players),logical(1)))) bad("Recovery concerns counted athletes only")
 } else if(!is.null(d$count)) bad("Count data outside a count")
 if(d$phase=="finished") {
  if(is.null(d$reason) || is.null(d$winner)!=(d$reason=="no_contest")) bad("Terminal result must identify winner or no contest")
  if(d$reason=="punitive" && (r==4 || .ra_taekwondo_penalties(d)[2L-d$winner]<10)) bad("Punitive victory requires ten opponent penalties")
  if(d$reason=="point_gap" && (!d$point_gap || !r %in% c(2,3) || .ra_taekwondo_scores(d)[d$winner+1L]-.ra_taekwondo_scores(d)[2L-d$winner]<20)) bad("Point-gap victory requires the configured twenty-point margin")
  if(d$reason %in% c("golden_points","golden_penalties") && (r!=4 || .ra_taekwondo_scores(d)[d$winner+1L]<2)) bad("Golden victory requires two points")
  if(d$reason=="points" && (r!=3 || d$remaining_ms!=0 || .ra_taekwondo_scores(d)[d$winner+1L]<=.ra_taekwondo_scores(d)[2L-d$winner])) bad("Points victory follows the third round")
  if(d$reason=="superiority" && (r!=4 || d$remaining_ms!=0)) bad("Superiority follows the golden round")
 } else {
  if(!is.null(d$winner) || !is.null(d$reason)) bad("Ongoing match cannot have a result")
  if(.ra_taekwondo_evaluate(d)$phase=="finished") bad("A reached score or penalty limit must terminate play")
 }
}
.ra_taekwondo_schemas<-function(d) {
 ph<-d$phase;s<-list();i<-.ra_taekwondo_i;p<-i(0,1);empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="ready") {s$start_round<-empty;if(d$round==1) s$configure<-list(point_gap=list(type="boolean"))}
 if(ph %in% c("fighting","break")) s$elapse<-list(milliseconds=i(1,120000))
 if(ph=="fighting") {s$pause<-empty;s$knockdown<-list(players=list(type="array",minItems=1,maxItems=2,items=p))}
 if(ph=="paused") s$resume<-empty
 if(ph %in% c("fighting","paused","round_review")) s$score<-list(player=p,technique=list(type="enum",values=as.list(names(.ra_taekwondo_values))))
 if(ph %in% c("ready","fighting","paused","break","round_review")) s$gam_jeom<-list(player=p)
 if(ph=="round_review") s$close_round<-empty
 if(ph=="round_review" && d$round==4) s$set_pss_hits<-list(hits=list(type="array",minItems=2,maxItems=2,items=i(0,10000)))
 if(ph=="superiority") s$declare_superiority<-list(referee=p,judges=list(type="array",minItems=3,maxItems=3,items=p))
 if(ph=="count") {
  s$advance_count<-list(number=i(1,10))
  if(d$count$number<=8) s$ready_signal<-list(player=p)
  if(d$count$number>=8) s$recover<-empty
 }
 if(ph=="double_count_review") s$official_result<-list(winner=p)
 c(s,list(referee_stop=list(winner=p),withdraw=list(player=p),disqualify=list(player=p),no_contest=empty))
}
.ra_taekwondo_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_taekwondo_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k=="configure") d$point_gap<-p$point_gap else if(k %in% c("start_round","resume")) d$phase<-"fighting" else if(k=="pause") d$phase<-"paused" else if(k=="elapse") {
  f<-if(d$phase=="break") "break_ms" else "remaining_ms"
  if(p$milliseconds>d[[f]]) .ra_fail("Elapsed time exceeds this interval")
  d[[f]]<-d[[f]]-p$milliseconds
  if(d[[f]]==0) {d$phase<-if(f=="break_ms") "ready" else "round_review";d<-.ra_taekwondo_evaluate(d)}
 } else if(k=="score") {
  d$techniques[[d$round]][[p$player+1L]][[p$technique]]<-d$techniques[[d$round]][[p$player+1L]][[p$technique]]+1L;d<-.ra_taekwondo_evaluate(d)
 } else if(k=="gam_jeom") {
  d$gam_jeom[[d$round]][[p$player+1L]]<-d$gam_jeom[[d$round]][[p$player+1L]]+1L;d<-.ra_taekwondo_evaluate(d)
 } else if(k=="close_round") {
  d<-.ra_taekwondo_evaluate(d)
  if(d$phase=="finished") return(d)
  if(d$round==4) {
   if(is.null(d$pss_hits)) .ra_fail("Golden-round PSS hit totals must be supplied")
   w<-.ra_taekwondo_superiority(d)
   if(is.null(w)) d$phase<-"superiority" else d<-.ra_taekwondo_finish(d,w,"superiority")
  } else if(d$round==3 && .ra_taekwondo_scores(d)[1]!=.ra_taekwondo_scores(d)[2]) d<-.ra_taekwondo_finish(d,as.integer(.ra_taekwondo_scores(d)[2]>.ra_taekwondo_scores(d)[1]),"points") else {
   d$round<-d$round+1L;d$phase<-"break";d$break_ms<-60000L;d$remaining_ms<-if(d$round==4) 60000L else 120000L
  }
 } else if(k=="set_pss_hits") d$pss_hits<-p$hits else if(k=="declare_superiority") {
  if(!is.null(.ra_taekwondo_superiority(d))) .ra_fail("Numeric superiority must be resolved before a judge vote")
  votes<-c(p$referee,unlist(p$judges));w<-if(sum(votes==0)==2) p$referee else as.integer(sum(votes==1)>2);d<-.ra_taekwondo_finish(d,w,"superiority")
 } else if(k=="knockdown") {
  if(anyDuplicated(unlist(p$players))) .ra_fail("Knockdown recipients must be distinct")
  d$phase<-"count";d$count<-list(players=p$players,number=0L,ready=list(FALSE,FALSE))
 } else if(k=="ready_signal") {
  if(!p$player %in% unlist(d$count$players)) .ra_fail("Only a counted athlete may signal readiness")
  d$count$ready[[p$player+1L]]<-TRUE
 } else if(k=="advance_count") {
  if(p$number!=d$count$number+1L) .ra_fail("Counts advance one number at a time")
  d$count$number<-p$number
  if(d$count$number==10) {
   not_ready<-Filter(function(t) !d$count$ready[[t+1L]],d$count$players)
   if(length(not_ready)==1) d<-.ra_taekwondo_finish(d,1L-not_ready[[1]],"referee_stop") else if(length(not_ready)==2) {
    sc<-.ra_taekwondo_scores(d)
    if(sc[1]!=sc[2]) d<-.ra_taekwondo_finish(d,as.integer(sc[2]>sc[1]),"double_count_score") else {d$phase<-"double_count_review";d["count"]<-list(NULL)}
   } else {d$phase<-"paused";d["count"]<-list(NULL)}
  }
 } else if(k=="recover") {
  if(!all(vapply(d$count$players,function(t) d$count$ready[[t+1L]],logical(1)))) .ra_fail("All counted athletes must have signalled readiness by eight")
  d$phase<-"fighting";d["count"]<-list(NULL)
 } else if(k %in% c("referee_stop","official_result")) d<-.ra_taekwondo_finish(d,p$winner,if(k=="referee_stop") "referee_stop" else "official_result") else if(k %in% c("withdraw","disqualify")) d<-.ra_taekwondo_finish(d,1L-p$player,if(k=="withdraw") "withdrawal" else "disqualification") else if(k=="no_contest") d<-.ra_taekwondo_finish(d,NULL,"no_contest")
 d
}
.ra_taekwondo_legal<-function(state) {
 d<-state$data;s<-.ra_taekwondo_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Round, clock, score, penalty and count conditions must permit the event."),external_observations=list("Confirmed scoring techniques, referee penalties, count signals and PSS hit totals."))),constraints=list(phase=d$phase,round=d$round,remaining_ms=d$remaining_ms,break_ms=d$break_ms,scores=as.list(.ra_taekwondo_scores(d)),gam_jeom=as.list(.ra_taekwondo_penalties(d,TRUE)),count=d$count),completeness=FALSE,reason="official_2019_taekwondo_events")
}
.ra_taekwondo_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;review<-d$phase %in% c("round_review","superiority","double_count_review")
 list(status=if(done) "terminal" else if(review) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "no_contest" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(review) "official_review_required" else "ongoing",evidence_required=if(review) list("Official round review or superiority decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
