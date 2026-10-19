.ra_wrestling_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);a<-function(x,n) list(type="array",minItems=n,maxItems=n,items=x);nullable_player<-i(0,1);nullable_player$nullable<-TRUE;empty<-setNames(list(),character())
 activity<-list(type="object",properties=list(player=i(0,1),remaining_ms=i(0,30000)),required=list("player","remaining_ms"),nullable=TRUE)
 record<-list(holds=a(a(i(0,1000),4),2),penalty_points=a(i(0,10000),2),cautions=a(i(0,3),2),last_point=nullable_player,activity=activity,remaining_ms=i(0,180000))
 award<-list(type="object",properties=list(player=i(0,1),value=list(type="integer",enum=as.list(c(1L,2L,4L,5L)))),required=list("player","value"))
 list(start=empty,pause=empty,resume=empty,begin_second_period=empty,elapse=list(milliseconds=i(1,180000)),score_action=list(awards=list(type="array",minItems=1,maxItems=16,items=award),fall_winner=nullable_player),passivity_warning=list(player=i(0,1)),start_activity=list(player=i(0,1)),resolve_activity=empty,caution=list(player=i(0,1),opponent_points=i(1,2)),fall=list(winner=i(0,1)),correct_record=record,disqualify=list(loser=i(0,1)),forfeit=list(loser=i(0,1)),both_disqualified=empty,confirm=empty)
}
.ra_wrestling_scores<-function(d) lapply(1:2,function(i) sum(unlist(d$holds[[i]])*c(1,2,4,5))+d$penalty_points[[i]])
.ra_wrestling_tie_winner<-function(d) {
 s<-unlist(.ra_wrestling_scores(d));if(s[[1]]!=s[[2]]) return(if(s[[1]]>s[[2]]) 0L else 1L)
 keys<-lapply(1:2,function(i) {row<-unlist(d$holds[[i]]);values<-c(1,2,4,5);maximum<-max(c(1,values[row>0]));count<-if(maximum>1) row[[match(maximum,values)]] else s[[i]];c(maximum,count,-d$cautions[[i]])})
 cmp<-.ra_ps_compare(keys[[1]],keys[[2]]);if(cmp!=0) return(if(cmp>0) 0L else 1L);d$last_point
}
.ra_wrestling_finish<-function(d,w,reason) {d$phase<-"review";d["winner"]<-list(w);d$reason<-reason;d["activity"]<-list(NULL);d}
.ra_wrestling_settle<-function(d) {
 d["winner"]<-list(NULL);d["reason"]<-list(NULL);s<-unlist(.ra_wrestling_scores(d))
 if(max(unlist(d$cautions))==3) d<-.ra_wrestling_finish(d,2L-which(unlist(d$cautions)==3)[[1L]],"three_cautions") else if(abs(s[[1]]-s[[2]])>=10) d<-.ra_wrestling_finish(d,if(s[[1]]>s[[2]]) 0L else 1L,"technical_superiority") else if(!is.null(d$activity) && d$activity$remaining_ms==0) d$phase<-"paused" else if(d$remaining_ms==0) {
  if(d$period==1) d$phase<-"interval" else {w<-.ra_wrestling_tie_winner(d);if(is.null(w)) d$phase<-"decision" else d<-.ra_wrestling_finish(d,w,"points")}
 } else d$phase<-"paused";d
}
.ra_wrestling_add<-function(d,i,value,technical) {
 if(technical) {j<-match(value,c(1,2,4,5));d$holds[[i+1L]][[j]]<-d$holds[[i+1L]][[j]]+1L} else d$penalty_points[[i+1L]]<-d$penalty_points[[i+1L]]+value
 d$last_point<-i;if(!is.null(d$activity) && d$activity$player==i) d["activity"]<-list(NULL);d
}
.ra_wrestling_mandatory<-function(d) d$period==1 && d$remaining_ms<=60000 && all(unlist(.ra_wrestling_scores(d))==0) && is.null(d$activity)
.ra_wrestling_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");s<-unlist(.ra_wrestling_scores(d));terminal<-d$phase %in% c("review","finished")
 if((sum(s)==0)!=is.null(d$last_point) || (!is.null(d$last_point) && s[[d$last_point+1L]]==0)) bad("Last-point identity must agree with the score record")
 if(terminal!=!is.null(d$reason)) bad("Result phase and reason disagree")
 if(d$phase=="ready" && (d$period!=1 || d$remaining_ms!=180000 || sum(s)>0 || any(unlist(d$cautions)>0) || any(unlist(d$warned)) || !is.null(d$activity))) bad("Unstarted contest must have an empty record")
 if(d$phase=="interval" && (d$period!=1 || d$remaining_ms!=0 || !is.null(d$activity))) bad("Interval requires an expired first period")
 if(d$phase %in% c("active","paused") && d$remaining_ms==0 && is.null(d$activity)) bad("Expired period requires resolution")
 if(d$phase=="active" && (.ra_wrestling_mandatory(d) || (!is.null(d$activity) && d$activity$remaining_ms==0))) bad("Resolve the mandatory activity decision before resuming")
 if(!is.null(d$activity) && (!(d$phase %in% c("active","paused")) || d$activity$remaining_ms>d$remaining_ms || !d$warned[[d$activity$player+1L]])) bad("Activity clock must fit the remaining live period")
 if(!terminal) {
  if(!is.null(d$winner) || max(unlist(d$cautions))==3 || abs(s[[1]]-s[[2]])>=10) bad("A winning result must enter review")
  if(d$phase=="decision" && (d$period!=2 || d$remaining_ms!=0 || !is.null(.ra_wrestling_tie_winner(d)))) bad("Only unresolved expired records require adjudication")
 } else if(d$reason=="both_disqualified") {if(!is.null(d$winner)) bad("Double disqualification has no winner")} else {
  w<-d$winner;if(is.null(w)) bad("A decisive result requires a winner")
  if(d$reason=="technical_superiority" && s[[w+1L]]-s[[2L-w]]<10) bad("Technical superiority requires a ten-point margin")
  if(d$reason=="three_cautions" && d$cautions[[2L-w]]!=3) bad("Three cautions must belong to the losing wrestler")
  if(d$reason=="points" && (d$period!=2 || d$remaining_ms!=0 || !identical(as.numeric(.ra_wrestling_tie_winner(d)),as.numeric(w)))) bad("Points winner must satisfy final score criteria")
 };invisible(TRUE)
}
.ra_wrestling_available<-function(d) {
 names<-switch(d$phase,ready="start",active=c("pause","elapse","score_action","passivity_warning","start_activity","caution","fall"),paused=c("resume","score_action","passivity_warning","start_activity","caution","fall","correct_record"),interval=c("begin_second_period","correct_record"),decision="correct_record",review="confirm",character())
 if(d$phase=="paused" && !is.null(d$activity) && d$activity$remaining_ms==0) names<-c(names,"resolve_activity")
 if(d$phase=="review" && d$reason %in% c("technical_superiority","points","three_cautions")) names<-c(names,"correct_record")
 if(d$phase!="finished") names<-c(names,"disqualify","forfeit","both_disqualified");names
}
.ra_wrestling_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!(k %in% .ra_wrestling_available(d))) .ra_fail("Event is not permitted in this wrestling phase")
 .ra_fields(p,.ra_wrestling_schemas()[[k]],"/params")
 if(k %in% c("start","resume")) {
  if(d$remaining_ms==0 || .ra_wrestling_mandatory(d) || (!is.null(d$activity) && d$activity$remaining_ms==0)) .ra_fail("Resolve the period or activity decision before resuming")
  d$phase<-"active"
 } else if(k=="pause") d$phase<-"paused" else if(k=="begin_second_period") {d$period<-2L;d$remaining_ms<-180000L;d$phase<-"paused"} else if(k=="elapse") {
  limit<-d$remaining_ms;if(!is.null(d$activity)) limit<-min(limit,d$activity$remaining_ms)
  if(d$period==1 && all(unlist(.ra_wrestling_scores(d))==0) && is.null(d$activity) && d$remaining_ms>60000) limit<-min(limit,d$remaining_ms-60000)
  if(p$milliseconds>limit) .ra_fail("Time cannot cross a period or mandatory activity boundary")
  d$remaining_ms<-d$remaining_ms-p$milliseconds;if(!is.null(d$activity)) d$activity$remaining_ms<-d$activity$remaining_ms-p$milliseconds
  if(d$remaining_ms==0 || .ra_wrestling_mandatory(d) || (!is.null(d$activity) && d$activity$remaining_ms==0)) d<-.ra_wrestling_settle(d)
 } else if(k=="score_action") {
  for(award in p$awards) d<-.ra_wrestling_add(d,award$player,award$value,TRUE)
  if(!is.null(p$fall_winner)) d<-.ra_wrestling_finish(d,p$fall_winner,"fall") else d<-.ra_wrestling_settle(d)
 } else if(k=="passivity_warning") {
  if(d$remaining_ms==0 || d$warned[[p$player+1L]]) .ra_fail("The initial passivity warning is recorded once")
  d$warned[[p$player+1L]]<-TRUE;d$phase<-"paused"
 } else if(k=="start_activity") {
  i<-p$player
  if(d$remaining_ms<30000 || !is.null(d$activity) || !(d$warned[[i+1L]] || .ra_wrestling_mandatory(d))) .ra_fail("An activity period requires a prior warning or the scoreless first-period rule and thirty seconds")
  d$warned[[i+1L]]<-TRUE;d$activity<-list(player=i,remaining_ms=30000L);d$phase<-"paused"
 } else if(k=="resolve_activity") {
  i<-d$activity$player;d["activity"]<-list(NULL);d<-.ra_wrestling_add(d,1L-i,1L,FALSE);d<-.ra_wrestling_settle(d)
 } else if(k=="caution") {
  i<-p$player;d$cautions[[i+1L]]<-d$cautions[[i+1L]]+1L;d<-.ra_wrestling_add(d,1L-i,p$opponent_points,FALSE);d<-.ra_wrestling_settle(d)
 } else if(k=="fall") d<-.ra_wrestling_finish(d,p$winner,"fall") else if(k=="correct_record") {for(name in names(p)) d[name]<-list(p[[name]]);d<-.ra_wrestling_settle(d)} else if(k %in% c("disqualify","forfeit")) d<-.ra_wrestling_finish(d,1L-p$loser,k) else if(k=="both_disqualified") d<-.ra_wrestling_finish(d,NULL,k) else if(k=="confirm") d$phase<-"finished";d
}
.ra_wrestling_legal<-function(state) {
 d<-state$data;s<-.ra_wrestling_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(.ra_wrestling_available(d),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Official completed-action awards, cautions, period and activity clocks must permit the event."),external_observations=list("Referee judgments, completed action sequences and elapsed active milliseconds."))),constraints=list(phase=d$phase,period=d$period,remaining_ms=d$remaining_ms,scores=.ra_wrestling_scores(d),activity=d$activity,mandatory_activity=.ra_wrestling_mandatory(d),video_challenges=FALSE),completeness=FALSE,reason="official_freestyle_rulings")
}
.ra_wrestling_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase %in% c("review","decision")) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "no_contest" else if(d$reason=="forfeit") "forfeit" else if(d$reason %in% c("disqualify","three_cautions")) "disqualification" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="decision") "official_decision_required" else "ongoing",evidence_required=if(d$phase=="decision") list("Official scoring-record correction.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
