.ra_curling_empty<-function() lapply(1:16,function(i) list(status="unplayed",rank=NULL))
.ra_curling_schemas<-function() {
 team<-list(type="integer",minimum=0,maximum=1);stone<-list(type="object",properties=list(status=list(type="string",enum=as.list(c("unplayed","dead","guard","other","house"))),rank=list(type="integer",minimum=1,maximum=16,nullable=TRUE)),required=list("status","rank"));board<-list(type="array",minItems=16,maxItems=16,items=stone)
 list(start_match=list(hammer=team,ends=list(type="integer",enum=list(8L,10L))),deliver_stone=list(board=board),measure_stones=list(board=board),confirm_end=setNames(list(),character()),concede=list(team=team))
}
.ra_curling_next<-function(d) {team<-if(d$delivered%%2==0) 1L-d$hammer else d$hammer;list(team=team,index=team*8L+d$delivered%/%2L)}
.ra_curling_points<-function(d) {
 ranks<-lapply(0:1,function(team) vapply(Filter(function(s) s$status=="house",d$board[(team*8L+1L):(team*8L+8L)]),function(s) s$rank,numeric(1)))
 nearest<-vapply(ranks,function(r) if(length(r)>0) min(r) else 17,numeric(1));result<-list(0L,0L)
 if(nearest[[1]]==nearest[[2]]) return(result)
 team<-if(nearest[[1]]<nearest[[2]]) 1L else 2L;result[[team]]<-sum(ranks[[team]]<nearest[[3L-team]]);result
}
.ra_curling_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 totals<-vapply(1:2,function(t) sum(vapply(d$end_scores,function(r) r[[t]],numeric(1))),numeric(1))
 if(any(unlist(d$scores)!=totals)) bad("Match totals must equal confirmed end scores")
 if(any(vapply(d$end_scores,function(r) r[[1]]>0 && r[[2]]>0,logical(1)))) bad("Only one team scores in an end")
 for(s in d$board) if((s$status=="house")!=!is.null(s$rank)) bad("Only stones in the house have a scoring proximity rank")
 for(team in 0:1) {
  count<-d$delivered%/%2L+as.integer(d$delivered%%2==1 && team==1L-d$hammer)
  for(i in 0:7) if((d$board[[team*8L+i+1L]]$status!="unplayed")!=(i<count)) bad("Stone inventory conflicts with alternating deliveries")
 }
 if(d$phase=="setup" && (d$delivered!=0 || length(d$end_scores)>0 || !is.null(d$conceded_by))) bad("The match has not started")
 if(d$phase=="delivering" && d$delivered>=16) bad("The end requires score review")
 if(d$phase=="end_review" && d$delivered!=16) bad("Complete all sixteen deliveries before end review")
 if(d$phase!="finished" && !is.null(d$conceded_by)) bad("A conceded match is finished")
 if(d$phase=="finished" && is.null(d$conceded_by)) {
  if(length(d$end_scores)<d$scheduled_ends || d$scores[[1]]==d$scores[[2]] || d$delivered!=16) bad("A completed match requires a confirmed decisive final end")
  if(any(unlist(d$end_scores[[length(d$end_scores)]])!=unlist(.ra_curling_points(d)))) bad("Final end score disagrees with the stones")
 }
 if(d$phase %in% c("delivering","end_review") && length(d$end_scores)>=d$scheduled_ends && d$scores[[1]]!=d$scores[[2]]) bad("A decided match cannot continue to an extra end")
 invisible(TRUE)
}
.ra_curling_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_curling_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown curling event")
 .ra_fields(p,s[[k]],"/params")
 if(d$phase=="finished") .ra_fail("The match is finished")
 if(k=="start_match") {
  if(d$phase!="setup") .ra_fail("The match has already started")
  d$hammer<-p$hammer;d$scheduled_ends<-p$ends;d$phase<-"delivering"
 } else if(k=="deliver_stone") {
  if(d$phase!="delivering") .ra_fail("A delivery is not due")
  nexts<-.ra_curling_next(d);team<-nexts$team;index<-nexts$index+1L;new<-p$board
  for(i in 1:16) {
   before<-d$board[[i]];after<-new[[i]]
   if(i==index) {if(after$status=="unplayed") .ra_fail("The delivered stone must resolve on or off the playing area")
   } else if(before$status=="unplayed" && !identical(after,before)) .ra_fail("Future stones cannot enter play") else if(before$status=="dead" && !identical(after,before)) .ra_fail("A removed stone cannot return during a delivery") else if(!(before$status %in% c("unplayed","dead")) && after$status=="unplayed") .ra_fail("A delivered stone cannot become unplayed")
  }
  violation<-d$delivered<5 && any(vapply(1:16,function(i) (i-1L)%/%8!=team && d$board[[i]]$status=="guard" && new[[i]]$status=="dead",logical(1)))
  if(violation) {new<-d$board;new[[index]]<-list(status="dead",rank=NULL)}
  d$board<-new;d$delivered<-d$delivered+1L;if(d$delivered==16) d$phase<-"end_review"
 } else if(k=="measure_stones") {
  if(d$phase!="end_review") .ra_fail("Measurements require completed-end review")
  for(i in 1:16) {
   before<-d$board[[i]];after<-p$board[[i]]
   if((before$status %in% c("unplayed","dead") && !identical(after,before)) || (!(before$status %in% c("unplayed","dead")) && after$status %in% c("unplayed","dead"))) .ra_fail("Measurements may change proximity and house membership, not stone inventory")
  };d$board<-p$board
 } else if(k=="confirm_end") {
  if(d$phase!="end_review") .ra_fail("Complete and review the end first")
  result<-.ra_curling_points(d);d$end_scores[[length(d$end_scores)+1L]]<-result;d$scores<-lapply(1:2,function(i) d$scores[[i]]+result[[i]])
  if(length(d$end_scores)>=d$scheduled_ends && d$scores[[1]]!=d$scores[[2]]) d$phase<-"finished" else {
   if(result[[1]]>0) d$hammer<-1L else if(result[[2]]>0) d$hammer<-0L
   d$delivered<-0L;d$board<-.ra_curling_empty();d$phase<-"delivering"
  }
 } else if(k=="concede") {d$conceded_by<-p$team;d$phase<-"finished"};d
}
.ra_curling_legal<-function(state) {
 d<-state$data;names<-switch(d$phase,setup=c("start_match","concede"),delivering=c("deliver_stone","concede"),end_review=c("measure_stones","confirm_end","concede"),character());s<-.ra_curling_schemas();nexts<-if(d$phase=="delivering") .ra_curling_next(d) else list(team=NULL,index=NULL)
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Delivery order, stone inventory and end phase must permit the event."),external_observations=list("Official resulting stone statuses and proximity ranks after delivery or measurement."))),constraints=list(phase=d$phase,end_number=length(d$end_scores)+as.integer(d$phase!="finished" || !is.null(d$conceded_by)),delivering_team=nexts$team,delivering_player=if(!is.null(nexts$team)) d$delivered%/%4L else NULL,stone_index=nexts$index,five_rock_protection=d$phase=="delivering" && d$delivered<5,provisional_end_score=if(d$phase=="end_review") .ra_curling_points(d) else NULL),completeness=FALSE,reason="official_curling_stone_rulings")
}
.ra_curling_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-if(done) {if(!is.null(d$conceded_by)) 1L-d$conceded_by else if(d$scores[[1]]>d$scores[[2]]) 0L else 1L} else NULL
 list(status=if(done) "terminal" else if(d$phase=="end_review") "awaiting_adjudication" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(d$conceded_by)) "concession" else if(done) "match_finished" else if(d$phase=="end_review") "end_review" else "ongoing",evidence_required=if(d$phase=="end_review") list("Official end-score agreement.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
