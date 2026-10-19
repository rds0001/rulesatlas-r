.ra_mg_active <- function(d) which(unlist(d$status)=="active")-1L
.ra_mg_validate <- function(d) {
 for(p in 1:2) if(length(d$scores[[p]])>18 || length(d$scores[[p]])>d$hole+1) .ra_fail("Completed scorecards cannot exceed the current hole",code="INVALID_STATE")
 if(d$phase=="play") {a<-.ra_mg_active(d);if(length(a)!=2 || !(d$turn %in% a) || length(d$scores[[d$turn+1L]])!=d$hole || any(vapply(a,function(p) !(length(d$scores[[p+1L]]) %in% c(d$hole,d$hole+1)),logical(1)))) .ra_fail("The current player must still owe a score on this hole",code="INVALID_STATE")}
 if(d$phase=="playoff") {
  if(length(.ra_mg_active(d))!=2 || any(lengths(d$scores)!=18) || sum(unlist(d$scores[[1]]))!=sum(unlist(d$scores[[2]]))) .ra_fail("Playoffs require tied completed scorecards",code="INVALID_STATE")
  if(!is.null(d$playoff_scores[[d$turn+1L]]) || sum(!vapply(d$playoff_scores,is.null,logical(1)))>1) .ra_fail("The next playoff participant must not have completed this hole",code="INVALID_STATE")
 }
 if(d$phase=="finished") {if(is.null(d$winner) || !(d$winner %in% 0:1)) .ra_fail("A finished game requires a winner",code="INVALID_STATE")} else if(!is.null(d$winner)) .ra_fail("An ongoing game has no winner",code="INVALID_STATE")
 if(d$phase=="finished") {
  w<-d$winner+1L
  if("withdrawn" %in% unlist(d$status)) {if(d$status[[w]]!="active" || d$status[[3L-w]]!="withdrawn") .ra_fail("Withdrawal winner must be the remaining player",code="INVALID_STATE")} else {
   if(any(lengths(d$scores)!=18) || d$attempts!=0 || d$reset_required) .ra_fail("Finish requires completed scorecards and no pending stroke",code="INVALID_STATE")
   totals<-vapply(d$scores,function(s) sum(unlist(s)),numeric(1))
   if(totals[1]==totals[2]) {if(any(vapply(d$playoff_scores,is.null,logical(1))) || d$playoff_scores[[w]]>=d$playoff_scores[[3L-w]]) .ra_fail("Tied totals require a winning playoff score",code="INVALID_STATE")} else if(totals[w]>=totals[3L-w]) .ra_fail("Winner must have the lowest total",code="INVALID_STATE")
  }
 }
 if(d$phase=="play" && (any(!vapply(d$playoff_scores,is.null,logical(1))) || d$playoff_round!=0)) .ra_fail("Playoff data requires completed tied regulation",code="INVALID_STATE")
 if(d$attempts==0 && d$reset_required) .ra_fail("Ball replacement follows an attempted stroke",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_mg_finish <- function(d,score) {
 t<-d$turn;d$attempts<-0L;d$reset_required<-FALSE
 if(d$phase=="playoff") {
  d$playoff_scores[[t+1L]]<-score
  if(is.null(d$playoff_scores[[2L-t]])) d$turn<-1L-t else if(d$playoff_scores[[1]]!=d$playoff_scores[[2]]) {d$winner<-if(d$playoff_scores[[1]]<d$playoff_scores[[2]]) 0L else 1L;d$phase<-"finished"} else {d$playoff_round<-d$playoff_round+1L;d$playoff_scores<-list(NULL,NULL);d$turn<-d$playoff_round%%2L}
  return(d)
 }
 d$scores[[t+1L]]<-c(d$scores[[t+1L]],list(score));a<-.ra_mg_active(d);waiting<-a[vapply(a,function(p) length(d$scores[[p+1L]])==d$hole,logical(1))]
 if(length(waiting)) d$turn<-waiting[1] else if(d$hole<17) {d$hole<-d$hole+1L;d$turn<-d$hole%%2L} else {totals<-vapply(d$scores,function(s) sum(unlist(s)),numeric(1));if(totals[1]==totals[2]) {d$phase<-"playoff";d$turn<-0L} else {d$phase<-"finished";d$winner<-if(totals[1]<totals[2]) 0L else 1L}}
 d
}
.ra_mg_apply <- function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;t<-d$turn
 if(k=="withdraw") {
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
  if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
  d$status[[q+1L]]<-"withdrawn";d$phase<-"finished";d$winner<-1L-q;return(d)
 }
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 if(k=="replace_ball") {.ra_fields(p,list(),"/params");if(!d$reset_required) .ra_fail("Replace only a ball declared out of bounds");d$reset_required<-FALSE;return(d)}
 if(k=="stroke") {
  .ra_fields(p,list(result=list(type="string",enum=list("holed","in_play","out_of_bounds"))),"/params")
  if(d$reset_required) .ra_fail("Replace an out-of-bounds ball before the next stroke")
  d$attempts<-d$attempts+1L
  if(p$result=="holed") return(.ra_mg_finish(d,d$attempts))
  if(d$attempts==6) return(.ra_mg_finish(d,7L))
  d$reset_required<-p$result=="out_of_bounds";return(d)
 }
 .ra_fail("Unknown action")
}
.ra_mg_legal <- function(state) {
 d<-state$data;actions<-lapply(.ra_mg_active(d),function(p) list(type="withdraw",params=list(player=p)))
 if(d$reset_required) actions<-c(actions,list(list(type="replace_ball",params=structure(list(),names=character())))) else actions<-c(actions,lapply(c("holed","in_play","out_of_bounds"),function(r) list(type="stroke",params=list(result=r))))
 list(mode="enumerated",actions=actions,action_types=list(),constraints=list(turn=d$turn,hole=d$hole+1L,attempts=d$attempts,playoff_round=d$playoff_round),completeness=TRUE,reason="all_declared_stroke_results")
}
.ra_mg_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,strokes=sum(unlist(d$scores[[p+1L]])),holes_completed=length(d$scores[[p+1L]]),status=d$status[[p+1L]])) else list(),reason=if(done) if("withdrawn" %in% unlist(d$status)) "withdrawal" else if(all(!vapply(d$playoff_scores,is.null,logical(1)))) "sudden_death" else "lowest_total" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
