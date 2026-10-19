.ra_darts_throws <- c(list(c(0L,0L)),unlist(lapply(1:20,function(s) lapply(1:3,function(m) c(s,m))),recursive=FALSE),list(c(25L,1L),c(25L,2L)))
.ra_darts_validate <- function(d) {
  scores<-unlist(d$remaining);t<-d$turn+1L;used<-d$darts_used
  if(1 %in% scores || sum(scores==0)>1) .ra_fail("Invalid remaining scores",code="INVALID_STATE")
  if(used==3 && !0 %in% scores) .ra_fail("A three-dart turn must be closed",code="INVALID_STATE")
  if(scores[t]>d$turn_start || d$turn_start-scores[t]>60*used) .ra_fail("Turn score is inconsistent",code="INVALID_STATE")
  if(0 %in% scores && (scores[t]!=0 || used==0)) .ra_fail("Invalid completed leg",code="INVALID_STATE")
}
.ra_darts_legal <- function(state) lapply(.ra_darts_throws,function(v) list(type="throw",params=list(segment=v[1],multiplier=v[2])))
.ra_darts_apply <- function(state,action) {
  if(action$type!="throw") .ra_fail("Unknown action")
  .ra_fields(action$params,list(segment=list(type="integer",minimum=0,maximum=25),multiplier=list(type="integer",minimum=0,maximum=3)),"/params")
  s<-action$params$segment;m<-action$params$multiplier
  if(!any(vapply(.ra_darts_throws,function(v) v[1]==s && v[2]==m,logical(1)))) .ra_fail("Invalid dartboard target")
  d<-state$data;t<-d$turn+1L
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  remaining<-d$remaining[[t]]-s*m;d$darts_used<-d$darts_used+1L
  bust<-remaining<0 || remaining==1 || (remaining==0 && m!=2)
  d$remaining[[t]]<-if(bust) d$turn_start else remaining
  if(bust || (remaining!=0 && d$darts_used==3)) {d$turn<-2L-t;d$darts_used<-0L;d$turn_start<-d$remaining[[3L-t]]}
  d
}
.ra_darts_outcome <- function(state,pack) {
  d<-state$data;terminal<-0 %in% unlist(d$remaining);w<-if(terminal) which(unlist(d$remaining)==0)[1] else NULL
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=if(is.null(w)) list() else list(state$players[[w]]$id),losers=if(is.null(w)) list() else list(state$players[[3L-w]]$id),placements=list(),reason=if(terminal) "double_out" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
