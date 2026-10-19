.ra_rugby_values<-function(code) if(code=="union") list(try_score=5L,penalty=3L,drop_goal=3L,penalty_try=7L) else list(try_score=4L,penalty=2L,drop_goal=1L,penalty_try=4L,eight_point_try=4L)
.ra_rugby_schemas<-function(code) {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);team<-i(0,1);slot<-i(0,22);b<-list(type="boolean");e<-setNames(list(),character());n<-if(code=="union") 15L else 13L;maxn<-if(code=="union") 23L else 17L
 s<-list(set_lineup=list(team=team,roster_size=i(n,maxn),starters=list(type="array",minItems=n,maxItems=n,items=slot)),start_half=e,end_half=e,score=list(team=team,player=slot,kind=list(type="string",enum=as.list(names(.ra_rugby_values(code))))),award_penalty_kick=list(team=team),resolve_kick=list(player=slot,scored=b),card=list(team=team,player=slot,colour=list(type="string",enum=list("yellow","red"))),elapse_play=list(milliseconds=i(1,3600000)),substitute=list(team=team,out=slot),authorise_return=list(team=team,player=slot),abandon=e,official_result=list(winner=c(team,list(nullable=TRUE))),concede=list(team=team));s$substitute[["in"]]<-slot;s
}
.ra_rugby_active<-function(t) as.list(which(vapply(seq_along(t$status),function(i) t$status[[i]]=="active" && t$sin_bin_ms[[i]]==0,logical(1)))-1L)
.ra_rugby_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d["pending_kick"]<-list(NULL);d$extra_penalty<-FALSE;d}
.ra_rugby_player<-function(d,team,i) {if(!(i %in% unlist(.ra_rugby_active(d$teams[[team+1L]])))) .ra_fail("The event requires an eligible active player")}
.ra_rugby_validate<-function(d,code) {
 union<-code=="union";size<-if(union) 15L else 13L;roster<-if(union) 23L else 17L;bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in d$teams) {
  n<-length(t$status)
  if(n<size || n>roster || any(vapply(c("yellow","sin_bin_ms","return_authorised"),function(k) length(t[[k]])!=n,logical(1)))) bad("Rugby roster vectors must match the declared team size")
  if(sum(unlist(t$status)=="active")>size-t$permanent_reductions) bad("Dismissal permanently reduces the permitted on-field complement")
  if(t$permanent_reductions>sum(unlist(t$status)=="dismissed")) bad("Permanent team reductions require dismissed players")
  if(t$substitutions>8) bad("This format allows eight replacement or interchange events")
  for(i in seq_len(n)) {s<-t$status[[i]];if(s=="dismissed" && t$sin_bin_ms[[i]]>0 || t$return_authorised[[i]] && s!="off") bad("Player status conflicts with a temporary sanction or return permission");if(union && t$yellow[[i]]>=2 && s!="dismissed") bad("A second yellow card dismisses the union player")}
 }
 if(d$phase=="pre_match" && (any(unlist(d$scores)!=0) || d$half!=1 || any(vapply(d$teams,function(t) t$substitutions>0 || any(unlist(t$yellow)>0) || any(unlist(t$sin_bin_ms)>0) || t$permanent_reductions>0,logical(1))))) bad("Unstarted match cannot contain playing history")
 if(d$phase=="halftime" && d$half!=1) bad("Halftime follows only the first half")
 if(d$extra_penalty && (union || is.null(d$pending_kick) || d$pending_kick$kind!="conversion")) bad("Additional penalty follows only a league eight-point-try conversion")
 if(!is.null(d$pending_kick) && d$phase!="play") bad("Only a playing half may retain a pending kick")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="draw")!=is.null(d$winner) && d$reason!="official") bad("Finished match requires a consistent result")
  if(d$reason=="points" && (d$half!=2 || d$scores[[d$winner+1L]]<=d$scores[[2L-d$winner]])) bad("Points victory requires two completed halves and a lead")
  if(d$reason=="draw" && (d$half!=2 || d$scores[[1]]!=d$scores[[2]])) bad("Regulation draw requires tied scores after two halves")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("An open or abandoned match has no final competition result");invisible(TRUE)
}
.ra_rugby_apply<-function(state,action,code) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;union<-code=="union";size<-if(union) 15L else 13L;s<-.ra_rugby_schemas(code);values<-.ra_rugby_values(code)
 if(!(k %in% names(s))) .ra_fail("Unknown rugby event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished" || phase=="abandoned" && k!="official_result") .ra_fail("Match is not open for playing events")
 if(k=="set_lineup") {
  if(phase!="pre_match") .ra_fail("Rosters are fixed after kick-off")
  n<-p$roster_size;a<-unlist(p$starters);if(length(unique(a))!=size || max(a)>=n) .ra_fail("Starting lineup must name distinct roster players")
  d$teams[[p$team+1L]]<-list(status=lapply(0:(n-1L),function(i) if(i %in% a) "active" else "bench"),yellow=as.list(rep(0L,n)),sin_bin_ms=as.list(rep(0L,n)),return_authorised=as.list(rep(FALSE,n)),permanent_reductions=0L,substitutions=0L)
 } else if(k=="start_half") {
  if(!(phase %in% c("pre_match","halftime")) || any(vapply(d$teams,function(t) length(.ra_rugby_active(t))==0,logical(1)))) .ra_fail("No eligible half can start")
  if(phase=="pre_match" && any(vapply(d$teams,function(t) length(.ra_rugby_active(t))!=size,logical(1)))) .ra_fail("This format starts with a full team")
  d$phase<-"play";if(phase=="halftime") d$half<-2L
 } else if(k=="end_half") {
  if(phase!="play" || !is.null(d$pending_kick)) .ra_fail("Resolve any awarded kick before the official half-end event")
  if(d$half==1) d$phase<-"halftime" else {tie<-d$scores[[1]]==d$scores[[2]];d<-.ra_rugby_finish(d,if(tie) NULL else if(d$scores[[1]]>d$scores[[2]]) 0L else 1L,if(tie) "draw" else "points")}
 } else if(k=="score") {
  if(phase!="play" || !is.null(d$pending_kick)) .ra_fail("Scoring requires an open half without a pending kick")
  team<-p$team;.ra_rugby_player(d,team,p$player);kind<-p$kind;d$scores[[team+1L]]<-d$scores[[team+1L]]+values[[kind]]
  if(kind=="try_score" || kind %in% c("penalty_try","eight_point_try") && !union) d$pending_kick<-list(team=team,kind="conversion",value=2L)
  d$extra_penalty<-kind=="eight_point_try"
 } else if(k=="award_penalty_kick") {
  if(phase!="play" || !is.null(d$pending_kick)) .ra_fail("Only one penalty kick may be pending")
  d$pending_kick<-list(team=p$team,kind="penalty",value=values$penalty)
 } else if(k=="resolve_kick") {
  r<-d$pending_kick;if(phase!="play" || is.null(r)) .ra_fail("No kick is pending")
  .ra_rugby_player(d,r$team,p$player);if(p$scored) d$scores[[r$team+1L]]<-d$scores[[r$team+1L]]+r$value
  d["pending_kick"]<-list(if(d$extra_penalty) list(team=r$team,kind="penalty",value=2L) else NULL);d$extra_penalty<-FALSE
 } else if(k=="card") {
  if(!(phase %in% c("play","halftime"))) .ra_fail("Discipline is recorded during the match")
  ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player+1L
  if(i>length(t$status) || t$status[[i]]=="dismissed") .ra_fail("Dismissed or absent player cannot receive another card")
  if(p$colour=="yellow") t$yellow[[i]]<-t$yellow[[i]]+1L
  red<-p$colour=="red" || union && t$yellow[[i]]>=2
  if(red) {if(t$status[[i]]=="active") t$permanent_reductions<-t$permanent_reductions+1L;t$status[[i]]<-"dismissed";t$sin_bin_ms[[i]]<-0L;t$return_authorised[[i]]<-FALSE} else t$sin_bin_ms[[i]]<-t$sin_bin_ms[[i]]+600000L
  d$teams[[ti]]<-t;if(length(.ra_rugby_active(t))<if(union) 1L else 9L) {d$phase<-"abandoned";d["pending_kick"]<-list(NULL);d$extra_penalty<-FALSE}
 } else if(k=="elapse_play") {
  if(phase!="play") .ra_fail("Only actual playing time serves temporary sanctions")
  for(i in 1:2) d$teams[[i]]$sin_bin_ms<-lapply(d$teams[[i]]$sin_bin_ms,function(x) max(0L,x-p$milliseconds))
 } else if(k=="substitute") {
  if(!(phase %in% c("play","halftime"))) .ra_fail("Replacement requires a match opportunity certified by officials")
  ti<-p$team+1L;t<-d$teams[[ti]];out<-p$out+1L;inc<-p[["in"]]+1L;.ra_rugby_player(d,p$team,p$out)
  if(inc>length(t$status) || inc==out || t$sin_bin_ms[[inc]]>0 || !(t$status[[inc]] %in% c("bench","off")) || union && t$status[[inc]]=="off" && !t$return_authorised[[inc]]) .ra_fail("Incoming player is unavailable or lacks required return permission")
  counted<-!union || t$status[[inc]]=="bench"
  if(counted && t$substitutions>=8) .ra_fail("The replacement allowance is exhausted")
  t$status[[out]]<-"off";t$status[[inc]]<-"active";t$return_authorised[[inc]]<-FALSE;t$substitutions<-t$substitutions+as.integer(counted);d$teams[[ti]]<-t
 } else if(k=="authorise_return") {
  if(!union || !(phase %in% c("play","halftime"))) .ra_fail("Special returning replacements apply only to this union format")
  ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player+1L
  if(i>length(t$status) || t$status[[i]]!="off" || t$return_authorised[[i]]) .ra_fail("Permission requires a previously replaced player")
  d$teams[[ti]]$return_authorised[[i]]<-TRUE
 } else if(k=="abandon") {if(phase=="pre_match") .ra_fail("The match has not started");d$phase<-"abandoned";d["pending_kick"]<-list(NULL);d$extra_penalty<-FALSE
 } else if(k=="official_result") {if(phase!="abandoned") .ra_fail("An external competition result applies only after abandonment");d<-.ra_rugby_finish(d,p$winner,"official")
 } else if(k=="concede") d<-.ra_rugby_finish(d,1L-p$team,"concession");d
}
.ra_rugby_legal<-function(state,code) {
 d<-state$data;phase<-d$phase;names<-character()
 if(phase=="pre_match") names<-c("set_lineup","start_half","concede") else if(phase=="abandoned") names<-"official_result" else if(phase %in% c("play","halftime")) {
  names<-c("card","substitute","abandon","concede");if(code=="union") names<-c(names,"authorise_return")
  if(phase=="halftime") names<-c(names,"start_half") else if(!is.null(d$pending_kick)) names<-c(names,"resolve_kick","elapse_play") else names<-c(names,"score","award_penalty_kick","end_half","elapse_play")
 }
 s<-.ra_rugby_schemas(code)
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Half, active-player eligibility, pending kick and replacement allowance must permit the event."),external_observations=list("Official score, replacement entitlement, card, playing-time duration or completed half including required added time."))),constraints=list(phase=phase,half=d$half,active=lapply(d$teams,.ra_rugby_active),replacements_remaining=lapply(d$teams,function(t) 8L-t$substitutions),pending_kick=d$pending_kick),completeness=FALSE,reason=paste0("official_rugby_",code,"_events"))
}
.ra_rugby_outcome<-function(state,pack,code) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;abandoned<-d$phase=="abandoned"
 list(status=if(done) "terminal" else if(abandoned) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "draw" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(abandoned) "competition_result_required" else "ongoing",evidence_required=if(abandoned) list("Official competition result after abandonment.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
