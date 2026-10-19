.ra_gaelic_schemas<-function(kind) {
 s<-.ra_rugby_schemas("union");i<-function(a,b) list(type="integer",minimum=a,maximum=b)
 s$score<-list(team=i(0,1),player=i(0,22),kind=list(type="string",enum=list("goal","point")));s$resolve_kick<-list(player=i(0,22),result=list(type="string",enum=list("goal","point","none")));s$card$colour$enum<-as.list(c("yellow","red",if(kind=="football") "black"));s$set_goalkeeper<-list(team=i(0,1),player=i(0,22));s
}
.ra_gaelic_enrich<-function(d,kind) {if(kind!="football") for(i in 1:2) {n<-length(d$teams[[i]]$status);d$teams[[i]]$black<-as.list(rep(0L,n));d$teams[[i]]$sin_bin_ms<-as.list(rep(0L,n))};d}
.ra_gaelic_project<-function(d) {d$extra_penalty<-FALSE;d}
.ra_gaelic_validate<-function(d,kind) {
 d<-.ra_gaelic_enrich(d,kind);.ra_rugby_validate(.ra_gaelic_project(d),"union");bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="pre_match" && any(vapply(d$teams,function(t) any(unlist(t$black)>0),logical(1)))) bad("Unstarted match cannot contain black cards")
 if(any(unlist(d$scores)!=3L*unlist(d$goals)+unlist(d$points))) bad("Total must equal three per goal plus one per point")
 for(t in d$teams) {
  if(length(t$black)!=length(t$status) || kind!="football" && any(unlist(t$black)>0)) bad("Black-card vector must match the declared code and roster")
  if(t$substitutions>5) bad("This regulation format permits five ordinary permanent substitutions")
  if(!is.null(t$goalkeeper) && !(t$goalkeeper %in% unlist(.ra_rugby_active(t)))) bad("Goalkeeper must be an eligible active player")
  if(any(vapply(seq_along(t$status),function(i) t$yellow[[i]]+t$black[[i]]>=2 && t$status[[i]]!="dismissed",logical(1)))) bad("Combined repeat cards require dismissal")
 }
 if(!is.null(d$pending_kick) && (d$pending_kick$kind!="penalty" || d$pending_kick$value!=3)) bad("Only a three-point penalty-goal opportunity may remain pending");invisible(TRUE)
}
.ra_gaelic_apply<-function(state,action,kind) {
 d<-.ra_gaelic_enrich(state$data,kind);k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_gaelic_schemas(kind)
 if(!(k %in% names(s))) .ra_fail("Unknown Gaelic games event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished" || phase=="abandoned" && k!="official_result") .ra_fail("Match is not open")
 if(k=="score") {
  if(phase!="play" || !is.null(d$pending_kick) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Resolve the pending penalty and goalkeeper assignments before scoring")
  ti<-p$team+1L;.ra_rugby_player(d,p$team,p$player);field<-if(p$kind=="goal") "goals" else "points";d[[field]][[ti]]<-d[[field]][[ti]]+1L;d$scores[[ti]]<-d$scores[[ti]]+if(p$kind=="goal") 3L else 1L
 } else if(k=="resolve_kick") {
  r<-d$pending_kick
  if(phase!="play" || is.null(r) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("An eligible penalty and both goalkeepers are required")
  .ra_rugby_player(d,r$team,p$player)
  if(p$result!="none") {field<-if(p$result=="goal") "goals" else "points";ti<-r$team+1L;d[[field]][[ti]]<-d[[field]][[ti]]+1L;d$scores[[ti]]<-d$scores[[ti]]+if(p$result=="goal") 3L else 1L};d["pending_kick"]<-list(NULL)
 } else if(k=="set_goalkeeper") {
  if(!(phase %in% c("pre_match","play","halftime"))) .ra_fail("Goalkeeper designation requires an open match")
  .ra_rugby_player(d,p$team,p$player);d$teams[[p$team+1L]]$goalkeeper<-p$player
 } else if(k=="card") {
  if(!(phase %in% c("play","halftime"))) .ra_fail("Cards require a started match")
  ti<-p$team+1L;t<-d$teams[[ti]];i<-p$player+1L;colour<-p$colour
  if(i>length(t$status) || t$status[[i]]=="dismissed") .ra_fail("Player is absent or already dismissed")
  if(colour=="black" && !(p$player %in% unlist(.ra_rugby_active(t)))) .ra_fail("A black-card suspension requires an active eligible player")
  if(colour %in% c("yellow","black")) t[[colour]][[i]]<-t[[colour]][[i]]+1L
  red<-colour=="red" || t$yellow[[i]]+t$black[[i]]>=2
  if(red) {if(t$status[[i]]=="active") t$permanent_reductions<-t$permanent_reductions+1L;t$status[[i]]<-"dismissed";t$sin_bin_ms[[i]]<-0L;t$return_authorised[[i]]<-FALSE} else if(colour=="black") t$sin_bin_ms[[i]]<-600000L
  if(!is.null(t$goalkeeper) && t$goalkeeper==p$player && !(p$player %in% unlist(.ra_rugby_active(t)))) t["goalkeeper"]<-list(NULL)
  d$teams[[ti]]<-t;if(length(.ra_rugby_active(t))==0) {d$phase<-"abandoned";d["pending_kick"]<-list(NULL)}
 } else {
  if(k=="start_half" && any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Designate both goalkeepers before starting the half")
  if(k=="substitute") {t<-d$teams[[p$team+1L]];inc<-p[["in"]]+1L;if(inc<=length(t$status) && t$status[[inc]]=="bench" && t$substitutions>=5) .ra_fail("The five-substitution allowance is exhausted")}
  projected<-state;projected$data<-.ra_gaelic_project(d);d<-.ra_rugby_apply(projected,action,"union");d$extra_penalty<-NULL
  if(k=="set_lineup") {d$teams[[p$team+1L]]$black<-as.list(rep(0L,p$roster_size));d$teams[[p$team+1L]]$goalkeeper<-p$starters[[1]]}
  if(k=="substitute") {ti<-p$team+1L;if(!is.null(d$teams[[ti]]$goalkeeper) && d$teams[[ti]]$goalkeeper==p$out) d$teams[[ti]]$goalkeeper<-p[["in"]]}
 };if(kind!="football") for(i in 1:2) {d$teams[[i]]$black<-NULL;d$teams[[i]]$sin_bin_ms<-NULL};d
}
.ra_gaelic_legal<-function(state,kind) {
 d<-.ra_gaelic_enrich(state$data,kind);phase<-d$phase;names<-character()
 if(phase=="pre_match") names<-c("set_lineup","set_goalkeeper","start_half","concede") else if(phase=="abandoned") names<-"official_result" else if(phase %in% c("play","halftime")) {
  names<-c("card","set_goalkeeper","substitute","authorise_return","abandon","concede")
  if(phase=="halftime") names<-c(names,"start_half") else {names<-c(names,"elapse_play");if(is.null(d$pending_kick)) names<-c(names,"end_half","award_penalty_kick",if(!any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) "score") else if(all(vapply(d$teams,function(t) !is.null(t$goalkeeper),logical(1)))) names<-c(names,"resolve_kick")}
 }
 s<-.ra_gaelic_schemas(kind)
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Half, player and goalkeeper eligibility, pending penalty and substitution allowance must permit the event."),external_observations=list("Official score, card, replacement entitlement and completed playing time."))),constraints=list(phase=phase,half=d$half,totals=d$scores,active=lapply(d$teams,.ra_rugby_active),substitutions_remaining=lapply(d$teams,function(t) 5L-t$substitutions),pending_kick=d$pending_kick),completeness=FALSE,reason=paste0("official_gaelic_",kind,"_events"))
}
.ra_gaelic_outcome<-function(state,pack,kind) .ra_rugby_outcome(state,pack,"union")
