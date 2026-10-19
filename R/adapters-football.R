# Compact football rule state. Physical and medical observations are not modeled.
.ra_fb_play <- c("first_half","second_half","extra_first","extra_second")
.ra_fb_breaks <- c("halftime","extra_break","extra_halftime")
.ra_fb_extra <- c("extra_break","extra_first","extra_halftime","extra_second")
.ra_fb_active <- function(team) as.list(which(unlist(team$status)=="active")-1L)
.ra_fb_penalty_winner <- function(d) {
  a<-unlist(d$shootout_attempts);g<-unlist(d$shootout_goals)
  for(t in 1:2) if(max(a)<=5 && g[t]>g[3L-t]+max(0,5-a[3L-t])) return(t-1L)
  if(min(a)>=5 && a[1]==a[2] && g[1]!=g[2]) return(if(g[1]>g[2]) 0L else 1L)
  NULL
}
.ra_fb_validate <- function(d) {
  phase<-d$phase;r<-d$rules;shoot<-phase %in% c("shootout_setup","shootout") || d$shootout_started
  bad<-function(message) .ra_fail(message,code="INVALID_STATE")
  if(r$extra_time && !r$knockout) bad("Extra time requires knockout format")
  if(d$extra_started && !(r$knockout && r$extra_time)) bad("Extra time is disabled")
  if(phase %in% .ra_fb_extra && !d$extra_started) bad("Extra-time phase mismatch")
  if(shoot && (!r$knockout || d$goals[[1]]!=d$goals[[2]])) bad("Shoot-out requires a tied knockout match")
  if(d$shootout_started!=(phase %in% c("shootout","finished","abandoned") && shoot)) bad("Shoot-out phase mismatch")
  if(d$extra_started && phase %in% c("pre_match","first_half","halftime","second_half")) bad("Extra time has not begun")
  for(team in d$teams) {
    if(length(team$yellow)!=length(team$status) || length(team$red)!=length(team$status)) bad("Player vectors differ in length")
    for(i in seq_along(team$status)) if(team$red[[i]]!=(team$status[[i]]=="dismissed") || (team$yellow[[i]]==2 && !team$red[[i]])) bad("Card and eligibility mismatch")
    n<-length(.ra_fb_active(team))
    if(n>11 || (phase %in% c(.ra_fb_play,.ra_fb_breaks) && n<7)) bad("Invalid active player count")
    if(!is.null(team$goalkeeper) && !(team$goalkeeper %in% unlist(.ra_fb_active(team)))) bad("Goalkeeper must be active")
    bonus<-as.integer(d$extra_started && r$extra_substitute)
    if(team$substitutions>r$substitution_limit+bonus || team$windows>r$window_limit+bonus) bad("Substitution allowance exceeded")
    if(team$windows>team$substitutions) bad("Substitution windows exceed substitutions")
    eligible<-unlist(team$eligible);taken<-unlist(team$taken)
    if(!shoot && (length(eligible) || length(taken))) bad("Kicker eligibility exists before shoot-out setup")
    if(d$shootout_started && !is.null(team$goalkeeper) && !team$goalkeeper %in% eligible) bad("Shoot-out goalkeeper must be eligible")
    if(anyDuplicated(eligible) || any(!eligible %in% unlist(.ra_fb_active(team)))) bad("Invalid eligible kickers")
    if(anyDuplicated(taken) || any(!taken %in% eligible)) bad("Invalid kicker cycle")
  }
  a<-unlist(d$shootout_attempts);g<-unlist(d$shootout_goals);first<-d$shootout_first+1L
  if(any(g>a) || !((a[first]-a[3L-first]) %in% c(0,1))) bad("Invalid shoot-out counters")
  if(!d$shootout_started && (any(a!=0) || any(g!=0))) bad("Shoot-out has not started")
  if(phase=="shootout" && !is.null(.ra_fb_penalty_winner(d))) bad("Shoot-out already decided")
  if(phase=="finished" && r$knockout && d$goals[[1]]==d$goals[[2]] && is.null(.ra_fb_penalty_winner(d))) bad("Knockout winner is unresolved")
  if(!is.null(d$pending_penalty) && !phase %in% .ra_fb_play) bad("Pending penalty requires a playing period")
  if(phase=="pre_match" && (any(unlist(d$goals)!=0) || d$extra_started || d$shootout_started)) bad("Invalid pre-match counters")
}
.ra_fb_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";abandoned<-d$phase=="abandoned";w<-NULL
  if(terminal) w<-if(d$shootout_started) .ra_fb_penalty_winner(d) else if(d$goals[[1]]==d$goals[[2]]) NULL else if(d$goals[[1]]>d$goals[[2]]) 0L else 1L
  list(status=if(terminal) "terminal" else if(abandoned) "awaiting_adjudication" else "ongoing",result_kind=if(terminal) {if(is.null(w)) "draw" else "win"} else "none",winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(terminal && d$shootout_started) "shootout_decided" else if(terminal) "match_finished" else if(abandoned) "match_abandoned" else "ongoing",evidence_required=if(abandoned) list("Competition decision on abandoned match.") else list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
.ra_fb_after_play <- function(d) {
  if(d$phase=="first_half") d$phase<-"halftime"
  else if(d$phase=="extra_first") d$phase<-"extra_halftime"
  else if(d$goals[[1]]!=d$goals[[2]] || !d$rules$knockout) d$phase<-"finished"
  else if(d$phase=="second_half" && d$rules$extra_time) {d$phase<-"extra_break";d$extra_started<-TRUE}
  else {
    d$phase<-"shootout_setup"
    for(j in 1:2) {t<-d$teams[[j]];t$yellow<-as.list(rep(0L,length(t$yellow)));t$eligible<-.ra_fb_active(t);t$taken<-list();d$teams[[j]]<-t}
  }
  d
}
.ra_fb_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;phase<-d$phase
  if(phase %in% c("finished","abandoned")) .ra_fail("Match is not open")
  schemas<-.ra_builtins$S001$action_schemas
  if(!kind %in% names(schemas)) .ra_fail("Unknown action")
  .ra_fields(p,schemas[[kind]],"/params")
  if(kind=="configure") {
    if(phase!="pre_match") .ra_fail("Rules are fixed after kick-off")
    d$rules<-p
  } else if(kind=="set_lineup") {
    if(phase!="pre_match") .ra_fail("Lineup is fixed after kick-off")
    j<-p$team+1L;t<-d$teams[[j]];n<-p$roster_size;starters<-unlist(p$starters)
    if(any(unlist(t$yellow)!=0) || any(unlist(t$red))) .ra_fail("Lineup replacement cannot erase disciplinary records")
    if(anyDuplicated(starters) || max(starters)>=n || !p$goalkeeper %in% starters || n-length(starters)>15) .ra_fail("Invalid starting lineup")
    t$yellow<-as.list(rep(0L,n));t$red<-as.list(rep(FALSE,n));t$status<-lapply(0:(n-1L),function(i) if(i %in% starters) "active" else "bench");t$goalkeeper<-p$goalkeeper;d$teams[[j]]<-t
  } else if(kind=="start_period") {
    mapping<-list(pre_match="first_half",halftime="second_half",extra_break="extra_first",extra_halftime="extra_second")
    if(!phase %in% names(mapping)) .ra_fail("No period can start now")
    if(any(vapply(d$teams,function(t) length(.ra_fb_active(t))<7 || is.null(t$goalkeeper),logical(1)))) .ra_fail("Each team needs seven players and a goalkeeper")
    d$phase<-mapping[[phase]]
  } else if(kind=="abandon_match") {
    if(phase=="pre_match") .ra_fail("Match has not started")
    d$phase<-"abandoned";d["pending_penalty"]<-list(NULL)
  } else if(kind=="goal") {
    if(!phase %in% .ra_fb_play) .ra_fail("Goals require a playing period")
    if(!is.null(d$pending_penalty) || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Resolve the pending restart or goalkeeper first")
    j<-p$team+1L;d$goals[[j]]<-d$goals[[j]]+1L
  } else if(kind=="end_period") {
    if(!phase %in% .ra_fb_play) .ra_fail("No playing period to end")
    if(!is.null(d$pending_penalty)) .ra_fail("A pending penalty must be resolved before the period ends")
    d<-.ra_fb_after_play(d)
  } else if(kind=="award_penalty") {
    if(!phase %in% .ra_fb_play || !is.null(d$pending_penalty)) .ra_fail("Cannot award a penalty now")
    d$pending_penalty<-p$team
  } else if(kind=="resolve_penalty") {
    if(is.null(d$pending_penalty)) .ra_fail("No penalty is pending")
    if(any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("A goalkeeper must be designated first")
    j<-d$pending_penalty+1L;d$goals[[j]]<-d$goals[[j]]+as.integer(p$scored);d["pending_penalty"]<-list(NULL)
  } else if(kind=="replace_before_start") {
    if(phase!="pre_match") .ra_fail("Match has already started")
    j<-p$team+1L;t<-d$teams[[j]];b<-p[["in"]]+1L
    if(b>length(t$status) || t$status[[b]]!="bench" || length(.ra_fb_active(t))>=11) .ra_fail("Invalid pre-match replacement")
    t$status[[b]]<-"active";d$teams[[j]]<-t
  } else if(kind=="card") {
    j<-p$team+1L;t<-d$teams[[j]];i<-p$player+1L
    if(i>length(t$status) || t$red[[i]]) .ra_fail("Player is unavailable for a new card")
    if(p$color=="yellow") t$yellow[[i]]<-t$yellow[[i]]+1L
    if(p$color=="red" || t$yellow[[i]]==2) {
      t$red[[i]]<-TRUE;t$status[[i]]<-"dismissed"
      if(!is.null(t$goalkeeper) && t$goalkeeper==i-1L) t["goalkeeper"]<-list(NULL)
      t$eligible<-Filter(function(x) x!=i-1L,t$eligible);t$taken<-Filter(function(x) x!=i-1L,t$taken)
      if(!d$shootout_started && !phase %in% c("shootout_setup","pre_match") && length(.ra_fb_active(t))<7) d$phase<-"abandoned"
    }
    d$teams[[j]]<-t
  } else if(kind=="set_goalkeeper") {
    j<-p$team+1L;t<-d$teams[[j]];i<-p$player
    if(!i %in% unlist(.ra_fb_active(t)) || (d$shootout_started && !i %in% unlist(t$eligible))) .ra_fail("Goalkeeper must be active and eligible")
    t$goalkeeper<-i;d$teams[[j]]<-t
  } else if(kind=="substitute") {
    if(!phase %in% c(.ra_fb_play,.ra_fb_breaks)) .ra_fail("Substitution is unavailable in this phase")
    j<-p$team+1L;t<-d$teams[[j]];out<-unlist(p$out);inc<-unlist(p[["in"]]);bonus<-as.integer(d$extra_started && d$rules$extra_substitute)
    if(length(out)!=length(inc) || anyDuplicated(out) || anyDuplicated(inc)) .ra_fail("Invalid substitution vectors")
    if(any(!out %in% unlist(.ra_fb_active(t))) || any(vapply(inc,function(i) i>=length(t$status) || t$status[[i+1L]]!="bench",logical(1)))) .ra_fail("Only active players may leave and unused substitutes enter")
    window<-as.integer(phase %in% .ra_fb_play)
    if(t$substitutions+length(inc)>d$rules$substitution_limit+bonus || t$windows+window>d$rules$window_limit+bonus) .ra_fail("Substitution allowance exhausted")
    for(k in seq_along(out)) {a<-out[k];b<-inc[k];t$status[[a+1L]]<-"off";t$status[[b+1L]]<-"active";if(!is.null(t$goalkeeper) && t$goalkeeper==a) t$goalkeeper<-b}
    t$substitutions<-t$substitutions+length(inc);t$windows<-t$windows+window;d$teams[[j]]<-t
  } else if(kind=="start_shootout") {
    if(phase!="shootout_setup") .ra_fail("Shoot-out is not ready")
    for(j in 1:2) {
      t<-d$teams[[j]];excluded<-unlist(p$excluded[[j]])
      if(anyDuplicated(excluded) || any(!excluded %in% unlist(t$eligible)) || (!is.null(t$goalkeeper) && t$goalkeeper %in% excluded)) .ra_fail("Invalid shoot-out exclusions")
      t$eligible<-Filter(function(i) !i %in% excluded,t$eligible);d$teams[[j]]<-t
    }
    sizes<-vapply(d$teams,function(t) length(t$eligible),integer(1))
    if(sizes[1]==0 || sizes[1]!=sizes[2] || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1)))) .ra_fail("Shoot-out teams must have equal eligible counts and goalkeepers")
    d$shootout_first<-p$first_team;d$shootout_started<-TRUE;d$phase<-"shootout"
  } else if(kind=="replace_shootout_goalkeeper") {
    if(phase!="shootout") .ra_fail("Not in a shoot-out")
    j<-p$team+1L;t<-d$teams[[j]];i<-p$player;old<-t$goalkeeper
    if(is.null(old) || i>=length(t$status) || i %in% unlist(t$eligible) || !t$status[[i+1L]] %in% c("active","bench")) .ra_fail("Replacement must be an excluded player or unused substitute")
    if(t$status[[i+1L]]=="bench") {
      limit<-d$rules$substitution_limit+as.integer(d$extra_started && d$rules$extra_substitute)
      if(t$substitutions>=limit) .ra_fail("Substitution allowance exhausted")
      t$substitutions<-t$substitutions+1L
    }
    t$status[[old+1L]]<-"off";t$status[[i+1L]]<-"active";t$goalkeeper<-i
    t$eligible<-lapply(t$eligible,function(x) if(x==old) i else x);t$taken<-lapply(t$taken,function(x) if(x==old) i else x);d$teams[[j]]<-t
  } else if(kind=="exclude_kicker") {
    if(phase!="shootout") .ra_fail("Not in a shoot-out")
    j<-p$team+1L;t<-d$teams[[j]];other<-d$teams[[3L-j]];i<-p$player
    if(length(t$eligible)<=length(other$eligible) || !i %in% unlist(t$eligible) || (!is.null(t$goalkeeper) && i==t$goalkeeper)) .ra_fail("Exclusion must equalize eligible teams")
    t$eligible<-Filter(function(x) x!=i,t$eligible);t$taken<-Filter(function(x) x!=i,t$taken);d$teams[[j]]<-t
  } else if(kind=="shootout_kick") {
    if(phase!="shootout") .ra_fail("Not in a shoot-out")
    j<-p$team+1L;i<-p$player;first<-d$shootout_first;a<-d$shootout_attempts;expected<-if(a[[1]]==a[[2]]) first else 1L-first;t<-d$teams[[j]]
    if(j-1L!=expected || any(vapply(d$teams,function(t) is.null(t$goalkeeper),logical(1))) || length(t$eligible)!=length(d$teams[[3L-j]]$eligible)) .ra_fail("Wrong turn or unresolved shoot-out eligibility")
    if(setequal(unlist(t$taken),unlist(t$eligible))) t$taken<-list()
    if(!i %in% unlist(t$eligible) || i %in% unlist(t$taken)) .ra_fail("Kicker is ineligible or has already kicked this cycle")
    t$taken[[length(t$taken)+1L]]<-i;a[[j]]<-a[[j]]+1L;d$shootout_goals[[j]]<-d$shootout_goals[[j]]+as.integer(p$scored);d$shootout_attempts<-a;d$teams[[j]]<-t
    if(!is.null(.ra_fb_penalty_winner(d))) d$phase<-"finished"
  }
  d
}
.ra_fb_legal <- function(state) {
  phase<-state$data$phase;kinds<-character()
  if(!phase %in% c("finished","abandoned")) kinds<-c("card","set_goalkeeper")
  if(!phase %in% c("pre_match","finished","abandoned")) kinds<-c(kinds,"abandon_match")
  if(phase=="pre_match") kinds<-c(kinds,"configure","set_lineup","start_period","replace_before_start")
  if(phase %in% .ra_fb_breaks) kinds<-c(kinds,"start_period","substitute")
  if(phase %in% .ra_fb_play) kinds<-c(kinds,"substitute",if(!is.null(state$data$pending_penalty)) "resolve_penalty" else c("goal","end_period","award_penalty"))
  if(phase=="shootout_setup") kinds<-c(kinds,"start_shootout")
  if(phase=="shootout") kinds<-c(kinds,"exclude_kicker","shootout_kick","replace_shootout_goalkeeper")
  if(phase %in% .ra_fb_play && any(vapply(state$data$teams,function(t) is.null(t$goalkeeper),logical(1)))) kinds<-setdiff(kinds,c("goal","resolve_penalty"))
  d<-state$data;bonus<-as.integer(d$extra_started && d$rules$extra_substitute)
  constraints<-list(phase=phase,teams=list())
  for(t in d$teams) {
    remaining<-d$rules$substitution_limit+bonus-t$substitutions;windows<-d$rules$window_limit+bonus-t$windows
    kickers<-Filter(function(i) !i %in% unlist(t$taken),t$eligible);if(!length(kickers)) kickers<-t$eligible
    constraints$teams[[length(constraints$teams)+1L]]<-list(active=.ra_fb_active(t),bench=as.list(which(unlist(t$status)=="bench")-1L),substitutions_remaining=remaining,windows_remaining=windows,next_kickers=kickers)
  }
  if(phase=="shootout") constraints$next_team<-if(d$shootout_attempts[[1]]==d$shootout_attempts[[2]]) d$shootout_first else 1L-d$shootout_first
  if("substitute" %in% kinds && !any(vapply(constraints$teams,function(t) t$substitutions_remaining>0 && length(t$bench)>0 && (phase %in% .ra_fb_breaks || t$windows_remaining>0),logical(1)))) kinds<-setdiff(kinds,"substitute")
  list(mode="event_types",actions=list(),action_types=lapply(kinds,function(k) list(type=k,params_schema=.ra_builtins$S001$action_schemas[[k]],preconditions=list("Player eligibility, phase and allowances must permit the event."),external_observations=list("Referee-confirmed event; period ends include all required added time."))),constraints=constraints,completeness=FALSE,reason="externally_adjudicated_events")
}
