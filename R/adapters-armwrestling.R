.ra_aw_player <- list(type="integer",minimum=0,maximum=1)
.ra_aw_wins <- function(d) vapply(0:1,function(p) sum(unlist(d$bouts)==p),numeric(1))
.ra_aw_end <- function(d,w,reason) {
 d$bouts<-c(d$bouts,list(w));d$last_reason<-reason
 if(.ra_aw_wins(d)[w+1L]==2) {d$phase<-"finished";d$winner<-w;d$break_ms<-0L} else {d$phase<-"break";d$break_ms<-60000L};d
}
.ra_aw_foul <- function(d,p,losing=FALSE) {
 d$fouls[[p+1L]]<-d$fouls[[p+1L]]+1L
 if(losing || d$fouls[[p+1L]]>=2) return(.ra_aw_end(d,1L-p,if(losing) "losing_position_foul" else "two_fouls"))
 d$phase<-"setup";d$grip<-"referee";d$setup_ms<-60000L;d
}
.ra_aw_validate <- function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");counts<-.ra_aw_wins(d)
 if(length(d$bouts)>3 || any(counts>2) || (length(d$bouts)==3 && d$bouts[[1]]==d$bouts[[2]])) bad("The first two bout wins decide the match")
 if((d$grip=="free" && d$setup_ms==60000) || (d$grip=="referee" && d$setup_ms!=60000)) bad("Referee grip follows the expired free-setup minute or a restart")
 if(d$phase=="finished") {
  if(is.null(d$winner) || counts[d$winner+1L]!=2 || d$break_ms) bad("Match winner must have two bout wins")
 } else if(!is.null(d$winner) || max(counts)>=2) bad("Ongoing match cannot already have a winner")
 if(d$phase=="break") {if(!length(d$bouts) || !d$break_ms) bad("A completed nonfinal bout requires its remaining break")} else if(d$break_ms) bad("Break time exists only between bouts")
 if(d$phase %in% c("setup","ready","pull")) {
  if(!is.null(d$last_reason) || any(unlist(d$fouls)>=2)) bad("An ongoing bout cannot retain a completed-bout reason or two fouls")
 } else {
  if(is.null(d$last_reason) || !length(d$bouts)) bad("Completed bout requires a result reason")
  if(d$last_reason=="two_fouls" && d$fouls[[2L-tail(unlist(d$bouts),1)]]!=2) bad("Two-foul loss requires two fouls by the losing athlete")
  if(d$last_reason=="losing_position_foul" && !d$fouls[[2L-tail(unlist(d$bouts),1)]]) bad("Losing-position foul requires a foul by the losing athlete")
 }
 invisible(TRUE)
}
.ra_aw_apply <- function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;player<-.ra_aw_player
 if(k=="advance_setup") {
  .ra_fields(p,list(milliseconds=list(type="integer",minimum=1,maximum=60000)),"/params")
  if(d$phase!="setup" || d$grip!="free" || d$setup_ms+p$milliseconds>60000) .ra_fail("Advance only the remaining free-setup minute")
  d$setup_ms<-d$setup_ms+p$milliseconds;if(d$setup_ms==60000) d$grip<-"referee";return(d)
 }
 if(k=="confirm_grip") {.ra_fields(p,list(),"/params");if(d$phase!="setup") .ra_fail("Confirm the adjudicated legal grip only during setup");d$phase<-"ready";return(d)}
 if(k=="go") {.ra_fields(p,list(),"/params");if(d$phase!="ready") .ra_fail("The referee start command requires a confirmed grip");d$phase<-"pull";return(d)}
 if(k %in% c("warning","foul","early_start","grip_movement")) {
  schema<-list(player=player);if(k=="foul") schema$losing_position<-list(type="boolean")
  .ra_fields(p,schema,"/params");who<-p$player
  if(!(d$phase %in% c("setup","ready","pull"))) .ra_fail("Sanctions require an active bout")
  if(k=="early_start" && d$phase!="ready") .ra_fail("An early start occurs after grip confirmation and before the go command")
  if(k=="grip_movement" && (d$grip!="referee" || !(d$phase %in% c("setup","ready")))) .ra_fail("Movement in referee grip is penalized before the go command")
  if(k=="warning") {d$warnings[[who+1L]]<-d$warnings[[who+1L]]+1L;if(d$warnings[[who+1L]]==2) {d$warnings[[who+1L]]<-0L;return(.ra_aw_foul(d,who))};return(d)}
  if(k=="foul" && p$losing_position && d$phase!="pull") .ra_fail("A losing-position foul requires an active pull")
  return(.ra_aw_foul(d,who,isTRUE(p$losing_position)))
 }
 if(k=="slip") {
  .ra_fields(p,list(),"/params");if(d$phase!="pull" || d$strapped) .ra_fail("An unpenalized hand slip requires an unstrapped active pull")
  d$phase<-"setup";d$grip<-"referee";d$setup_ms<-60000L;d$strapped<-TRUE;return(d)
 }
 if(k=="pin") {.ra_fields(p,list(winner=player),"/params");if(d$phase!="pull") .ra_fail("An official pin requires an active pull");return(.ra_aw_end(d,p$winner,"pin"))}
 if(k=="advance_break") {
  .ra_fields(p,list(milliseconds=list(type="integer",minimum=1,maximum=60000)),"/params")
  if(d$phase!="break" || p$milliseconds>d$break_ms) .ra_fail("Advance only the remaining between-bout minute")
  d$break_ms<-d$break_ms-p$milliseconds
  if(!d$break_ms) {d$phase<-"setup";d$grip<-"free";d$setup_ms<-0L;d$warnings<-list(0L,0L);d$fouls<-list(0L,0L);d$strapped<-FALSE;d["last_reason"]<-list(NULL)}
  return(d)
 }
 if(k=="withdraw") {
  .ra_fields(p,list(player=player),"/params");w<-1L-p$player
  while(.ra_aw_wins(d)[w+1L]<2) d$bouts<-c(d$bouts,list(w))
  d$phase<-"finished";d$winner<-w;d$last_reason<-"withdrawal";d$break_ms<-0L;return(d)
 }
 .ra_fail("Unknown action")
}
.ra_aw_legal <- function(state) {
 d<-state$data;schemas<-list();ph<-d$phase;player<-.ra_aw_player;empty<-setNames(list(),character())
 if(ph=="setup") {schemas$confirm_grip<-empty;if(d$grip=="free") schemas$advance_setup<-list(milliseconds=list(type="integer",minimum=1,maximum=60000-d$setup_ms))}
 if(ph=="ready") {schemas$go<-empty;schemas$early_start<-list(player=player)}
 if(ph %in% c("setup","ready","pull")) {
  schemas$warning<-list(player=player);schemas$foul<-list(player=player,losing_position=list(type="boolean"))
  if(d$grip=="referee" && ph %in% c("setup","ready")) schemas$grip_movement<-list(player=player)
 }
 if(ph=="pull") {schemas$pin<-list(winner=player);if(!d$strapped) schemas$slip<-empty}
 if(ph=="break") schemas$advance_break<-list(milliseconds=list(type="integer",minimum=1,maximum=d$break_ms))
 schemas$withdraw<-list(player=player)
 list(mode="event_types",actions=list(),action_types=lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Bout phase, grip and foul status must permit the event."),external_observations=list("Official grip, start, slip, warning, foul, pin or withdrawal decision."))),constraints=list(grip=d$grip,strapped=d$strapped,warnings=d$warnings,fouls=d$fouls,bout_wins=as.list(.ra_aw_wins(d))),completeness=FALSE,reason="adjudicated_armwrestling_events")
}
.ra_aw_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;counts<-.ra_aw_wins(d)
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,bouts=counts[p+1L])) else list(),reason=if(done && d$last_reason=="withdrawal") "withdrawal" else if(done) "two_bout_wins" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
