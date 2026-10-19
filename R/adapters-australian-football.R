.ra_aussie_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);team<-i(0,1);slot<-i(0,21);e<-setNames(list(),character())
 s<-list(set_lineup=list(team=team,players=list(type="array",minItems=18,maxItems=18,items=slot)),start_quarter=e,score=list(team=team,player=c(slot,list(nullable=TRUE)),kind=list(type="string",enum=list("goal","behind"))),interchange=list(team=team,out=slot),sound_siren=e,award_after_siren_kick=list(team=team,player=slot),resolve_after_siren_kick=list(kind=list(type="string",enum=list("goal","behind","none"))),end_quarter=e,concede=list(team=team),abandon=e,official_result=list(winner=c(team,list(nullable=TRUE))));s$interchange[["in"]]<-slot;s
}
.ra_aussie_total<-function(d) lapply(1:2,function(i) 6L*d$goals[[i]]+d$behinds[[i]])
.ra_aussie_finish<-function(d,w,reason) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-reason;d["pending_kick"]<-list(NULL);d}
.ra_aussie_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(any(vapply(d$lineups,function(x) length(unique(unlist(x)))!=18,logical(1)))) bad("Each side requires eighteen distinct active players")
 if(d$phase=="pre_match" && (d$quarter!=1 || any(unlist(d$goals)>0) || any(unlist(d$behinds)>0))) bad("Unstarted match has no scoring history")
 if(d$phase=="interval" && d$quarter>=4) bad("There is no interval after the fourth quarter")
 if(!is.null(d$pending_kick) && d$phase!="siren") bad("After-siren kick requires a completed playing period")
 if(!is.null(d$pending_kick) && !(d$pending_kick$player %in% unlist(d$lineups[[d$pending_kick$team+1L]]))) bad("Pending kicker must be active")
 if(d$phase=="finished") {
  if(is.null(d$reason) || !(d$reason %in% c("draw","official")) && is.null(d$winner)) bad("Finished match requires a result")
  scores<-.ra_aussie_total(d)
  if(d$reason=="points" && (d$quarter!=4 || scores[[d$winner+1L]]<=scores[[2L-d$winner]])) bad("Points victory requires completed regulation and a lead")
  if(d$reason=="draw" && (d$quarter!=4 || scores[[1]]!=scores[[2]] || !is.null(d$winner))) bad("Draw requires tied regulation totals")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Open match cannot retain a result")
 if(d$expiry_decided && !(d$phase %in% c("siren","finished","abandoned"))) bad("Expiry decision belongs to a completed quarter");invisible(TRUE)
}
.ra_aussie_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;s<-.ra_aussie_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown Australian football event")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished" || phase=="abandoned" && k!="official_result") .ra_fail("Match is not open")
 if(k=="set_lineup") {
  if(phase!="pre_match" || length(unique(unlist(p$players)))!=18) .ra_fail("Set eighteen distinct starters before the match")
  d$lineups[[p$team+1L]]<-p$players
 } else if(k=="start_quarter") {
  if(!(phase %in% c("pre_match","interval"))) .ra_fail("No quarter is ready to begin")
  if(phase=="interval") d$quarter<-d$quarter+1L
  d$phase<-"play";d$expiry_decided<-FALSE
 } else if(k=="score") {
  if(phase!="play" || is.null(p$player) && p$kind=="goal" || !is.null(p$player) && !(p$player %in% unlist(d$lineups[[p$team+1L]]))) .ra_fail("Official score requires live play and an active player")
  field<-if(p$kind=="goal") "goals" else "behinds";d[[field]][[p$team+1L]]<-d[[field]][[p$team+1L]]+1L
 } else if(k=="interchange") {
  if(!(phase %in% c("play","interval"))) .ra_fail("Interchange requires a playing quarter or interval")
  line<-d$lineups[[p$team+1L]];ids<-unlist(line)
  if(!(p$out %in% ids) || p[["in"]] %in% ids) .ra_fail("Interchange pairs an active player and a bench player")
  line[[match(p$out,ids)]]<-p[["in"]];d$lineups[[p$team+1L]]<-line
 } else if(k=="sound_siren") {if(phase!="play") .ra_fail("Only an active quarter can reach its siren");d$phase<-"siren"
 } else if(k=="award_after_siren_kick") {
  if(phase!="siren" || !is.null(d$pending_kick) || d$expiry_decided || !(p$player %in% unlist(d$lineups[[p$team+1L]]))) .ra_fail("Only one eligible unresolved after-siren kick may be awarded")
  d$pending_kick<-p;d$expiry_decided<-TRUE
 } else if(k=="resolve_after_siren_kick") {
  if(is.null(d$pending_kick)) .ra_fail("No after-siren kick is pending")
  if(p$kind!="none") {field<-if(p$kind=="goal") "goals" else "behinds";ti<-d$pending_kick$team+1L;d[[field]][[ti]]<-d[[field]][[ti]]+1L};d["pending_kick"]<-list(NULL)
 } else if(k=="end_quarter") {
  if(phase!="siren" || !is.null(d$pending_kick)) .ra_fail("Sound the siren and resolve any after-siren kick before closing the quarter")
  if(d$quarter<4) {d$phase<-"interval";d$expiry_decided<-FALSE} else {scores<-.ra_aussie_total(d);tie<-scores[[1]]==scores[[2]];d<-.ra_aussie_finish(d,if(tie) NULL else if(scores[[1]]>scores[[2]]) 0L else 1L,if(tie) "draw" else "points")}
 } else if(k=="concede") d<-.ra_aussie_finish(d,1L-p$team,"concession") else if(k=="abandon") {d$phase<-"abandoned";d["pending_kick"]<-list(NULL)
 } else if(k=="official_result") {if(phase!="abandoned") .ra_fail("External competition result requires abandonment");d<-.ra_aussie_finish(d,p$winner,"official")};d
}
.ra_aussie_legal<-function(state) {
 d<-state$data;phase<-d$phase;names<-switch(phase,pre_match=c("set_lineup","start_quarter"),play=c("score","interchange","sound_siren"),interval=c("interchange","start_quarter"),siren=if(!is.null(d$pending_kick)) "resolve_after_siren_kick" else c("end_quarter",if(!d$expiry_decided) "award_after_siren_kick"),abandoned="official_result",character())
 if(!(phase %in% c("finished","abandoned"))) names<-c(names,"concede","abandon")
 s<-.ra_aussie_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Quarter, active lineup and after-siren entitlement must permit the event."),external_observations=list("Official goals, behinds, completed quarter siren, awarded mark/free kick and its final score."))),constraints=list(phase=phase,quarter=d$quarter,totals=.ra_aussie_total(d),active=d$lineups,bench=lapply(d$lineups,function(line) as.list(setdiff(0:21,unlist(line)))),pending_kick=d$pending_kick),completeness=FALSE,reason="official_classic_australian_football_events")
}
.ra_aussie_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner;abandoned<-d$phase=="abandoned"
 list(status=if(done) "terminal" else if(abandoned) "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(is.null(w)) "draw" else "win"} else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(abandoned) "competition_result_required" else "ongoing",evidence_required=if(abandoned) list("Official competition result.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
