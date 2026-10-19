.ra_diamond_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_diamond_e<-function(...) list(type="enum",values=list(...))
.ra_diamond_a<-function(x,n,m=n) list(type="array",minItems=n,maxItems=m,items=x)
.ra_diamond_due<-function(d) d$lineup[[d$batting+1L]][[d$next_slot[[d$batting+1L]]+1L]]
.ra_diamond_finish<-function(d,w,r) {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d}
.ra_diamond_clear<-function(d) {d$last_runs<-list();d$last_participants<-list();d$last_home_run<-FALSE;d$last_force<-FALSE;d$appealed<-list();d}
.ra_diamond_roster<-function(d) all(vapply(1:2,function(t) all(unlist(d$status[[t]])[unlist(d$lineup[[t]])+1L]=="active"),logical(1)))
.ra_diamond_limit<-function(d) {
 if(d$inning>=d$regulation_innings) return(1L)
 if(d$softball && d$inning>=3) return(if(d$inning==3) 15L else if(d$inning==4) 10L else 7L)
 NULL
}
.ra_diamond_next<-function(d) {
 if(d$batting==1) d$inning<-d$inning+1L
 d$batting<-1L-d$batting;d$outs<-0L;d$bases<-rep(list(NULL),3);d$temporary<-rep(list(NULL),3);d$phase<-"play";d$half_runs<-0L;d$visits<-list(0L,0L);d$pitcher_locked[[d$batting+1L]]<-FALSE
 if(d$softball && d$inning>=8) d$bases[[2]]<-d$lineup[[d$batting+1L]][[(d$next_slot[[d$batting+1L]]-1L)%%9L+1L]]
 .ra_diamond_clear(d)
}
.ra_diamond_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$regulation_innings!=(if(d$softball) 7L else 9L)) bad("Regulation length must match the selected discipline")
 for(t in 1:2) {
  line<-unlist(d$lineup[[t]])
  if(length(unique(line))!=9 || !d$pitcher[[t]] %in% line || !d$catcher[[t]] %in% line || d$pitcher[[t]]==d$catcher[[t]]) bad("Nine distinct batting slots require distinct active pitcher and catcher roles")
  if(any(vapply(0:24,function(i) d$status[[t]][[i+1L]]=="active" && !i %in% line,logical(1)))) bad("Active player must occupy a batting slot")
  if(any(vapply(0:24,function(i) d$entries[[t]][[i+1L]]>(if(d$softball && i<9) 2L else 1L),logical(1)))) bad("Player exceeds permitted entries")
  if(d$pitcher_banned[[t]][[d$pitcher[[t]]+1L]] && d$phase=="play") bad("Removed pitcher cannot pitch again")
 }
 runners<-unlist(d$bases)
 if(anyDuplicated(runners)>0 || any(!runners %in% unlist(d$lineup[[d$batting+1L]]))) bad("Bases require distinct players in the current batting lineup")
 if(d$phase=="play" && (d$outs>=3 || !.ra_diamond_roster(d))) bad("Play requires fewer than three outs and complete eligible lineups")
 if(d$phase=="review" && length(d$last_participants)==0) bad("Review requires a recorded completed play")
 if(anyDuplicated(unlist(d$last_runs))>0 || any(!unlist(d$last_runs) %in% unlist(d$last_participants))) bad("Scored runners must belong to the last play")
 if(length(d$last_runs)>d$score[[d$batting+1L]] || d$half_runs>d$score[[d$batting+1L]]) bad("Run ledger cannot exceed team score")
 if(d$last_force && d$outs==3 && length(d$last_runs)>0) bad("A force third out cancels all runs on that play")
 if(d$phase=="finished") {
  if(is.null(d$reason) || (d$reason=="no_contest")!=is.null(d$winner)) bad("Result must specify winner or no contest")
  if(d$reason %in% c("regulation","walkoff","run_ahead","forfeit") && d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Winning score must lead")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Unfinished game cannot contain a result")
}
.ra_diamond_schemas<-function(d) {
 ph<-d$phase;s<-list();ii<-.ra_diamond_i;ee<-.ra_diamond_e;aa<-.ra_diamond_a;t<-ii(0,1);p<-ii(0,24);b<-list(type="boolean");empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="suspended") return(list(resume=empty,official_result=list(winner=c(t,list(nullable=TRUE)))))
 if(ph=="play") s<-list(play=list(batter=c(p,list(nullable=TRUE)),kind=ee("hit","error","fielders_choice","out","dropped_third","infield_fly","home_run","runner_play"),destinations=aa(ii(-1,4),4),credited=aa(p,0,4),third_out=ee("none","tag","force","batter_first","preceding","left_early")),award_first=list(batter=p,reason=ee("walk","hit_by_pitch","interference")),mound_visit=list(team=t),temporary_runner=empty)
 if(ph=="review") s<-list(confirm_play=empty,appeal=list(runner=p,force=b,cancel_runs=aa(p,0,4)))
 if(ph=="change") s$resume_play<-empty
 if(ph %in% c("play","review","change")) s<-c(s,list(substitute=list(team=t,slot=ii(0,8),player=p,exception=b),set_pitcher=list(team=t,player=p,exception=b),set_catcher=list(team=t,player=p),eject=list(team=t,player=p),forfeit=list(team=t),suspend=empty))
 s
}
.ra_diamond_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;s<-.ra_diamond_schemas(d);t<-d$batting;tt<-t+1L;defense<-1L-t;dd<-defense+1L
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params")
 if(k %in% c("play","award_first")) {
  if(d$pitcher_banned[[dd]][[d$pitcher[[dd]]+1L]]) .ra_fail("A required pitching change must be completed")
  batter<-.ra_diamond_due(d);before<-d$bases;participants<-c(list(batter),before);d<-.ra_diamond_clear(d)
  if(batter %in% unlist(before)) .ra_fail("Next batter cannot already occupy a base")
  if(k=="award_first") {
   if(p$batter!=batter) .ra_fail("Use the next batter in the registered order")
   runs<-list()
   if(!is.null(before[[1]])) {
    if(!is.null(before[[2]])) {if(!is.null(before[[3]])) runs<-list(before[[3]]);d$bases[3]<-before[2];d$temporary[3]<-d$temporary[2]}
    d$bases[2]<-before[1];d$temporary[2]<-d$temporary[1]
   }
   d$bases[[1]]<-batter;d$temporary[1]<-list(NULL);complete<-TRUE
  } else {
   kind<-p$kind;dest<-unlist(p$destinations);complete<-kind!="runner_play"
   if(!identical(p$batter,if(complete) batter else NULL)) .ra_fail("Batter must match the next slot, or be null for a runner-only play")
   if(!complete) participants[1]<-list(NULL)
   for(j in 1:4) {
    x<-participants[[j]]
    if(is.null(x)!=(dest[j]== -1L)) .ra_fail("Destinations must correspond exactly to participating players")
    if(!is.null(x) && !dest[j] %in% c(0,4) && dest[j]<(j-1L)) .ra_fail("A completed play cannot leave a runner behind the occupied starting base")
   }
   remaining<-dest[dest>=1 & dest<=3]
   if(anyDuplicated(remaining)>0) .ra_fail("Two runners cannot finish on the same base")
   if(!identical(remaining,sort(remaining))) .ra_fail("Surviving runners cannot pass a preceding runner")
   if(complete && kind %in% c("hit","error") && dest[1]<1) .ra_fail("A safe batter must reach a base or home")
   if(kind %in% c("out","infield_fly") && dest[1]!=0) .ra_fail("The declared batter out must retire the batter")
   if(kind=="infield_fly" && (d$outs>=2 || is.null(before[[1]]) || is.null(before[[2]]))) .ra_fail("Infield fly requires fewer than two outs and occupied first and second bases")
   if(kind=="dropped_third" && !is.null(before[[1]]) && d$outs<2) .ra_fail("Uncaught third strike cannot permit this batter to run")
   if(kind=="home_run" && any(vapply(1:4,function(j) !is.null(participants[[j]]) && dest[j]!=4,logical(1)))) .ra_fail("Over-fence home run must bring every participating runner home")
   new_outs<-sum(dest==0)
   if(d$outs+new_outs>3) .ra_fail("Record only the outs needed to retire the side; later appeals are separate")
   d$outs<-d$outs+new_outs;third<-p$third_out
   if((d$outs==3)!=(third!="none")) .ra_fail("Third-out classification must match the out total")
   if(third=="left_early" && !d$softball) .ra_fail("Leaving early is a softball third-out classification")
   candidates<-unlist(participants[which(dest==4)]);runs<-p$credited
   if(anyDuplicated(unlist(runs))>0 || any(!unlist(runs) %in% candidates)) .ra_fail("Credited runs must be distinct runners who reached home")
   if(d$outs<3 && !setequal(unlist(runs),candidates)) .ra_fail("Every completed home crossing scores before a third out")
   force<-third %in% c("force","batter_first","left_early")
   if(force && length(runs)>0) .ra_fail("This third out cancels every run on the play")
   if(kind=="home_run" && !setequal(unlist(runs),candidates)) .ra_fail("Every home-run crossing must be credited")
   d$last_force<-force;d$last_home_run<-kind=="home_run";d$bases<-rep(list(NULL),3);d$temporary<-rep(list(NULL),3)
   for(j in 1:4) if(!is.null(participants[[j]]) && dest[j]>=1 && dest[j]<=3) d$bases[[dest[j]]]<-participants[[j]]
  }
  d$last_participants<-Filter(Negate(is.null),participants);d$last_runs<-runs;d$score[[tt]]<-d$score[[tt]]+length(runs);d$half_runs<-d$half_runs+length(runs);d$phase<-"review"
  if(complete) {d$next_slot[[tt]]<-(d$next_slot[[tt]]+1L)%%9L;d$pitcher_locked[[dd]]<-FALSE}
 } else if(k=="appeal") {
  i<-p$runner;cancel<-p$cancel_runs
  if(i %in% unlist(d$appealed) || !i %in% unlist(d$last_participants) || anyDuplicated(unlist(cancel))>0 || any(!unlist(cancel) %in% unlist(d$last_runs))) .ra_fail("Appeal must refer to the last play and its credited runs")
  jj<-which(vapply(d$bases,function(x) !is.null(x) && x==i,logical(1)))
  if(length(jj)>0) {d$bases[jj]<-list(NULL);d$temporary[jj]<-list(NULL)}
  d$outs<-min(3L,d$outs+1L)
  if(d$outs==3 && p$force) {cancel<-d$last_runs;d$last_force<-TRUE}
  if(i %in% unlist(d$last_runs) && !i %in% unlist(cancel)) .ra_fail("A scored runner retired on appeal must lose that run")
  d$last_runs<-Filter(function(x) !x %in% unlist(cancel),d$last_runs);d$score[[tt]]<-d$score[[tt]]-length(cancel);d$half_runs<-d$half_runs-length(cancel);d$appealed<-c(d$appealed,list(i))
 } else if(k=="confirm_play") {
  limit<-.ra_diamond_limit(d);lead<-d$score[[2]]-d$score[[1]]
  if(t==1 && !is.null(limit) && lead>=limit) {
   if(!d$last_home_run && lead>limit) {
    excess<-lead-limit;if(excess>length(d$last_runs)) .ra_fail("Winning threshold was already passed before the last play")
    d$score[[2]]<-d$score[[2]]-excess;d$half_runs<-d$half_runs-excess;d$last_runs<-head(d$last_runs,length(d$last_runs)-excess)
   }
   d<-.ra_diamond_finish(d,1L,if(d$inning>=d$regulation_innings) "walkoff" else "run_ahead")
  } else if(d$outs==3) {
   if(t==0 && !is.null(limit) && lead>=limit) d<-.ra_diamond_finish(d,1L,if(d$inning>=d$regulation_innings) "regulation" else "run_ahead") else if(t==1 && !is.null(limit) && abs(lead)>=limit) d<-.ra_diamond_finish(d,as.integer(lead>0),if(d$inning>=d$regulation_innings) "regulation" else "run_ahead") else d<-.ra_diamond_next(d)
  } else {d$phase<-"play";d<-.ra_diamond_clear(d)}
  if(d$phase=="play" && !.ra_diamond_roster(d)) .ra_fail("Replace ejected players or declare a forfeit before continuing")
 } else if(k=="substitute") {
  a<-p$team+1L;slot<-p$slot+1L;i<-p$player;old<-d$lineup[[a]][[slot]];st<-d$status[[a]][[i+1L]]
  reentry<-d$softball && i<9 && i==p$slot && d$entries[[a]][[i+1L]]==1 && st=="bench"
  if(st!="unused" && !reentry) .ra_fail("Incoming player is not eligible for this batting slot")
  if(i %in% unlist(d$lineup[[a]])) .ra_fail("Player already occupies a batting slot")
  if(old==d$pitcher[[a]] && d$pitcher_locked[[a]] && !p$exception) .ra_fail("Pitcher must complete the required batter before replacement")
  if(d$status[[a]][[old+1L]]!="ejected") d$status[[a]][[old+1L]]<-if(d$softball && old<9 && d$entries[[a]][[old+1L]]==1) "bench" else "retired"
  d$status[[a]][[i+1L]]<-"active";d$entries[[a]][[i+1L]]<-d$entries[[a]][[i+1L]]+1L;d$lineup[[a]][[slot]]<-i
  if(old==d$pitcher[[a]]) {d$pitcher[[a]]<-i;d$pitcher_locked[[a]]<-TRUE;d$visits[[a]]<-0L}
  if(old==d$catcher[[a]]) d$catcher[[a]]<-i
  if(a==tt) for(j in 1:3) if(!is.null(d$bases[[j]]) && d$bases[[j]]==old) d$bases[[j]]<-i
 } else if(k=="set_pitcher") {
  a<-p$team+1L;i<-p$player
  if(!i %in% unlist(d$lineup[[a]]) || d$status[[a]][[i+1L]]!="active" || d$pitcher_banned[[a]][[i+1L]] || i==d$catcher[[a]] || i==d$pitcher[[a]]) .ra_fail("Choose a different eligible active pitcher")
  if(d$pitcher_locked[[a]] && !p$exception) .ra_fail("Pitcher must face the required batter")
  d$pitcher_banned[[a]][[d$pitcher[[a]]+1L]]<-TRUE;d$pitcher[[a]]<-i;d$pitcher_locked[[a]]<-TRUE;d$visits[[a]]<-0L
 } else if(k=="set_catcher") {
  a<-p$team+1L;i<-p$player
  if(!i %in% unlist(d$lineup[[a]]) || d$status[[a]][[i+1L]]!="active" || i==d$pitcher[[a]]) .ra_fail("Catcher must be an active non-pitcher")
  d$catcher[[a]]<-i
 } else if(k=="mound_visit") {
  a<-p$team+1L;if(p$team!=defense) .ra_fail("Only the fielding team can request a defensive conference")
  d$visits[[a]]<-d$visits[[a]]+1L;d$conferences[[a]]<-d$conferences[[a]]+1L
  remove<-if(!d$softball) d$visits[[a]]>=2 else if(d$inning<=7) d$conferences[[a]]>3 else d$visits[[a]]>1
  if(remove) {d$pitcher_banned[[a]][[d$pitcher[[a]]+1L]]<-TRUE;d$phase<-"change"}
 } else if(k=="temporary_runner") {
  if(!d$softball || d$outs!=2 || !d$catcher[[tt]] %in% unlist(d$bases)) .ra_fail("Temporary runner is available only for a catcher on base with two outs")
  for(offset in 1:9) {i<-d$lineup[[tt]][[(d$next_slot[[tt]]-offset)%%9L+1L]];if(!i %in% unlist(d$bases)) break}
  j<-which(vapply(d$bases,function(x) !is.null(x) && x==d$catcher[[tt]],logical(1)))[1];d$temporary[[j]]<-d$catcher[[tt]];d$bases[[j]]<-i
 } else if(k=="eject") {
  a<-p$team+1L;i<-p$player
  if(d$status[[a]][[i+1L]]=="ejected") .ra_fail("Player is already ejected")
  d$status[[a]][[i+1L]]<-"ejected"
  if(i %in% unlist(d$lineup[[a]]) && d$phase=="play") {d$phase<-"change"}
 } else if(k=="resume_play") {
  if(!.ra_diamond_roster(d) || any(vapply(1:2,function(a) d$pitcher_banned[[a]][[d$pitcher[[a]]+1L]],logical(1)))) .ra_fail("Complete the required lineup and pitching changes")
  d$phase<-"play"
 } else if(k=="forfeit") {
  w<-1L-p$team;d$score<-list(0L,0L);d$score[[w+1L]]<-d$regulation_innings;d$half_runs<-0L;d<-.ra_diamond_clear(d);d<-.ra_diamond_finish(d,w,"forfeit")
 } else if(k=="suspend") {d$saved_phase<-d$phase;d$phase<-"suspended"} else if(k=="resume") d$phase<-d$saved_phase else if(k=="official_result") d<-.ra_diamond_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official_result")
 d
}
.ra_diamond_legal<-function(state) {
 d<-state$data;s<-.ra_diamond_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Inning, batting order, runner destinations and eligibility must permit the event."),external_observations=list("Official completed plays, run timing, appeals, substitutions and pitching exceptions."))),constraints=list(phase=d$phase,inning=d$inning,batting=d$batting,outs=d$outs,bases=d$bases,batter=.ra_diamond_due(d),score=d$score,next_slot=d$next_slot,pitcher_locked=d$pitcher_locked),completeness=FALSE,reason="official_completed_diamond_plays")
}
.ra_diamond_outcome<-function(state,pack) .ra_pelota_outcome(state,pack)
