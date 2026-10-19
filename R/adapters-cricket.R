.ra_cricket_batting<-function(d) if(d$innings==1) d$first_batting else 1L-d$first_batting
.ra_cricket_end<-function(d) {
 t<-.ra_cricket_batting(d);d["bowler"]<-list(NULL);d["striker"]<-list(NULL);d["non_striker"]<-list(NULL);d$free_hit<-FALSE
 if(d$innings==1) {d$phase<-"interval";d$target<-d$runs[[t+1L]]+1L} else {
  d$phase<-"finished";d$reason<-if(d$runs[[t+1L]]>=d$target) "chase" else if(d$runs[[t+1L]]==d$target-1L) "tie" else "defended"
  d["winner"]<-list(if(d$reason=="chase") t else if(d$reason=="tie") NULL else 1L-t)
 };d
}
.ra_cricket_check_end<-function(d) {
 t<-.ra_cricket_batting(d)
 if(d$innings==2 && d$runs[[t+1L]]>=d$target || d$wickets[[t+1L]]>=10 || d$balls[[t+1L]]>=120) d<-.ra_cricket_end(d)
 d
}
.ra_cricket_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 0:1) {
  if(sum(unlist(d$bowled[[2L-t]]))!=d$balls[[t+1L]]) bad("Bowler legal-ball totals must equal the opposing innings ball count")
  if(sum(unlist(d$batters[[t+1L]])=="out")!=d$wickets[[t+1L]]) bad("Wickets must agree with dismissed batters")
 }
 if(anyDuplicated(unlist(d$over_bowlers))>0 || anyDuplicated(unlist(d$previous_bowlers))>0) bad("Over bowler lists cannot repeat a player")
 t<-.ra_cricket_batting(d);f<-1L-t
 if(d$phase=="play") {
  positions<-c(d$striker,d$non_striker)
  if(anyDuplicated(positions)>0) bad("The two batting ends require different batters")
  if(any(vapply(positions,function(i) d$batters[[t+1L]][[i+1L]]!="in" || !d$eligible[[t+1L]][[i+1L]],logical(1)))) bad("Assigned batters must be eligible and at the crease")
  if(!identical(as.integer(which(unlist(d$batters[[t+1L]])=="in")-1L),as.integer(sort(positions)))) bad("Crease assignments and batting statuses must agree")
  if(!is.null(d$bowler) && (!d$eligible[[f+1L]][[d$bowler+1L]] || d$bowler %in% unlist(d$previous_bowlers) || !d$bowler %in% unlist(d$over_bowlers) || d$bowled[[f+1L]][[d$bowler+1L]]>=24)) bad("Current bowler violates eligibility, consecutive-over or quota restrictions")
  if(d$wickets[[t+1L]]>=10 || d$balls[[t+1L]]>=120 || d$innings==2 && d$runs[[t+1L]]>=d$target) bad("Completed innings cannot remain active")
 }
 if(d$innings==1 && d$phase %in% c("pre","play") && !is.null(d$target) || (d$innings==2 || d$phase=="interval") && (is.null(d$target) || d$target!=d$runs[[d$first_batting+1L]]+1L)) bad("Chase target must be one above the first innings total")
 if(d$phase=="interval" && d$innings!=1) bad("Only the first innings is followed by the innings interval")
 if(d$phase=="interval" || d$phase=="finished" && !is.null(d$reason) && d$reason %in% c("tie","defended")) {
  available<-sum(vapply(1:11,function(j) d$eligible[[t+1L]][[j]] && d$batters[[t+1L]][[j]]!="out",logical(1)))
  if(d$balls[[t+1L]]<120 && d$wickets[[t+1L]]<10 && available>=2) bad("An innings cannot end before its ball limit, dismissal limit or lack of eligible batters")
 }
 if(d$phase=="pre" && (d$innings!=1 || any(unlist(d$runs)!=0) || any(unlist(d$balls)!=0) || any(unlist(d$wickets)!=0))) bad("Pre-match state precedes scoring and dismissals")
 if(d$phase=="finished") {
  if(!is.null(d$reason) && d$reason %in% c("chase","tie","defended")) {
   if(d$innings!=2) bad("A regulation result needs two innings")
   expected<-if(d$runs[[t+1L]]>=d$target) t else if(d$runs[[t+1L]]==d$target-1L) NULL else 1L-t
   if(!identical(d$winner,expected)) bad("Final winner disagrees with the chase score")
   expected_reason<-if(d$runs[[t+1L]]>=d$target) "chase" else if(is.null(expected)) "tie" else "defended"
   if(d$reason!=expected_reason) bad("Result reason disagrees with the chase score")
  } else if(identical(d$reason,"forfeit")) {if(is.null(d$winner)) bad("Forfeit needs a winner")}
  else if(is.null(d$reason) || !d$reason %in% c("official_result","no_result")) bad("Finished match needs a reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only a finished match has a declared result")
}
.ra_cricket_schemas<-function(d) {
 ph<-d$phase;s<-list();empty<-setNames(list(),character());T<-list(type="integer",minimum=0,maximum=1);P<-list(type="integer",minimum=0,maximum=10);NP<-c(P,list(nullable=TRUE));B<-list(type="boolean");E<-function(...) list(type="enum",values=list(...))
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 if(ph=="pre") s$start_match<-list(first_batting=T)
 if(ph=="interval") s$start_second_innings<-empty
 if(ph=="play") {
  s<-c(s,list(nominate_batter=list(player=P,end=E("striker","non_striker")),nominate_bowler=list(player=P),replace_bowler=empty,retire=list(player=P,may_return=B),dismiss_non_delivery=list(player=P,kind=E("timed_out","obstructing","retired_out")),penalty_runs=list(team=T,runs=list(type="integer",minimum=5,maximum=5)),end_unavailable=empty))
  if(!is.null(d$bowler) && !is.null(d$striker) && !is.null(d$non_striker)) s$delivery<-list(kind=E("legal","wide","no_ball","dead"),bat_runs=list(type="integer",minimum=0,maximum=12),extra_runs=list(type="integer",minimum=0,maximum=12),extra_kind=E("none","byes","leg_byes"),completed_runs=list(type="integer",minimum=0,maximum=12),boundary=list(type="integer",minimum=0,maximum=6),dismissal=E("none","bowled","caught","lbw","stumped","hit_wicket","run_out","obstructing","hit_ball_twice"),out_player=NP)
 }
 c(s,list(exclude=list(team=T,player=P),abandon=empty,forfeit=list(team=T)))
}
.ra_cricket_dismiss<-function(d,i) {
 t<-.ra_cricket_batting(d)
 if(d$batters[[t+1L]][[i+1L]]=="out") .ra_fail("Batter has already been dismissed")
 d$batters[[t+1L]][[i+1L]]<-"out";d$wickets[[t+1L]]<-d$wickets[[t+1L]]+1L
 if(!is.null(d$striker) && d$striker==i) d["striker"]<-list(NULL)
 if(!is.null(d$non_striker) && d$non_striker==i) d["non_striker"]<-list(NULL)
 d
}
.ra_cricket_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_cricket_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-.ra_cricket_batting(d);f<-1L-t;i<-p$player
 if(k=="start_match") {d$first_batting<-p$first_batting;d$phase<-"play"
 } else if(k=="start_second_innings") {
  d$innings<-2L;d$phase<-"play";d["striker"]<-list(NULL);d["non_striker"]<-list(NULL);d["bowler"]<-list(NULL);d$over_bowlers<-list();d$previous_bowlers<-list();d$free_hit<-FALSE;d<-.ra_cricket_check_end(d)
 } else if(k=="nominate_batter") {
  end<-p$end
  if(!is.null(d[[end]]) || !d$eligible[[t+1L]][[i+1L]] || !d$batters[[t+1L]][[i+1L]] %in% c("unused","retired")) .ra_fail("Choose an eligible unused or returning batter for an empty end")
  d[[end]]<-i;d$batters[[t+1L]][[i+1L]]<-"in"
 } else if(k=="nominate_bowler") {
  if(!is.null(d$bowler) || !d$eligible[[f+1L]][[i+1L]] || d$bowled[[f+1L]][[i+1L]]>=24 || i %in% unlist(d$previous_bowlers) || i %in% unlist(d$over_bowlers)) .ra_fail("Bowler must be eligible, below quota and absent from the preceding and current over")
  d$bowler<-i;d$over_bowlers[[length(d$over_bowlers)+1L]]<-i
 } else if(k=="replace_bowler") {
  if(is.null(d$bowler)) .ra_fail("There is no current bowler to replace")
  d["bowler"]<-list(NULL)
 } else if(k=="retire") {
  if(d$batters[[t+1L]][[i+1L]]!="in") .ra_fail("Only a current batter may retire")
  if(p$may_return) {d$batters[[t+1L]][[i+1L]]<-"retired";if(!is.null(d$striker) && d$striker==i) d["striker"]<-list(NULL) else d["non_striker"]<-list(NULL)} else d<-.ra_cricket_dismiss(d,i)
  d<-.ra_cricket_check_end(d)
 } else if(k=="dismiss_non_delivery") {
  if(p$kind=="timed_out") {if(!is.null(d$striker) && !is.null(d$non_striker) || !d$batters[[t+1L]][[i+1L]] %in% c("unused","retired")) .ra_fail("Timed-out dismissal requires a due incoming batter and a vacant end")}
  else if(!d$batters[[t+1L]][[i+1L]] %in% c("in","retired")) .ra_fail("This dismissal requires a current or retired batter")
  d<-.ra_cricket_dismiss(d,i);d<-.ra_cricket_check_end(d)
 } else if(k=="exclude") {
  team<-p$team
  if(!d$eligible[[team+1L]][[i+1L]]) .ra_fail("Player is already excluded")
  d$eligible[[team+1L]][[i+1L]]<-FALSE
  if(team==t && d$batters[[team+1L]][[i+1L]]=="in") {
   d$batters[[team+1L]][[i+1L]]<-"retired"
   if(!is.null(d$striker) && d$striker==i) d["striker"]<-list(NULL)
   if(!is.null(d$non_striker) && d$non_striker==i) d["non_striker"]<-list(NULL)
  }
  if(team==f && !is.null(d$bowler) && d$bowler==i) d["bowler"]<-list(NULL)
 } else if(k=="delivery") {
  kind<-p$kind;bat<-p$bat_runs;extra<-p$extra_runs;running<-p$completed_runs;boundary<-p$boundary;w<-p$dismissal;out<-p$out_player;striker<-d$striker;other<-d$non_striker
  if(!boundary %in% c(0,4,6)) .ra_fail("A boundary is zero, four or six")
  if((w=="none")!=is.null(out)) .ra_fail("Dismissal kind and dismissed player must agree")
  if(kind=="dead") {
   if(bat!=0 || extra!=0 || running!=0 || boundary!=0 || w!="none" || p$extra_kind!="none") .ra_fail("This dead-delivery event carries no runs, dismissal or legal ball")
   return(d)
  }
  if(bat!=0 && extra!=0 || (extra!=0)!=(p$extra_kind!="none") && kind!="wide") .ra_fail("Bat runs and byes/leg-byes are mutually exclusive and extras must be classified")
  if(kind=="wide") {if(bat!=0 || p$extra_kind!="none" || boundary==6) .ra_fail("A wide cannot carry bat runs, byes or a six")}
  else if(extra==0 && p$extra_kind!="none") .ra_fail("Zero byes cannot carry an extras classification")
  if(boundary==6 && (extra!=0 || kind=="wide" || bat!=6 || running!=0)) .ra_fail("Six requires exactly six bat runs and no running component")
  if(boundary==4 && bat+extra!=running+4) .ra_fail("Boundary-four totals must equal completed runs plus four")
  if(boundary==0 && bat+extra!=running) .ra_fail("Non-boundary scoring must equal the completed running component")
  if(w!="none") {
   if(!out %in% c(striker,other)) .ra_fail("A delivery dismissal must concern a current batter")
   if(!w %in% c("run_out","obstructing") && out!=striker) .ra_fail("This dismissal can only remove the striker")
   if((kind=="no_ball" || d$free_hit) && !w %in% c("run_out","obstructing","hit_ball_twice")) .ra_fail("This dismissal is unavailable on a no-ball or free hit")
   if(kind=="wide" && !w %in% c("stumped","hit_wicket","run_out","obstructing")) .ra_fail("This dismissal is unavailable on a wide")
   if(w %in% c("bowled","caught","lbw","stumped","hit_wicket") && (bat!=0 || extra!=0 || running!=0 || boundary!=0)) .ra_fail("This wicket cannot retain bat or running runs")
  }
  bonus<-as.integer(kind %in% c("wide","no_ball"));total<-bat+extra+bonus
  if(d$innings==2 && boundary==0 && d$runs[[t+1L]]+total>=d$target) {total<-d$target-d$runs[[t+1L]];w<-"none";out<-NULL}
  d$runs[[t+1L]]<-d$runs[[t+1L]]+total
  if(running%%2L==1) {tmp<-d$striker;d["striker"]<-list(d$non_striker);d["non_striker"]<-list(tmp)}
  if(w!="none") d<-.ra_cricket_dismiss(d,out)
  if(kind=="legal") {
   d$balls[[t+1L]]<-d$balls[[t+1L]]+1L;d$bowled[[f+1L]][[d$bowler+1L]]<-d$bowled[[f+1L]][[d$bowler+1L]]+1L
   if(d$balls[[t+1L]]%%6L==0) {
    tmp<-d$striker;d["striker"]<-list(d$non_striker);d["non_striker"]<-list(tmp);d$previous_bowlers<-d$over_bowlers;d$over_bowlers<-list();d["bowler"]<-list(NULL)
   } else if(d$bowled[[f+1L]][[d$bowler+1L]]>=24) d["bowler"]<-list(NULL)
  }
  d$free_hit<-kind=="no_ball" || d$free_hit && kind=="wide";d<-.ra_cricket_check_end(d)
 } else if(k=="penalty_runs") {
  d$runs[[p$team+1L]]<-d$runs[[p$team+1L]]+5L
  if(d$innings==2) d$target<-d$runs[[d$first_batting+1L]]+1L
  d<-.ra_cricket_check_end(d)
 } else if(k=="end_unavailable") {
  available<-sum(vapply(1:11,function(j) d$eligible[[t+1L]][[j]] && d$batters[[t+1L]][[j]]!="out",logical(1)))
  if(available>=2) .ra_fail("At least two eligible undismissed batters remain")
  d<-.ra_cricket_end(d)
 } else if(k=="abandon") d$phase<-"abandoned"
 else if(k=="forfeit") {d$phase<-"finished";d$winner<-1L-p$team;d$reason<-"forfeit"}
 else if(k=="official_result") {d$phase<-"finished";d["winner"]<-list(p$winner);d$reason<-if(is.null(p$winner)) "no_result" else "official_result"}
 d
}
.ra_cricket_legal<-function(state) {
 d<-state$data;s<-.ra_cricket_schemas(d);t<-.ra_cricket_batting(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Innings, eligibility, scoring, dismissal and bowling restrictions must permit the event."),external_observations=list("Official completed delivery, boundary, dismissal, penalty or eligibility decision."))),constraints=list(phase=d$phase,innings=d$innings,batting=t,runs=d$runs,wickets=d$wickets,balls=d$balls,target=d$target,striker=d$striker,non_striker=d$non_striker,bowler=d$bowler,free_hit=d$free_hit,bowled=d$bowled),completeness=FALSE,reason="official_cricket_events")
}
.ra_cricket_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else if(d$phase=="abandoned") "awaiting_adjudication" else "ongoing",result_kind=if(done) {if(d$reason=="tie") "draw" else if(is.null(w)) "no_contest" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(!is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else if(d$phase=="abandoned") "official_result_required" else "ongoing",evidence_required=if(d$phase=="abandoned") list("Official result decision.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
