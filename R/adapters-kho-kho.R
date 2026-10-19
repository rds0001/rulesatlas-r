.ra_khokho_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_khokho_e<-function(...) list(type="enum",values=list(...))
.ra_khokho_finish<-function(d,w,r="regulation") {d$phase<-"finished";d["winner"]<-list(w);d$reason<-r;d$break_ms<-0L;d}
.ra_khokho_begin<-function(d) {
 d$phase<-"ready";d$clock_ms<-540000L;d$batch<-0L;d$defenders<-list(0L,1L,2L);d$pending<-list();d$handoffs<-0L;d$needs_kho<-FALSE;d$chaser<-d$slots[[d$chasing+1L]][[9]];d$contact_warned<-rep(list(as.list(rep(FALSE,12))),2)
 if(any(vapply(1:2,function(t) any(unlist(d$red[[t]])[unlist(d$slots[[t]])+1L]),logical(1)))) d$phase<-"replacement";d
}
.ra_khokho_batch<-function(d) {d$batch<-(d$batch+1L)%%3L;d$pending<-as.list(seq.int(d$batch*3L,d$batch*3L+2L));d$handoffs<-0L;d$needs_kho<-TRUE;d}
.ra_khokho_point<-function(d,n=1L) {d$score[[d$chasing+1L]]<-d$score[[d$chasing+1L]]+n;if(d$turn==4 && d$score[[d$chasing+1L]]>d$score[[2L-d$chasing]]) d<-.ra_khokho_finish(d,d$chasing);d}
.ra_khokho_out<-function(d,slot) {d$defenders<-Filter(function(x) x!=slot,d$defenders);d<-.ra_khokho_point(d);if(!length(d$defenders) && !length(d$pending) && d$phase!="finished") d<-.ra_khokho_batch(d);d}
.ra_khokho_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 for(t in 1:2) if(length(unique(unlist(d$slots[[t]])))!=9) bad("Nine playing places require distinct registered players")
 if(anyDuplicated(unlist(d$defenders)) || anyDuplicated(unlist(d$pending)) || length(intersect(unlist(d$defenders),unlist(d$pending)))) bad("Active and pending defending places must be distinct")
 if(any(unlist(c(d$defenders,d$pending)) %/% 3L!=d$batch)) bad("Defenders must belong to the current batch")
 if(d$phase=="ready") {
  if(!d$chaser %in% unlist(d$slots[[d$chasing+1L]]) || any(vapply(1:2,function(t) any(unlist(d$red[[t]])[unlist(d$slots[[t]])+1L]),logical(1)))) bad("Playing teams need an eligible chaser and nine eligible places")
  if(d$clock_ms==0) bad("Expired turn must await closure")
  if(!length(d$defenders) && !length(d$pending)) bad("Live defense requires a current or entering batch")
 }
 if((d$phase=="interval")!=(d$break_ms>0)) bad("Interval timer must agree with phase")
 if(d$phase=="finished") {
  if(d$reason=="regulation") {w<-if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]);if(!isTRUE(all.equal(d$winner,w))) bad("Result must agree with points")}
  else if(d$reason=="innings") {if(is.null(d$winner) || d$winner!=d$first_chaser || d$score[[d$winner+1L]]<=d$score[[2L-d$winner]]) bad("Innings win requires the first chaser to retain the lead")}
 } else if(!is.null(d$winner) || d$reason!="ongoing") bad("Unfinished match cannot have a result")
 invisible(TRUE)
}
.ra_khokho_schemas<-function(d) {
 I<-.ra_khokho_i;E<-.ra_khokho_e;T<-I(0,1);P<-I(0,11);S<-I(0,8);B<-list(type="boolean");ph<-d$phase;s<-setNames(list(),character());empty<-setNames(list(),character())
 if(ph=="finished") return(s)
 if(ph=="abandoned") return(list(official_result=list(winner=c(T,list(nullable=TRUE)))))
 if(ph=="pre") s$start_match<-list(first_chaser=T)
 if(ph %in% c("pre","interval")) s$set_order<-list(team=T,players=list(type="array",minItems=9,maxItems=9,items=P))
 if(ph=="ready") {s$elapse<-list(ms=I(1,540000));s$defender_out<-list(slot=S,kind=E("touch","self_out"));s$defender_contact<-list(slot=S);s$correct_kho<-list(player=P);s$enter_batch<-list(slots=list(type="array",minItems=1,maxItems=3,items=S));s$close_early<-empty;s$official_stop<-empty}
 if(ph=="stopped") s$resume<-empty
 if(ph %in% c("ready","stopped","pre","replacement")) {s$substitute<-list(team=T,slot=S,incoming=P);s$card<-list(team=T,player=P,kind=E("yellow","red"))}
 if(ph=="replacement") s$resume<-empty
 if(ph=="turn_end") s$close_turn<-empty
 if(ph=="follow_choice") s$follow_on<-list(require=B)
 if(ph=="interval") s$elapse_break<-list(ms=I(1,540000))
 s$concede<-list(team=T);s$abandon<-empty;s
}
.ra_khokho_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_khokho_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-d$chasing;other<-1L-t
 if(k=="start_match") {if(any(vapply(1:2,function(j) any(unlist(d$red[[j]])[unlist(d$slots[[j]])+1L]),logical(1)))) .ra_fail("Replace excluded starters before the match");d$first_chaser<-p$first_chaser;d$chasing<-p$first_chaser;d<-.ra_khokho_begin(d)}
 else if(k=="set_order") {if(length(unique(unlist(p$players)))!=9 || !setequal(unlist(p$players),unlist(d$slots[[p$team+1L]]))) .ra_fail("Batch order must permute the nine current playing places");d$slots[[p$team+1L]]<-p$players}
 else if(k=="elapse") {if(p$ms>d$clock_ms) .ra_fail("Elapsed time cannot cross turn expiry");d$clock_ms<-d$clock_ms-p$ms;if(d$clock_ms==0) d$phase<-"turn_end"}
 else if(k %in% c("defender_out","defender_contact")) {
  slot<-p$slot
  if(!slot %in% unlist(d$defenders)) .ra_fail("Only a current defender can be put out or warned")
  if(k=="defender_out" && p$kind=="touch" && d$needs_kho) .ra_fail("A correct kho is required before chasing the next batch")
  i<-d$slots[[other+1L]][[slot+1L]]
  if(k=="defender_contact" && !d$contact_warned[[other+1L]][[i+1L]]) d$contact_warned[[other+1L]][[i+1L]]<-TRUE else d<-.ra_khokho_out(d,slot)
 } else if(k=="correct_kho") {
  i<-p$player
  if(i==d$chaser || !i %in% unlist(d$slots[[t+1L]]) || d$red[[t+1L]][[i+1L]]) .ra_fail("A correct kho passes to another eligible chaser")
  d$chaser<-i;d$needs_kho<-FALSE
  if(length(d$pending)) {d$handoffs<-d$handoffs+1L;if(d$handoffs==2) {d<-.ra_khokho_point(d,length(d$pending));d$pending<-list();if(!length(d$defenders) && d$phase!="finished") d<-.ra_khokho_batch(d)}}
 } else if(k=="enter_batch") {
  slots<-unlist(p$slots)
  if(anyDuplicated(slots) || any(!slots %in% unlist(d$pending))) .ra_fail("Only distinct pending places of the prescribed batch may enter")
  d$defenders<-as.list(sort(c(unlist(d$defenders),slots)));d$pending<-Filter(function(i) !i %in% slots,d$pending)
 } else if(k=="substitute") {
  team<-p$team;slot<-p$slot;i<-p$incoming;old<-d$slots[[team+1L]][[slot+1L]]
  if(i %in% unlist(d$slots[[team+1L]]) || d$red[[team+1L]][[i+1L]]) .ra_fail("Replacement must be an eligible reserve")
  if(d$phase!="pre" && team==other && slot %in% unlist(d$defenders)) .ra_fail("A live defender cannot be substituted before being out")
  d$slots[[team+1L]][[slot+1L]]<-i
  if(d$phase!="pre" && team==t && old==d$chaser) {d$chaser<-i;d$needs_kho<-TRUE}
 } else if(k=="card") {
  team<-p$team;i<-p$player
  if(d$red[[team+1L]][[i+1L]]) .ra_fail("Player is already excluded")
  if(p$kind=="yellow") d$yellow[[team+1L]][[i+1L]]<-TRUE else {
   d$red[[team+1L]][[i+1L]]<-TRUE
   if(i %in% unlist(d$slots[[team+1L]]) && d$phase!="pre") {slot<-match(i,unlist(d$slots[[team+1L]]))-1L;if(team==other && slot %in% unlist(d$defenders)) d<-.ra_khokho_out(d,slot);if(d$phase!="finished") d$phase<-"replacement"}
  }
 } else if(k=="resume") {
  if(any(vapply(1:2,function(j) any(unlist(d$red[[j]])[unlist(d$slots[[j]])+1L]),logical(1)))) .ra_fail("All excluded playing places must be replaced")
  d$phase<-"ready"
 } else if(k=="official_stop") d$phase<-"stopped"
 else if(k=="close_early") d$phase<-"turn_end"
 else if(k=="close_turn") {
  turn<-d$turn
  if(turn==4) d<-.ra_khokho_finish(d,if(d$score[[1]]==d$score[[2]]) NULL else as.integer(d$score[[2]]>d$score[[1]]))
  else if(turn==3 && d$followed_on && d$score[[d$first_chaser+1L]]>d$score[[2L-d$first_chaser]]) d<-.ra_khokho_finish(d,d$first_chaser,"innings")
  else if(turn==2) {lead<-d$score[[d$first_chaser+1L]]-d$score[[2L-d$first_chaser]];d$phase<-"follow_choice";if(lead!=9) {d$followed_on<-lead>9;d$chasing<-if(lead>9) 1L-d$first_chaser else d$first_chaser;d$turn<-3L;d$phase<-"interval";d$break_ms<-540000L}}
  else {d$turn<-turn+1L;d$chasing<-1L-t;d$phase<-"interval";d$break_ms<-300000L}
 } else if(k=="follow_on") {d$followed_on<-p$require;d$chasing<-if(p$require) 1L-d$first_chaser else d$first_chaser;d$turn<-3L;d$phase<-"interval";d$break_ms<-540000L}
 else if(k=="elapse_break") {if(p$ms>d$break_ms) .ra_fail("Elapsed time exceeds the interval");d$break_ms<-d$break_ms-p$ms;if(d$break_ms==0) d<-.ra_khokho_begin(d)}
 else if(k=="concede") d<-.ra_khokho_finish(d,1L-p$team,"concession")
 else if(k=="abandon") {d$phase<-"abandoned";d$break_ms<-0L}
 else if(k=="official_result") d<-.ra_khokho_finish(d,p$winner,if(is.null(p$winner)) "no_contest" else "official")
 d
}
.ra_khokho_legal<-function(state) {d<-state$data;s<-.ra_khokho_schemas(d);list(mode="event_types",actions=list(),action_types=unname(lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Current phase, eligibility and prescribed batch order must permit the event."),external_observations=list("Official legal outs, correct kho handovers, sanctions and elapsed time.")))),constraints=d[c("phase","turn","clock_ms","score","chasing","batch","defenders","pending","chaser","needs_kho","followed_on")],completeness=FALSE,reason="traditional_kho_kho_league_protocol")}
