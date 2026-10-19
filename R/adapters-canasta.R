.ra_canasta_schemas<-function() {
 i<-function(a,b) list(type="integer",minimum=a,maximum=b);a<-function(x,n,m) list(type="array",minItems=n,maxItems=m,items=x);card<-i(0,107);g<-list(type="object",properties=list(rank=i(1,13),cards=a(card,1,11)),required=list("rank","cards"));e<-setNames(list(),character())
 list(configure_scores=list(scores=a(i(-100000,100000),2,2)),deal=list(deck=a(card,108,108)),draw_stock=e,take_discard=list(groups=a(g,1,12)),meld=list(groups=a(g,1,12)),discard=list(card=card),ask_to_go_out=e,answer_permission=list(player=i(0,3),allow=list(type="boolean")),end_stock=e,next_round=e)
}
.ra_canasta_rank<-function(c) if(c%%54<52) c%%54%%13+1L else 0L
.ra_canasta_wild<-function(c) .ra_canasta_rank(c) %in% c(0L,2L)
.ra_canasta_red<-function(c) .ra_canasta_rank(c)==3 && floor((c%%54)/13) %in% c(1L,2L)
.ra_canasta_value<-function(c) {r<-.ra_canasta_rank(c);if(r==0) 50L else if(.ra_canasta_red(c)) 100L else if(r %in% c(1L,2L)) 20L else if(r>=8) 10L else 5L}
.ra_canasta_valid_meld<-function(m) {
 cards<-m$cards;r<-m$rank;natural<-Filter(function(c) !.ra_canasta_wild(c),cards)
 if(length(cards)<3 || anyDuplicated(unlist(cards))) return(FALSE)
 if(r==3) return(length(cards)<=4 && all(vapply(cards,function(c) .ra_canasta_rank(c)==3 && !.ra_canasta_red(c),logical(1))))
 r %in% c(1L,4:13) && length(natural)>=2 && all(vapply(natural,function(c) .ra_canasta_rank(c)==r,logical(1))) && sum(vapply(cards,.ra_canasta_wild,logical(1)))<=3
}
.ra_canasta_complete<-function(m) m$rank!=3 && length(m$cards)>=7
.ra_canasta_frozen<-function(d,team) length(d$melds[[team+1L]])==0 || any(vapply(d$discard,function(c) .ra_canasta_wild(c) || .ra_canasta_rank(c)==3,logical(1)))
.ra_canasta_threshold<-function(d,team) {score<-d$round_start_scores[[team+1L]];if(score<0) 15L else if(score<1500) 50L else if(score<3000) 90L else 120L}
.ra_canasta_turn_complete<-function(melds,cards) any(vapply(melds,function(m) {used<-Filter(function(c) c %in% unlist(cards),m$cards);m$rank!=3 && length(used)>=7 && sum(vapply(used,function(c) !.ra_canasta_wild(c),logical(1)))>=4},logical(1)))
.ra_canasta_round_points<-function(d) {
 lapply(0:1,function(team) {
  melds<-d$melds[[team+1L]];reds<-length(d$red_threes[[team+1L]]);bonus<-(if(reds==4) 800L else reds*100L)*(if(length(melds)>0) 1L else -1L)
  bonus<-bonus+sum(vapply(Filter(.ra_canasta_complete,melds),function(m) if(any(vapply(m$cards,.ra_canasta_wild,logical(1)))) 300L else 500L,integer(1)))
  if(!is.null(d$out_team) && d$out_team==team) bonus<-bonus+100L+100L*as.integer(d$concealed)
  bonus+sum(vapply(melds,function(m) sum(vapply(m$cards,.ra_canasta_value,integer(1))),numeric(1)))-sum(vapply(Filter(function(i) i%%2==team,0:3),function(i) sum(vapply(d$hands[[i+1L]],.ra_canasta_value,integer(1))),numeric(1)))
 })
}
.ra_canasta_end<-function(d,reason,out=NULL) {
 d$end_reason<-reason;d["out_team"]<-list(out);d$concealed<-!is.null(out) && d$concealed_candidate && .ra_canasta_turn_complete(d$melds[[d$turn%%2+1L]],d$turn_cards_melded);d$permission_pending<-FALSE
 d$last_round_scores<-.ra_canasta_round_points(d);d$scores<-lapply(1:2,function(i) d$round_start_scores[[i]]+d$last_round_scores[[i]]);d$phase<-if(max(unlist(d$scores))>=5000 && d$scores[[1]]!=d$scores[[2]]) "finished" else "round_end";d
}
.ra_canasta_out<-function(d) {
 team<-d$turn%%2
 if(!any(vapply(d$melds[[team+1L]],.ra_canasta_complete,logical(1))) || identical(d$permission,FALSE)) .ra_fail("Going out requires a partnership canasta and no binding refusal")
 .ra_canasta_end(d,"go_out",team)
}
.ra_canasta_draw_reds<-function(d,player) {
 while(length(d$stock)>0) {
  c<-d$stock[[1]];d$stock<-d$stock[-1L]
  if(.ra_canasta_red(c)) {d$red_threes[[player%%2+1L]]<-c(d$red_threes[[player%%2+1L]],list(c));if(length(d$stock)==0) return(list(data=d,drawn=FALSE))} else {d$hands[[player+1L]]<-c(d$hands[[player+1L]],list(c));return(list(data=d,drawn=TRUE))}
 };list(data=d,drawn=FALSE)
}
.ra_canasta_reset<-function(d) {d["draw_source"]<-list(NULL);d$turn_cards_melded<-list();d$concealed_candidate<-!d$has_melded[[d$turn+1L]];d["permission"]<-list(NULL);d$permission_asked<-FALSE;d$permission_pending<-FALSE;d$must_go_out<-FALSE;d}
.ra_canasta_meld<-function(d,groups,future_cards=0L) {
 t<-d$turn;team<-t%%2;cards<-unlist(lapply(groups,function(g) g$cards),use.names=FALSE);ranks<-vapply(groups,function(g) g$rank,numeric(1))
 if(anyDuplicated(cards) || anyDuplicated(ranks) || !all(cards %in% unlist(d$hands[[t+1L]]))) .ra_fail("Meld additions must use distinct held cards and distinct ranks")
 proposed<-d$melds[[team+1L]];opening<-length(proposed)==0
 for(g in groups) {
  idx<-which(vapply(proposed,function(m) m$rank==g$rank,logical(1)))
  if(length(idx)==0) {idx<-length(proposed)+1L;proposed[[idx]]<-list(rank=g$rank,cards=list())}
  proposed[[idx]]$cards<-c(proposed[[idx]]$cards,g$cards)
  if(!.ra_canasta_valid_meld(proposed[[idx]])) .ra_fail("Meld requires matching naturals, at most three wild cards, and valid black-three use")
 }
 remaining<-length(d$hands[[t+1L]])-length(cards)+future_cards;has_canasta<-any(vapply(proposed,.ra_canasta_complete,logical(1)))
 if(remaining==0 && (!has_canasta || identical(d$permission,FALSE))) .ra_fail("Cannot empty the hand without a permitted going-out condition")
 black<-any(ranks==3)
 if(black && (remaining>1 || !has_canasta || identical(d$permission,FALSE))) .ra_fail("Black threes may be melded only while going out")
 contributed<-c(d$turn_cards_melded,as.list(cards));concealed_canasta<-.ra_canasta_turn_complete(proposed,contributed)
 waive<-remaining<=1 && d$concealed_candidate && concealed_canasta && identical(d$draw_source,"stock") && !identical(d$permission,FALSE)
 amount<-sum(vapply(as.list(cards),.ra_canasta_value,integer(1)))
 if(opening && amount<.ra_canasta_threshold(d,team) && !waive) .ra_fail("Initial meld does not meet the partnership minimum")
 d$melds[[team+1L]]<-proposed;d$hands[[t+1L]]<-Filter(function(c) !(c %in% cards),d$hands[[t+1L]]);d$turn_cards_melded<-contributed;d$has_melded[[t+1L]]<-TRUE
 if(black || opening && amount<.ra_canasta_threshold(d,team)) d$must_go_out<-TRUE;d
}
.ra_canasta_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE");phase<-d$phase
 if(any(unlist(c(d$scores,d$round_start_scores))%%5!=0)) bad("Canasta cumulative scores use five-point increments")
 if(phase=="setup") {if(any(vapply(d$hands,length,integer(1))>0) || length(d$stock)>0 || length(d$discard)>0 || any(vapply(d$melds,length,integer(1))>0) || any(vapply(d$red_threes,length,integer(1))>0)) bad("Setup must have no dealt cards");return(invisible(TRUE))}
 cards<-unlist(c(d$hands,list(d$stock,d$discard),d$red_threes,lapply(d$melds,function(team) lapply(team,function(m) m$cards))),use.names=FALSE)
 if(length(cards)!=108 || any(sort(cards)!=0:107)) bad("Every physical card must occur exactly once")
 if(any(vapply(as.list(unlist(d$hands)),.ra_canasta_red,logical(1))) || any(vapply(as.list(unlist(d$red_threes)),function(c) !.ra_canasta_red(c),logical(1)))) bad("Red threes must be exposed outside hands")
 for(team in d$melds) if(anyDuplicated(vapply(team,function(m) m$rank,numeric(1))) || any(vapply(team,function(m) !.ra_canasta_valid_meld(m),logical(1)))) bad("Partnership melds require unique valid ranks")
 if(phase %in% c("round_end","finished")) {
  if(is.null(d$end_reason) || any(unlist(d$last_round_scores)!=unlist(.ra_canasta_round_points(d))) || any(unlist(d$scores)!=unlist(d$round_start_scores)+unlist(d$last_round_scores))) bad("Completed hand requires its exact score ledger")
  if(d$end_reason=="go_out" && (length(d$hands[[d$turn+1L]])>0 || is.null(d$out_team) || d$out_team!=d$turn%%2 || !any(vapply(d$melds[[d$out_team+1L]],.ra_canasta_complete,logical(1))))) bad("Going-out result requires an empty hand and a team canasta")
  if((phase=="finished")!=(max(unlist(d$scores))>=5000 && d$scores[[1]]!=d$scores[[2]])) bad("Game ends only after a completed hand reaches five thousand with unequal totals")
 } else {
  if(any(unlist(d$scores)!=unlist(d$round_start_scores)) || !is.null(d$end_reason) || !is.null(d$out_team) || d$concealed) bad("Open hand cannot retain a result")
  if(any(vapply(d$hands,length,integer(1))==0)) bad("An emptied hand must end the round")
 }
 if(phase=="draw" && (!is.null(d$draw_source) || length(d$turn_cards_melded)>0 || d$must_go_out)) bad("New turn cannot retain melding activity")
 if(phase=="meld" && is.null(d$draw_source)) bad("Melding follows a draw or legal discard pickup")
 if(d$permission_pending && (!d$permission_asked || !is.null(d$permission))) bad("Pending permission must await its partner answer")
 if(!is.null(d$permission) && !d$permission_asked) bad("Binding permission requires a recorded request")
 if(d$must_go_out && length(d$hands[[d$turn+1L]])>1) bad("Special final meld must leave at most the final discard");invisible(TRUE)
}
.ra_canasta_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;phase<-d$phase;t<-d$turn;team<-t%%2;s<-.ra_canasta_schemas()
 if(!(k %in% names(s))) .ra_fail("Unknown Canasta action")
 .ra_fields(p,s[[k]],"/params")
 if(phase=="finished") .ra_fail("Game is finished")
 actor<-action$actor;expected<-if(k=="answer_permission") (t+2L)%%4 else t
 if(!is.null(actor) && !(k %in% c("deal","configure_scores","next_round")) && actor!=state$players[[expected+1L]]$id) .ra_fail("Wrong actor for this Canasta decision")
 if(d$permission_pending && k!="answer_permission") .ra_fail("The partner must answer the pending going-out request")
 if(k=="configure_scores") {
  if(phase!="setup" || d$round!=1 || any(unlist(p$scores)%%5!=0) || max(unlist(p$scores))>=5000 && p$scores[[1]]!=p$scores[[2]]) .ra_fail("Configure initial five-point cumulative totals before the first deal")
  d$scores<-p$scores;d$round_start_scores<-p$scores
 } else if(k=="deal") {
  if(phase!="setup" || length(unique(unlist(p$deck)))!=108) .ra_fail("Supply the complete double deck once")
  deck<-p$deck
  for(n in 0:43) {i<-(d$dealer+1L+n)%%4+1L;d$hands[[i]]<-c(d$hands[[i]],list(deck[[n+1L]]))};d$stock<-deck[45:108]
  while(length(d$stock)>0) {c<-d$stock[[1]];d$stock<-d$stock[-1L];d$discard<-c(d$discard,list(c));if(!(.ra_canasta_rank(c) %in% c(0L,2L,3L))) break}
  for(step in 0:3) {
   i<-(d$dealer+1L+step)%%4;reds<-Filter(.ra_canasta_red,d$hands[[i+1L]]);d$hands[[i+1L]]<-Filter(function(c) !.ra_canasta_red(c),d$hands[[i+1L]]);d$red_threes[[i%%2+1L]]<-c(d$red_threes[[i%%2+1L]],reds)
   for(j in seq_along(reds)) {draw<-.ra_canasta_draw_reds(d,i);d<-draw$data;if(!draw$drawn) return(.ra_canasta_end(d,"red_three_exhaustion"))}
  };d$phase<-"draw";d<-.ra_canasta_reset(d)
 } else if(k=="next_round") {
  if(phase!="round_end") .ra_fail("Complete the current hand before dealing again")
  d$phase<-"setup";d$round<-d$round+1L;d$turn<-(d$dealer+2L)%%4;d$dealer<-(d$dealer+1L)%%4;d$hands<-rep(list(list()),4);d$stock<-list();d$discard<-list();d$melds<-rep(list(list()),2);d$red_threes<-rep(list(list()),2);d$has_melded<-as.list(rep(FALSE,4));d$round_start_scores<-d$scores;d$last_round_scores<-list(0L,0L);d["out_team"]<-list(NULL);d$concealed<-FALSE;d["end_reason"]<-list(NULL);d<-.ra_canasta_reset(d)
 } else if(k=="ask_to_go_out") {
  if(!(phase %in% c("draw","meld")) || d$permission_asked || length(d$turn_cards_melded)>0 || identical(d$draw_source,"discard")) .ra_fail("Ask only before melding or indicating a discard-pile pickup")
  d$permission_asked<-TRUE;d$permission_pending<-TRUE
 } else if(k=="answer_permission") {
  if(!d$permission_pending || p$player!=(t+2L)%%4) .ra_fail("Only the current player partner may answer")
  d$permission<-p$allow;d$permission_pending<-FALSE
 } else if(k=="draw_stock") {
  if(phase!="draw" || length(d$stock)==0) .ra_fail("Draw one available stock card before melding")
  d$draw_source<-"stock";draw<-.ra_canasta_draw_reds(d,t);d<-draw$data
  if(!draw$drawn) d<-.ra_canasta_end(d,"red_three_exhaustion") else d$phase<-"meld"
 } else if(k=="take_discard") {
  if(phase!="draw" || length(d$discard)==0 || length(d$hands[[t+1L]])==1 && length(d$discard)==1) .ra_fail("Discard pickup is unavailable")
  top<-d$discard[[length(d$discard)]];r<-.ra_canasta_rank(top);groups<-p$groups;used<-unlist(lapply(groups,function(g) g$cards))
  if(.ra_canasta_wild(top) || r==3 || !(top %in% used)) .ra_fail("The natural top discard must be used immediately in a meld")
  if(.ra_canasta_frozen(d,team) && sum(vapply(d$hands[[t+1L]],function(c) .ra_canasta_rank(c)==r && c %in% used,logical(1)))<2) .ra_fail("A frozen pile requires two matching natural cards from the hand")
  rest<-d$discard[-length(d$discard)];d$discard<-list();d$hands[[t+1L]]<-c(d$hands[[t+1L]],list(top));d$draw_source<-"discard"
  d<-.ra_canasta_meld(d,groups,sum(vapply(rest,function(c) !.ra_canasta_red(c),logical(1))))
  for(c in rest) if(.ra_canasta_red(c)) d$red_threes[[team+1L]]<-c(d$red_threes[[team+1L]],list(c)) else d$hands[[t+1L]]<-c(d$hands[[t+1L]],list(c))
  d$phase<-"meld";if(length(d$hands[[t+1L]])==0) d<-.ra_canasta_out(d)
 } else if(k=="meld") {
  if(phase!="meld" || d$must_go_out) .ra_fail("Draw first; a special final meld must be followed by its discard")
  d<-.ra_canasta_meld(d,p$groups);if(length(d$hands[[t+1L]])==0) d<-.ra_canasta_out(d)
 } else if(k=="discard") {
  if(phase!="meld" || !(p$card %in% unlist(d$hands[[t+1L]]))) .ra_fail("Discard one held card after drawing")
  if(identical(d$permission,TRUE) && length(d$hands[[t+1L]])!=1) .ra_fail("A positive partner answer binds this turn to going out")
  d$hands[[t+1L]]<-Filter(function(c) c!=p$card,d$hands[[t+1L]]);d$discard<-c(d$discard,list(p$card))
  if(length(d$hands[[t+1L]])==0) d<-.ra_canasta_out(d) else {d$turn<-(t+1L)%%4;d$phase<-"draw";d<-.ra_canasta_reset(d)}
 } else if(k=="end_stock") {
  if(phase!="draw" || length(d$stock)>0) .ra_fail("Voluntary stock exhaustion is available only when no stock remains")
  top<-if(length(d$discard)>0) d$discard[[length(d$discard)]] else NULL
  mandatory<-!is.null(top) && !.ra_canasta_wild(top) && .ra_canasta_rank(top)!=3 && !.ra_canasta_frozen(d,team) && any(vapply(d$melds[[team+1L]],function(m) m$rank==.ra_canasta_rank(top),logical(1))) && !(length(d$hands[[t+1L]])==1 && length(d$discard)==1)
  if(mandatory) .ra_fail("An unfrozen matching discard must be taken when the stock is empty")
  d<-.ra_canasta_end(d,"stock_empty")
 };d
}
.ra_canasta_legal<-function(state) {
 d<-state$data;phase<-d$phase;t<-d$turn;names<-character()
 if(d$permission_pending) names<-"answer_permission" else if(phase=="setup") names<-c("deal",if(d$round==1) "configure_scores") else if(phase=="round_end") names<-"next_round" else if(phase=="draw") names<-c(if(length(d$stock)>0) "draw_stock" else "end_stock",if(length(d$discard)>0) "take_discard",if(!d$permission_asked) "ask_to_go_out") else if(phase=="meld") names<-c("discard",if(!d$must_go_out) "meld",if(!d$permission_asked && length(d$turn_cards_melded)==0 && !identical(d$draw_source,"discard")) "ask_to_go_out")
 s<-.ra_canasta_schemas()
 list(mode="event_types",actions=list(),action_types=lapply(names,function(k) list(type=k,params_schema=s[[k]],preconditions=list("Turn, held-card membership, meld composition, opening minimum, frozen pile and partner permission must permit the action."),external_observations=list("External shuffled double deck; partner answer is an explicit player decision."))),constraints=list(phase=phase,turn=t,partnership=t%%2,initial_minimum=.ra_canasta_threshold(d,t%%2),discard_frozen=.ra_canasta_frozen(d,t%%2),stock_count=length(d$stock),hand_counts=lapply(d$hands,length),canastas=lapply(d$melds,function(row) sum(vapply(row,.ra_canasta_complete,logical(1)))),permission_pending=d$permission_pending),completeness=FALSE,reason="bounded_parameterized_canasta_actions")
}
.ra_canasta_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-if(d$scores[[1]]>d$scores[[2]]) 0L else 1L
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) lapply(Filter(function(i) i%%2==w,0:3),function(i) state$players[[i+1L]]$id) else list(),losers=if(done) lapply(Filter(function(i) i%%2!=w,0:3),function(i) state$players[[i+1L]]$id) else list(),placements=if(done) lapply(0:1,function(i) list(partnership=i,place=if(i==w) 1L else 2L,score=d$scores[[i+1L]])) else list(),reason=if(done) "five_thousand_after_completed_hand" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
