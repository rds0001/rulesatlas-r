.ra_66_points <- c(0,2,3,4,10,11)
.ra_66_scores <- function(d) vapply(1:2,function(p) sum(.ra_66_points[unlist(d$won[[p]])%%6+1L])+if(length(d$won[[p]])) sum(ifelse(unlist(d$marriages[[p]])==d$trump_suit,40,20)) else 0,numeric(1))
.ra_66_playable <- function(d) {
 h<-unlist(d$hands[[d$turn+1L]])
 if(!length(d$trick) || (length(d$stock)>0 && is.null(d$closer))) return(sort(h))
 lead<-d$trick[[1]];same<-h[h%/%6==lead%/%6];higher<-same[same%%6>lead%%6];trump<-h[h%/%6==d$trump_suit]
 sort(if(length(higher)) higher else if(length(same)) same else if(length(trump)) trump else h)
}
.ra_66_award <- function(d,w,n,reason) {d$phase<-"hand_end";d$hand_winner<-w;d$hand_value<-n;d$reason<-reason;d$match_points[[w+1L]]<-d$match_points[[w+1L]]+n;if(d$match_points[[w+1L]]>=7) d$phase<-"finished";d}
.ra_66_reached <- function(d,p) {
 if(.ra_66_scores(d)[p+1L]<66) return(d)
 if(!is.null(d$closer)) {
  if(p!=d$closer) return(.ra_66_award(d,p,if(!d$close_opponent_tricks) 3L else 2L,"closure_failed"))
  return(.ra_66_award(d,p,if(!d$close_opponent_tricks) 3L else if(d$close_opponent_score<33) 2L else 1L,"closed_sixty_six"))
 }
 .ra_66_award(d,p,if(!length(d$won[[2L-p]])) 3L else if(.ra_66_scores(d)[2L-p]<33) 2L else 1L,"sixty_six")
}
.ra_66_validate <- function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="deal") {
  if(any(lengths(d$hands)>0) || length(d$stock) || any(lengths(d$won)>0) || length(d$trick) || !is.null(d$trump_suit) || any(lengths(d$marriages)>0) || !is.null(d$closer) || !is.null(d$hand_winner) || d$hand_value || !is.null(d$reason) || max(unlist(d$match_points))>=7) bad("A new hand starts with cleared card and result fields")
  return(invisible(TRUE))
 }
 if(!identical(sort(as.integer(c(unlist(d$hands),unlist(d$stock),unlist(d$won),unlist(d$trick)))),0:23)) bad("Each of the twenty-four cards must occur once")
 if(is.null(d$trump_suit) || length(d$stock)%%2 || any(lengths(d$won)%%2!=0)) bad("Trump, stock and captured pairs are inconsistent")
 if(length(d$stock) && tail(unlist(d$stock),1)%/%6!=d$trump_suit) bad("Exposed bottom card must have the trump suit")
 if(d$turn!=(if(length(d$trick)) 1L-d$leader else d$leader)) bad("Current player must follow the trick leader")
 sizes<-vapply(0:1,function(p) length(d$hands[[p+1L]])+as.integer(length(d$trick)>0 && p==d$leader),numeric(1))
 if(sizes[1]!=sizes[2] || (length(d$stock)>0 && is.null(d$closer) && (!(sizes[1] %in% c(5,6)) || (d$phase=="play" && sizes[1]!=6)))) bad("Hand sizes must agree with trick and stock status")
 if(anyDuplicated(unlist(d$marriages))) bad("A suit marriage can be announced only once")
 for(p in 0:1) for(s in unlist(d$marriages[[p+1L]])) if(any((s*6+c(2,3)) %in% c(unlist(d$stock),unlist(d$hands[[2L-p]])))) bad("An announced king and queen cannot be in the stock or opposing hand")
 if(is.null(d$closer)) {if(d$close_opponent_score || d$close_opponent_tricks) bad("Closure snapshots require a closed stock")} else if(!length(d$stock) || d$close_opponent_score>.ra_66_scores(d)[2L-d$closer] || d$close_opponent_tricks>length(d$won[[2L-d$closer]])/2) bad("A closed stock retains its cards and a plausible opponent snapshot")
 if(d$phase=="play") {
  if(!any(lengths(d$hands)>0) || !is.null(d$hand_winner) || d$hand_value || !is.null(d$reason) || max(unlist(d$match_points))>=7 || max(.ra_66_scores(d))>=66) bad("Ongoing hand must have cards and no achieved winning score")
 } else {
  w<-d$hand_winner;n<-d$hand_value;r<-d$reason
  if(is.null(w) || n<1 || d$match_points[[w+1L]]<n || (d$phase=="finished")!=(d$match_points[[w+1L]]>=7) || d$match_points[[2L-w]]>=7 || length(d$trick)) bad("Completed hand and match result are inconsistent")
  if(r=="sixty_six") {
   expected<-if(!length(d$won[[2L-w]])) 3 else if(.ra_66_scores(d)[2L-w]<33) 2 else 1
   if(!is.null(d$closer) || .ra_66_scores(d)[w+1L]<66 || n!=expected) bad("Open-stock hand value must follow the losing score and captured tricks")
  } else if(r=="closed_sixty_six") {
   expected<-if(!d$close_opponent_tricks) 3 else if(d$close_opponent_score<33) 2 else 1
   if(is.null(d$closer) || d$closer!=w || .ra_66_scores(d)[w+1L]<66 || n!=expected) bad("Successful closure uses the opponent snapshot")
  } else if(r=="closure_failed") {
   if(is.null(d$closer) || d$closer!=1L-w || n!=(if(!d$close_opponent_tricks) 3 else 2) || .ra_66_scores(d)[2L-w]>=66 || (any(lengths(d$hands)>0) && .ra_66_scores(d)[w+1L]<66)) bad("Failed closure awards the opponent two or three game points")
  } else if(r=="last_trick") {
   if(!is.null(d$closer) || any(lengths(d$hands)>0) || length(d$stock) || n!=1 || d$leader!=w || max(.ra_66_scores(d))>=66) bad("Without sixty-six, the final trick wins one game point")
  } else bad("Unknown completed-hand reason")
 }
 invisible(TRUE)
}
.ra_66_apply <- function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;t<-d$turn;cs<-list(type="integer",minimum=0,maximum=23)
 if(k=="next_hand") {
  .ra_fields(p,list(),"/params");if(d$phase!="hand_end") .ra_fail("Finish the current hand before dealing the next")
  d$phase<-"deal";d$dealer<-1L-d$dealer;d$hands<-list(list(),list());d$stock<-list();d$won<-list(list(),list());d$trick<-list();d["trump_suit"]<-list(NULL);d$marriages<-list(list(),list());d["closer"]<-list(NULL);d$close_opponent_score<-0L;d$close_opponent_tricks<-0L;d["hand_winner"]<-list(NULL);d$hand_value<-0L;d["reason"]<-list(NULL);d$turn<-1L-d$dealer;d$leader<-d$turn;return(d)
 }
 if(k=="deal") {
  .ra_fields(p,list(cards=list(type="array",minItems=24,maxItems=24,items=cs)),"/params");cards<-p$cards
  if(d$phase!="deal" || anyDuplicated(unlist(cards))) .ra_fail("Supply a permutation of all twenty-four cards for the next hand")
  lead<-1L-d$dealer;d$hands[[lead+1L]]<-cards[seq(1,12,2)];d$hands[[2L-lead]]<-cards[seq(2,12,2)];d$stock<-cards[13:24];d$trump_suit<-cards[[24]]%/%6;d$phase<-"play";d$turn<-lead;d$leader<-lead;return(d)
 }
 if(d$phase!="play") .ra_fail("No card action outside a playing hand")
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 if(k=="exchange_trump") {
  .ra_fields(p,list(),"/params");jack<-d$trump_suit*6+1L
  if(length(d$trick) || !is.null(d$closer) || !length(d$stock) || !length(d$won[[t+1L]]) || !(jack %in% unlist(d$hands[[t+1L]]))) .ra_fail("A leader with a won trick may exchange the trump jack while the stock is open")
  d$hands[[t+1L]]<-c(as.list(setdiff(unlist(d$hands[[t+1L]]),jack)),tail(d$stock,1));d$stock[[length(d$stock)]]<-jack;return(d)
 }
 if(k=="close_stock") {
  .ra_fields(p,list(),"/params");if(length(d$trick) || !is.null(d$closer) || !length(d$stock)) .ra_fail("Only the leader can close a nonempty open stock before leading")
  d$closer<-t;d$close_opponent_score<-.ra_66_scores(d)[2L-t];d$close_opponent_tricks<-length(d$won[[2L-t]])/2;return(d)
 }
 if(k!="play") .ra_fail("Unknown action")
 .ra_fields(p,list(card=cs,marriage=list(type="boolean")),"/params");c<-p$card
 if(!(c %in% .ra_66_playable(d))) .ra_fail("Play a held card, obeying mandatory suit, heading and trumping after stock closure or exhaustion")
 if(p$marriage) {
  s<-c%/%6
  if(length(d$trick) || !(c%%6 %in% c(2,3)) || !((s*6+(5-c%%6)) %in% unlist(d$hands[[t+1L]])) || s %in% unlist(d$marriages)) .ra_fail("Announce a held king-queen pair by leading one of its cards once per suit")
  d$marriages[[t+1L]]<-c(d$marriages[[t+1L]],list(s));d<-.ra_66_reached(d,t);if(d$phase!="play") return(d)
 }
 d$hands[[t+1L]]<-as.list(setdiff(unlist(d$hands[[t+1L]]),c));d$trick<-c(d$trick,list(c))
 if(length(d$trick)==1) {d$turn<-1L-t;return(d)}
 a<-d$trick[[1]];b<-d$trick[[2]];second<-(a%/%6==b%/%6 && b%%6>a%%6) || (a%/%6!=b%/%6 && b%/%6==d$trump_suit);w<-if(second) 1L-d$leader else d$leader
 d$won[[w+1L]]<-c(d$won[[w+1L]],d$trick);d$trick<-list();d$turn<-w;d$leader<-w;d<-.ra_66_reached(d,w);if(d$phase!="play") return(d)
 if(!any(lengths(d$hands)>0)) {
  if(!is.null(d$closer)) return(.ra_66_award(d,1L-d$closer,if(!d$close_opponent_tricks) 3L else 2L,"closure_failed"))
  return(.ra_66_award(d,w,1L,"last_trick"))
 }
 if(length(d$stock) && is.null(d$closer)) {d$hands[[w+1L]]<-c(d$hands[[w+1L]],d$stock[1]);d$hands[[2L-w]]<-c(d$hands[[2L-w]],d$stock[2]);d$stock<-d$stock[-c(1,2)]}
 d
}
.ra_66_legal <- function(state) {
 d<-state$data;t<-d$turn
 if(d$phase=="deal") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(cards=list(type="array",minItems=24,maxItems=24,items=list(type="integer",minimum=0,maximum=23))),preconditions=list("Permutation of all twenty-four cards."),external_observations=list("First twelve alternate from nondealer; the stock bottom is exposed trump."))),constraints=setNames(list(),character()),completeness=FALSE,reason="external_deck_required"))
 if(d$phase=="hand_end") return(list(list(type="next_hand",params=setNames(list(),character()))))
 result<-list()
 for(card in .ra_66_playable(d)) {
  result<-c(result,list(list(type="play",params=list(card=card,marriage=FALSE))))
  if(!length(d$trick) && card%%6 %in% c(2,3) && (card%/%6*6+(5-card%%6)) %in% unlist(d$hands[[t+1L]]) && !(card%/%6 %in% unlist(d$marriages))) result<-c(result,list(list(type="play",params=list(card=card,marriage=TRUE))))
 }
 if(!length(d$trick) && length(d$stock)>0 && is.null(d$closer)) {
  result<-c(result,list(list(type="close_stock",params=setNames(list(),character()))))
  if(length(d$won[[t+1L]]) && (d$trump_suit*6+1L) %in% unlist(d$hands[[t+1L]])) result<-c(result,list(list(type="exchange_trump",params=setNames(list(),character()))))
 }
 result
}
.ra_66_outcome <- function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$hand_winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,game_points=d$match_points[[p+1L]])) else list(),reason=if(done) "seven_game_points" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
