.ra_mahjong_tile<-function(c) c%/%4L
.ra_mahjong_sequence<-function(cards) {
 rr<-sort(vapply(cards,.ra_mahjong_tile,numeric(1)))
 length(rr)==3 && rr[1]<27 && rr[1]%/%9L==rr[3]%/%9L && identical(as.integer(rr),seq(as.integer(rr[1]),length.out=3L))
}
.ra_mahjong_groups<-function(counts) {
 if(!any(counts>0)) return(TRUE)
 r<-which(counts>0)[1]
 if(counts[r]>=3) {cc<-counts;cc[r]<-cc[r]-3L;if(.ra_mahjong_groups(cc)) return(TRUE)}
 if(r<=27 && (r-1L)%%9L<=6 && counts[r+1L]>0 && counts[r+2L]>0) {
  cc<-counts;cc[r:(r+2L)]<-cc[r:(r+2L)]-1L
  if(.ra_mahjong_groups(cc)) return(TRUE)
 };FALSE
}
.ra_mahjong_winning<-function(hand,melds) {
 if(length(hand)!=14L-3L*length(melds)) return(FALSE)
 counts<-tabulate(vapply(hand,.ra_mahjong_tile,numeric(1))+1L,nbins=34)
 if(length(melds)==0) {
  if(sum(counts==2)==7) return(TRUE)
  if(identical(as.integer(which(counts>0)-1L),c(0L,8L,9L,17L,18L,26L,27:33)) && max(counts)==2) return(TRUE)
 }
 for(r in which(counts>=2)) {cc<-counts;cc[r]<-cc[r]-2L;if(.ra_mahjong_groups(cc)) return(TRUE)}
 FALSE
}
.ra_mahjong_kongs<-function(d) sum(vapply(d$melds,function(ms) sum(vapply(ms,function(m) m$kind=="kong",logical(1))),numeric(1)))
.ra_mahjong_can_replace<-function(d) length(d$wall)>0 && length(d$dead_wall)>0 && .ra_mahjong_kongs(d)<4
.ra_mahjong_replacement<-function(d) {
 d$hands[[d$turn+1L]]<-c(d$hands[[d$turn+1L]],tail(d$dead_wall,1));d$dead_wall<-head(d$dead_wall,-1);d$dead_wall<-c(tail(d$wall,1),d$dead_wall);d$wall<-head(d$wall,-1);d$phase<-"discard";d$drawn<-TRUE;d
}
.ra_mahjong_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 cards<-c(unlist(d$hands),unlist(d$rivers),unlist(d$wall),unlist(d$dead_wall),unlist(lapply(d$melds,function(ms) lapply(ms,function(m) m$cards))))
 if(d$phase=="setup") {if(length(cards)>0 || !is.null(d$pending) || !is.null(d$winner)) bad("Setup precedes the complete external tile deal");return(invisible(NULL))}
 if(length(cards)!=136 || !identical(as.integer(sort(cards)),0:135)) bad("All 136 physical tiles must occur exactly once")
 if(length(d$dead_wall)!=14 || .ra_mahjong_kongs(d)>4) bad("The declared hand retains fourteen dead-wall tiles and at most four kongs")
 for(t in 0:3) {
  for(m in d$melds[[t+1L]]) {
   cc<-m$cards
   if(m$kind=="chow" && !.ra_mahjong_sequence(cc) || m$kind=="pung" && (length(cc)!=3 || length(unique(vapply(cc,.ra_mahjong_tile,numeric(1))))!=1) || m$kind=="kong" && (length(cc)!=4 || length(unique(vapply(cc,.ra_mahjong_tile,numeric(1))))!=1)) bad("Declared meld tiles must match their kind")
   if(m$kind=="chow" && (is.null(m$from_player) || m$from_player!=(t-1L)%%4L)) bad("A chow must originate from the preceding seat")
   if(m$open!=(!is.null(m$from_player)) || !m$open && m$kind!="kong" || !is.null(m$from_player) && m$from_player==t) bad("Meld source and visibility are inconsistent")
  }
  expected<-if(t==d$turn && d$phase %in% c("discard","kong_reaction") || !is.null(d$winner) && t==d$winner && d$phase=="finished") 14L else 13L
  if(length(d$hands[[t+1L]])+3L*length(d$melds[[t+1L]])!=expected) bad("Concealed hand size disagrees with melds and phase")
 }
 if(d$phase %in% c("reaction","kong_reaction")) {
  p<-d$pending
  if(is.null(p) || p$player!=d$turn) bad("Claim window needs the source player and tile")
  if(d$phase=="reaction") {if(length(d$rivers[[p$player+1L]])==0 || tail(unlist(d$rivers[[p$player+1L]]),1)!=p$tile) bad("Claims refer to the latest discard")}
  else {
   if(!p$tile %in% unlist(d$hands[[p$player+1L]]) || p$meld_index>=length(d$melds[[p$player+1L]])) bad("Added-kong tile and meld must exist")
   m<-d$melds[[p$player+1L]][[p$meld_index+1L]]
   if(m$kind!="pung" || .ra_mahjong_tile(m$cards[[1]])!=.ra_mahjong_tile(p$tile)) bad("Added kong must extend a matching pung")
  }
  if(!is.null(d$claims[[p$player+1L]])) bad("Source player cannot claim their own tile")
  for(who in 0:3) {
   claim<-d$claims[[who+1L]]
   if(is.null(claim)) next
   cc<-claim$cards;kind<-claim$kind
   if(anyDuplicated(unlist(cc))>0 || any(!unlist(cc) %in% unlist(d$hands[[who+1L]]))) bad("Recorded claims require distinct owned tiles")
   if(d$phase=="kong_reaction" && !kind %in% c("pass","win")) bad("Only a win can interrupt an added kong")
   if(kind %in% c("pass","win") && length(cc)>0 || kind=="win" && !.ra_mahjong_winning(c(d$hands[[who+1L]],list(p$tile)),d$melds[[who+1L]])) bad("Recorded pass or win claim is inconsistent")
   if(kind=="chow" && (who!=(p$player+1L)%%4L || length(cc)!=2 || !.ra_mahjong_sequence(c(cc,list(p$tile))))) bad("Recorded chow claim is inconsistent")
   if(kind %in% c("pung","kong") && (length(cc)!=(if(kind=="pung") 2L else 3L) || any(vapply(cc,.ra_mahjong_tile,numeric(1))!=.ra_mahjong_tile(p$tile)) || kind=="kong" && !.ra_mahjong_can_replace(d))) bad("Recorded matching-set claim is inconsistent")
  }
 } else if(!is.null(d$pending) || any(vapply(d$claims,function(x) !is.null(x),logical(1)))) bad("Claims exist only during a claim window")
 if(d$phase=="finished") {
  if(identical(d$reason,"exhausted")) {if(length(d$wall)>0 || !is.null(d$winner)) bad("Exhaustion needs an empty live wall and no winner")}
  else if(!is.null(d$reason) && d$reason %in% c("self_draw","discard_win","robbed_kong")) {if(is.null(d$winner) || !.ra_mahjong_winning(d$hands[[d$winner+1L]],d$melds[[d$winner+1L]])) bad("Winning result requires a structurally complete hand")}
  else bad("Finished hand requires a result reason")
 } else if(!is.null(d$winner) || !is.null(d$reason)) bad("Only a finished hand has a result")
}
.ra_mahjong_schemas<-function(d) {
 ph<-d$phase;empty<-setNames(list(),character());P<-list(type="integer",minimum=0,maximum=3);C<-list(type="integer",minimum=0,maximum=135)
 if(ph=="setup") return(list(deal=list(deck=list(type="array",minItems=136,maxItems=136,items=C),dealer=P)))
 if(ph=="draw") return(list(draw=empty))
 if(ph=="discard") {
  s<-list(discard=list(tile=C))
  if(d$drawn) s<-c(s,list(win=empty,kong=list(cards=list(type="array",minItems=1,maxItems=4,items=C))))
  return(s)
 }
 if(ph %in% c("reaction","kong_reaction")) return(list(claim=list(player=P,kind=list(type="enum",values=as.list(if(ph=="kong_reaction") c("pass","win") else c("pass","win","chow","pung","kong"))),cards=list(type="array",minItems=0,maxItems=3,items=C))))
 list()
}
.ra_mahjong_resolve<-function(d) {
 p<-d$pending;source<-p$player;c<-p$tile;robbing<-d$phase=="kong_reaction";claimants<-Filter(function(i) i!=source && d$claims[[i+1L]]$kind!="pass",setdiff(0:3,source))
 if(length(claimants)==0) {
  if(robbing) {
   d$hands[[source+1L]]<-Filter(function(x) x!=c,d$hands[[source+1L]]);m<-d$melds[[source+1L]][[p$meld_index+1L]];m$cards<-c(m$cards,list(c));m$kind<-"kong";d$melds[[source+1L]][[p$meld_index+1L]]<-m;d<-.ra_mahjong_replacement(d)
  } else {
   d$turn<-(source+1L)%%4L;d$phase<-"draw";d$drawn<-FALSE
   if(length(d$wall)==0) {d$phase<-"finished";d$reason<-"exhausted"}
  }
 } else {
  priority<-c(chow=1L,pung=2L,kong=2L,win=3L);weights<-vapply(claimants,function(i) priority[d$claims[[i+1L]]$kind]*10L-(i-source)%%4L,numeric(1));who<-claimants[which.max(weights)];claim<-d$claims[[who+1L]];kind<-claim$kind
  if(robbing) d$hands[[source+1L]]<-Filter(function(x) x!=c,d$hands[[source+1L]]) else d$rivers[[source+1L]]<-head(d$rivers[[source+1L]],-1)
  if(kind=="win") {d$hands[[who+1L]]<-c(d$hands[[who+1L]],list(c));d$phase<-"finished";d$winner<-who;d$reason<-if(robbing) "robbed_kong" else "discard_win"}
  else {
   d$hands[[who+1L]]<-Filter(function(x) !x %in% unlist(claim$cards),d$hands[[who+1L]])
   d$melds[[who+1L]][[length(d$melds[[who+1L]])+1L]]<-list(kind=kind,cards=c(claim$cards,list(c)),open=TRUE,from_player=source);d$turn<-who;d$phase<-"discard";d$drawn<-FALSE
   if(kind=="kong") d<-.ra_mahjong_replacement(d)
  }
 }
 d["pending"]<-list(NULL);d$claims<-rep(list(NULL),4);d
}
.ra_mahjong_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;s<-.ra_mahjong_schemas(d)
 if(!k %in% names(s)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,s[[k]],"/params");t<-if(!is.null(p$player)) p$player else d$turn
 if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
 if(k=="deal") {
  if(!identical(as.integer(sort(unlist(p$deck))),0:135)) .ra_fail("Supply each physical tile exactly once")
  dealer<-p$dealer;d$hands<-lapply(1:4,function(i) p$deck[seq(i,52L,by=4L)]);d$hands[[dealer+1L]]<-c(d$hands[[dealer+1L]],p$deck[53]);d$phase<-"discard";d$turn<-dealer;d$dealer<-dealer;d$wall<-p$deck[54:122];d$dead_wall<-p$deck[123:136];d$drawn<-TRUE
 } else if(k=="draw") {
  if(length(d$wall)==0) {d$phase<-"finished";d$reason<-"exhausted"}
  else {d$hands[[t+1L]]<-c(d$hands[[t+1L]],d$wall[1]);d$wall<-d$wall[-1];d$phase<-"discard";d$drawn<-TRUE}
 } else if(k=="discard") {
  c<-p$tile
  if(!c %in% unlist(d$hands[[t+1L]])) .ra_fail("Discard a tile currently held")
  d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$rivers[[t+1L]]<-c(d$rivers[[t+1L]],list(c));d$phase<-"reaction";d$pending<-list(player=t,tile=c,meld_index=0L);d$claims<-rep(list(NULL),4);d$drawn<-FALSE
 } else if(k=="win") {
  if(!.ra_mahjong_winning(d$hands[[t+1L]],d$melds[[t+1L]])) .ra_fail("Hand is not a permitted complete winning structure")
  d$phase<-"finished";d$winner<-t;d$reason<-"self_draw"
 } else if(k=="kong") {
  cc<-p$cards
  if(!.ra_mahjong_can_replace(d) || anyDuplicated(unlist(cc))>0 || any(!unlist(cc) %in% unlist(d$hands[[t+1L]]))) .ra_fail("Kong requires owned distinct tiles, a replacement draw and fewer than four existing kongs")
  if(length(cc)==4 && length(unique(vapply(cc,.ra_mahjong_tile,numeric(1))))==1) {
   d$hands[[t+1L]]<-Filter(function(x) !x %in% unlist(cc),d$hands[[t+1L]]);d$melds[[t+1L]][[length(d$melds[[t+1L]])+1L]]<-list(kind="kong",cards=cc,open=FALSE,from_player=NULL);d<-.ra_mahjong_replacement(d)
  } else if(length(cc)==1) {
   ix<-which(vapply(d$melds[[t+1L]],function(m) m$kind=="pung" && .ra_mahjong_tile(m$cards[[1]])==.ra_mahjong_tile(cc[[1]]),logical(1)))
   if(length(ix)==0) .ra_fail("An added kong must extend an exposed matching pung")
   d$phase<-"kong_reaction";d$pending<-list(player=t,tile=cc[[1]],meld_index=ix[1]-1L);d$claims<-rep(list(NULL),4)
  } else .ra_fail("Declare four identical tiles or one tile extending an exposed pung")
 } else if(k=="claim") {
  source<-d$pending$player;c<-d$pending$tile;kind<-p$kind;cc<-p$cards
  if(t==source || !is.null(d$claims[[t+1L]])) .ra_fail("Each other player may respond exactly once")
  if(anyDuplicated(unlist(cc))>0 || any(!unlist(cc) %in% unlist(d$hands[[t+1L]]))) .ra_fail("Claim tiles must be distinct and owned")
  if(kind %in% c("pass","win")) {
   if(length(cc)>0) .ra_fail("Pass and win claims do not nominate meld tiles")
   if(kind=="win" && !.ra_mahjong_winning(c(d$hands[[t+1L]],list(c)),d$melds[[t+1L]])) .ra_fail("Claimed tile does not complete a winning hand")
  } else if(kind=="chow") {
   if(t!=(source+1L)%%4L || length(cc)!=2 || !.ra_mahjong_sequence(c(cc,list(c)))) .ra_fail("Only the next player may claim a suited sequence")
  } else if(kind=="pung") {
   if(length(cc)!=2 || any(vapply(cc,.ra_mahjong_tile,numeric(1))!=.ra_mahjong_tile(c))) .ra_fail("Pung requires two matching tiles")
  } else if(kind=="kong") {
   if(length(cc)!=3 || any(vapply(cc,.ra_mahjong_tile,numeric(1))!=.ra_mahjong_tile(c)) || !.ra_mahjong_can_replace(d)) .ra_fail("Exposed kong requires three matching tiles and a replacement draw")
  }
  d$claims[[t+1L]]<-list(kind=kind,cards=cc)
  if(all(vapply(setdiff(0:3,source),function(i) !is.null(d$claims[[i+1L]]),logical(1)))) d<-.ra_mahjong_resolve(d)
 }
 d
}
.ra_mahjong_legal<-function(state) {
 d<-state$data;s<-.ra_mahjong_schemas(d)
 list(mode="event_types",actions=list(),action_types=lapply(names(s),function(k) list(type=k,params_schema=s[[k]],preconditions=list("Turn, ownership, hand size, meld shape and claim priority must permit the action."),external_observations=if(k=="deal") list("External shuffled physical-tile deck.") else list())),constraints=list(phase=d$phase,turn=d$turn,live_tiles=length(d$wall),kongs=.ra_mahjong_kongs(d),pending=d$pending,can_win=d$phase=="discard" && d$drawn && .ra_mahjong_winning(d$hands[[d$turn+1L]],d$melds[[d$turn+1L]])),completeness=FALSE,reason="constrained_mahjong_claims")
}
.ra_mahjong_outcome<-function(state,pack) {
 d<-state$data;done<-d$phase=="finished";w<-d$winner
 list(status=if(done) "terminal" else "ongoing",result_kind=if(done) {if(is.null(w)) "draw" else "win"} else "none",winners=if(!is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) lapply(setdiff(0:3,w),function(i) state$players[[i+1L]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
