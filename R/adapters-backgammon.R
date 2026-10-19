.ra_bg_sign <- function(p) if(p==0) 1L else -1L
.ra_bg_candidates <- function(board,bar,off,p,die) {
  s<-.ra_bg_sign(p);sources<-if(bar[p+1L]>0) -1L else which(board*s>0)-1L;out<-list();home<-bar[p+1L]==0 && all(board*s<=0 | (if(p==0) 0:23>=18 else 0:23<=5))
  for(i in sources) {
    dest<-if(i== -1) if(p==0) die-1L else 24L-die else i+if(p==0) die else -die
    if(dest>=0 && dest<24) {if(board[dest+1L]*s>= -1) out<-c(out,list(c(i,dest)))} else if(i!= -1 && home) {
      distance<-if(p==0) 24L-i else i+1L;farther<-any(board*s>0 & (if(p==0) 0:23<i else 0:23>i))
      if(die==distance || (die>distance && !farther)) out<-c(out,list(c(i,24L)))
    }
  }
  out
}
.ra_bg_step <- function(board,bar,off,p,source,dest) {
  s<-.ra_bg_sign(p)
  if(source== -1) bar[p+1L]<-bar[p+1L]-1L else board[source+1L]<-board[source+1L]-s
  if(dest==24) off[p+1L]<-off[p+1L]+1L else {
    if(board[dest+1L]== -s) {bar[2L-p]<-bar[2L-p]+1L;board[dest+1L]<-0L}
    board[dest+1L]<-board[dest+1L]+s
  }
  list(board=board,bar=bar,off=off)
}
.ra_bg_cache <- new.env(parent=emptyenv())
.ra_bg_allowed <- function(d) {
  board<-unlist(d$board);bar<-unlist(d$bar);off<-unlist(d$off);p<-d$turn;dice<-sort(unlist(d$dice));cache_key<-paste(c(board,bar,off,p,dice),collapse=",")
  if(identical(.ra_bg_cache$key,cache_key)) return(.ra_bg_cache$moves)
  memo<-new.env(parent=emptyenv())
  max_uses<-function(board,bar,off,dice) {
    key<-paste(c(board,bar,off,dice),collapse=",")
    if(exists(key,envir=memo,inherits=FALSE)) return(get(key,envir=memo,inherits=FALSE))
    best<-0L
    for(die in sort(unique(dice),decreasing=TRUE)) {
      rest<-dice[-match(die,dice)]
      for(m in .ra_bg_candidates(board,bar,off,p,die)) {
        next_state<-.ra_bg_step(board,bar,off,p,m[1],m[2]);best<-max(best,1L+max_uses(next_state$board,next_state$bar,next_state$off,rest))
        if(best==length(dice)) {assign(key,best,envir=memo);return(best)}
      }
    }
    assign(key,best,envir=memo);best
  }
  scored<-list()
  for(die in sort(unique(dice),decreasing=TRUE)) {
    rest<-dice[-match(die,dice)]
    for(m in .ra_bg_candidates(board,bar,off,p,die)) {n<-.ra_bg_step(board,bar,off,p,m[1],m[2]);scored<-c(scored,list(c(1L+max_uses(n$board,n$bar,n$off,rest),m,die)))}
  }
  out<-list()
  if(length(scored)) {
    best<-max(vapply(scored,function(x) x[1],numeric(1)));keep<-Filter(function(x) x[1]==best,scored)
    if(best==1 && length(dice)==2 && dice[1]!=dice[2]) {high<-max(vapply(keep,function(x) x[4],numeric(1)));keep<-Filter(function(x) x[4]==high,keep)}
    out<-lapply(keep,function(x) x[2:4])
  }
  .ra_bg_cache$key<-cache_key;.ra_bg_cache$moves<-out;out
}
.ra_bg_end_turn <- function(d) {d$turn<-1L-d$turn;d$phase<-"roll";d$dice<-list();d["rolled"]<-list(NULL);d}
.ra_bg_validate <- function(d) {
  for(p in 0:1) if(sum(pmax(0,unlist(d$board)*.ra_bg_sign(p)))+d$bar[[p+1L]]+d$off[[p+1L]]!=15) .ra_fail("Each player must own fifteen checkers across board, bar and off",code="INVALID_STATE")
  if(bitwAnd(as.integer(d$cube),as.integer(d$cube-1L))!=0) .ra_fail("Cube must be a power of two",code="INVALID_STATE")
  if((d$cube==1)!=is.null(d$cube_owner)) .ra_fail("Only an undoubled cube is centered",code="INVALID_STATE")
  opening<-numeric(24);opening[c(1,12,17,19,24,13,8,6)]<-c(2,5,3,5,-2,-5,-3,-5)
  if(d$phase=="opening" && (any(unlist(d$board)!=opening) || any(unlist(d$bar)) || any(unlist(d$off)) || d$turn!=0)) .ra_fail("Opening requires the standard checker layout",code="INVALID_STATE")
  if(d$phase=="opening" && (d$cube!=1 || length(d$dice) || !is.null(d$rolled) || !is.null(d$winner))) .ra_fail("Opening roll requires a centered cube and no pending dice",code="INVALID_STATE")
  phase<-if(d$phase=="resign_offer") if(is.null(d$resume)) "" else d$resume else d$phase
  if(phase %in% c("roll","double_offer") && (length(d$dice) || !is.null(d$rolled))) .ra_fail("Roll phase cannot retain previous dice",code="INVALID_STATE")
  if(phase=="move") {
    if(is.null(d$rolled)) .ra_fail("Moves require a recorded roll",code="INVALID_STATE")
    original<-if(d$rolled[[1]]==d$rolled[[2]]) rep(unlist(d$rolled),2) else unlist(d$rolled)
    if(!length(d$dice) || any(vapply(unique(unlist(d$dice)),function(x) sum(unlist(d$dice)==x)>sum(original==x),logical(1))) || !length(.ra_bg_allowed(d))) .ra_fail("Remaining dice must belong to the roll and permit a move",code="INVALID_STATE")
  }
  if(d$phase=="double_offer" && (d$cube>=64 || (!is.null(d$cube_owner) && d$cube_owner!=d$turn))) .ra_fail("Only the cube owner may offer a permitted redouble",code="INVALID_STATE")
  if(d$phase=="resign_offer") {
    if(is.null(d$resume) || !d$resume %in% c("roll","move") || is.null(d$resign_value)) .ra_fail("Resignation must retain its prior phase and proposed value",code="INVALID_STATE")
  } else if(!is.null(d$resume) || !is.null(d$resign_value)) .ra_fail("Stale resignation offer",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(is.null(d$winner) || is.null(d$reason) || is.null(d$multiplier) || length(d$dice)) .ra_fail("A finished game needs a settled result",code="INVALID_STATE")
    if(d$reason=="borne_off") {
      w<-d$winner;loser<-1L-w;backgammon<-d$bar[[loser+1L]]>0 || any(unlist(d$board)*.ra_bg_sign(loser)>0 & (if(w==0) 0:23>=18 else 0:23<=5));expected<-if(d$off[[loser+1L]]>0) 1L else if(backgammon) 3L else 2L
      if(d$off[[w+1L]]!=15 || w!=d$turn || d$multiplier!=expected) .ra_fail("Bearing-off result and gammon multiplier disagree",code="INVALID_STATE")
    }
    if(d$reason=="double_declined" && (d$winner!=d$turn || d$multiplier!=1)) .ra_fail("A declined double settles at the old cube",code="INVALID_STATE")
    if(d$reason=="resignation" && d$winner==d$turn) .ra_fail("The resigning player cannot win",code="INVALID_STATE")
  } else if(!is.null(d$winner) || !is.null(d$reason) || !is.null(d$multiplier) || 15 %in% unlist(d$off)) .ra_fail("A completed game must be terminal",code="INVALID_STATE")
}
.ra_bg_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn;actor<-if(d$phase %in% c("double_offer","resign_offer")) 1L-t else t;empty<-structure(list(),names=character())
  if(k!="roll" && !is.null(action$actor) && action$actor!=state$players[[actor+1L]]$id) .ra_fail("Wrong actor")
  if(k=="roll") {
    .ra_fields(p,list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6))),"/params")
    if(!d$phase %in% c("opening","roll")) .ra_fail("A roll is unavailable while a move or offer is pending")
    a<-p$dice[[1]];b<-p$dice[[2]]
    if(d$phase=="opening") {if(a==b) return(d);d$turn<-if(a>b) 0L else 1L}
    d$rolled<-p$dice;d$dice<-if(a==b) rep(p$dice,2) else p$dice;d$phase<-"move"
    return(if(length(.ra_bg_allowed(d))) d else .ra_bg_end_turn(d))
  }
  if(k=="move") {
    .ra_fields(p,list(source=list(type="integer",minimum=-1,maximum=23),die=list(type="integer",minimum=1,maximum=6)),"/params")
    if(d$phase!="move") .ra_fail("Roll before moving")
    options<-Filter(function(m) m[1]==p$source && m[3]==p$die,.ra_bg_allowed(d))
    if(!length(options)) .ra_fail("Move must obey bar priority, blocking, bearing off and maximum dice usage")
    m<-options[[1]];next_state<-.ra_bg_step(unlist(d$board),unlist(d$bar),unlist(d$off),t,m[1],m[2]);d$board<-as.list(next_state$board);d$bar<-as.list(next_state$bar);d$off<-as.list(next_state$off);d$dice<-d$dice[-match(m[3],unlist(d$dice))]
    if(d$off[[t+1L]]==15) {
      loser<-1L-t;backgammon<-d$bar[[loser+1L]]>0 || any(unlist(d$board)*.ra_bg_sign(loser)>0 & (if(t==0) 0:23>=18 else 0:23<=5));d$multiplier<-if(d$off[[loser+1L]]>0) 1L else if(backgammon) 3L else 2L;d$winner<-t;d$reason<-"borne_off";d$phase<-"finished";d$dice<-list();return(d)
    }
    return(if(length(d$dice) && length(.ra_bg_allowed(d))) d else .ra_bg_end_turn(d))
  }
  if(k=="double") {
    .ra_fields(p,empty,"/params")
    if(d$phase!="roll" || (!is.null(d$cube_owner) && d$cube_owner!=t) || d$cube>=64) .ra_fail("Double before rolling, with cube ownership and below the table limit")
    d$phase<-"double_offer";return(d)
  }
  if(k=="respond_double") {
    .ra_fields(p,list(accept=list(type="boolean")),"/params")
    if(d$phase!="double_offer") .ra_fail("No double is pending")
    if(p$accept) {d$cube<-d$cube*2L;d$cube_owner<-1L-t;d$phase<-"roll"} else {d$winner<-t;d$multiplier<-1L;d$reason<-"double_declined";d$phase<-"finished"}
    return(d)
  }
  if(k=="offer_resignation") {
    .ra_fields(p,list(value=list(type="integer",minimum=1,maximum=3)),"/params")
    if(!d$phase %in% c("roll","move")) .ra_fail("Offer resignation only during your own normal turn")
    d$resume<-d$phase;d$resign_value<-p$value;d$phase<-"resign_offer";return(d)
  }
  if(k=="respond_resignation") {
    .ra_fields(p,list(accept=list(type="boolean")),"/params")
    if(d$phase!="resign_offer") .ra_fail("No resignation is pending")
    if(p$accept) {d$winner<-1L-t;d$multiplier<-d$resign_value;d$reason<-"resignation";d$phase<-"finished";d$dice<-list()} else d$phase<-d$resume
    d["resume"]<-list(NULL);d["resign_value"]<-list(NULL);return(d)
  }
  .ra_fail("Unknown action")
}
.ra_bg_legal <- function(state) {
  d<-state$data;actions<-list();types<-list();empty<-structure(list(),names=character())
  if(d$phase %in% c("opening","roll")) types<-list(list(type="roll",params_schema=list(dice=list(type="array",minItems=2,maxItems=2,items=list(type="integer",minimum=1,maximum=6))),preconditions=list("Opening dice are one per player; ties are rerolled, higher player uses both. Later doubles give four moves."),external_observations=list("Actual dice.")))
  if(d$phase=="move") actions<-lapply(.ra_bg_allowed(d),function(m) list(type="move",params=list(source=m[1],die=m[3])))
  if(d$phase=="double_offer") actions<-lapply(c(TRUE,FALSE),function(x) list(type="respond_double",params=list(accept=x)))
  else if(d$phase=="resign_offer") actions<-lapply(c(TRUE,FALSE),function(x) list(type="respond_resignation",params=list(accept=x))) else {
    if(d$phase=="roll" && (is.null(d$cube_owner) || d$cube_owner==d$turn) && d$cube<64) actions<-c(actions,list(list(type="double",params=empty)))
    if(d$phase %in% c("roll","move")) actions<-c(actions,lapply(1:3,function(x) list(type="offer_resignation",params=list(value=x))))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn,dice=d$dice,cube=d$cube,cube_owner=d$cube_owner),completeness=FALSE,reason="legal_current_roll_moves_and_external_dice")
}
.ra_bg_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";w<-d$winner
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p==w) 1L else 2L,points=d$cube*d$multiplier*(if(p==w) 1L else -1L))) else list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
