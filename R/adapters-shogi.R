.ra_sh_total <- c(2L,4L,4L,4L,4L,2L,2L,18L)
.ra_sh_base <- function(c) ifelse(abs(c)>8,abs(c)-8,abs(c))
.ra_sh_side <- function(c) {if(c>0) 0L else if(c<0) 1L else -1L}
.ra_sh_zone <- function(p,row) if(p==0) row>=6 else row<=2
.ra_sh_inside <- function(x,y) x>=0 && x<9 && y>=0 && y<9
.ra_sh_targets <- function(b,i) {
  c<-b[[i+1L]];p<-.ra_sh_side(c);k<-abs(c);x<-i%%9;y<-i%/%9;f<-if(p==0) 1L else -1L;out<-integer();steps<-list();rays<-list()
  if(k==1) {for(a in -1:1) for(v in -1:1) if(a!=0 || v!=0) steps<-c(steps,list(c(a,v)))}
  else if(k %in% c(2,11,12,13,16)) steps<-list(c(-1,f),c(0,f),c(1,f),c(-1,0),c(1,0),c(0,-f))
  else if(k==3) steps<-list(c(-1,f),c(0,f),c(1,f),c(-1,-f),c(1,-f))
  else if(k==4) steps<-list(c(-1,2*f),c(1,2*f))
  else if(k==5) rays<-list(c(0,f))
  else if(k %in% c(6,14)) {rays<-list(c(-1,-1),c(-1,1),c(1,-1),c(1,1));if(k==14) steps<-list(c(-1,0),c(1,0),c(0,-1),c(0,1))}
  else if(k %in% c(7,15)) {rays<-list(c(-1,0),c(1,0),c(0,-1),c(0,1));if(k==15) steps<-list(c(-1,-1),c(-1,1),c(1,-1),c(1,1))}
  else if(k==8) steps<-list(c(0,f))
  for(delta in steps) {
    a<-x+delta[1];v<-y+delta[2]
    if(.ra_sh_inside(a,v) && .ra_sh_side(b[[v*9+a+1L]])!=p) out<-c(out,v*9+a)
  }
  for(delta in rays) {
    a<-x+delta[1];v<-y+delta[2]
    while(.ra_sh_inside(a,v)) {
      j<-v*9+a
      if(.ra_sh_side(b[[j+1L]])!=p) out<-c(out,j)
      if(b[[j+1L]]!=0) break
      a<-a+delta[1];v<-v+delta[2]
    }
  }
  out
}
.ra_sh_check <- function(b,p) {
  king<-if(p==0) 1L else -1L;k<-match(king,unlist(b))-1L
  if(is.na(k)) return(TRUE)
  for(i in 0:80) if(b[[i+1L]]!=0 && .ra_sh_side(b[[i+1L]])!=p && k %in% .ra_sh_targets(b,i)) return(TRUE)
  FALSE
}
.ra_sh_dead <- function(p,k,y) (k %in% c(5,8) && y==(if(p==0) 8L else 0L)) || (k==4 && (if(p==0) y>=7 else y<=1))
.ra_sh_board_moves <- function(b,p) {
  out<-list()
  for(i in 0:80) {
    c<-b[[i+1L]]
    if(c==0 || .ra_sh_side(c)!=p) next
    k<-abs(c)
    for(j in .ra_sh_targets(b,i)) {
      if(abs(b[[j+1L]])==1) next
      eligible<-k %in% c(3,4,5,6,7,8) && (.ra_sh_zone(p,i%/%9) || .ra_sh_zone(p,j%/%9));forced<-.ra_sh_dead(p,k,j%/%9)
      for(promotion in if(forced) TRUE else if(eligible) c(FALSE,TRUE) else FALSE) {
        trial<-b;trial[[j+1L]]<-if(promotion) c+if(p==0) 8L else -8L else c;trial[[i+1L]]<-0L
        if(!.ra_sh_check(trial,p)) out<-c(out,list(list(i,j,promotion)))
      }
    }
  }
  out
}
.ra_sh_key <- function(d) paste0(paste0(intToUtf8(as.integer(unlist(d$board)+81L),multiple=TRUE),collapse=""),"|",d$turn,"|",paste0(unlist(d$hands),collapse=","))
.ra_sh_cache <- new.env(parent=emptyenv())
.ra_sh_legal_parts <- function(d) {
  cache_key<-.ra_sh_key(d)
  if(identical(.ra_sh_cache$key,cache_key)) return(.ra_sh_cache$parts)
  b<-d$board;p<-d$turn;moves<-.ra_sh_board_moves(b,p);drops<-list();hand<-d$hands[[p+1L]]
  for(kind in 2:8) {
    if(hand[[kind-1L]]==0) next
    for(j in 0:80) {
      if(b[[j+1L]]!=0 || .ra_sh_dead(p,kind,j%/%9)) next
      if(kind==8 && any(unlist(b)[(0:8)*9+j%%9+1L]==(if(p==0) 8L else -8L))) next
      trial<-b;trial[[j+1L]]<-if(p==0) kind else -kind
      if(.ra_sh_check(trial,p)) next
      if(kind==8 && .ra_sh_check(trial,1L-p) && !length(.ra_sh_board_moves(trial,1L-p))) next
      drops<-c(drops,list(c(kind,j)))
    }
  }
  out<-list(moves=moves,drops=drops);.ra_sh_cache$key<-cache_key;.ra_sh_cache$parts<-out;out
}
.ra_sh_repeated <- function(d) sum(unlist(d$history)==.ra_sh_key(d))>=4
.ra_sh_repetition_result <- function(d) {
  positions<-which(unlist(d$history)==.ra_sh_key(d))
  if(length(positions)<4) return(list(winner=NULL,reason=NULL))
  start<-tail(positions,4)[1];checkers<-integer()
  for(p in 0:1) {
    relevant<-logical()
    for(i in seq.int(start+1L,length(d$history))) if(as.integer(strsplit(d$history[[i]],"|",fixed=TRUE)[[1]][2])==1L-p) relevant<-c(relevant,d$checks[[i]])
    if(length(relevant) && all(relevant)) checkers<-c(checkers,p)
  }
  if(length(checkers)==1) list(winner=1L-checkers[1],reason="perpetual_check") else list(winner=NULL,reason="fourfold_repetition")
}
.ra_sh_impasse_points <- function(d,p) {
  points<-0L
  for(c in d$board) if(c!=0 && .ra_sh_side(c)==p && .ra_sh_base(c)!=1) points<-points+if(.ra_sh_base(c) %in% c(6,7)) 5L else 1L
  for(k in 2:8) points<-points+d$hands[[p+1L]][[k-1L]]*(if(k %in% c(6,7)) 5L else 1L)
  points
}
.ra_sh_impasse_possible <- function(d) all(vapply(0:1,function(p) .ra_sh_zone(p,(match(if(p==0) 1L else -1L,unlist(d$board))-1L)%/%9) && !.ra_sh_check(d$board,p),logical(1)))
.ra_sh_validate <- function(d) {
  b<-unlist(d$board);allowed<-c(0:8,11:16)
  if(any(!abs(b) %in% allowed)) .ra_fail("Unknown or impossible promoted piece type",code="INVALID_STATE")
  for(kind in 1:8) {
    actual<-sum(.ra_sh_base(b[b!=0])==kind)+if(kind>=2) sum(vapply(d$hands,function(h) h[[kind-1L]],numeric(1))) else 0L
    if(actual!=.ra_sh_total[kind]) .ra_fail("Board and hands must conserve all forty original pieces",code="INVALID_STATE")
  }
  for(p in 0:1) {
    if(sum(b==(if(p==0) 1L else -1L))!=1) .ra_fail("Each side requires exactly one king",code="INVALID_STATE")
    for(x in 0:8) if(sum(b[(0:8)*9+x+1L]==(if(p==0) 8L else -8L))>1) .ra_fail("Two unpromoted pawns on one file are forbidden",code="INVALID_STATE")
    for(i in 0:80) if(b[i+1L]!=0 && .ra_sh_side(b[i+1L])==p && .ra_sh_dead(p,abs(b[i+1L]),i%/%9)) .ra_fail("An immobile pawn, lance or knight must be promoted",code="INVALID_STATE")
  }
  if(.ra_sh_check(d$board,1L-d$turn)) .ra_fail("The nonmoving king cannot remain in check",code="INVALID_STATE")
  if(!length(d$history) || tail(unlist(d$history),1)!=.ra_sh_key(d) || length(d$history)!=length(d$checks) || tail(unlist(d$checks),1)!=.ra_sh_check(d$board,d$turn)) .ra_fail("Position and check history mismatch",code="INVALID_STATE")
  for(h in d$history) {
    parts<-strsplit(h,"|",fixed=TRUE)[[1]]
    if(length(parts)!=3 || nchar(parts[1])!=81 || any(utf8ToInt(parts[1])<65 | utf8ToInt(parts[1])>97) || !parts[2] %in% c("0","1")) .ra_fail("Malformed position history",code="INVALID_STATE")
    counts<-strsplit(parts[3],",",fixed=TRUE)[[1]]
    if(length(counts)!=14 || any(!grepl("^[0-9]+$",counts)) || any(as.numeric(counts)>18)) .ra_fail("Malformed position history",code="INVALID_STATE")
  }
  if(!is.null(d$decision) && d$decision$kind=="impasse" && !.ra_sh_impasse_possible(d)) .ra_fail("Agreed impasse requires both kings safely in enemy camps",code="INVALID_STATE")
}
.ra_sh_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn
  if(k %in% c("resign","offer_impasse","accept_impasse")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
    if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
    if(k=="resign") {d$decision<-list(kind="resign",player=q);return(d)}
    if(!.ra_sh_impasse_possible(d)) .ra_fail("Both kings must be safely in enemy camps for agreed impasse")
    if(k=="offer_impasse") d$impasse_offer<-q else {
      if(is.null(d$impasse_offer) || d$impasse_offer==q) .ra_fail("Only the opponent may accept an impasse offer")
      d$decision<-list(kind="impasse",player=q)
    }
    return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  parts<-.ra_sh_legal_parts(d)
  if(k=="move") {
    .ra_fields(p,list(source=list(type="integer",minimum=0,maximum=80),destination=list(type="integer",minimum=0,maximum=80),promote=list(type="boolean")),"/params");i<-p$source;j<-p$destination
    if(!any(vapply(parts$moves,function(m) m[[1]]==i && m[[2]]==j && m[[3]]==p$promote,logical(1)))) .ra_fail("Illegal piece move, promotion choice or king exposure")
    captured<-d$board[[j+1L]]
    if(captured!=0) {ix<-.ra_sh_base(captured)-1L;d$hands[[t+1L]][[ix]]<-d$hands[[t+1L]][[ix]]+1L}
    c<-d$board[[i+1L]];d$board[[j+1L]]<-if(p$promote) c+if(t==0) 8L else -8L else c;d$board[[i+1L]]<-0L
  } else if(k=="drop") {
    .ra_fields(p,list(piece=list(type="integer",minimum=2,maximum=8),destination=list(type="integer",minimum=0,maximum=80)),"/params");kind<-p$piece;j<-p$destination
    if(!any(vapply(parts$drops,function(m) m[1]==kind && m[2]==j,logical(1)))) .ra_fail("Drop requires a held piece, a legal empty square, no doubled pawn and no immediate pawn-drop mate")
    d$hands[[t+1L]][[kind-1L]]<-d$hands[[t+1L]][[kind-1L]]-1L;d$board[[j+1L]]<-if(t==0) kind else -kind
  } else .ra_fail("Unknown action")
  d$turn<-1L-t;d$history<-c(d$history,list(.ra_sh_key(d)));d$checks<-c(d$checks,list(.ra_sh_check(d$board,1L-t)));d["impasse_offer"]<-list(NULL);d
}
.ra_sh_legal <- function(state) {
  d<-state$data;parts<-.ra_sh_legal_parts(d)
  actions<-c(lapply(parts$moves,function(m) list(type="move",params=list(source=m[[1]],destination=m[[2]],promote=m[[3]]))),lapply(parts$drops,function(m) list(type="drop",params=list(piece=m[1],destination=m[2]))),lapply(0:1,function(p) list(type="resign",params=list(player=p))))
  if(.ra_sh_impasse_possible(d)) {
    actions<-c(actions,lapply(0:1,function(p) list(type="offer_impasse",params=list(player=p))))
    if(!is.null(d$impasse_offer)) actions<-c(actions,list(list(type="accept_impasse",params=list(player=1L-d$impasse_offer))))
  }
  actions
}
.ra_sh_outcome <- function(state,pack) {
  d<-state$data;decision<-d$decision;w<-NULL;reason<-NULL
  if(!is.null(decision)) {
    if(decision$kind=="resign") {w<-1L-decision$player;reason<-"resignation"} else {
      points<-vapply(0:1,function(p) .ra_sh_impasse_points(d,p),numeric(1));w<-if(min(points)>=24) NULL else which.max(points)-1L;reason<-"agreed_twenty_four_point_impasse"
    }
  } else {
    parts<-.ra_sh_legal_parts(d)
    if(!length(parts$moves) && !length(parts$drops)) {w<-1L-d$turn;reason<-"no_legal_move"} else {r<-.ra_sh_repetition_result(d);w<-r$winner;reason<-r$reason}
  }
  done<-!is.null(reason)
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(is.null(w)) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
