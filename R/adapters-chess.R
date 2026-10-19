.ra_chess_orth <- list(c(1,0),c(-1,0),c(0,1),c(0,-1))
.ra_chess_diag <- list(c(1,1),c(1,-1),c(-1,1),c(-1,-1))
.ra_chess_knight <- list(c(2,1),c(2,-1),c(-2,1),c(-2,-1),c(1,2),c(1,-2),c(-1,2),c(-1,-2))
.ra_chess_at <- function(r,c) if(r>=0 && r<8 && c>=0 && c<8) r*8+c else NULL
.ra_chess_sign <- function(p) if(p==0) 1L else -1L
.ra_chess_attacked <- function(b,square,by) {
  r<-square%/%8;c<-square%%8;s<-.ra_chess_sign(by)
  for(dc in c(-1,1)) {j<-.ra_chess_at(r-s,c+dc);if(!is.null(j) && b[[j+1L]]==s) return(TRUE)}
  for(delta in .ra_chess_knight) {j<-.ra_chess_at(r+delta[1],c+delta[2]);if(!is.null(j) && b[[j+1L]]==2*s) return(TRUE)}
  dirs<-c(.ra_chess_orth,.ra_chess_diag)
  for(k in seq_along(dirs)) {
    delta<-dirs[[k]];dr<-delta[1];dc<-delta[2];rr<-r+dr;cc<-c+dc;j<-.ra_chess_at(rr,cc);distance<-1L
    while(!is.null(j)) {
      v<-b[[j+1L]]
      if(v!=0) {
        if(v*s>0 && ((abs(v)==6 && distance==1) || abs(v)==5 || abs(v)==if(k<=4) 4 else 3)) return(TRUE)
        break
      }
      rr<-rr+dr;cc<-cc+dc;j<-.ra_chess_at(rr,cc);distance<-distance+1L
    }
  }
  FALSE
}
.ra_chess_raw <- function(d,i,j,promotion=NULL) {
  x<-d;b<-x$board;piece<-b[[i+1L]];captured<-b[[j+1L]];p<-d$turn;s<-.ra_chess_sign(p);b[[i+1L]]<-0L;b[[j+1L]]<-piece
  if(abs(piece)==1 && !is.null(d$ep) && j==d$ep && captured==0 && i%%8!=j%%8) {captured<-b[[j-8*s+1L]];b[[j-8*s+1L]]<-0L}
  if(!is.null(promotion)) b[[j+1L]]<-s*promotion
  if(abs(piece)==6) {
    x$rights[(2*p+1):(2*p+2)]<-list(FALSE,FALSE)
    if(abs(j-i)==2) {from<-if(j>i) i+3 else i-4;to<-if(j>i) i+1 else i-1;b[[to+1L]]<-b[[from+1L]];b[[from+1L]]<-0L}
  }
  for(k in 1:4) if(i==c(7,0,63,56)[k] || j==c(7,0,63,56)[k]) x$rights[[k]]<-FALSE
  x$board<-b;x["ep"]<-list(if(abs(piece)==1 && abs(j-i)==16) (i+j)%/%2 else NULL);x$halfmove<-if(abs(piece)==1 || captured!=0) 0L else d$halfmove+1L;x$turn<-1L-p;x
}
.ra_chess_safe <- function(d,i,j,promotion=NULL) {
  x<-.ra_chess_raw(d,i,j,promotion);p<-d$turn;king<-which(unlist(x$board)==6*.ra_chess_sign(p))-1L;!.ra_chess_attacked(x$board,king,1L-p)
}
.ra_chess_moves <- function(d) {
  b<-d$board;p<-d$turn;s<-.ra_chess_sign(p);result<-list()
  for(i in 0:63) {
    v<-b[[i+1L]];if(v*s<=0) next
    r<-i%/%8;c<-i%%8;dest<-numeric();piece<-abs(v)
    if(piece==1) {
      j<-.ra_chess_at(r+s,c)
      if(!is.null(j) && b[[j+1L]]==0) {
        dest<-c(dest,j);jj<-.ra_chess_at(r+2*s,c)
        if(r==if(p==0) 1 else 6) if(b[[jj+1L]]==0) dest<-c(dest,jj)
      }
      for(dc in c(-1,1)) {
        j<-.ra_chess_at(r+s,c+dc)
        if(!is.null(j) && ((b[[j+1L]]*s<0 && abs(b[[j+1L]])!=6) || (!is.null(d$ep) && j==d$ep))) dest<-c(dest,j)
      }
    } else if(piece %in% c(2,6)) {
      for(delta in if(piece==2) .ra_chess_knight else c(.ra_chess_orth,.ra_chess_diag)) {
        j<-.ra_chess_at(r+delta[1],c+delta[2]);if(!is.null(j) && b[[j+1L]]*s<=0 && abs(b[[j+1L]])!=6) dest<-c(dest,j)
      }
      home<-if(p==0) 4 else 60
      if(piece==6 && i==home && !.ra_chess_attacked(b,i,1L-p)) {
        for(side in 0:1) {
          empty<-if(side==0) c(1,2) else c(-1,-2,-3);path<-if(side==0) c(1,2) else c(-1,-2)
          if(d$rights[[2*p+side+1L]] && all(vapply(empty,function(o) b[[i+o+1L]]==0,logical(1))) && all(vapply(path,function(o) !.ra_chess_attacked(b,i+o,1L-p),logical(1)))) dest<-c(dest,i+if(side==0) 2 else -2)
        }
      }
    } else {
      dirs<-if(piece==4) .ra_chess_orth else if(piece==3) .ra_chess_diag else c(.ra_chess_orth,.ra_chess_diag)
      for(delta in dirs) {
        dr<-delta[1];dc<-delta[2];rr<-r+dr;cc<-c+dc;j<-.ra_chess_at(rr,cc)
        while(!is.null(j)) {
          if(b[[j+1L]]*s>0 || abs(b[[j+1L]])==6) break
          dest<-c(dest,j);if(b[[j+1L]]!=0) break
          rr<-rr+dr;cc<-cc+dc;j<-.ra_chess_at(rr,cc)
        }
      }
    }
    for(j in dest) for(promotion in if(piece==1 && j%/%8 %in% c(0,7)) as.list(2:5) else list(NULL)) {
      if(.ra_chess_safe(d,i,j,promotion)) result[[length(result)+1L]]<-list(start=i,end=j,promotion=promotion)
    }
  }
  result
}
.ra_chess_key <- function(d) {
  ep<-NULL;p<-d$turn;s<-.ra_chess_sign(p)
  if(!is.null(d$ep)) {
    j<-d$ep;r<-j%/%8;c<-j%%8
    for(dc in c(-1,1)) {i<-.ra_chess_at(r-s,c+dc);if(!is.null(i) && d$board[[i+1L]]==s && .ra_chess_safe(d,i,j)) ep<-j}
  }
  paste0(paste0(letters[unlist(d$board)+7L],collapse=""),"|",p,"|",paste0(as.integer(unlist(d$rights)),collapse=""),"|",if(is.null(ep)) "-" else ep)
}
.ra_chess_transition <- function(d,m) {
  x<-.ra_chess_raw(d,m$start,m$end,m$promotion);x$history<-if(x$halfmove==0) list(.ra_chess_key(x)) else c(d$history,list(.ra_chess_key(x)))
  if(!is.null(d$offer) && d$offer!=d$turn) x["offer"]<-list(NULL)
  x
}
.ra_chess_dead <- function(d) {
  b<-unlist(d$board);ix<-which(b!=0 & abs(b)!=6);pieces<-b[ix]
  if(!length(ix)) return(TRUE)
  if(any(abs(pieces) %in% c(1,4,5))) return(FALSE)
  if(length(ix)==1) return(TRUE)
  all(abs(pieces)==3) && length(unique(((ix-1L)%/%8+(ix-1L)%%8)%%2))==1
}
.ra_chess_validate <- function(d) {
  b<-unlist(d$board);p<-d$turn
  if(!is.null(d$decision) && (d$decision$kind=="draw")!=is.null(d$decision$winner)) .ra_fail("Draw and winner decision mismatch",code="INVALID_STATE")
  if(sum(b==6)!=1 || sum(b==-6)!=1 || any(abs(b[c(1:8,57:64)])==1)) .ra_fail("Exactly one king per side and no back-rank pawns are required",code="INVALID_STATE")
  for(q in 0:1) {
    s<-.ra_chess_sign(q);ix<-which(b*s>0)-1L;pieces<-abs(b[ix+1L]);pawns<-sum(pieces==1)
    promotions<-max(0,sum(pieces==2)-2)+max(0,sum(pieces==4)-2)+max(0,sum(pieces==5)-1)
    for(c in 0:1) promotions<-promotions+max(0,sum(pieces==3 & (ix%/%8+ix%%8)%%2==c)-1)
    if(length(ix)>16 || pawns>8 || promotions>8-pawns) .ra_fail("Material requires more promotions than missing pawns allow",code="INVALID_STATE")
  }
  if(.ra_chess_attacked(d$board,which(b==6*.ra_chess_sign(1L-p))-1L,p)) .ra_fail("The side that just moved cannot leave its king in check",code="INVALID_STATE")
  for(k in 1:4) {
    king<-c(4,4,60,60)[k];rook<-c(7,0,63,56)[k];s<-c(1,1,-1,-1)[k]
    if(d$rights[[k]] && (b[king+1L]!=6*s || b[rook+1L]!=4*s)) .ra_fail("Castling right requires the king and rook on their original squares",code="INVALID_STATE")
  }
  if(!is.null(d$ep)) {
    j<-d$ep;s<-.ra_chess_sign(p)
    if(j%/%8!=if(p==0) 5 else 2) .ra_fail("Invalid en-passant target after a double pawn step",code="INVALID_STATE")
    if(b[j+1L]!=0 || b[j-8*s+1L]!=-s || b[j+8*s+1L]!=0 || d$halfmove!=0) .ra_fail("Invalid en-passant target after a double pawn step",code="INVALID_STATE")
  }
  if(length(d$history)!=d$halfmove+1 || !length(d$history) || tail(unlist(d$history),1)!=.ra_chess_key(d) || any(!grepl("^[a-m]{64}\\|[01]\\|[01]{4}\\|(-|[0-9]{1,2})$",unlist(d$history)))) .ra_fail("Invalid repetition history or halfmove count",code="INVALID_STATE")
}
.ra_chess_move_schema <- function() list(start=list(type="integer",minimum=0,maximum=63),end=list(type="integer",minimum=0,maximum=63),promotion=list(type="integer",minimum=2,maximum=5,nullable=TRUE))
.ra_chess_has_move <- function(d,m) any(vapply(.ra_chess_moves(d),function(x) x$start==m$start && x$end==m$end && (is.null(x$promotion) && is.null(m$promotion) || !is.null(x$promotion) && !is.null(m$promotion) && x$promotion==m$promotion),logical(1)))
.ra_chess_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(kind %in% c("resign","time_forfeit")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");loser<-p$player;other<-1L-loser;b<-unlist(d$board);no_mate<-.ra_chess_dead(d) || !any(b*.ra_chess_sign(other)>0 & abs(b)!=6)
    d$decision<-list(kind=if(no_mate) "draw" else kind,winner=if(no_mate) NULL else other);return(d)
  }
  if(kind=="adjudicate_dead_position") {.ra_fields(p,structure(list(),names=character()),"/params");d$decision<-list(kind="draw",winner=NULL);return(d)}
  if(kind=="accept_draw") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");player<-p$player
    if(is.null(d$offer) || d$offer==player) .ra_fail("Only the opponent may accept an outstanding offer")
    if(!is.null(action$actor) && action$actor!=state$players[[player+1L]]$id) .ra_fail("Wrong actor")
    d$decision<-list(kind="draw",winner=NULL);return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="offer_draw") {.ra_fields(p,structure(list(),names=character()),"/params");d$offer<-t;return(d)}
  schema<-.ra_chess_move_schema()
  if(kind=="claim_draw") {
    .ra_fields(p,list(reason=list(type="string",enum=list("threefold","fifty_moves")),move=list(type="object",properties=schema,required=as.list(names(schema)),nullable=TRUE)),"/params");x<-d
    if(!is.null(p$move)) {if(!.ra_chess_has_move(d,p$move)) .ra_fail("The intended move must be legal");x<-.ra_chess_transition(d,p$move)}
    if(if(p$reason=="threefold") sum(unlist(x$history)==.ra_chess_key(x))<3 else x$halfmove<100) .ra_fail("Requested draw condition is not met")
    d$decision<-list(kind="draw",winner=NULL);return(d)
  }
  if(kind!="move") .ra_fail("Unknown action")
  .ra_fields(p,schema,"/params")
  if(!.ra_chess_has_move(d,p)) .ra_fail("Move is illegal or leaves the moving king in check")
  .ra_chess_transition(d,p)
}
.ra_chess_result <- function(d) {
  if(!is.null(d$decision)) return(d$decision)
  moves<-.ra_chess_moves(d)
  if(!length(moves)) {
    check<-.ra_chess_attacked(d$board,which(unlist(d$board)==6*.ra_chess_sign(d$turn))-1L,1L-d$turn)
    return(list(kind=if(check) "checkmate" else "stalemate",winner=if(check) 1L-d$turn else NULL))
  }
  if(.ra_chess_dead(d)) return(list(kind="dead_material",winner=NULL))
  if(d$halfmove>=150) return(list(kind="seventy_five_moves",winner=NULL))
  if(sum(unlist(d$history)==.ra_chess_key(d))>=5) return(list(kind="fivefold_repetition",winner=NULL))
  list(kind=NULL,winner=NULL)
}
.ra_chess_legal <- function(state) {
  d<-state$data;actions<-c(lapply(.ra_chess_moves(d),function(m) list(type="move",params=m)),list(list(type="offer_draw",params=structure(list(),names=character()))))
  if(!is.null(d$offer)) actions<-c(actions,list(list(type="accept_draw",params=list(player=1L-d$offer))))
  for(reason in c("threefold","fifty_moves")) {
    ok<-if(reason=="threefold") sum(unlist(d$history)==.ra_chess_key(d))>=3 else d$halfmove>=100
    if(ok) actions<-c(actions,list(list(type="claim_draw",params=list(reason=reason,move=NULL))))
  }
  ms<-.ra_chess_move_schema();types<-list(list(type="claim_draw",params_schema=list(reason=list(type="string",enum=list("threefold","fifty_moves")),move=list(type="object",properties=ms,required=as.list(names(ms)),nullable=TRUE)),preconditions=list("Current or indicated legal next position satisfies the claimed condition."),external_observations=list()))
  for(k in c("resign","time_forfeit")) types<-c(types,list(list(type=k,params_schema=list(player=list(type="integer",minimum=0,maximum=1)),preconditions=list("Losing player identified; draw if the opponent cannot mate under the supported material checks."),external_observations=list("Official resignation or time-forfeit decision."))))
  types<-c(types,list(list(type="adjudicate_dead_position",params_schema=structure(list(),names=character()),preconditions=list("An adjudicator establishes that no legal continuation permits checkmate."),external_observations=list("Dead-position ruling beyond elementary material cases."))))
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn,in_check=.ra_chess_attacked(d$board,which(unlist(d$board)==6*.ra_chess_sign(d$turn))-1L,1L-d$turn)),completeness=FALSE,reason="legal_moves_and_adjudication")
}
.ra_chess_outcome <- function(state,pack) {
  result<-.ra_chess_result(state$data);kind<-result$kind;winner<-result$winner;done<-!is.null(kind)
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(is.null(winner)) "draw" else if(kind=="time_forfeit") "forfeit" else "win" else "none",winners=if(!is.null(winner)) list(state$players[[winner+1L]]$id) else list(),losers=if(!is.null(winner)) list(state$players[[2L-winner]]$id) else list(),placements=list(),reason=if(done) kind else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
