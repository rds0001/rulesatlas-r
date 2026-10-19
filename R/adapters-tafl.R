.ra_tafl_corners <- c(0L,6L,42L,48L)
.ra_tafl_directions <- list(c(1L,0L),c(-1L,0L),c(0L,1L),c(0L,-1L))
.ra_tafl_neighbor <- function(i,dx,dy) {x<-i%%7+dx;y<-i%/%7+dy;if(x>=0 && x<7 && y>=0 && y<7) y*7+x else NULL}
.ra_tafl_team <- function(piece) {if(piece==1) 0L else if(piece %in% c(2,3)) 1L else -1L}
.ra_tafl_moves <- function(d) {
  b<-d$board;t<-d$turn;out<-list()
  for(i in 0:48) {
    p<-b[[i+1L]]
    if(.ra_tafl_team(p)!=t) next
    for(delta in .ra_tafl_directions) {
      j<-.ra_tafl_neighbor(i,delta[1],delta[2])
      while(!is.null(j) && b[[j+1L]]==0) {
        if(p==3 || !j %in% c(.ra_tafl_corners,24L)) out<-c(out,list(c(i,j)))
        j<-.ra_tafl_neighbor(j,delta[1],delta[2])
      }
    }
  }
  out
}
.ra_tafl_key <- function(d) paste0(paste0(unlist(d$board),collapse=""),d$turn)
.ra_tafl_royal <- function(b,k,dest) {
  adjacent<-lapply(.ra_tafl_directions,function(delta) .ra_tafl_neighbor(k,delta[1],delta[2]))
  if(!dest %in% unlist(adjacent)) return(FALSE)
  if(k==24) return(all(vapply(adjacent,function(j) !is.null(j) && b[[j+1L]]==1,logical(1))))
  if(24 %in% unlist(adjacent)) return(all(vapply(adjacent,function(j) !is.null(j) && (j==24 || b[[j+1L]]==1),logical(1))))
  for(delta in list(c(1,0),c(0,1))) {
    a<-.ra_tafl_neighbor(k,delta[1],delta[2]);c<-.ra_tafl_neighbor(k,-delta[1],-delta[2])
    if(dest %in% c(a,c) && !is.null(a) && !is.null(c) && (b[[a+1L]]==1 || a %in% .ra_tafl_corners) && (b[[c+1L]]==1 || c %in% .ra_tafl_corners)) return(TRUE)
  }
  FALSE
}
.ra_tafl_result <- function(d) {
  b<-unlist(d$board)
  if(!3 %in% b) return(list(winner=0L,reason="king_captured"))
  if((match(3,b)-1L) %in% .ra_tafl_corners) return(list(winner=1L,reason="king_escaped"))
  if(!length(.ra_tafl_moves(d))) return(list(winner=1L-d$turn,reason="no_legal_move"))
  if(sum(unlist(d$history)==.ra_tafl_key(d))>=3) return(list(winner=NULL,reason="threefold_repetition"))
  list(winner=NULL,reason=NULL)
}
.ra_tafl_validate <- function(d) {
  b<-unlist(d$board)
  if(sum(b==1)>8 || sum(b==2)>4 || sum(b==3)>1) .ra_fail("Piece counts exceed the starting armies",code="INVALID_STATE")
  if(any(b[c(.ra_tafl_corners,24L)+1L] %in% c(1,2))) .ra_fail("Only the king may occupy corners or throne",code="INVALID_STATE")
  if(!length(d$history) || tail(unlist(d$history),1)!=.ra_tafl_key(d)) .ra_fail("History must end at the current board and turn",code="INVALID_STATE")
  if(any(!grepl("^[0123]{49}[01]$",unlist(d$history)))) .ra_fail("Invalid position history encoding",code="INVALID_STATE")
  if(sum(b==3)==0 && d$turn!=1) .ra_fail("A king capture must follow an attacker move",code="INVALID_STATE")
  if(sum(b==3)==1 && (match(3,b)-1L) %in% .ra_tafl_corners && d$turn!=0) .ra_fail("A king escape must follow a defender move",code="INVALID_STATE")
}
.ra_tafl_apply <- function(state,action) {
  d<-state$data;p<-action$params;t<-d$turn
  if(action$type!="move") .ra_fail("Unknown action")
  .ra_fields(p,list(source=list(type="integer",minimum=0,maximum=48),destination=list(type="integer",minimum=0,maximum=48)),"/params");i<-p$source;j<-p$destination
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(!any(vapply(.ra_tafl_moves(d),function(m) m[1]==i && m[2]==j,logical(1)))) .ra_fail("Move an owned piece orthogonally through empty squares; only the king may stop on a restricted square")
  b<-d$board;b[[j+1L]]<-b[[i+1L]];b[[i+1L]]<-0L;captured<-integer()
  for(delta in .ra_tafl_directions) {
    q<-.ra_tafl_neighbor(j,delta[1],delta[2])
    if(is.null(q) || !b[[q+1L]] %in% c(1,2) || .ra_tafl_team(b[[q+1L]])==t) next
    behind<-.ra_tafl_neighbor(q,delta[1],delta[2])
    if(!is.null(behind) && (.ra_tafl_team(b[[behind+1L]])==t || (b[[behind+1L]]==0 && behind %in% c(.ra_tafl_corners,24L)))) captured<-c(captured,q)
  }
  for(q in captured) b[[q+1L]]<-0L
  if(t==0 && 3 %in% unlist(b)) {
    king<-match(3,unlist(b))-1L
    if(.ra_tafl_royal(b,king,j)) {captured<-c(captured,king);b[[king+1L]]<-0L}
  }
  d$board<-b;d$turn<-1L-t
  if(length(captured)) d$history<-list()
  d$history<-c(d$history,list(.ra_tafl_key(d)));d
}
.ra_tafl_legal <- function(state) lapply(.ra_tafl_moves(state$data),function(m) list(type="move",params=list(source=m[1],destination=m[2])))
.ra_tafl_outcome <- function(state,pack) {
  r<-.ra_tafl_result(state$data);done<-!is.null(r$reason);w<-r$winner
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(is.null(w)) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) r$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
