.ra_mine_cell <- list(type="integer",minimum=0,maximum=80)
.ra_mine_layout <- list(type="array",items=.ra_mine_cell,minItems=10,maxItems=10)
.ra_mine_neighbors <- function(i) {
  r<-i%/%9;c<-i%%9;out<-numeric()
  for(dr in -1:1) for(dc in -1:1) if((dr!=0 || dc!=0) && r+dr>=0 && r+dr<9 && c+dc>=0 && c+dc<9) out<-c(out,(r+dr)*9+c+dc)
  out
}
.ra_mine_count <- function(d,i) sum(.ra_mine_neighbors(i) %in% unlist(d$mines))
.ra_mine_validate <- function(d) {
  if(any(unlist(d$revealed) & unlist(d$flags))) .ra_fail("A revealed cell cannot be flagged",code="INVALID_STATE")
  if(is.null(d$mines)) {
    if(any(unlist(d$revealed)) || any(unlist(d$flags)) || !is.null(d$exploded)) .ra_fail("Layout is not configured",code="INVALID_STATE")
    return(invisible(NULL))
  }
  if(length(unique(unlist(d$mines)))!=10) .ra_fail("Mine positions must be distinct",code="INVALID_STATE")
  hit<-Filter(function(i) d$revealed[[i+1L]],d$mines);expected<-if(is.null(d$exploded)) list() else list(d$exploded)
  if(length(hit)!=length(expected) || any(unlist(hit)!=unlist(expected))) .ra_fail("Explosion state mismatch",code="INVALID_STATE")
}
.ra_mine_reveal <- function(d,i) {
  if(i %in% unlist(d$mines)) {d$revealed[[i+1L]]<-TRUE;d$exploded<-i;return(d)}
  todo<-i
  while(length(todo)) {
    j<-tail(todo,1);todo<-head(todo,-1)
    if(d$revealed[[j+1L]] || d$flags[[j+1L]]) next
    d$revealed[[j+1L]]<-TRUE
    if(.ra_mine_count(d,j)==0) todo<-c(todo,Filter(function(k) !d$revealed[[k+1L]] && !d$flags[[k+1L]],.ra_mine_neighbors(j)))
  }
  d
}
.ra_mine_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(is.null(d$mines)) {
    if(action$type!="set_mines") .ra_fail("Supply the mine layout first")
    .ra_fields(p,list(mines=.ra_mine_layout),"/params");d$mines<-as.list(sort(unlist(p$mines)));return(d)
  }
  if(!action$type %in% c("reveal","toggle_flag","chord")) .ra_fail("Unknown action")
  .ra_fields(p,list(cell=.ra_mine_cell),"/params");i<-p$cell
  if(action$type=="toggle_flag") {
    if(d$revealed[[i+1L]]) .ra_fail("Revealed cells cannot be flagged")
    d$flags[[i+1L]]<-!d$flags[[i+1L]]
  } else if(action$type=="reveal") {
    if(d$revealed[[i+1L]] || d$flags[[i+1L]]) .ra_fail("Cell cannot be revealed")
    d<-.ra_mine_reveal(d,i)
  } else {
    around<-.ra_mine_neighbors(i)
    if(!d$revealed[[i+1L]] || .ra_mine_count(d,i)==0 || sum(unlist(d$flags[around+1L]))!=.ra_mine_count(d,i)) .ra_fail("Chording requires a revealed number and matching flag count")
    targets<-Filter(function(j) !d$revealed[[j+1L]] && !d$flags[[j+1L]],around)
    if(!length(targets)) .ra_fail("No hidden neighbors to chord")
    for(j in targets) {d<-.ra_mine_reveal(d,j);if(!is.null(d$exploded)) break}
  }
  d
}
.ra_mine_legal <- function(state) {
  d<-state$data
  if(is.null(d$mines)) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_mines",params_schema=list(mines=.ra_mine_layout),preconditions=list("Ten distinct mine positions on the 9-by-9 board."),external_observations=list("A fixed externally supplied layout."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_layout_required"))
  result<-list()
  for(i in 0:80) {
    if(!d$revealed[[i+1L]]) {
      if(!d$flags[[i+1L]]) result[[length(result)+1L]]<-list(type="reveal",params=list(cell=i))
      result[[length(result)+1L]]<-list(type="toggle_flag",params=list(cell=i))
    } else {
      around<-.ra_mine_neighbors(i)
      if(.ra_mine_count(d,i)>0 && sum(unlist(d$flags[around+1L]))==.ra_mine_count(d,i) && any(vapply(around,function(j) !d$revealed[[j+1L]] && !d$flags[[j+1L]],logical(1)))) result[[length(result)+1L]]<-list(type="chord",params=list(cell=i))
    }
  }
  result
}
.ra_mine_outcome <- function(state,pack) {
  d<-state$data;lost<-!is.null(d$exploded);won<-!is.null(d$mines) && !lost && all(unlist(d$revealed) | (0:80 %in% unlist(d$mines)));terminal<-lost || won
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(won) "win" else if(lost) "other" else "none",winners=if(won) list(state$players[[1]]$id) else list(),losers=if(lost) list(state$players[[1]]$id) else list(),placements=list(),reason=if(lost) "mine_revealed" else if(won) "all_safe_cells_revealed" else if(is.null(d$mines)) "awaiting_layout" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
