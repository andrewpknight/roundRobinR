#' Create dummy variables
#'
#' Generate a series of dummy variables for the actor and partner, following
#' the process designed by Kenny & Snijders (1999). This generates N dummies
#' for actor and N dummies for partner, where N is the maximum group size
#' in the dataset
#'
#' @param group.id string variable name that is the group identifier
#' @param act.id string variable name that is the actor identifier
#' @param part.id string variable name that is the partner identifier
#' @param d data.frame structured in directed dyadic long-form
#' @param include.self boolean indicating whether to include self ratings
#' @param merge.original boolean indicating whether to return the input
#' dataset with the new identifiers
#' @import data.table
#' @return data.frame giving a new set of unique identifiers and the dummy
#' variables needed to run an SRM using multilevel modeling
#' @export
#'
#' @examples
#' o = createDummies(group.id="groupId", act.id="actId", part.id="partId", d=sampleDyadData)
createDummies <- function(group.id, act.id, part.id, d, include.self=FALSE, merge.original=FALSE) {

   .N <- orig_group_id <- NULL
  # sort the dataset by group.id, act.id, part.id
  d <- d[with(d,order(d[,group.id], d[,act.id], d[, part.id])), ]

  # get just the identifiers
  d.sub <- d[,c(group.id, act.id, part.id)]

  # get the unique groups
  grps <- unique(d.sub[,c(group.id)])

  # create a unique actor id and a unique partner id (in case the input data set has these as nested values (rather than unique)
  d.sub$act_indiv_id <- paste(d.sub[,c(group.id)], d.sub[,c(act.id)], sep="_-")

  d.sub$part_indiv_id <- paste(d.sub[,c(group.id)], d.sub[,c(part.id)], sep="_-")

  # get the unique individuals as anyone who shows up on the actor or partner side
  acts <- unique(d.sub$act_indiv_id)
  parts <- unique(d.sub$part_indiv_id)
  all.indivs <- data.frame(unique(c(acts,parts)), stringsAsFactors=F)
  colnames(all.indivs) <- "string_indiv_id"

  # create a unique indiv_id number for everyone
  all.indivs$unique_indiv_id <- 1:length(all.indivs$string_indiv_id)

  # Split back apart into the group and indiv identifiers
  all.indivs$orig_group_id <- as.numeric(t(matrix(unlist(strsplit(all.indivs$string_indiv_id, split="_-")), nrow=2))[,1])

  all.indivs$orig_indiv_id <- as.numeric(t(matrix(unlist(strsplit(all.indivs$string_indiv_id, split="_-")), nrow=2))[,2])

  # Using all.indivs and the orig_group_id and the unique_indiv_id create the dyad dataset, with dummies

  # get the maximum group size
  d.dt <- data.table::data.table(all.indivs)
  agg <- data.frame(d.dt[,list(group_size = .N), by=list(orig_group_id)])
  max_group_size <- max(agg[,2])

  # Create the dyad dataset with an act_number and part_number variable, build it from the ground up
  grps <- unique(all.indivs$orig_group_id)
  grp <- grps[1]
  count <- 1
  for(grp in grps) {

    # Get the people in this group
    members <- unique(all.indivs[all.indivs$orig_group_id == grp, c("unique_indiv_id")])

    act_num <- 1
    for(act in members) {

      part_num <- 1
      for(part in members) {

        res.line <- c(grp, act, act_num, part, part_num)

        if(count == 1) {

          res <- res.line

        } else {

          res <- rbind(res, res.line)

        }
        part_num <- part_num + 1
        count <- count + 1

      }
      act_num <- act_num + 1

    }

  }
  res <- data.frame(res)
  colnames(res) <- c("orig_group_id", "unique_act_id", "act_num", "unique_part_id", "part_num")

  # create the dummies for each group
  res[,c(paste("a",1:max_group_size, sep=""), paste("p", 1:max_group_size, sep=""))] <- NA
  for(i in 1:max_group_size) {
    v <- paste("a", i, sep="")
    res[,v] <- ifelse(res$act_num == i, 1, 0)

    v <- paste("p", i, sep="")
    res[,v] <- ifelse(res$part_num == i, 1, 0)
  }

  # If self ratings should be excluded...
  if(!include.self) {

    res <- res[res$unique_act_id != res$unique_part_id, ]

  }

  # Add a unique dyad_id
  res$dyad_id <- NA
  count <- 1
  for(grp in grps) {

    # Get the actors and partners in this team
    actors <- unique(res[res$orig_group_id == grp, c("unique_act_id")])
    partners <- unique(res[res$orig_group_id == grp, c("unique_part_id")])
    indivs <- sort(unique(c(actors, partners)))
    for(a in 1:length(indivs)) {
      for(p in 1:length(indivs)) {
        if(a > p) {
          res$dyad_id <- ifelse((res$unique_act_id == indivs[a] & res$unique_part_id == indivs[p]) | (res$unique_act_id == indivs[p] & res$unique_part_id == indivs[a]), count, res$dyad_id)
          count <- count + 1
        }
      }
    }
  }

  # Add the original identifiers to this data.frame
  res1 <- merge(res, all.indivs[,c("orig_group_id", "unique_indiv_id", "orig_indiv_id")], by.x=c("orig_group_id", "unique_act_id"), by.y=c("orig_group_id", "unique_indiv_id"), all.x=T)
  colnames(res1)[length(res1)] <- c("orig_act_id")

  res2 <- merge(res1, all.indivs[,c("orig_group_id", "unique_indiv_id", "orig_indiv_id")], by.x=c("orig_group_id", "unique_part_id"), by.y=c("orig_group_id", "unique_indiv_id"), all.x=T)
  colnames(res2)[length(res2)] <- c("orig_part_id")

  colnames(res2) <- c(group.id, "pdSRM_part_id", "pdSRM_act_id", "pdSRM_act_num", "pdSRM_part_num",paste("a",1:max_group_size, sep=""), paste("p", 1:max_group_size, sep=""), "pdSRM_dyad_id", act.id, part.id)

  # Order the rows columns in a nicer way

  res3 <- res2[with(res2,order(res2[,group.id], res2[,"pdSRM_act_id"], res2[, "pdSRM_part_id"])),c(group.id, act.id, part.id, "pdSRM_act_id", "pdSRM_part_id", "pdSRM_dyad_id", "pdSRM_act_num", "pdSRM_part_num", paste("a",1:max_group_size, sep=""), paste("p", 1:max_group_size, sep=""))]

  # Merge with the original dataset
  if(merge.original) {
    res4 <- merge(res3, d, by=c(group.id, act.id, part.id), all.x=T)
    return(res4)
  } else {
    return(res3)
  }
}
