# make 2 vecs same length
make_vec_same_length = function(vec1,vec2){
  if(length(vec2) == length(vec1)){return(list(vec1,vec2))
  }else if(length(vec2) < length(vec1)){
    rows_to_add = length(vec1) - length(vec2)
    temp = rep(NA, each = rows_to_add)
    newvec = c(vec2, temp)
    return(list(vec1,newvec))
  }else if(length(vec2) > length(vec1)){
    rows_to_add = length(vec2) - length(vec1)
    temp = rep(NA, each = rows_to_add)
    newvec = c(vec1, temp)
    return(list(newvec, vec2))
  }
}

# correct usage
# vec_good = make_vec_same_length(vec1, vec2) %>>% `[`(1) %>% unlist()

# or for dates
# vec_good = do.call("c", make_vec_same_length(vec1, vec2) %>>% `[`(1) )
