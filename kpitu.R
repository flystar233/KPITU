library(R6)

# 定义solution类
solution <- R6Class("solution",
  public = list(
    index = -1,
    objective = NULL,
    neighbor = list(),
    contribution = -1,
    repoints = NULL,
    left = -1,
    initialize = function(m) {
      self$objective = rep(0, m)
    }
  )
)

# 定义reference_point类
reference_point <- R6Class("reference_point",
  public = list(
    direction = NULL,
    neighbor = list(),
    associate = list()
  )
)

# 定义转移函数
transfer <- function(A, B) {
  if (sum(A$objective - B$objective) > 0) {
    return(1)
  } else {
    return(0)
  }
}

# 定义选择函数
select <- function(A, B) {
  return(sum(A$objective - B$objective))
}

Associate <- function(p, w) {
  obj_mat <- sapply(p, function(i) i$objective)
  w_mat <- t(sapply(w, function(i) i$direction))
  d_mat <- w_mat %*% obj_mat
  d_mat <- sweep(d_mat, 1, sqrt(rowSums(w_mat^2)), "/")

  length2 <- colSums(obj_mat^2)
  for (i in 1:ncol(obj_mat)) {
    d_2 <- pmax(length2[i] - d_mat[, i]^2, 0)
    d_mat[, i] <- d_2
    min_index <- which.min(d_2)
    w[[min_index]]$associate <- append(w[[min_index]]$associate, list(p[[i]]))
    p[[i]]$repoints <- w[[min_index]]
  }
  return(list(p, w))
}

#  for (i in 1:ncol(obj_mat)) {
#    length2 <- sum(obj_mat[, i]^2)
#    for (j in 1:length(d_mat[, i])) {
#      d_2 <- length2 - d_mat[j, i]^2
#      if (d_2 < 0) {
#        d_mat[j, i] <- 0
#      } else {
#        d_mat[j, i] <- d_2
#      }
#    }
#    w[[which.min(d_mat[, i])]]$associate <- append(w[[which.min(d_mat[, i])]]$associate, list(p[[i]]))
#    p[[i]]$repoints <- w[[which.min(d_mat[, i])]]
#  }

main_function <- function(data, K) {
  points <- data
  num <- nrow(points)
  dim <- ncol(points)
  
  for (i in 1:dim) {
    Min <- min(points[, i])
    Max <- max(points[, i])
    if (Max != Min) {
      points[, i] <- (points[, i] - Min) / (Max - Min)
    } else {
      points[, i] <- Min
    }
  } #归一化数据
  
  div <- 0
  H <- 0
  while (H < num) {
    div <- div + 1
    H <- factorial(div + dim - 1) / (factorial(div) * factorial(dim - 1))
  }
  div <- div - 1
  if (div >= 20) {
    div <- 20
  }
  
  list_range <- seq(0, 1, length.out = div + 1)
  direction <- list()
  w_generator <- function(now_dim, now_sum, now_array) {
    if (now_dim == 1) {
      for (i in list_range) {
        temp_array <- now_array
        if (round(i + now_sum - 1, 5) == 0) {
          temp_array <- c(temp_array, i)
          direction <<- c(direction, list(temp_array))
        }
      }
    } else {
      for (i in list_range) {
        temp_array <- now_array
        if (round(i + now_sum - 1, 5) <= 0) {
          temp_array <- c(temp_array, i)
          w_generator(now_dim - 1, now_sum + i, temp_array)
        }
      }
    }
  }
  w_generator(dim, 0, vector())
  direction <- do.call(rbind, direction)
  Repoints <- replicate(length(direction)/2, reference_point$new(), simplify = FALSE)
  for (i in 1:(length(direction)/2)) {
    Repoints[[i]]$direction <- direction[i, ]
    direction_i <- matrix(rep(direction[i, ], times = dim(direction)[1]), nrow = dim(direction)[1], ncol = dim(direction)[2], byrow = TRUE)
    distance_list <- colSums(sweep(t(direction), 2, direction_i, "-")^2)
    distance_sort <- order(distance_list)
    temp_min_d <- distance_list[distance_sort[2]]
    current_index <- 2
    while (round(temp_min_d - distance_list[distance_sort[current_index]], 5) == 0) {
      Repoints[[i]]$neighbor <- append(Repoints[[i]]$neighbor, list(Repoints[[distance_sort[current_index]]]))
      current_index <- current_index + 1
    }
  }
  P <- replicate(num, solution$new(dim), simplify = FALSE)
  for (i in 1:num) {
    P[[i]]$index <- i
    P[[i]]$objective <- points[i, ]
  }
  result  <- Associate(P, Repoints)
  P <- result[[1]]
  Repoints <- result[[2]]
  
  
  for (im in 1:num) {
        P[[im]]$neighbor <- list()
        for (i1 in P[[im]]$repoints$associate) {
          if (!any(sapply(P[[im]]$neighbor, identical, i1)) && !identical(i1, P[[im]])) {
            P[[im]]$neighbor <- append(P[[im]]$neighbor, list(i1))
          }
        }
        for (i2 in P[[im]]$repoints$neighbor) {
          for (i3 in i2$associate) {
            if (!any(sapply(P[[im]]$neighbor, identical, i3))) {
              P[[im]]$neighbor <- append(P[[im]]$neighbor, list(i3))
            }
          }
        }
  }
  
  Current <- P
  Internal <- list()
  Peripheral <- list()
  Reserve <- list()
  for (j in Current) {
    for (mm in j$neighbor) {
      if (transfer(j, mm) == 1) {
        j$left <- 1
      }
    }
    if (j$left != 1) {
      if (min(j$repoints$direction) == 0 && !(dim %in% c(5, 8, 10))) {
        Peripheral <- append(Peripheral, list(j))
      } else {
        Internal <- append(Internal, list(j))
      }
    } else {
      Reserve <- append(Reserve, list(j))
    }
  }
  Peripheral_index <- as.vector(sapply(Peripheral, function(j) j$index))
  Internal_index <- as.vector(sapply(Internal, function(j) j$index))
  
  if (length(Internal_index) == 0) {
    if (length(Peripheral_index) < K){
        if (dim == 8 || dim == 10){
            add_index <- c()
            neighbor_num <- length(Peripheral_index)
            for (i in Peripheral) {
              neighbor_num <- neighbor_num + length(i$neighbor)
              for (j in i$neighbor) {
                add_index <- c(add_index, j$index)
              }
            }
            if (neighbor_num <= K) {
              Internal_index <- c(Peripheral_index, add_index)
            } else {
              gain_list_all <- c()
              for (i in Peripheral) {
                gain_value <- 0
                gain_list <- c()
                for (j in i$neighbor) {
                  for (k in i$neighbor) {
                    gain_value <- gain_value + select(j, k)
                  }
                  gain_list <- c(gain_list, gain_value)
                }
                gain_sort <- order(gain_list)
                for (j in seq_along(gain_sort)) {
                  gain_list[gain_sort[j]] <- length(gain_sort) - j
                }
                gain_list_all <- c(gain_list_all, gain_list)
              }
              gain_sort_all <- order(gain_list_all)
              select_index <- sapply(gain_sort_all[1:(K-length(Peripheral_index))], function(k) add_index[k])
              Internal_index <- c(Peripheral_index, select_index)
            }

        }else {
         for (i in Peripheral) {
            gain_value <- 0
            gain_list <- c()
            for (j in i$neighbor) {
              for (k in i$neighbor) {
                 gain_value <- gain_value + select(j, k)
              }
              gain_list <- c(gain_list, gain_value)
            }

            SK <- order(gain_list)
            SK <- SK[1:ceiling(K / length(Internal) - 1)]

            for (sel_index in SK) {
              Peripheral_index <- c(Peripheral_index, i$neighbor[[sel_index]]$index)
            }
          }
          if (length(Peripheral_index) > K) {
            Internal_index <- Peripheral_index[1:K]
          }
        }           
    }
    else if (length(Peripheral_index) > K) {
        gain_list <- c()
        for (i in Peripheral) {
          gain_value <- 0
          for (j in Peripheral) {
            gain_value <- gain_value + select(i, j)
          }
          gain_list <- c(gain_list, gain_value)
        }

        SK <- order(gain_list)
        SK <- SK[1:K]
        Internal_index <- Peripheral_index[SK]
      }else {
        Internal_index <- Peripheral_index
      }

  }else if (length(Internal_index) < K) {
        for (i in Internal) {
          gain_value <- 0
          gain_list <- c()
          for (j in i$neighbor) {
            for (k in i$neighbor) {
              gain_value <- gain_value + select(j, k)
            }
            gain_list <- c(gain_list, gain_value)
          }

          SK <- order(gain_list)
          SK <- SK[1:ceiling(K / length(Internal) - 1)]

          for (sel_index in SK) {
            Internal_index <- c(Internal_index, i$neighbor[[sel_index]]$index)
          }
        }
        if (length(Internal_index) > K) {
          Internal_index <- Internal_index[1:K]
        }
  }else{
  }
    internal_points <- data[Internal_index, ]
    return(internal_points)
}
