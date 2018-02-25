#' Stage series
#'
#' @param x data.frame; input data
#' @param stages a vector of names of columns
#'
#' @return a stage.series object
#' @export
#'
#' @examples
as.stage.series <- function(x, stages) {
  factorised <- list()
  for (stage in stages) {
    # todo check existance
    factorised[[stage]] <- as.factor(x[, stage])
  }
  factorised <- data.frame(factorised)

  # Formulate flows
  flows <- data.frame(
    'StageSrc' = character(),
    'StageTar' = character(),
    'LevelSrc' = character(),
    'LevelTar' = character(),
    'Count' = integer()
  )

  # todo chisq test
  for (i in 1:(length(stages) - 1)) {
    stage.src <- stages[i]
    stage.tar <- stages[i + 1]
    transitions <-
      table(factorised[, stage.src], factorised[, stage.tar])
    transitions <- data.frame(transitions)
    colnames(transitions) <- c('LevelSrc', 'LevelTar', 'Count')
    transitions$StageSrc <- stage.src
    transitions$StageTar <- stage.tar
    flows <- rbind(flows, transitions)
  }

  flows <-
    flows[, c('StageSrc', 'LevelSrc', 'StageTar', 'LevelTar', 'Count')]

  # Formulate stocks
  stocks <- data.frame('Stage' = character(),
                       'Level' = character(),
                       'Count' = integer())

  for (stage in stages) {
    stock <- table(factorised[, stage])
    stock <- data.frame(stock)
    colnames(stock) <- c('Level', 'Count')
    stock$Stage <- stage
    stocks <- rbind(stocks, stock)
  }
  stocks <- stocks[, c('Stage', 'Level', 'Count')]

  # Formulate hierarchical structure
  hei <- lapply(stages, function(stage)
    levels(factorised[, stage]))
  names(hei) <- stages

  res <- list(
    Data = factorised,
    Flows = flows,
    Stocks = stocks,
    Hierarchy = hei
  )

  class(res) <- 'stage.series'
  res
}


print.stage.series <- function(ss) {
  cat('Number of sample:', nrow(ss$Data), '\n')
  cat('Stages:', names(ss$Hierarchy), '\n')
}




#' Sankey diagram with hierarchy.
#'
#' @param x.ss a stage.series object
#' @param bar.width width of bars
#' @param bar.dist.min the minimal distance between bars in the same stage
#' @param xlab label of stages
#' @param ylab label of states or levels
#' @param band.col colour of flow bands
#' @param band.alpha opacity of flow bands
#'
#' @return hierarchical Sankey with ggplot
#' @export
#'
#' @examples
hierarchical.sankey <-
  function(x.ss,
           bar.width = 10,
           bar.dist.min = 10,
           xlab = 'Stage',
           ylab = 'State',
           band.col = 'green',
           band.alpha = 0.3) {
    hier <- x.ss$Hierarchy
    # Prepare graphic parameters
    sts <- names(hier)
    sts.n <- length(sts)
    sts.size <- sapply(hier, length)
    sts.size.max <- max(sts.size)

    bar.height.total <- 50 * sts.size.max
    interval <- 80
    height <- bar.height.total + (sts.size.max - 1) * bar.dist.min
    width <- sts.n * bar.width + (sts.n - 1) * interval


    # locate stock bars
    stocks <- x.ss$Stocks
    stocks <- cbind(
      stocks,
      x0 = -1,
      x1 = -1,
      y0 = -1,
      y1 = -1
    )

    for (i in 1:sts.n) {
      lv <- sts[i]
      nodes.n <- sts.size[i]
      cnts <- stocks[stocks$Stage == lv, ]$Count
      hei <- bar.height.total * cnts / sum(cnts)
      pad <- (height - bar.height.total) / (nodes.n - 1)
      x0 <- (i - 1) * (bar.width + interval)
      y0 <- cumsum(c(0, (hei + pad)[-nodes.n]))

      stocks[stocks$Stage == lv, 'x0'] <- x0
      stocks[stocks$Stage == lv, 'y0'] <- y0
      stocks[stocks$Stage == lv, 'y1'] <- y0 + hei
    }
    stocks$x1 <- stocks$x0 + bar.width


    # locate flow bands
    flows <- x.ss$Flows
    flows <- cbind(
      flows,
      x0 = -1,
      x1 = -1,
      y0s = -1,
      y1s = -1,
      y0t = -1,
      y1t = -1
    )
    for (stage in sts) {
      flows[flows$StageSrc == stage, 'x0'] <-
        stocks[stocks$Stage == stage, 'x1']
      flows[flows$StageTar == stage, 'x1'] <-
        stocks[stocks$Stage == stage, 'x0']
    }

    dis <- function(y0, y1, ns) {
      int <- (y1 - y0) / sum(ns) * ns
      cint <- cumsum(int)
      cbind(y0 + cint - int, y0 + cint)
    }


    for (j in 1:nrow(stocks)) {
      stock <- stocks[j,]
      with(stock, {
        flow.sel <-
          flows[flows$StageSrc == Stage & flows$LevelSrc == Level,]
        if (nrow(flow.sel)) {
          ns <- flow.sel$Count
          flows[flows$StageSrc == Stage &
                  flows$LevelSrc == Level, c('y0s', 'y1s')] <<-
            dis(y0, y1, ns)
        }

        flow.sel <-
          flows[flows$StageTar == Stage & flows$LevelTar == Level,]
        if (nrow(flow.sel)) {
          ns <- flow.sel$Count
          flows[flows$StageTar == Stage &
                  flows$LevelTar == Level, c('y0t', 'y1t')] <<-
            dis(y0, y1, ns)
        }
      })
    }


    # Make the graph
    plt <- ggplot2::ggplot(data = stocks) +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = x0,
        xmax = x1,
        ymin = y0,
        ymax = y1,
        fill = Level
      )) +
      ggplot2::geom_text(ggplot2::aes(
        x = (x0 + x1) / 2,
        y = (y0 + y1) / 2,
        label = Level
      ))

    dat <- unique(stocks[c('Stage', 'x0', 'x1')])
    plt <-
      plt + ggplot2::scale_x_continuous(breaks = (dat$x0 + dat$x1) / 2, label = dat$Stage)

    n.step <- 50
    for (i in 1:nrow(flows)) {
      plt <- plt + with(flows[i,], {
        xx <- seq(-pi / 2, pi / 2, length.out = n.step)
        ys.upper <- y0s + (y0t - y0s) / 2 * (sin(xx) + 1)
        ys.lower <- y1s + (y1t - y1s) / 2 * (sin(xx) + 1)
        xs <- seq(x0, x1, length.out = n.step)
        dat <- data.frame(xs, ys.lower, ys.upper)
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = xs,
            ymin = ys.lower,
            ymax = ys.upper
          ),
          fill = band.col,
          alpha = band.alpha,
          data = dat
        )
      })
    }

    plt <- plt + ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(face = "bold")
      )

    plt
  }
