sanitize_problem_text <- function(text) {
  utf8_text <- suppressWarnings(iconv(text, from = "", to = "UTF-8", sub = " "))

  if (is.na(utf8_text)) {
    utf8_text <- text
  }

  utf8_text <- gsub("\u03b1", " alpha ", utf8_text, fixed = TRUE)
  utf8_text <- gsub("\u0391", " alpha ", utf8_text, fixed = TRUE)

  converted <- suppressWarnings(iconv(utf8_text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " "))

  if (is.na(converted)) {
    return(utf8_text)
  }

  converted
}

normalize_claim_text <- function(text) {
  text <- sanitize_problem_text(text)
  text <- gsub("%", " percent ", text, fixed = TRUE)
  text <- gsub("[\u2212\u2013\u2014]", "-", text)
  text <- tolower(text)
  text <- gsub("[^a-z0-9 -]+", " ", text)
  trimws(gsub("\\s+", " ", text))
}

clean_claim_text <- function(text) {
  cleaned <- trimws(text)
  cleaned <- sub("^that\\s+", "", cleaned, ignore.case = TRUE)
  cleaned <- sub("\\.+$", "", cleaned)
  cleaned
}

contains_claim_pattern <- function(text, patterns) {
  any(vapply(patterns, function(pattern) grepl(pattern, text, perl = TRUE), logical(1)))
}

infer_difference_orientation <- function(difference_definition) {
  normalized <- normalize_claim_text(difference_definition)

  second_minus_first_patterns <- c(
    "^second\\-?minus\\-?first$",
    "^second minus first$",
    "^x2\\s*-\\s*x1$",
    "^after\\s*-\\s*before$",
    "^with\\s*-\\s*without$",
    "after.*-.*before",
    "with.*-.*without",
    "treatment.*-.*before",
    "after.*-.*baseline",
    "post.*-.*pre"
  )

  first_minus_second_patterns <- c(
    "^first\\-?minus\\-?second$",
    "^first minus second$",
    "^x1\\s*-\\s*x2$",
    "^before\\s*-\\s*after$",
    "^without\\s*-\\s*with$",
    "before.*-.*after",
    "without.*-.*with",
    "baseline.*-.*after",
    "pre.*-.*post"
  )

  if (contains_claim_pattern(normalized, second_minus_first_patterns)) {
    return(list(
      factor = 1,
      label = "second_minus_first",
      readable = "d = second - first"
    ))
  }

  if (contains_claim_pattern(normalized, first_minus_second_patterns)) {
    return(list(
      factor = -1,
      label = "first_minus_second",
      readable = "d = first - second"
    ))
  }

  list(
    factor = 1,
    label = "second_minus_first",
    readable = "d = second - first"
  )
}

infer_claim_direction_details <- function(claim_text, difference_definition = "second_minus_first", parameter_symbol = "mu_d") {
  normalized_claim <- normalize_claim_text(claim_text)
  cleaned_claim <- clean_claim_text(claim_text)
  orientation <- infer_difference_orientation(difference_definition)

  negative_patterns <- c(
    "\\breduc",
    "\\bdecreas",
    "\\blower",
    "\\bless\\b",
    "\\bfewer\\b",
    "\\bsmaller\\b",
    "\\bshorter\\b",
    "\\bdiminish",
    "\\bdeclin",
    "\\bdrop\\b"
  )

  positive_patterns <- c(
    "\\bincreas",
    "\\bhigher\\b",
    "\\bgreater\\b",
    "\\bmore\\b",
    "\\blarger\\b",
    "\\blonger\\b",
    "\\bimprov",
    "\\bgain",
    "\\brais",
    "\\benhanc"
  )

  two_sided_patterns <- c(
    "\\bdiffer",
    "\\bchange",
    "\\beffect\\b",
    "\\baffect",
    "\\bassociation\\b",
    "\\brelationship\\b"
  )

  null_patterns <- c(
    "\\bequal\\b",
    "\\bthe same\\b",
    "\\bno difference\\b",
    "\\bno effect\\b",
    "\\bno change\\b",
    "\\bunchanged\\b"
  )

  has_negative <- contains_claim_pattern(normalized_claim, negative_patterns)
  has_positive <- contains_claim_pattern(normalized_claim, positive_patterns)
  has_two_sided <- contains_claim_pattern(normalized_claim, two_sided_patterns)
  has_null <- contains_claim_pattern(normalized_claim, null_patterns)

  if (has_null && !has_negative && !has_positive && !has_two_sided) {
    claim_location <- "null"
    tail <- "two"
    semantic_label <- "no-change / equality wording"
    reason <- "Equality wording usually points to the null hypothesis."
  } else if ((has_negative && has_positive) || has_two_sided) {
    claim_location <- "alternative"
    tail <- "two"
    semantic_label <- "change / difference wording"
    reason <- "The claim language suggests a non-directional change, so use a two-tailed alternative."
  } else if (has_negative) {
    claim_location <- "alternative"
    semantic_label <- "decrease wording"
    if (orientation$factor == 1) {
      tail <- "left"
      reason <- paste0(cleaned_claim, " implies a decrease, and ", orientation$readable, " makes decreases negative.")
    } else {
      tail <- "right"
      reason <- paste0(cleaned_claim, " implies a decrease, and ", orientation$readable, " makes decreases positive.")
    }
  } else if (has_positive) {
    claim_location <- "alternative"
    semantic_label <- "increase wording"
    if (orientation$factor == 1) {
      tail <- "right"
      reason <- paste0(cleaned_claim, " implies an increase, and ", orientation$readable, " makes increases positive.")
    } else {
      tail <- "left"
      reason <- paste0(cleaned_claim, " implies an increase, and ", orientation$readable, " makes increases negative.")
    }
  } else {
    claim_location <- "alternative"
    tail <- "two"
    semantic_label <- "ambiguous wording"
    reason <- "The claim wording is ambiguous, so the helper defaults to a two-tailed alternative. Check manually if needed."
  }

  hypothesis <- switch(
    tail,
    left = paste0("Ha: ", parameter_symbol, " < 0"),
    right = paste0("Ha: ", parameter_symbol, " > 0"),
    two = paste0("Ha: ", parameter_symbol, " != 0")
  )

  list(
    claim_text = cleaned_claim,
    difference_definition = orientation$readable,
    tail = tail,
    hypothesis = hypothesis,
    claim_location = claim_location,
    semantic_label = semantic_label,
    reason = reason
  )
}

validate_decision_text <- function(decision) {
  if (!decision %in% c("Reject H0", "Fail to reject H0")) {
    stop("decision must be either 'Reject H0' or 'Fail to reject H0'.", call. = FALSE)
  }

  decision
}

build_hypothesis_conclusion_sentence <- function(alpha, claim_text, decision, claim_location = "alternative") {
  alpha <- ensure_probability(alpha, "alpha")
  decision <- validate_decision_text(decision)
  claim_location <- match.arg(claim_location, c("alternative", "null"))
  cleaned_claim <- clean_claim_text(claim_text)

  opening <- if (decision == "Reject H0") {
    "We reject the null hypothesis and conclude that "
  } else {
    "We fail to reject the null hypothesis and conclude that "
  }

  evidence_phrase <- if (claim_location == "alternative") {
    if (decision == "Reject H0") {
      "there is sufficient evidence"
    } else {
      "there is insufficient evidence"
    }
  } else {
    if (decision == "Reject H0") {
      "there is sufficient evidence"
    } else {
      "there is insufficient evidence"
    }
  }

  verb_phrase <- if (claim_location == "alternative") {
    "to support the claim that "
  } else {
    "to reject the claim that "
  }

  paste0(
    opening,
    evidence_phrase,
    " at a ",
    format_number(alpha),
    " level of significance ",
    verb_phrase,
    cleaned_claim,
    "."
  )
}

infer_claim_tail_worked <- function(claim_text, difference_definition = "second_minus_first", parameter_symbol = "mu_d") {
  details <- infer_claim_direction_details(claim_text, difference_definition, parameter_symbol)

  steps <- c(
    paste0("Claim = ", details$claim_text),
    paste0("Difference definition = ", details$difference_definition),
    paste0("Detected wording type = ", details$semantic_label),
    paste0("Reasoning = ", details$reason),
    paste0("Use a ", tail_description(details$tail), " alternative hypothesis."),
    details$hypothesis
  )

  new_worked_calculation(
    title = "Claim Direction and Ha",
    notation = parameter_symbol,
    formula = "Use the claim wording and the direction of d to infer the tail of Ha.",
    steps = steps,
    answer = paste0("Tail = ", details$tail, "; ", details$hypothesis),
    result = details
  )
}

hypothesis_conclusion_worked <- function(alpha, decision, claim_text, claim_location = NULL) {
  alpha <- ensure_probability(alpha, "alpha")
  decision <- validate_decision_text(decision)

  if (is.null(claim_location)) {
    claim_location <- infer_claim_direction_details(claim_text)$claim_location
  }

  conclusion <- build_hypothesis_conclusion_sentence(alpha, claim_text, decision, claim_location)

  steps <- c(
    paste0("Decision = ", decision),
    paste0("Claim treated as part of the ", claim_location, " hypothesis."),
    paste0("Alpha = ", format_number(alpha)),
    paste0("Conclusion sentence = ", conclusion)
  )

  new_worked_calculation(
    title = "Hypothesis Test Conclusion",
    formula = "Match the decision with whether the claim belongs to Ha or H0.",
    steps = steps,
    answer = conclusion,
    result = list(decision = decision, claim_location = claim_location, conclusion = conclusion)
  )
}

paired_t_test_from_claim_worked <- function(
  before,
  after,
  alpha,
  claim_text,
  difference_definition = "second_minus_first",
  digits = RAW_DISPLAY_DIGITS
) {
  details <- infer_claim_direction_details(claim_text, difference_definition, parameter_symbol = "mu_d")
  alpha <- ensure_probability(alpha, "alpha")
  d <- paired_differences(before, after)

  n <- length(d)
  d_bar <- mean(d)
  s_d <- sd(d)
  t_value <- (d_bar - 0) / (s_d / sqrt(n))
  df <- n - 1
  p_value <- tail_p_value(t_value, details$tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(alpha, details$tail, distribution = "t", df = df)
  decision <- if (p_value < alpha) "Reject H0" else "Fail to reject H0"
  conclusion <- build_hypothesis_conclusion_sentence(alpha, claim_text, decision, details$claim_location)

  steps <- c(
    paste0("Claim = ", details$claim_text),
    paste0("Difference definition = ", details$difference_definition),
    paste0("Detected wording type = ", details$semantic_label),
    paste0("Use a ", tail_description(details$tail), " alternative: ", details$hypothesis),
    paste0("Differences (after - before) = {", join_values(d, digits), "}"),
    paste0("d_bar = ", format_number(d_bar, digits)),
    paste0("s_d = ", format_number(s_d, digits)),
    paste0("df = ", df),
    paste0("t = (d_bar - 0) / (s_d / sqrt(n)) = ", format_number(t_value, digits)),
    if (details$tail == "two") {
      paste0(
        "Critical values = ",
        format_interval(critical_value["lower"], critical_value["upper"], digits)
      )
    } else {
      paste0("Critical value = ", format_number(critical_value, digits))
    },
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision),
    paste0("Conclusion sentence = ", conclusion)
  )

  new_worked_calculation(
    title = "Paired t Test From Claim Text",
    notation = "t",
    formula = "Infer Ha from the claim, then use t = d_bar / (s_d / sqrt(n)).",
    steps = steps,
    answer = paste0(
      "Tail = ",
      details$tail,
      "; t = ",
      format_number(t_value, digits),
      "; P-value = ",
      format_number(p_value, digits),
      "; ",
      conclusion
    ),
    result = list(
      tail = details$tail,
      hypothesis = details$hypothesis,
      statistic = t_value,
      p_value = p_value,
      critical_value = critical_value,
      decision = decision,
      conclusion = conclusion,
      df = df
    )
  )
}

extract_problem_alpha <- function(problem_text) {
  cleaned <- gsub("[\r\n]+", " ", sanitize_problem_text(problem_text))
  patterns <- c(
    "(?:at\\s+the|at\\s+a)\\s+([0-9]*\\.?[0-9]+)\\s+level of significance",
    "alpha\\s*=\\s*([0-9]*\\.?[0-9]+)",
    "use\\s*=\\s*([0-9]*\\.?[0-9]+)"
  )

  for (pattern in patterns) {
    match <- regmatches(
      cleaned,
      regexec(pattern, cleaned, ignore.case = TRUE, perl = TRUE)
    )[[1]]

    if (length(match) >= 2) {
      return(as.numeric(match[2]))
    }
  }

  NULL
}

extract_problem_difference_definition <- function(problem_text) {
  cleaned <- gsub("[\u2212\u2013\u2014]", "-", gsub("[\r\n]+", " ", sanitize_problem_text(problem_text)))
  patterns <- c(
    "(?:let|use\\s+the\\s+formula)\\s+([a-z][a-z0-9_]*)\\s*=\\s*([^\\.]+?)(?=(?:\\.|$))",
    "(?:define|using)\\s+([a-z][a-z0-9_]*)\\s*=\\s*([^\\.]+?)(?=(?:\\.|$))"
  )

  for (pattern in patterns) {
    match <- regmatches(
      cleaned,
      regexec(pattern, cleaned, ignore.case = TRUE, perl = TRUE)
    )[[1]]

    if (length(match) >= 3) {
      variable_name <- trimws(match[2])
      expression <- trimws(match[3])
      return(paste0(variable_name, " = ", expression))
    }
  }

  NULL
}

extract_ordered_years <- function(problem_text) {
  cleaned <- sanitize_problem_text(problem_text)
  year_matches <- gregexpr("\\b(?:19|20)\\d{2}\\b", cleaned, perl = TRUE)
  years <- regmatches(cleaned, year_matches)[[1]]

  if (length(years) == 0) {
    return(character(0))
  }

  years[!duplicated(years)]
}

infer_problem_difference_definition <- function(problem_text, test_family = NULL, explicit_definition = NULL) {
  if (is.null(explicit_definition)) {
    explicit_definition <- extract_problem_difference_definition(problem_text)
  }

  if (!is.null(explicit_definition)) {
    readable <- if (grepl("^\\s*d\\s*=", explicit_definition, ignore.case = TRUE, perl = TRUE)) {
      infer_difference_orientation(explicit_definition)$readable
    } else {
      explicit_definition
    }

    return(list(
      raw = explicit_definition,
      readable = readable,
      source = "explicit"
    ))
  }

  if (!is.null(test_family) && test_family != "paired_mean") {
    return(list(
      raw = NULL,
      readable = NULL,
      source = "not_needed"
    ))
  }

  normalized <- normalize_claim_text(problem_text)

  inferred_pairs <- list(
    list(
      patterns = c("before and after", "before .* after", "recorded before .* after", "measured before .* after"),
      raw = "d = after - before",
      readable = "d = after - before"
    ),
    list(
      patterns = c("without and with", "without .* with", "sleep without .* with", "hours of sleep without .* with"),
      raw = "d = with - without",
      readable = "d = with - without"
    ),
    list(
      patterns = c("\\bpre\\b.*\\bpost\\b", "\\bpretest\\b.*\\bposttest\\b", "\\bbefore treatment\\b.*\\bafter treatment\\b"),
      raw = "d = post - pre",
      readable = "d = post - pre"
    ),
    list(
      patterns = c("population 1", "population 2"),
      raw = "d = population 2 - population 1",
      readable = "d = population 2 - population 1"
    ),
    list(
      patterns = c("model a", "model b"),
      raw = "d = model b - model a",
      readable = "d = model b - model a"
    )
  )

  for (pair_rule in inferred_pairs) {
    if (contains_claim_pattern(normalized, pair_rule$patterns)) {
      return(list(
        raw = pair_rule$raw,
        readable = pair_rule$readable,
        source = "inferred_from_prompt"
      ))
    }
  }

  years <- extract_ordered_years(problem_text)
  if (length(years) >= 2 && contains_claim_pattern(normalized, c("each year", "from each year", "between these two years", "between .* and .* years?"))) {
    return(list(
      raw = paste0("d = ", years[2], " - ", years[1]),
      readable = paste0("d = ", years[2], " - ", years[1]),
      source = "inferred_from_prompt"
    ))
  }

  list(
    raw = "second_minus_first",
    readable = "d = second - first",
    source = "default_paired_order"
  )
}

split_problem_sentences <- function(problem_text) {
  cleaned <- trimws(gsub("[\r\n]+", " ", sanitize_problem_text(problem_text)))
  sentences <- unlist(strsplit(cleaned, "(?<=[.!?])\\s+", perl = TRUE))
  sentences <- trimws(sentences)
  sentences[nzchar(sentences)]
}

score_claim_sentence <- function(sentence) {
  normalized <- normalize_claim_text(sentence)
  score <- 0

  if (contains_claim_pattern(normalized, c(
    "claims? that",
    "believes? that",
    "suspects? that",
    "contends? that",
    "argues? that",
    "maintains? that",
    "hypothesizes? that"
  ))) {
    score <- score + 8
  }

  if (contains_claim_pattern(normalized, c(
    "determine if",
    "determine whether",
    "would like to determine if",
    "would like to determine whether",
    "wants to determine if",
    "wants to determine whether",
    "conclude that",
    "conclude whether",
    "significant difference",
    "there is a difference",
    "there is no difference"
  ))) {
    score <- score + 7
  }

  if (contains_claim_pattern(normalized, c(
    "\\bdifference\\b",
    "\\bdifferent\\b",
    "\\bchanged\\b",
    "\\bincrease",
    "\\bdecrease",
    "\\breduce",
    "\\bimprov",
    "\\bgreater than\\b",
    "\\bless than\\b",
    "\\bat least\\b",
    "\\bat most\\b",
    "\\bequal to\\b",
    "\\bmore than\\b"
  ))) {
    score <- score + 5
  }

  if (contains_claim_pattern(normalized, c(
    "level of significance",
    "alpha",
    "results are given",
    "the following table",
    "assum",
    "randomly selected",
    "recorded",
    "measured"
  ))) {
    score <- score - 3
  }

  score
}

extract_problem_claim_text <- function(problem_text) {
  cleaned <- trimws(gsub("[\r\n]+", " ", sanitize_problem_text(problem_text)))
  patterns <- c(
    "(?:claims?|believes?|suspects?|contends?|argues?|maintains?|hypothesizes?)\\s+that\\s+([^.!?]+)",
    "(?:wants\\s+to\\s+know\\s+(?:if|whether)|would\\s+like\\s+to\\s+know\\s+(?:if|whether)|is\\s+interested\\s+in\\s+whether|seeks\\s+to\\s+determine\\s+(?:if|whether)|would\\s+like\\s+to\\s+determine\\s+(?:if|whether)|wants\\s+to\\s+determine\\s+(?:if|whether)|determine\\s+(?:if|whether))\\s+([^.!?]+)",
    "can\\s+[^.!?]*?conclude\\s+that\\s+([^.!?]+)",
    "can\\s+[^.!?]*?conclude\\s+whether\\s+([^.!?]+)",
    "claim\\s+that\\s+([^.!?]+)"
  )

  for (pattern in patterns) {
    match <- regmatches(
      cleaned,
      regexec(pattern, cleaned, ignore.case = TRUE, perl = TRUE)
    )[[1]]

    if (length(match) >= 2) {
      return(clean_claim_text(match[2]))
    }
  }

  sentences <- split_problem_sentences(problem_text)
  if (length(sentences) > 0) {
    sentence_scores <- vapply(sentences, score_claim_sentence, numeric(1))
    best_index <- which.max(sentence_scores)

    if (length(best_index) == 1 && sentence_scores[best_index] > 0) {
      return(clean_claim_text(sentences[best_index]))
    }
  }

  clean_claim_text(cleaned)
}

contains_paired_design_cues <- function(problem_text) {
  normalized <- normalize_claim_text(problem_text)

  contains_claim_pattern(normalized, c(
    "paired differences",
    "paired difference",
    "matched pairs",
    "matched pair",
    "before and after",
    "after and before",
    "same subject",
    "same subjects",
    "same participant",
    "same participants",
    "same individual",
    "same individuals",
    "same driver",
    "same drivers",
    "same patient",
    "same patients",
    "same county",
    "same counties",
    "each .* both",
    "asked to .* both",
    "drive both",
    "tested both",
    "measured twice",
    "each county .* each year",
    "for each county .* from each year",
    "each participant .* before .* after",
    "each driver .* both models"
  ))
}

detect_problem_test_family <- function(problem_text, difference_definition = NULL) {
  normalized <- normalize_claim_text(problem_text)
  normalized_difference <- if (is.null(difference_definition)) "" else normalize_claim_text(difference_definition)

  if (grepl("^d\\s*=", normalized_difference, perl = TRUE) || grepl("\\blet\\s+d\\s*=", normalized, perl = TRUE)) {
    return("paired_mean")
  }

  if (contains_claim_pattern(normalized, c("correlation", "linear relationship", "association between"))) {
    return("correlation")
  }

  if (contains_claim_pattern(normalized, c("standard deviation", "variance"))) {
    if (contains_claim_pattern(normalized, c("two variances", "ratio of variances"))) {
      return("two_variance")
    }

    return("one_variance")
  }

  if (contains_claim_pattern(normalized, c("proportion", "percentage", "percent", "rate"))) {
    if (contains_claim_pattern(normalized, c("p1", "p2", "two proportions", "difference in proportions", "two population proportions"))) {
      return("two_proportion")
    }

    return("one_proportion")
  }

  if (contains_paired_design_cues(problem_text)) {
    return("paired_mean")
  }

  if (contains_claim_pattern(normalized, c("mean", "average"))) {
    if (contains_claim_pattern(normalized, c("two samples", "sample 1", "sample 2", "population 1", "population 2", "group 1", "group 2", "independent samples"))) {
      return("two_mean")
    }

    return("one_mean")
  }

  if (contains_claim_pattern(normalized, c(
    "\\bdifference in\\b",
    "\\bdifference between\\b",
    "population 1",
    "population 2",
    "model a",
    "model b",
    "\\bdistance\\b",
    "\\bprice\\b",
    "\\bweight\\b",
    "\\bheight\\b",
    "\\bscore\\b",
    "\\btime\\b",
    "\\bcost\\b",
    "\\bamount\\b",
    "\\blifetime\\b",
    "\\bhours?\\b",
    "\\bminutes?\\b",
    "\\bfeet\\b",
    "\\bdollars?\\b",
    "\\bkwh\\b"
  ))) {
    if (contains_claim_pattern(normalized, c("population 1", "population 2", "two samples", "two models", "model a", "model b"))) {
      return("two_mean")
    }

    return("one_mean")
  }

  "unknown"
}

infer_parameter_symbol_from_test_family <- function(test_family, problem_text = NULL) {
  switch(
    test_family,
    paired_mean = "mu_d",
    one_mean = "mu",
    two_mean = "mu_1 - mu_2",
    one_proportion = "p",
    two_proportion = "p_1 - p_2",
    correlation = "rho",
    one_variance = {
      if (!is.null(problem_text) && grepl("standard deviation", normalize_claim_text(problem_text), perl = TRUE)) {
        "sigma"
      } else {
        "sigma^2"
      }
    },
    two_variance = "sigma_1^2 / sigma_2^2",
    "theta"
  )
}

extract_numeric_target_from_claim <- function(claim_text) {
  cleaned <- gsub(",", "", clean_claim_text(sanitize_problem_text(claim_text)))
  patterns <- c(
    "(?:at least|no less than|not less than|minimum of|at most|no more than|not more than|maximum of|greater than|more than|less than|fewer than|below|under|above|higher than|lower than|equal to|equals|exactly|different from|changed from|is|are)\\s+(-?[0-9]+(?:\\.[0-9]+)?)\\s*(%|percent|percentage)?",
    "(-?[0-9]+(?:\\.[0-9]+)?)\\s*(%|percent|percentage)"
  )

  for (pattern in patterns) {
    match <- regmatches(
      cleaned,
      regexec(pattern, cleaned, ignore.case = TRUE, perl = TRUE)
    )[[1]]

    if (length(match) >= 2) {
      value <- as.numeric(match[2])

      if (length(match) >= 3 && nzchar(match[3])) {
        value <- value / 100
      }

      return(value)
    }
  }

  NULL
}

default_null_value_for_test_family <- function(test_family) {
  switch(
    test_family,
    paired_mean = 0,
    two_mean = 0,
    two_proportion = 0,
    correlation = 0,
    NULL
  )
}

infer_problem_relation_details <- function(claim_text, difference_definition, parameter_symbol, test_family) {
  normalized <- normalize_claim_text(claim_text)
  numeric_target <- extract_numeric_target_from_claim(claim_text)

  null_left_patterns <- c("at least", "no less than", "not less than", "minimum of")
  null_right_patterns <- c("at most", "no more than", "not more than", "maximum of")
  alt_right_patterns <- c("greater than", "more than", "above", "higher than", "larger than", "exceeds")
  alt_left_patterns <- c("less than", "fewer than", "below", "under", "lower than", "smaller than")
  alt_two_patterns <- c("different from", "not equal to", "changed from", "changed between", "\\bchanged\\b", "\\bdifference\\b", "\\bdiffer\\b", "\\beffect\\b", "\\baffect")
  null_two_patterns <- c("equal to", "\\bequals\\b", "same as", "exactly")

  if (contains_claim_pattern(normalized, null_left_patterns)) {
    return(list(
      tail = "left",
      claim_location = "null",
      semantic_label = "minimum wording",
      reason = "Wording like 'at least' belongs to H0, so the alternative is left-tailed.",
      null_value = numeric_target
    ))
  }

  if (contains_claim_pattern(normalized, null_right_patterns)) {
    return(list(
      tail = "right",
      claim_location = "null",
      semantic_label = "maximum wording",
      reason = "Wording like 'at most' belongs to H0, so the alternative is right-tailed.",
      null_value = numeric_target
    ))
  }

  if (contains_claim_pattern(normalized, alt_right_patterns)) {
    return(list(
      tail = "right",
      claim_location = "alternative",
      semantic_label = "greater-than wording",
      reason = "Wording like 'greater than' makes the claim the right-tailed alternative.",
      null_value = numeric_target
    ))
  }

  if (contains_claim_pattern(normalized, alt_left_patterns)) {
    return(list(
      tail = "left",
      claim_location = "alternative",
      semantic_label = "less-than wording",
      reason = "Wording like 'less than' makes the claim the left-tailed alternative.",
      null_value = numeric_target
    ))
  }

  if (contains_claim_pattern(normalized, alt_two_patterns)) {
    return(list(
      tail = "two",
      claim_location = "alternative",
      semantic_label = "difference wording",
      reason = "Wording like 'changed' or 'different' makes the claim two-tailed.",
      null_value = if (!is.null(numeric_target)) numeric_target else default_null_value_for_test_family(test_family)
    ))
  }

  if (contains_claim_pattern(normalized, null_two_patterns)) {
    return(list(
      tail = "two",
      claim_location = "null",
      semantic_label = "equality wording",
      reason = "Equality wording places the claim in H0, so the alternative is two-tailed.",
      null_value = numeric_target
    ))
  }

  fallback <- infer_claim_direction_details(claim_text, difference_definition, parameter_symbol)

  list(
    tail = fallback$tail,
    claim_location = fallback$claim_location,
    semantic_label = fallback$semantic_label,
    reason = fallback$reason,
    null_value = if (!is.null(numeric_target)) numeric_target else default_null_value_for_test_family(test_family)
  )
}

format_hypothesis_value <- function(value) {
  if (is.null(value)) {
    return("value_from_claim")
  }

  if (is.numeric(value)) {
    return(format_number(value))
  }

  as.character(value)
}

tail_relation_word <- function(tail) {
  tail <- ensure_tail(tail)

  switch(
    tail,
    left = "Less than",
    right = "Greater than",
    two = "Not equal"
  )
}

format_two_tailed_critical_values <- function(critical_value, digits = RAW_DISPLAY_DIGITS) {
  if (length(critical_value) == 1) {
    critical_abs <- abs(as.numeric(critical_value)[1])
    return(paste0("(-", format_number(critical_abs, digits), ", ", format_number(critical_abs, digits), ")"))
  }

  if (!is.null(names(critical_value)) && all(c("lower", "upper") %in% names(critical_value))) {
    return(format_interval(critical_value["lower"], critical_value["upper"], digits))
  }

  if (length(critical_value) >= 2) {
    return(format_interval(critical_value[1], critical_value[2], digits))
  }

  "unavailable"
}

infer_problem_hypotheses_details <- function(problem_text) {
  problem_text <- trimws(problem_text)
  claim_text <- extract_problem_claim_text(problem_text)
  explicit_difference_definition <- extract_problem_difference_definition(problem_text)
  test_family <- detect_problem_test_family(problem_text, explicit_difference_definition)
  difference_definition_info <- infer_problem_difference_definition(
    problem_text,
    test_family = test_family,
    explicit_definition = explicit_difference_definition
  )
  difference_definition_raw <- difference_definition_info$raw
  parameter_symbol <- infer_parameter_symbol_from_test_family(test_family, problem_text)
  relation <- infer_problem_relation_details(
    claim_text,
    if (is.null(difference_definition_raw)) "second_minus_first" else difference_definition_raw,
    parameter_symbol,
    test_family
  )
  alpha <- extract_problem_alpha(problem_text)

  null_value <- relation$null_value
  if (is.null(null_value)) {
    null_value <- default_null_value_for_test_family(test_family)
  }

  value_text <- format_hypothesis_value(null_value)
  alternative_symbol <- switch(
    relation$tail,
    left = "<",
    right = ">",
    two = "!="
  )

  list(
    problem_text = problem_text,
    claim_text = claim_text,
    alpha = alpha,
    difference_definition_raw = difference_definition_raw,
    difference_definition = difference_definition_info$readable,
    difference_definition_source = difference_definition_info$source,
    test_family = test_family,
    parameter_symbol = parameter_symbol,
    tail = relation$tail,
    claim_location = relation$claim_location,
    semantic_label = relation$semantic_label,
    reason = relation$reason,
    null_value = null_value,
    null_value_text = value_text,
    h0 = paste0("H0: ", parameter_symbol, " = ", value_text),
    ha = paste0("Ha: ", parameter_symbol, " ", alternative_symbol, " ", value_text)
  )
}

problem_hypotheses_worked <- function(problem_text) {
  details <- infer_problem_hypotheses_details(problem_text)

  steps <- c(
    paste0("Claim extracted from problem = ", details$claim_text),
    paste0("Detected test family = ", details$test_family),
    paste0("Detected parameter symbol = ", details$parameter_symbol),
    if (!is.null(details$difference_definition)) {
      paste0(
        "Difference definition = ",
        details$difference_definition,
        " (",
        details$difference_definition_source,
        ")"
      )
    } else {
      "Difference definition = not explicitly stated in the prompt"
    },
    if (!is.null(details$alpha)) {
      paste0("Alpha = ", format_number(details$alpha))
    } else {
      "Alpha = not extracted from the prompt"
    },
    paste0("Detected wording type = ", details$semantic_label),
    paste0("Reasoning = ", details$reason),
    details$h0,
    details$ha
  )

  new_worked_calculation(
    title = "Problem Text to Hypotheses",
    notation = details$parameter_symbol,
    formula = "Infer the test family, parameter, and tail from the full prompt text.",
    steps = steps,
    answer = paste0(details$h0, "; ", details$ha),
    result = details
  )
}

problem_step_answers_worked <- function(problem_text, statistic = NULL, conclusion = NULL) {
  details <- infer_problem_hypotheses_details(problem_text)

  step_1_answer <- tail_relation_word(details$tail)
  answer_parts <- c(
    paste0("Step 1 answer = ", step_1_answer),
    paste0("H0 = ", details$h0),
    paste0("Ha = ", details$ha)
  )

  steps <- c(
    paste0("Claim extracted from problem = ", details$claim_text),
    paste0("Detected test family = ", details$test_family),
    paste0("Detected parameter symbol = ", details$parameter_symbol),
    paste0("Step 1 blank answer = ", step_1_answer),
    details$h0,
    details$ha
  )

  if (!is.null(statistic)) {
    answer_parts <- c(answer_parts, paste0("Step 2 test statistic = ", format_number(statistic)))
    steps <- c(steps, paste0("Step 2 test statistic = ", format_number(statistic)))
  }

  if (!is.null(conclusion)) {
    answer_parts <- c(answer_parts, paste0("Step 3 conclusion = ", conclusion))
    steps <- c(steps, paste0("Step 3 conclusion = ", conclusion))
  }

  new_worked_calculation(
    title = "Problem Step Answers",
    notation = details$parameter_symbol,
    formula = "Convert the parsed hypotheses and test output into HAWKS step answers.",
    steps = steps,
    answer = paste(answer_parts, collapse = "; "),
    result = list(
      hypotheses = details,
      step_1_answer = step_1_answer,
      statistic = statistic,
      conclusion = conclusion
    )
  )
}

paired_t_test_from_problem_worked <- function(before, after, problem_text, digits = RAW_DISPLAY_DIGITS) {
  details <- infer_problem_hypotheses_details(problem_text)

  if (details$test_family != "paired_mean") {
    stop("paired_t_test_from_problem only supports prompts that parse as paired-mean tests.", call. = FALSE)
  }

  if (is.null(details$alpha)) {
    stop("Could not extract alpha from the problem text. Include a phrase like 'at the 0.05 level of significance'.", call. = FALSE)
  }

  d <- paired_differences(before, after)
  n <- length(d)
  d_bar <- mean(d)
  s_d <- sd(d)
  t_value <- (d_bar - 0) / (s_d / sqrt(n))
  df <- n - 1
  p_value <- tail_p_value(t_value, details$tail, distribution = "t", df = df)
  critical_value <- tail_critical_value(details$alpha, details$tail, distribution = "t", df = df)
  decision <- if (p_value < details$alpha) "Reject H0" else "Fail to reject H0"
  conclusion <- build_hypothesis_conclusion_sentence(details$alpha, details$claim_text, decision, details$claim_location)

  steps <- c(
    paste0("Claim extracted from problem = ", details$claim_text),
    if (!is.null(details$difference_definition)) {
      paste0(
        "Difference definition = ",
        details$difference_definition,
        " (",
        details$difference_definition_source,
        ")"
      )
    } else {
      "Difference definition = not needed for this prompt"
    },
    paste0("Detected parameter symbol = ", details$parameter_symbol),
    paste0("Detected alternative = ", details$ha),
    paste0("Alpha = ", format_number(details$alpha)),
    paste0("Differences (after - before) = {", join_values(d, digits), "}"),
    paste0("d_bar = ", format_number(d_bar, digits)),
    paste0("s_d = ", format_number(s_d, digits)),
    paste0("df = ", df),
    paste0("t = (d_bar - 0) / (s_d / sqrt(n)) = ", format_number(t_value, digits)),
    if (details$tail == "two") {
      paste0("Critical values = ", format_two_tailed_critical_values(critical_value, digits))
    } else {
      paste0("Critical value = ", format_number(critical_value, digits))
    },
    paste0("P-value = ", format_number(p_value, digits)),
    paste0("Decision: ", decision),
    paste0("Conclusion sentence = ", conclusion)
  )

  new_worked_calculation(
    title = "Paired t Test From Full Problem Text",
    notation = "t",
    formula = "Parse the prompt for Ha and alpha, then use t = d_bar / (s_d / sqrt(n)).",
    steps = steps,
    answer = paste0(
      details$h0,
      "; ",
      details$ha,
      "; t = ",
      format_number(t_value, digits),
      "; ",
      conclusion
    ),
    result = list(
      hypotheses = details,
      statistic = t_value,
      p_value = p_value,
      critical_value = critical_value,
      decision = decision,
      conclusion = conclusion,
      df = df
    )
  )
}

paired_problem_step_answers_worked <- function(before, after, problem_text, digits = RAW_DISPLAY_DIGITS) {
  full_test <- paired_t_test_from_problem_worked(before, after, problem_text, digits)
  details <- full_test$result$hypotheses
  statistic <- full_test$result$statistic
  conclusion <- full_test$result$conclusion
  step_1_answer <- tail_relation_word(details$tail)

  steps <- c(
    paste0("Step 1 relation from Ha = ", step_1_answer),
    paste0("H0 = ", details$h0),
    paste0("Ha = ", details$ha),
    paste0("Step 2 raw t statistic = ", format_number(statistic, digits)),
    paste0("Step 3 conclusion = ", conclusion)
  )

  new_worked_calculation(
    title = "Paired Problem Step Answers",
    notation = "t",
    formula = "Use the full prompt to infer Ha, then compute the paired t statistic and conclusion.",
    steps = steps,
    answer = paste0(
      "Step 1 answer = ",
      step_1_answer,
      "; Step 2 test statistic = ",
      format_number(statistic, digits),
      "; Step 3 conclusion = ",
      conclusion
    ),
    result = list(
      step_1_answer = step_1_answer,
      h0 = details$h0,
      ha = details$ha,
      statistic = statistic,
      conclusion = conclusion
    )
  )
}
