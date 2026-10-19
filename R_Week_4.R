############################################################
# RDS Block 4 - Defensive R
# My RStudio examples and revision notes
############################################################

# HOW I'M USING THIS FILE
# Each section follows the order of the slides.
# I can highlight a few lines and press Cmd + Enter to run them.
# Lines starting with # are comments, so R ignores them.
# Some functions are redefined later to show different approaches.
# It's best to run one section at a time rather than everything at once.
# I've commented out calls that produce intentional errors or extra output.
# IMPORTANT: The quit() example can close my R session.
# Git commands go in the Terminal, not the R Console.


# ---- SLIDES 6-7: Fail Fast - starting example ------------------
# This function checks whether numbers are divisible by 3.
# It returns TRUE if the remainder is 0, or FALSE otherwise.
# %% finds the remainder, while == checks whether two values are equal.
# 'puntil' tells the function how many values I want to check.
# Problem: asking for more values than arr contains leaves extra FALSEs.

myarray_mod3 <- function(arr, puntil) {
  v <- vector(mode = "logical", length = puntil)
  for (i in 1:length(arr)) {
    if (i > puntil) break
    v[i] <- (arr[i] %% 3) == 0
  }
  return(v)
}

test_arr <- c(3, 4, 5, 6, 2, 3, 9, 11, 12, 10, 12, 12)
test_arr_output <- myarray_mod3(test_arr, 10)
test_arr_output2 <- myarray_mod3(test_arr, 20)

# I can type these in the Console to check what happened:
# test_arr_output
# test_arr_output2
# length(test_arr)
# Asking for 20 values gives me a length-20 logical vector,
# even though arr only has 12 values. The extra ones are FALSE.


# ---- SLIDE 8: Checking function arguments ----------------------
# stopifnot() checks that every condition is TRUE.
# If even one condition is FALSE, R stops with an error.
# getOption("warn") checks my current warning setting.
# This is a general example of how to check function inputs.

f <- function(a, b, c) {
  if (getOption("warn") > 0) {
    stopifnot(
      is.numeric(a),      # Check that a is numeric
      is.vector(a),       # Check that a is a vector
      length(a) == 1,     # Check that a contains one value
      is.finite(a),       # Check that a is finite (not Inf or NA)
      a > 0,              # Check that a is positive
      is.character(b)     # Check that b contains character data
      # I could add more conditions here if needed.
    )
  }
  # The actual calculations would go here.
}

# These checks only run when the warning setting is above 0.
# options(warn = 1)  # Turn the warning setting up
# options(warn = 0)  # Put it back to its usual value


# ---- SLIDE 9: Adding checks to myarray_mod3 --------------------
# I now check the input arguments BEFORE doing any calculations.
# This avoids getting an output that looks valid when my input is wrong.
# The checks below make sure puntil is numeric, arr is a vector,
# and there are enough values in arr for the requested output.

myarray_mod3 <- function(arr, puntil) {
  if (getOption("warn") > 0) {
    stopifnot(
      is.numeric(puntil),
      is.vector(arr),
      length(arr) >= puntil
    )
  }
  
  v <- vector(mode = "logical", length = puntil)
  for (i in 1:length(arr)) {
    if (i > puntil) break
    v[i] <- (arr[i] %% 3) == 0
  }
  return(v)
}

# To test what happens when I ask for too many values:
# options(warn = 1)
# myarray_mod3(test_arr, 20)  # Error: length(arr) >= puntil is FALSE
# options(warn = 0)
# The correct function to change this setting is options(), not setOption().


# ---- SLIDE 10: Adding a non-negative input check ---------------
# Now I also check that puntil is at least 0.
# If one of the checks fails, stopifnot() stops the function early.

myarray_mod3 <- function(arr, puntil) {
  if (getOption("warn") > 0) {
    stopifnot(
      is.numeric(puntil),
      is.vector(arr),
      puntil >= 0,
      puntil <= length(arr)
    )
  }
  
  v <- vector(mode = "logical", length = puntil)
  for (i in 1:length(arr)) {
    if (i > puntil) break
    v[i] <- (arr[i] %% 3) == 0
  }
  return(v)
}

# I can try each of these separately:
# options(warn = 1)
# myarray_mod3(test_arr, 10)  # Valid input
# myarray_mod3(test_arr, 20)  # Too many values requested
# myarray_mod3(test_arr, -1)  # Negative input
# options(warn = 0)
# Other useful checks would be whether puntil is one whole number,
# and whether the values in arr are numeric.


# ---- SLIDE 11: Fail Conspicuously -------------------------------
# The idea here is to make problems obvious rather than hiding them.
# If count is 0 or negative, I print an informative message.
# I also print how many lines were written to the file.
# open = "a+" means new output is appended to the text file.
# This example opens the file before checking count, so returning
# early can leave the file connection open.

my_lines <- function(count) {
  fileConn <- file("outputfile.txt", open = "a+")
  mylines <- vector(mode = "character", length = count)
  
  if (count <= 0) {
    print("Count must be >0")
    return()
  }
  
  i <- 1
  while (i < count) {
    mylines[i] <- "all work and no play..."
    i <- i + 1
  }
  
  writeLines(mylines, fileConn)
  close(fileConn)
  print(paste0("Written ", length(mylines), "lines"))
}

# These calls can create or change outputfile.txt in my working folder:
# my_lines(6)
# my_lines(0)
# The while loop uses i < count rather than i <= count,
# so it doesn't fill the final position with the repeated text.


# ---- SLIDE 12: Limiting the amount of output --------------------
# I can set a maximum number of lines so a large input doesn't
# result in an enormous amount of output.
# break() exits the loop once I reach that limit.
# Note: the vector is still created with the original count,
# so there can be empty positions after the loop stops.

my_lines <- function(count) {
  max_print_lines <- 10  # Maximum number of lines to print
  fileConn <- file("outputfile.txt", open = "a+")
  mylines <- vector(mode = "character", length = count)
  
  if (count <= 0) {
    print("Count must be >0")
    return()
  }
  
  i <- 1
  while (i < count) {
    mylines[i] <- "all work and no play..."
    if (i >= max_print_lines) {
      mylines[i] <- paste0("Reached Max Lines:", max_print_lines)
      break()
    }
    i <- i + 1
  }
  
  writeLines(mylines, fileConn)
  close(fileConn)
  print(paste0("Written ", length(mylines), "lines"))
}

# my_lines(11)


# ---- SLIDE 13: Fail Appropriately -------------------------------
# An error response should match how serious the problem is.
# Here, quit() closes R if count <= 0, which is far too drastic.
# IMPORTANT: my_lines(0) with this version could close my session!

my_lines <- function(count) {
  fileConn <- file("outputfile.txt", open = "a+")
  mylines <- vector(mode = "character", length = count)
  print(paste0("Written ", length(mylines), "lines"))
}

# my_lines(6)
# DO NOT RUN: my_lines(0)


# ---- SLIDE 14: Fail Creatively ---------------------------------
# Instead of stopping completely, this version tries to return
# a usable result even if the requested length doesn't match.
# The downside is that the output length can differ from puntil.

myarray_mod3 <- function(arr, puntil) {
  v <- vector(mode = "logical", length = puntil)
  for (i in 1:length(arr)) {
    if (i > puntil) {
      return(v[1:i - 1])
    }
    v[i] <- (arr[i] %% 3) == 0
  }
  return(v[1:length(arr)])
}

# In R, 1:i - 1 means (1:i) - 1, which includes an index of 0.
# Index 0 is ignored. To make the intended range clearer, I could
# write 1:(i - 1), although this needs care when i is 1.
test_arr_output <- myarray_mod3(test_arr, 10)
test_arr_output2 <- myarray_mod3(test_arr, 20)
# length(test_arr_output)
# length(test_arr_output2)


# ---- SLIDES 15-16: Handling log() values in plots ---------------
# log(0) gives -Inf, while log(negative numbers) gives NaN.
# R can leave non-finite values off a plot instead of stopping.
# Negative inputs to log() also produce a warning.

plotme <- c(0, 6, 7, 8, 80, 100)
# plot(plotme)       # Plot the original values
# plot(log(plotme))  # The -Inf value is left off the plot

plotme <- c(-1, 0, 6, 7, 8, 80, 100)
# plot(log(plotme))  # Warning: NaNs produced


# ---- SLIDE 19: Documenting my code ------------------------------
# Comments help me remember what my functions are supposed to do.
# I should describe the purpose of the code, not just repeat each line.

#################################################
#       Simple functions for basic maths        #
#                My RDS notes                   #
#################################################

# Multiply a numeric value by 2.
multiply_by_two <- function(x) {
  return(x * 2)  # Work out x * 2 before returning the result
}

# multiply_by_two(4)


# ---- SLIDE 22: Checking code style with lintr -------------------
# lintr checks my script for style issues and points out lines
# that could be easier to read or format more consistently.
# I need to use the path to my own script when running lint().
# install.packages("lintr")  # Only needed if not already installed
# library(lintr)
# lint("~/RDS_T2_UsingRForAnalysis.R")


# ---- SLIDE 24: Choosing sensible function names ----------------
# A function name should describe what it actually does.
# This first function has a misleading name: it multiplies by 4,
# even though the name says it multiplies by 2.

multiply_by_two <- function(x) {
  return(x * 4)  # The name doesn't match the calculation
}

# It's also better to be consistent about function naming.
# Using a dot in a name can look like a method in other languages.
multiply.by_two <- function(x) {
  return(x * 4)
}

# multiply_by_two(3)  # Returns 12, not 6, after this change


# ---- SLIDES 25-46: Git and GitHub notes -------------------------
# These are TERMINAL commands, not R commands.
# I'm leaving them commented out so the .R script still runs safely.
#
# Slide 27: Open a Terminal from RStudio's Tools menu.
# Slide 29: I can make a Git project using New Project > Version Control > Git.
# Slide 33: commit = save a version locally; push = upload to GitHub;
#           pull = bring remote updates into my local copy.
# Slides 34-38: Save, stage the file, commit the change, then push it.
#
# Slide 42: View recent changes to HEAD:
# git reflog
#
# Slide 43: Switch to an earlier commit (this is an example ID):
# git checkout d6b4624
#
# Slide 44: Check what HEAD points to now:
# git reflog
#
# Slide 44: Two ways I could undo changes (use a real commit ID):
# git reset <commit>    # Moves the branch; I need to be careful with this
# git revert <commit>   # Makes a new commit reversing an older one
#
# Slide 45: Stage, commit and push my changes:
# git add -A
# git commit -m "yet another edit"
# git push -u origin main
#
# Slide 46: Protect my accounts, passwords, access tokens and SSH keys.


# ---- SLIDES 47-49: Things to revisit ---------------------------
# Slide 47: Links to extra tutorials about defensive R programming.
# Slide 48: Some of these topics go beyond what I need right now.
# Slide 49: Set up GitHub or check access to the university GitLab.

# END OF NOTES
