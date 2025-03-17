# Copyright (c) 2025 Miguel Lechón
# Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.
# THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# YT
YT <- local({
  # Yours Truelib - Tool to manage repeat code across R codebases - v0.5.0
  # https://dull.systems/yours_truelib

  # Version history:
  # 0.5.0  (2025-03-17) First public release
  # --------------------------------------------------------------------------

  # Statements preceded by gratuitous semicolons end the script early if some condition is not met.
  # That is their only side effect, so you can read them as comments.

  # YT
  IVH <- local({
    # .Internal vhash128 - Dependency-free R hash function. Outputs 128-bit hashes of raw(n) data - v0.2.0
    # https://dull.systems/Internal_vhash128

    # Version history:
    # 0.2.0  (2025-03-16) First public release
    # --------------------------------------------------------------------------

    assert <- function(expr, m) if(!isTRUE(expr)) stop(m)
    ; assert(getRversion() >= '3.0.0', 'This tool requires R >= 3.0.0') # because of bitwShiftR

    vhash <- function(x) 0
    if('vhash' %in% base::builtins(internal = TRUE)){
      # Back in November 2021 Luke Tierney contributed the `vhash` internal R function to 
      # `R/src/main/unique.c` along with this comment:
      # > hash tables (temporary support for R-level experimenting and debugging)
      # We rely on the portion of that function that hashes raw(n) input.
      # At the time of writing, it remains available and unchanged across 4.1.3 <= R <= 4.4.3.
      vhash <- function(v) return(.Internal(vhash(v, 31)))
    }

    vhash_works_as_expected <- function(vhash_f){
      return(vhash_f(charToRaw('')) == 236295992 && vhash_f(charToRaw('dolorem ipsum')) == 238522098)
    }

    if(!vhash_works_as_expected(vhash)){
      vhash <- function(s){
        # Fallback for R < 4.1.3 or in case `vhash` is ever dropped or altered.
        # It's comically slow for anything other than small inputs.
        to_unsigned <- function(x){
          res <- x %% 0x100000000
          out_of_integer_range_mask <- (res >= 2^31)
          res[out_of_integer_range_mask] <- res[out_of_integer_range_mask] - 2^32
          return(as.integer(res))
        }

        scatter_bytes <- function(x){
          res <- to_unsigned(3141592653 * as.integer(x))
          res <- bitwShiftR(res, 1)
          return(res)
        }

        product_modulo <- function(a, b, mod){
          res <- 0
          a <- a %% mod
          while(b > 0){
            if(b %% 2) res <- (res + a) %% mod
            a <- (2*a) %% mod
            b <- b %/% 2
          }
          return(res)
        }

        scatter_int <- function(x){
          res <- to_unsigned(product_modulo(x, 3141592653, 2^32))
          res <- bitwShiftR(res, 1)
          return(res)
        }

        res <- 48 + length(s)*100
        ss <- scatter_bytes(s)
        for(e in ss) res <- bitwXor(to_unsigned(res), e) * 97
        res <- scatter_int(res)
        return(res)
      }
    }
    ; assert(vhash_works_as_expected(vhash), 'The `vhash` base hash function does not behave as expected')

    vhash128 <- function(v){
      # Extension of `vhash` that produces 128-bit hashes.
      # It passes https://github.com/aappleby/smhasher, except for the "seed" tests, because this function lacks a seed.
      
      ; assert(is.raw(v), '`vhash128` can only hash objects of type raw(n)')
      h0 <- vhash(v)                                              # hash the whole `v`
      length(v) <- max(length(v), 16)                             # pad `v` if shorter than 16 bytes
      lengths <- length(v) %/% 4  + (seq(0, 3) < length(v) %% 4)  # divide `v` into four roughly equal portions
    
      # Generate 31 bits of hash for each of the four independent blocks.
      # `h0` is xor'ed as a seed to make all blocks dependent on all data.
      result <- raw(16)
      prev_hash <- writeBin(h0, con = raw(), endian = 'little')
      from <- 1
      for(i in seq(0, 3)){
        v[from:(from+3)] <- xor(v[from:(from+3)], prev_hash)      # xor prev_hash into first 4 bytes of current block
        n <- lengths[i+1]
        
        target_bytes <- (4*i+1):(4*(i+1))
        result[target_bytes] <- writeBin(vhash(v[from:(from+n-1)]), con = raw(), endian = 'little') # hash one block
        
        from <- from + n
        prev_hash <- xor(prev_hash, result[target_bytes])         # combine latest hash into prev
      }
      
      # Repurpose bits 27--30 of `h0` as 32nd bit of each of the four blocks of `result` to get the full 128-bit hash
      for(i in seq_len(4)) if(bitwAnd(h0, bitwShiftL(1, 31-i)) > 0) result[[4*i]] <- result[[4*i]] | as.raw(c(0x80))
      
      return(result)
    }

    vhash128_test <- function(){
      ; assert(paste(vhash128(charToRaw('')), collapse = '') == '3156ba7b88b82c73f0da542a6f1c75ff', 
               '`vhash128` does not behave as expected')
      ; assert(paste(vhash128(charToRaw('ABCDEFGHIJKLMNOPQRSTUVWXYZ')), collapse = '') == 
                 '8034c2519633ff56b0f7ca94b2587c90', '`vhash128` does not behave as expected')
    }

    vhash128_benchmark <- function(){
      vhash128_test()
      inputs <- list(raw(1e0), raw(1e1), raw(1e2), raw(1e3), raw(1e4), raw(1e5))
      res <- numeric(0)
      for(input in inputs){
        t0 <- Sys.time()
        vhash128(input)
        t1 <- Sys.time()
        res <- c(res, t1-t0)
      }
      return(res)
    }

    return(list(hash = vhash128, test = vhash128_test, benchmark = vhash128_benchmark))
  })

  vhash128 <- IVH$hash

  assert <- function(expr, m, cleanup_expr = NULL){
    if(!isTRUE(expr)){
      if(!is.null(cleanup_expr)) eval(expr = substitute(cleanup_expr), envir = parent.frame())
      stop(m)
    }
  }

  starts_with <- function(x, prefix) return(substring(x, 1L, nchar(prefix)) == prefix)
  dir_exists <- function(dirname) isTRUE(file.info(dirname)$isdir)
  file_size <- function(filename) file.info(filename)$size

  read_file_set <- function(paths){
    # Provides a consistent view of a set of files by checking that they don't change while we read them.
    # Gets their mtimes and sizes; reads their contents; asserts that mtimes and sizes have not changed;
    # returns contents.
    res <- character(0)
    file_info <- file.info(paths)
    no_size <- paths[!is.finite(file_info$size)]
    ; assert(length(no_size) == 0, sprintf('Could not get file size for: `%s`.', paste(no_size, collapse = ', ')))
    for(path in paths) res[[path]] <- readChar(path, nchars = file_info[path, 'size'], useBytes = TRUE)
    file_info_after <- file.info(paths)
    altered <- paths[rowSums(file_info[c('size', 'mtime')] != file_info_after[c('size', 'mtime')]) != 0]
    ; assert(length(altered) == 0, sprintf('Files changed while reading them: %s', paste(altered, collapse = ', ')))
    return(res)
  }

  write_atomically <- function(contents, path){
    # Writes `contents` to a temporary file and renames it for atomicity.
    dir <- dirname(path)
    if(!dir_exists(dir)) assert(dir.create(path = dir, recursive = TRUE), paste('Unable to create folder %s', dir))
    tmp_file <- tempfile(tmpdir = dir)
    writeChar(contents, tmp_file, eos = NULL)
    
    assert(file_size(tmp_file) == nchar(contents, type = 'bytes') && file.rename(tmp_file, path),
           sprintf('Unexpected error writing to `%s`.', path), cleanup_expr = file.remove(tmp_file))
  }

  blank_YT_marker <- '# YT#######################################################################'
  YT_marker_char_count <- nchar(blank_YT_marker)

  YT_regexp <- paste0( # Match '# YT' if first non-whitespace characters on any given line
    '(?:^|\n)',        # beginning of file or newline
    '[ \t]*',          # zero or more tabs and spaces
    '(# YT\\S*)'       # YT marker and everything immediately attached to it (captured)
  )

  make_space_for_YT_markers <- function(s){
    # Looks for YT markers inside `s` and ensures space for the full hash after them
    res <- s
    
    regexp_matches <- gregexpr(YT_regexp, s, perl = TRUE)[[1]]
    lengths <- as.vector(attr(regexp_matches, 'capture.length'))
    if(!identical(lengths, -1L)){
      change_mask <- (lengths != YT_marker_char_count)
      lengths <- lengths[change_mask]
      offsets <- as.vector(attr(regexp_matches, 'capture.start'))[change_mask]
      
      if(any(change_mask)){
        res <- substr(s, 1, offsets[[1]]-1)
        
        n <- length(offsets)
        offsets[[n+1]] <- nchar(s)+1
        # New snippets will seldom occur during organic use of YT, so this looped appending is OK
        for(i in seq_len(n)){
          res <- paste0(res, blank_YT_marker, substr(s, offsets[[i]]+lengths[[i]], offsets[[i+1]]-1))
        }
      }
    }
    return(res)
  }

  YT_plan_memory <- function(user_files, store_files, store_path){
    # Compute an "update plan" based on in-memory copy of input files.
    # This function does not read from or write to disk.
    is_lowercase_hex <- function(s) return(!grepl('[^0-9a-f]', s))
    is_decimal <- function(s) return(!grepl('[^0-9]', s))

    hash_algorithm_id <- 'VH'
    root_hash <- paste0(hash_algorithm_id, '00000000000000000000000000000000')

    LEVEL_NOTE <- 1
    LEVEL_WARNING <- 2
    LEVEL_ERROR <- 3
    problem_descs <- c('note', 'warning', 'error')

    process_file_contents <- function(file_contents, is_store_file, parents){
      res <- list(
        file = list(contents = NA_character_, patched_YT_headers = character(0)),
        snippets = data.frame(
          hash = character(), parent_hash = character(), contents = character(), name = character(), 
          do_store = logical(), offset_char_beg = integer(), offset_char_end = integer(), stringsAsFactors = FALSE
        ),
        problems = data.frame(line_number = integer(), message = character(), level = integer(), stringsAsFactors = FALSE)
      )
      append_snippet <- function(hash, parent_hash, contents, name, do_store, offset_char_beg, offset_char_end){
        res$snippets[nrow(res$snippets)+1,] <<- 
          list(hash = hash, parent_hash = parent_hash, contents = contents, name = name, do_store = do_store,
               offset_char_beg = offset_char_beg, offset_char_end = offset_char_end)
      }
      append_problem <- function(offset, message, level){
        line_number <- sum(gregexpr('\n', substr(file_contents, 1, offset), fixed = TRUE)[[1]] > 0) + 1
        res$problems[nrow(res$problems)+1,] <<- list(line_number = line_number, message = message, level = level) 
      }
      append_warning <- function(offset, message) append_problem(offset, message, LEVEL_WARNING)
      append_error <- function(offset, message) append_problem(offset, message, LEVEL_ERROR)

      reverse_data_frame <- function(df) df[rev(seq_len(nrow(df))),]

      regexp_matches <- gregexpr(YT_regexp, file_contents, perl = TRUE)[[1]]
      start <- as.vector(attr(regexp_matches, 'capture.start'))
      YT_marker_char_offsets <- setdiff(start, -1L)
      ; assert(length(YT_marker_char_offsets) == 0 || 
               all(as.vector(attr(regexp_matches, 'capture.length')) == YT_marker_char_count),
               'YT internal error: Unexpected wrong length in YT marker')

      # By construction, stored files can contain only one top-level snippet, so we ignore the (nested) rest
      if(is_store_file && length(YT_marker_char_offsets) > 1) YT_marker_char_offsets = YT_marker_char_offsets[[1]]

      # Patch in reverse order so that nested child snippets are up-to-date when processing their parent
      for(i_segment in rev(seq_along(YT_marker_char_offsets))){
        offset_segment_beg <- YT_marker_char_offsets[[i_segment]]
        offset_segment_end <- nchar(file_contents)+1
        code_segment <- substr(file_contents, offset_segment_beg, offset_segment_end-1)
        
        offset_header_end <- regexpr(pattern = '[\n\r]', code_segment)
        if(offset_header_end == -1){ # no line after header
          append_error(offset_segment_beg, 'Contains no expressions')
          next
        }

        offset_header_end <- offset_header_end + (offset_segment_beg-1) # make offset absolute

        expressions <- try(parse(text = code_segment, n = 1, keep.source = TRUE), silent = TRUE)
        if(inherits(expressions, 'try-error')){
          append_error(offset_segment_beg, 'Malformed expression')
          next
        }
          
        if(length(expressions) < 1){
          append_error(offset_segment_beg, 'Contains no expressions')
          next
        }
       
        segment <- local({
          # It's the first expression and is preceded by a '\n[ \t]*# YT', so there can be no character in its first 
          # line that belongs to a prior expression. We can then expand the 'srcref' range to the beginning on the line.
          char_range <- attr(expressions, 'srcref')[[1]]
          srcref_first_byte_field_index <- 2 # see `?srcfile`; look for 'first_byte'
          char_range[[srcref_first_byte_field_index]] <- 1 # recover possibly lost leading whitespace before expression
          return(as.character(char_range))
        })

        # tighter bound on offset_segment_end
        offset_segment_end <- (offset_segment_beg + YT_marker_char_count +  # header
                               sum(nchar(segment)) +                        # line lengths
                                 length(segment) - 1)                       # newlines between lines
        
        expr <- expressions[[1]]
        snippet_name <- NA_character_
        if(!is_store_file){
          # Strip LHS of assignment and use it as snippet name
          op <- deparse(expr[[1]])

          if(length(expr) != 3 || op != '<-'){
            append_error(offset_segment_beg, 
                         'This tool expects snippets to be assigned using the `<-` assigment operator')
            next
          }
          
          if(any(grepl('<-', deparse(expr[[2]])))){
            append_error(offset_segment_beg, 'This tool does not support the substring "<-" inside snippet names')
            next
          }
          
          snippet_name <- make.names(expr[[2]])
          expr <- expr[[3]]
          # strip leading 'LHS <- ' without disturbing preceding whitespace
          segment[[1]] <- sub('^(\\s*).*<-\\s*(.*)', '\\1\\2', segment[[1]])
        }

        trimws_re <- '^\\s*(.*?)\\s*$' # Drop whitespace around the expression prior to hashing
        hash_input <- paste(sub(trimws_re, '\\1', segment), collapse = '\n')
        new_hash <- paste0(hash_algorithm_id, paste(vhash128(charToRaw(hash_input)), collapse = ''))

        segment <- paste(segment, collapse = '\n') # use canonical newlines

        # sample YT marker '# YT#VH01234567012345670123456701234567#VH01234567012345670123456701234567#'
        #             tens  000000000111111111122222222223333333333444444444455555555556666666666777777
        #            units  123456789012345678901234567890123456789012345678901234567890123456789012345
        header <- substr(code_segment, 1, YT_marker_char_count)
        separators <- (substr(header, 5, 5) == '#' && substr(header, 40, 40) == '#' && substr(header, 75, 75) == '#')
        old_hash <- substr(header, 6, 39)
        old_parent_hash <- substr(header, 41, 74)
        hash_algorithms <- c(substr(old_hash, 1, 2), substr(old_parent_hash, 1, 2))
        known_hash_algorithm <- all(hash_algorithms == hash_algorithm_id)

        new_parent_hash <- old_parent_hash
 
        legal_hashes <- (is_lowercase_hex(substr(old_hash, 3, 34)) && is_lowercase_hex(substr(old_parent_hash, 3, 34)))
        if(!separators || !legal_hashes || !known_hash_algorithm){
          new_parent_hash <- root_hash 
        } else if(new_hash != old_hash){
          new_parent_hash <- old_hash
        }

        if(new_hash %in% names(parents)) new_parent_hash <- parents[[new_hash]]

        new_header <- paste0('# YT#', new_hash, '#', new_parent_hash, '#')

        do_store <- (!is_store_file && !(new_hash %in% names(parents)))

        if(do_store){
          # This tool assumes that it can strip away leading and trailing whitespace on any given line of code without
          # affecting the meaning of a program. That may not be true in the presence of multiline strings. 
          expr_after_ws_manipulation <- parse(text = hash_input, n = 1, keep.source = TRUE)[[1]]
          if(!identical(deparse(expr_after_ws_manipulation), deparse(expr))){
            append_warning(offset_segment_beg, 
                           paste('The meaning of this snippet depends on indentation,',
                                 'possibly due to its use of multi-line strings'))
          }
        }

        append_snippet(hash = new_hash, parent_hash = new_parent_hash, contents = segment, name = snippet_name, 
                       do_store = do_store, offset_char_beg = offset_segment_beg, offset_char_end = offset_segment_end)

        if(!identical(header, new_header)){
          substr(file_contents, offset_segment_beg, offset_segment_beg+74) <- new_header
          res$file$patched_YT_headers <- c(snippet_name, res$file$patched_YT_headers)
        }
      }
      
      # We process them in reverse order, but report snippets and problems as they appear on the file
      res$snippets <- reverse_data_frame(res$snippets)
      res$problems <- reverse_data_frame(res$problems)
      
      res$file$contents <- file_contents
      return(res)
    }

    nodes <- local({
      result <- data.frame(folder = character(), fname = character(), depth = integer(), parent_hash = character(), 
                           stringsAsFactors = FALSE)
      result[root_hash,] <- list(folder = NA_character_, fname = NA_character_, depth = 0, parent_hash = root_hash)
     
      for(fname in sort(names(store_files))){
        folder <- basename(dirname(fname))
        store_files[fname] <- make_space_for_YT_markers(store_files[fname])
        file_contents <- store_files[[fname]]
        info <- process_file_contents(file_contents, is_store_file = TRUE, parents = character(0))
        ; assert(nrow(info$problems) == 0, sprintf('Unsupported stored snippet: line %d: %s', 
                                                   info$problems$line_number[[1]], info$problems$message[[1]]))
        ; assert(nrow(info$snippets) == 1, sprintf('File %s should contain exactly one top-level snippet', fname))
        ; assert(length(info$file$patched_YT_headers) == 0, 
                 sprintf('Stored snippet should not require patching: %s', fname))
        hash <- info$snippets$hash[[1]]
        ; assert(!(hash %in% rownames(result)), sprintf('Repeat library node hash: %s', info$hash))
        parent_hash <- info$snippets$parent_hash[[1]]
      
        parent <- result[parent_hash,]
        ; assert(nrow(parent) == 1, sprintf('Node %s lacks a parent', fname))
        ; assert(is.na(parent[['folder']]) || parent[['folder']] == folder, 
                 sprintf('Node %s has a parent on library %s instead of library %s', fname, parent[['folder']], folder))
       
        depth_hash_R <- strsplit(basename(fname), split='[-_\\.]')[[1]] 
        ; assert(length(depth_hash_R) == 3 && all(nchar(depth_hash_R) == c(4, 34, 1)) &&
                 is_decimal(depth_hash_R[[1]]) && starts_with(depth_hash_R[[2]], 'VH') &&
                 is_lowercase_hex(substr(depth_hash_R[[2]], 3, 34)) && depth_hash_R[[3]] == 'R',
                 sprintf('Unexpected file %s in library %s', fname, folder))
        depth <- as.integer(depth_hash_R[[1]])
        ; assert(parent[['depth']]+1 == depth, sprintf('Node %s expected at depth %d', fname, parent[['depth']]+1))
        result[hash,] <- list(folder = folder, fname = basename(fname), depth = depth, parent_hash = parent_hash)
      }
      return(result)
    })

    notes <- character(0)
    
    nodes[['user_fnames']] <- I(list(character())) # extra column to point to snippet in user paths
    
    file_updates <- data.frame(path = character(), contents = character(), description = character(), 
                               stringsAsFactors = FALSE)
    append_update <- function(path, contents, description){
      file_updates[nrow(file_updates)+1, ] <<- list(path = path, contents = contents, description = description)
    }
    
    parents <- setNames(nodes[['parent_hash']], row.names(nodes))

    user_snippets <- data.frame(fname = character(0), name = character(0), hash = character(0), 
                                stringsAsFactors = FALSE)

    for(fname in sort(names(user_files))){
      user_files[fname] <- make_space_for_YT_markers(user_files[fname])
      info <- process_file_contents(user_files[[fname]], is_store_file = FALSE, parents)

      if(length(info$file$patched_YT_headers)){
        desc <- paste(sprintf('Patch YT header of `%s:%s`.', fname, info$file$patched_YT_headers), collapse = ' ')
        append_update(fname, info$file$contents, desc)
      }
      
      for(i_row in seq_len(nrow(info$snippets))){
        element <- info$snippets[i_row,]
        user_snippets[nrow(user_snippets)+1,] <- list(fname = fname, name = element$name, hash = element$hash)
        
        if(element$hash %in% rownames(nodes)){
          node_user_fnames <- nodes[element$hash, 'user_fnames'][[1]]
          nodes[element$hash, 'user_fnames'][[1]] <- list(c(node_user_fnames, fname))
        }

        if(element$do_store){
          store_file_path <- local({
            node_depth <- nodes[element$parent_hash, 'depth']+1
            store_file_name <- sprintf('%04d-%s.R', node_depth, element$hash)
            dir <- nodes[element[['parent_hash']], 'folder']
            if(is.na(dir)) dir <- element[['name']]
            return(file.path(store_path, dir, store_file_name))
          })
          
          contents <- paste0('# YT#', element$hash, '#', element$parent_hash, '#\n', element$contents)
          append_update(store_file_path, contents, 
                        sprintf('Store `%s:%s` into `%s`.', fname, element[['name']], dirname(store_file_path)))
        }
      }

      for(i_row in seq_len(nrow(info$problems))){
        problem <- info$problems[i_row,]
        notes[length(notes)+1] <- 
          sprintf('[%s] `%s:%d`: %s.', problem_descs[problem$level], fname, problem$line_number, problem$message)
      }
    }

    get_leaf_descendants <- function(nodes, hash){
      res <- character(0)
      child_mask <- (hash == nodes[['parent_hash']])
      child_hashes <- rownames(nodes[child_mask,])
      for(child_hash in child_hashes){
        descendants <- get_leaf_descendants(nodes, child_hash)
        if(length(descendants)) res <- union(res, descendants)
        else res <- union(res, child_hash)
      }
      return(res)
    }
    
    for(i_row in seq_len(nrow(user_snippets))){
      element <- user_snippets[i_row,]
      
      leaf_descendants <- get_leaf_descendants(nodes, element$hash)
      for(leaf_hash in leaf_descendants){
        folder <- nodes[leaf_hash, 'folder']
        leaf_fname <- nodes[leaf_hash, 'fname']
        user_fnames <- nodes[leaf_hash, 'user_fnames'][[1]]
        note <- sprintf('[%s] Found successor to `%s:%s` in `%s`.',
                        problem_descs[LEVEL_NOTE], element$fname, element$name,  file.path(store_path, folder, leaf_fname))
        if(length(user_fnames)){
          note <- paste0(note, ' This snippet is present in ', paste(sprintf('`%s`', user_fnames), collapse = ', '), '.')
        }
        notes <- c(notes, note)
      }
    }
  
    return(list(file_updates = file_updates, notes = notes))
  }

  YT_plan <- function(
    dirs = '.',
    files = list.files(path = dirs, pattern = '*\\.[rR]$', full.names = TRUE, recursive = TRUE),
    store_path = getOption('YT_store_path')
  ){
    # Compute an "update plan" based on input files. This function reads those files from disk.
    ; assert(
      is.character(store_path) && length(store_path) == 1 && nchar(store_path) > 0,
      paste('Provide a store path either through the `store_path` parameter or by setting the `YT_store_path`',
            'global option `options(YT_store_path = "your/store/path")')
    )
    store_file_names <- list.files(path = store_path, full.names = TRUE, recursive = TRUE)
    
    user_file_names <- local({ # exclude `store` files overlapping target files
      map_abs_to_rel_files <- setNames(files, normalizePath(files))
      files <- setdiff(normalizePath(files), normalizePath(store_file_names))
      return(unname(map_abs_to_rel_files[files]))
    })
  
    # Read all files in one go, to get a consistent view of the work that needs to be done
    input_files <- read_file_set(c(user_file_names, store_file_names))
    plan <- YT_plan_memory(input_files[user_file_names], input_files[store_file_names], store_path)
  }

  YT_execute <- function(plan){
    # Execute an "update plan". This function patches YT markers of existing files and 
    # writes complete snippets to new files.
    for(i_update in seq_len(nrow(plan$file_updates))){
      update <- plan$file_updates[i_update,]
      write_atomically(contents = update$contents, path = update$path)
    }
  }

  YT_test <- function(){
    # Unit and integration tests. They don't write to disk and stop if a single problem is found.
    IVH$test() # run test on dependencies

    input <- paste0(
      '# YT\n',
      '# YTcontent_attached_to_marker_with_no_whitespace\n',
      '# YT  whitespace_and_then_content\n'
    )
    expected_output <- paste0(
      '# YT#######################################################################\n',
      '# YT#######################################################################\n',
      '# YT#######################################################################  whitespace_and_then_content\n'
    )

    output <- make_space_for_YT_markers(input)
    ; assert(identical(output, expected_output), 'Unexpected return from `make_space_for_YT_markers`')
    
    get_address <- function(x) { res <- base::tracemem(x); untracemem(x); return(res) }
    ; assert(!isTRUE(capabilities('profmem')) ||
             identical(get_address(make_space_for_YT_markers(output)), get_address(output)),
             '`make_space_for_YT_markers should not generate a new string when changes are not needed')
    
    execute_plan_in_memory <- function(input, plan){
      file_updates <- plan[['file_updates']] 
      for(i_row in seq_len(nrow(file_updates))){
        path <- file_updates[i_row, 'path']
        input[path] <- file_updates[i_row, 'contents']
      }
      return(input)
    }
    
    test_step <- function(description, input, updates = list(), notes = character(0)){
      file_updates <- data.frame(path = character(), contents = character(), description = character(), 
                                 stringsAsFactors = FALSE)
      for(i in seq_along(updates)) file_updates[i,] <- updates[[i]]

      expected_output = list(file_updates = file_updates, notes = notes)
      store_mask <- starts_with(names(input), 'store/')
      plan <- YT_plan_memory(user_files = input[!store_mask], store_files = input[store_mask], store_path = 'store')
      if(!identical(plan, expected_output)) browser()
      ; assert(identical(plan, expected_output), paste('YT Test error:', description))
      return(execute_plan_in_memory(input, plan))
    }

    local({ # TEST: f, YT, drop f from store, YT, tries to store it again
      input <- test_step(
        description = 'store f',
        input = c('user_dir/f.R' = '# YT\nf <- local({TRUE})'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nf <- local({TRUE})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0001-VH886452e291a30bf77310194ceb20bda8.R',
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nlocal({TRUE})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )
      input <- input[1] # drop f from store
      input <- test_step(
        description =  'YT with correctly hashed f missing from store',
        input = input,
        updates = list(
          c('store/f/0001-VH886452e291a30bf77310194ceb20bda8.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nlocal({TRUE})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )
    })

    local({ # TEST
      input <- test_step(
        description = 'Warning messages for unsupported inputs',
        input = c('user_dir/f.R' = list(
           paste(c('# YT',
                   'true = 2+)',
                   '# YT',
                   'true = TRUE',
                   '# YT',
                   '`true<-` <- TRUE',
                   '# YT'
                   ), collapse = '\n')
           )
        ),
        notes = c('[error] `user_dir/f.R:1`: Malformed expression.',
                  '[error] `user_dir/f.R:3`: This tool expects snippets to be assigned using the `<-` assigment operator.',
                  '[error] `user_dir/f.R:5`: This tool does not support the substring "<-" inside snippet names.',
                  '[error] `user_dir/f.R:7`: Contains no expressions.')
      )
    })

    local({ # TEST: Two snippets in one file -- returned order should be file order
      input <- test_step(
        description = 'Two snippets in one file',
        input = c('user_dir/f.R' = '# YT\ntrue <- TRUE\n# YT\nfalse <- FALSE'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH421f35a0e542cb141f91a2af743458f1#VH00000000000000000000000000000000#\ntrue <- TRUE\n# YT#VHe395f692b33746bf5f52d3b29a0d195e#VH00000000000000000000000000000000#\nfalse <- FALSE',
            'Patch YT header of `user_dir/f.R:true`. Patch YT header of `user_dir/f.R:false`.'),
          c('store/true/0001-VH421f35a0e542cb141f91a2af743458f1.R',
            '# YT#VH421f35a0e542cb141f91a2af743458f1#VH00000000000000000000000000000000#\nTRUE',
            'Store `user_dir/f.R:true` into `store/true`.'),
          c('store/false/0001-VHe395f692b33746bf5f52d3b29a0d195e.R', 
            '# YT#VHe395f692b33746bf5f52d3b29a0d195e#VH00000000000000000000000000000000#\nFALSE',
            'Store `user_dir/f.R:false` into `store/false`.')
        )
      )
    })

    local({ # TEST: indented snippets preserve indentation when stored
      input <- test_step(
        description = 'Indented YT marker',
        input = c('user_dir/f.R' = '  # YT\n  f <- local({TRUE})'),
        updates = list(
          c('user_dir/f.R', 
            '  # YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\n  f <- local({TRUE})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0001-VH886452e291a30bf77310194ceb20bda8.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\n  local({TRUE})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )
    })
   
    local({ # TEST: f1, YT, f1->f2, YT, f2->f1, YT. ancestor(f1) == root
      input <- test_step(
        description = 'f1: vanilla update of a newly created snippet',
        input = c('user_dir/f.R' = '# YT\nf <- local({TRUE})'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nf <- local({TRUE})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0001-VH886452e291a30bf77310194ceb20bda8.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nlocal({TRUE})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )

      input['user_dir/f.R'] <- sub('TRUE', 'FALSE', input['user_dir/f.R'])
      input <- test_step(
        description = 'f1->f2: update on a previously stored snippet',
        input = input,
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH6d7f824e50408aa7ae59a571950107b3#VH886452e291a30bf77310194ceb20bda8#\nf <- local({FALSE})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0002-VH6d7f824e50408aa7ae59a571950107b3.R', 
            '# YT#VH6d7f824e50408aa7ae59a571950107b3#VH886452e291a30bf77310194ceb20bda8#\nlocal({FALSE})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )
      
      f2 <- input['user_dir/f.R'] # saved for later

      input['user_dir/f.R'] <- sub('FALSE', 'TRUE', input['user_dir/f.R'])
      input <- test_step(
        description = 'f2->f1: restore contents of snippet',
        input = input,
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nf <- local({TRUE})',
            'Patch YT header of `user_dir/f.R:f`.')
        ),
        notes = '[note] Found successor to `user_dir/f.R:f` in `store/f/0002-VH6d7f824e50408aa7ae59a571950107b3.R`.'
      )
     
      input['user_dir/g.R'] <- f2
      input <- test_step(
        description = 'reintroduce latest version of the snippet in new user file',
        input = input,
        updates = list(),
        notes = paste('[note] Found successor to `user_dir/f.R:f` in', 
                      '`store/f/0002-VH6d7f824e50408aa7ae59a571950107b3.R`.', 
                      'This snippet is present in `user_dir/g.R`.')
      )
    })
    
    local({ # TEST: UTF8
      input <- test_step(
        description = 'multibyte character encodings both prior to and inside of code snippet',
        input = c('user_dir/f.R' = '# hï‽\n# YT\nf <- local({"υτ"})'),
        updates = list(
          c('user_dir/f.R', 
            '# hï‽\n# YT#VHe1f5ef46a69b570a09fe4d09b2579b5a#VH00000000000000000000000000000000#\nf <- local({"υτ"})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0001-VHe1f5ef46a69b570a09fe4d09b2579b5a.R', 
            '# YT#VHe1f5ef46a69b570a09fe4d09b2579b5a#VH00000000000000000000000000000000#\nlocal({"υτ"})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )
    })

    local({ # TEST: Insensitivity to leading and trailing whitespace inside the snippet
      input <- test_step(
        description = 'Multiline snippet',
        input = c('user_dir/f.R' = '# YT\nf <- local({\nTRUE\n})'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH414d8561cefe11bc3dff483b6296b7d8#VH00000000000000000000000000000000#\nf <- local({\nTRUE\n})',
            'Patch YT header of `user_dir/f.R:f`.'),
          c('store/f/0001-VH414d8561cefe11bc3dff483b6296b7d8.R', 
            '# YT#VH414d8561cefe11bc3dff483b6296b7d8#VH00000000000000000000000000000000#\nlocal({\nTRUE\n})',
            'Store `user_dir/f.R:f` into `store/f`.')
        )
      )

      input['user_dir/f.R'] <- sub('TRUE', '  TRUE', input['user_dir/f.R'])
      input <- test_step(description = 'Leading and trailing spaces do not affect snippet hash', input = input)
    })

    local({ # TEST: Tricky snippet names
      input <- test_step(
        description = 'LHS contains spaces',
        input = c('user_dir/f.R' = '# YT\n"f f" <- local({TRUE})'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\n"f f" <- local({TRUE})',
            'Patch YT header of `user_dir/f.R:f.f`.'),
          c('store/f.f/0001-VH886452e291a30bf77310194ceb20bda8.R', 
            '# YT#VH886452e291a30bf77310194ceb20bda8#VH00000000000000000000000000000000#\nlocal({TRUE})',
            'Store `user_dir/f.R:f.f` into `store/f.f`.')
        )
      )
    })

    local({ # TEST: Nested snippets
      input <- test_step(
        description = 'Nested snippets',
        input = c('user_dir/f.R' = '# YT\nf <- local({\n  # YT\n  t <- TRUE\n})'),
        updates = list(
          c('user_dir/f.R', 
            '# YT#VH10d17b9121b0e6421bbc6f79c02f2f74#VH00000000000000000000000000000000#\nf <- local({\n  # YT#VH421f35a0e542cb141f91a2af743458f1#VH00000000000000000000000000000000#\n  t <- TRUE\n})',
            'Patch YT header of `user_dir/f.R:f`. Patch YT header of `user_dir/f.R:t`.'),
          c('store/f/0001-VH10d17b9121b0e6421bbc6f79c02f2f74.R', 
            '# YT#VH10d17b9121b0e6421bbc6f79c02f2f74#VH00000000000000000000000000000000#\nlocal({\n  # YT#VH421f35a0e542cb141f91a2af743458f1#VH00000000000000000000000000000000#\n  t <- TRUE\n})',
            'Store `user_dir/f.R:f` into `store/f`.'),
          c('store/t/0001-VH421f35a0e542cb141f91a2af743458f1.R', 
            '# YT#VH421f35a0e542cb141f91a2af743458f1#VH00000000000000000000000000000000#\n  TRUE',
            'Store `user_dir/f.R:t` into `store/t`.')
        )
      )

      input <- test_step(description = 'Nested snippets in the code store', input = input)
    })

    local({ # TEST: Multiline strings and whitespace
      input <- test_step(
        description = 'Multiline strings and whitespace',
        input = c('user_dir/s.R' = '# YT\ns <- " a \n b\n c "'),
        updates = list(
          c('user_dir/s.R', 
            '# YT#VHe813f1f9215a24a335ff4b2f20eca4fa#VH00000000000000000000000000000000#\ns <- " a \n b\n c "',
            'Patch YT header of `user_dir/s.R:s`.'),
          c('store/s/0001-VHe813f1f9215a24a335ff4b2f20eca4fa.R', 
            '# YT#VHe813f1f9215a24a335ff4b2f20eca4fa#VH00000000000000000000000000000000#\n" a \n b\n c "',
            'Store `user_dir/s.R:s` into `store/s`.')
        ),
        notes = paste('[warning] `user_dir/s.R:1`: The meaning of this snippet depends on indentation,',
                      'possibly due to its use of multi-line strings.')
      )

      input <- test_step(description = 'No warnings for multiline strings if present in code store', input = input)
    })
  }

  return(list(plan = YT_plan, execute = YT_execute, test = YT_test))
})

#### Example customization of YT - modify as needed ####

original_jit_level <- compiler::enableJIT(0) # byte-compiling this script takes longer than interpreting it

YT$test() # checks that YT behaves as expected

plan <- YT$plan( # looks for newly modified snippets and out-of-date YT markers
  dirs = c('mock_packages/A', 'mock_packages/B'), # origin folders [character(n)]; use the `files` argument for more control
  store_path = 'repeat_code_store'      # destination folder [character(1)]
)

if(length(plan[['notes']])) cat(paste(c('Please note:', plan[['notes']], ''), collapse = '\n'))

if(nrow(plan[['file_updates']]) > 0){
  update_descs = sprintf('%d: %s', seq_len(nrow(plan$file_updates)), plan$file_updates[['description']])
  cat(paste(c('Actions needed to address discrepancies in YT libraries:', update_descs, ''), collapse = '\n'))
  prompt <- 'Proceed? [y/n] '
  answer <- if(interactive()) readline(prompt = prompt) else { cat(prompt); readLines('stdin', n = 1) }
  if(identical(toupper(answer), 'Y')){
    YT$execute(plan)            # patches YT markers of origin files; copies new snippets into store folder
    cat('Done\n')
  }
  else cat('No action taken\n')
} else {
  cat('Everything up to date. Nothing to do.\n')
}

invisible(compiler::enableJIT(original_jit_level))
