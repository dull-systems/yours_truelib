# Yours Truelib [![status](http://assets.dull.systems:8080/status?id=yours_truelib/README.md)](https://dull.systems/yours_truelib "")

Tool to manage repeat code snippets across R codebases.

# Problem
- You have two R packages: A and B. 
- While working on package B, you encounter a problem you already solved in A. 
- You copy the relevant code from A to B. 
- From now on, every time you tweak one instance of the duplicated code, you have to decide whether to 
tweak the other instance (and adapt the surrounding code to it) or let both copies diverge. 
- Some days you don't have time to deal with this added friction. You accumulate post-it notes reminding you of pending
postponed decisions.

# Solution
Use `YT.R` to keep track of repeat code. Reconcile the differences when it is convenient for you to do so.

To do this, take the piece of code you are about to duplicate (usually a `function` or a `local` block) and prefix
it with a single-line `# YT` comment. Then run `YT.R` to "register" that piece of code and you're free to copy
it, along with its `# YT` marker, somewhere else. You can also modify it. Any time you want to know the status of 
the different copies of your shared snippets of code, run `YT.R` again and follow its instructions.

`YT.R` completes each `# YT` marker it encounters with a hash of the expression that follows it. It also stores the expression in a
central snippet folder. This way, it remembers all the code snippets it sees and knows which ones descend from which.

<details><summary><big><b>Guided step-by-step example</b></big> <i><small>[show/hide]</small></i></summary>

Download the contents of this repository and step into the newly created folder:
```
> git clone https://github.com/dull-systems/yours_truelib.git
Cloning into 'yours_truelib'...
> cd yours_truelib
```

Open `YT.R` in your editor of choice and scroll almost to the end. You will find the following call:
```R
plan <- YT$plan( # looks for newly modified snippets and out-of-date YT markers
  dirs = c('mock_packages/A', 'mock_packages/B'), # origin folders [character(n)]; use the `files` argument for more control
  store_path = 'repeat_code_store'      # destination folder [character(1)]
)
```
The `YT$plan(...)` call is meant to track the contents of the two sample folders `mock_packages/A` and `mock_packages/B` included
in this repository.

---

If you run or `source` `YT.R`, you will get this uninteresting answer:
```
> Rscript YT.R 
Everything up to date. Nothing to do.
```
That is because there is indeed nothing to do about the folders we are tracking. But that is about to change.

---

Inside `mock_packages/A` there is a single file that defines and then calls `my_shared_function(...)`. Edit it and add these
four characters: `# YT` on a separate line preceding the function definition. The top of the file should now look resemble this:
```R
# YT
my_shared_function <- function(x, y){
   ...
```
Run `YT.R` again and answer "y" when prompted:
```
> Rscript YT.R 
Actions needed to address discrepancies in YT libraries:
1: Patch YT header of `mock_packages/A/sample_code_A.R:my_shared_function`.
2: Store `mock_packages/A/sample_code_A.R:my_shared_function` into `repeat_code_store/my_shared_function`.
Proceed? [y/n] y
Done
```
You have instructed `YT.R` to track this snippet and its future copies.

If you look again at the contents of `mock_packages/A/sample_code_A.R`, you will see that the `# YT` tag has been expanded
to include two hashes: one for the code of this function and another one for the predecessor of this piece of code. This
second hash is all zeros because this is the first time that `YT.R` sees this function, so it has no predecessor.
```R
# YT#VH83b24fba60848c5c894e944bb3fc4bab#VH00000000000000000000000000000000#
my_shared_function <- function(x, y){
  ...
```
Additionally, a new `repeat_code_store` folder has been created and holds a copy of the tagged function:
```
└── repeat_code_store
    └── my_shared_function
        └── 0001-VH83b24fba60848c5c894e944bb3fc4bab.R
```
Notice that the name of the folder matches the name of the function we're tracking. It is taken from the left-hand side of the assignment statement immediately after the `# YT` marker. 

---

File `mock_packages/B/sample_code_B.R` contains an exact copy of `my_shared_function(...)`. Tag it like you just did 
with the one under `mock_packages/A/` to get:
```R
stopifnot(2+2 == 4)

# YT
my_shared_function <- function(x, y){
  ...
```
Now `YT.R` to detects the copy and offers to patch that marker:
```
> Rscript YT.R 
Actions needed to address discrepancies in YT libraries:
1: Patch YT header of `mock_packages/B/sample_code_B.R:my_shared_function`.
Proceed? [y/n] y
Done
```
The tag on `sample_code_B.R` has been modified. You are now set up to track changes to the two instances of
`my_shared_function`. 

---

We will make `mock_packages/B/sample_code_B.R:my_shared_function` diverge from the original code. Edit it and extend the `'and'` string:
```R
  ...
  return(paste(description_of_x, 'and furthermore', description_of_y))
  ...
```
Now run `YT.R` once more:
```
> Rscript YT.R 
Actions needed to address discrepancies in YT libraries:
1: Patch YT header of `mock_packages/B/sample_code_B.R:my_shared_function`.
2: Store `mock_packages/B/sample_code_B.R:my_shared_function` into `repeat_code_store/my_shared_function`.
Proceed? [y/n] y
Done
```
The `YT` tag in `sample_code_B` now reads:
```
# YT#VH9019600df0a530b3fa6e40b1b187e2bf#VH83b24fba60848c5c894e944bb3fc4bab#
```
This is a concatenation of the hash of the new contents of the function and the hash of the old contents.

The `repeat_code_store` folder now keeps track of the two known versions of that function:
```
└── repeat_code_store
    └── my_shared_function
        ├── 0001-VH83b24fba60848c5c894e944bb3fc4bab.R
        └── 0002-VH9019600df0a530b3fa6e40b1b187e2bf.R
```

You have now two slightly different copies of the same code snippet. Since they are tagged, `YT.R` will tell you about
them when you run it:
```
> Rscript YT.R 
Please note:
[note] Found successor to `mock_packages/A/sample_code_A.R:my_shared_function` in `repeat_code_store/my_shared_function/0002-VH9019600df0a530b3fa6e40b1b187e2bf.R`. This snippet is present in `mock_packages/B/sample_code_B.R`.
Everything up to date. Nothing to do.
```
Both "packages" will keep working as expected with their slightly different pieces of code. You are free to address this situation when you find an opportunity. Just run `YT.R` and pick up the task were you left it.
</details>

<details><summary><big><b>Customizing YT.R </b></big><i><small>[show/hide]</small></i></summary>

The core of `YT.R` is confined inside the first `local` section of the
file. That local section returns a list of three functions (`plan`, `execute` and `test`) that can be though of as a lightweight library. The rest of that script is **a suggestion** on how to string calls to those functions into a simple command-line tool. At the time of writing this 
guide, the code looks like this:
```R
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
```
The `YT$plan(...)` function looks for discrepancies between headers and contents of `YT`-tagged snippets, returns a structure 
describing the `plan$file_updates` that would address them. It also returns a vector of 
`plan$notes` warning about the use of possibly outdated versions of shared snippets.

Some users may prefer a different workflow (e.g. a non-interactive script that operates unconditionally when 
double-clicked; a git hook invoked from every package sharing code; a CI/CD script that checks that all `# YT` marker
hashes are up to date, ...). That is why the functionality of the library is divided across `YT$plan` and `YT$execute`.

First-time users may be satisfied by simply tweaking the sample default parameterization of `YT$plan` above.

Here's the signature of that function:
```R
YT_plan <- function(
  dirs = '.',
  files = list.files(path = dirs, pattern = '*\\.[rR]$', full.names = TRUE, recursive = TRUE),
  store_path = getOption('YT_store_path')
)
```
The meaning of the three parameters is fairly straightforward:
- `dirs`: Folders to check for `YT` snippets. Unused if the value of `files` does not depend on it.
- `files`: Paths to files possibly containing `YT` snippets.
- `store_path`: Path of folder for storage of copies of YT snippets.

By default, this function will look for all `*.r` and `*.R` files under the corrent folder and try to store any shared snippets under the folder indicated by the `YT_store_path` option.
</details>
 
# Alternatives

<details><summary><big><b>Shared utility packages </b></big><i><small>[show/hide]</small></i></summary>

You can place your shared code inside a separate utility package and make your "top-level" packages depend on it[^1]. 

This approach has the drawback that, if you make any of those "top-level" packages available to the public, you will also have to publish the shared
utility package. This means that your shared internal functions now carry the same documentation and versioning costs as those other functions you intended to publish in the first place. That's because they are now visible and somebody other than you might depend on them.

There's yet another issue with this approach:  Distributing a family of packages that have shared dependencies puts you and your users on the fast track 
to a Diamond Dependency Conflict[^2].
</details>

<details><summary><big><b>Plain shared folders  </b></big><i><small>[show/hide]</small></i></summary>

Instead of resorting to `YT`, why can't you just put your scripts in a subfolder of `R/` and manage them with 
`git subtree`, mercurial subrepositories or some other, more standard, tool?

The sad reality is that subrepository management tends to require the use of subfolders and the R package format is very restrictive when it 
comes to the contents of the `R/` subfolder:
 > The `R/` and `man/` subdirectories may contain OS-specific subdirectories named `unix/` or `windows/`.[^3]

Folders other than those two are ignored. This topic has been raised a few times in the `R-devel` mailing list[^4].

One way of bypassing this restriction is through module management tools, such as `box`[^5] or `modules`[^6].
</details>

# Questions and answers

<details><summary><big><b>Can my team of developers use YT? </b></big><i><small>[show/hide]</small></i></summary>

Yes. Place both `YT.R` and the shared "store" folder under version control and treat that repository like any 
other code repository. 

(Version control is, in fact, redundant. A plain network folder would work just fine. That's
because `YT.R` only adds files and folders to the shared "store" folder and never alters or removes them.)
</details>

<details><summary><big><b>Can I trust YT.R? </b></big><i><small>[show/hide]</small></i></summary>

**NO!** If you followed the instructions of the "Guided step-by-step example" without first studying `YT.R`, you placed your data in jeopardy. 

Luckily for you, conducting a security audit of `YT.R` is a simple task. If you open the file and read the code, you 
will see that:
- It doesn't make any network call.
- Only one of its functions (`read_file_set`) ever reads from disk. It only reads the files that `YT_plan` tells it to read, which
are in turn dictated by explicit arguments to that function.
- Only one of its functions (`write_atomically`) ever writes to disk. It only writes the files that `YT_execute` tells it to write, and only
after explicit confirmation from the user.

</details>

<details><summary><big><b>Can I nest YT snippets? </b></big><i><small>[show/hide]</small></i></summary>

You can.
</details>

<details><summary><big><b>What if I start using YT and I later regret it? </b></big><i><small>[show/hide]</small></i></summary>

This tool is designed to be minimally invasive. It doesn't ask you to restructure your code. It just tweaks the `# YT`
comments that you offer it. As soon as you decide to stop using it, you've already stopped.

Still, if you want to _leave no trace_ of your use of `YT.R`, remove that script, remove the "store" folder and strip away the `# YT` comments. Then, scroll back to the
"Alternatives" section and choose your next adventure!
</details>

[^1]: Here's a non-exhaustive list of shared utility code packages on CRAN:
[admiraldev](https://cran.r-project.org/web/packages/admiraldev/index.html), 
[csutil](https://cran.r-project.org/web/packages/csutil/index.html),
[easy.utils](https://cran.r-project.org/web/packages/easy.utils/index.html),
[FastUtils](https://cran.r-project.org/web/packages/FastUtils/index.html),
[fuj](https://cran.r-project.org/web/packages/fuj/index.html),
[hutils](https://cran.r-project.org/web/packages/hutils/index.html),
[JWileymisc](https://cran.r-project.org/web/packages/JWileymisc/index.html),
[mark](https://cran.r-project.org/web/packages/mark/index.html),
[mosaicCore](https://cran.r-project.org/web/packages/mosaicCore/index.html),
[sfsmisc](https://cran.r-project.org/web/packages/sfsmisc/index.html),
[splutil](https://cran.r-project.org/web/packages/splutil/index.html),
[statnet.common](https://cran.r-project.org/web/packages/statnet.common/index.html),
[toscutil](https://cran.r-project.org/web/packages/toscutil/index.html),
[ttutils](https://cran.r-project.org/web/packages/ttutils/index.html),
[ufs](https://cran.r-project.org/web/packages/ufs/index.html),
[yulab.utils](https://cran.r-project.org/web/packages/yulab.utils/index.html).

[^2]: [Diamond dependency conflict](https://jlbp.dev/what-is-a-diamond-dependency-conflict) 
[*(+archived)*](https://web.archive.org/web/20240521121542/https://jlbp.dev/what-is-a-diamond-dependency-conflict)
This site explains the problem in the context of Java libraries, but it applies to R as well.

[^3]: [Writing R extensions (CRAN manual); Package subdirectories](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories)
[*(+archived)*](https://web.archive.org/web/20240822214024/https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories)

[^4]: See [this](https://stat.ethz.ch/pipermail/r-devel/2009-December/thread.html#56022)
[*(+archived)*](https://web.archive.org/web/20221206221929/https://stat.ethz.ch/pipermail/r-devel/2009-December/thread.html#5602 )
 and [this](https://stat.ethz.ch/pipermail/r-devel/2010-February/thread.html#56513)
[*(+archived)*](https://web.archive.org/web/20221103172632/https://stat.ethz.ch/pipermail/r-devel/2010-February/thread.html#56513)

[^5]: [Box](https://klmr.me/box/): "Write Reusable, Composable and Modular R Code".

[^6]: [Wahani modules](https://wahani.github.io/modules/): "Provides modules as an organizational unit for source code".
