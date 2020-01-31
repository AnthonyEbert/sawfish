# sawfish 0.1.1

Changes to make sawfish simpler to use.

* find_files() now converts to absolute urls by default. This means that users don't need to know about url_convert().
* download.files() now passes optional arguments to download.file(). The reason such a simple function, instead of just using download.file() with lapply(), is that download.file() also requires a vector input for the destfile argument.
* http://www.abc.net.au/ removed since it no longer provides a good example.
