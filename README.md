data-embed
==========
Library and executable for packing files into Haskell executables and accessing
said files.

Usage
-----
In order to access files embedded in an executable, you need to use the
functions from the `Data.Embed` module.

    import Data.Embed
    main = print (embeddeddFile' "my_file.txt")

Packing files into Haskell source is not currently supported, so you will need
to compile your program. You will also want to create `my_file.txt` so we
have something to play with.

    $ ghc --make my_prog.hs
    $ echo -n "Hello!" > my_file.txt

Now use the included `embedtool` program to pack `my_file.txt` into
`my_prog`.

    $ embedtool -w my_prog my_file.txt

Finally, run your program.

    $ ./my_prog
    "Hello!"

You can also recursively embed entire directories into your executable.
The `--replace` switch indicates that we want to overwrite any previous data
bundle attached to the executable we're writing to.

    $ mkdir my_data_dir
    $ mv my_file.txt my_data_dir/
    $ embedtool -w --replace my_prog my_data_dir

However, if we now try to run the program, we get an error.

    $ ./my_prog
    my_prog: no such embedded file: `my_file.txt'

To figure out why, we list all files embedded in `my_prog`.

    $ embedtool -l my_prog
    my_data_dir/my_file.txt

Aha! As far as the application is concerned, the file is no longer called
`my_file.txt`, but `my_data_dir/my_file.txt`! While this is useful if we want
to embed multiple files with the same name but in different directories, it
can also be annoying, like in this case. Fortunately, we can control how many
leading directories are included in file names using the `-p` option to
`embedtool`, once again fixing our example.

    $ embedtool -w --replace -p1 my_prog my_data_dir
    $ embedtool -l my_prog
    my_data_dir/my_file.txt
    $ ./my_prog
    "Hello!"

This will strip one leading directory from the names of all added files.
If a file name would "disappear" entirely due to directory stripping, that file
is not embedded in the executable:

    $ embedtool -w --replace -p2 my_prog my_data_dir

The above command would strip two leading directories from all included file
names, but `my_data_dir/my_file.txt` has only one, and so would not be embedded:

    $ embedtool -l my_prog
    $ ./my_prog
    my_prog: no such embedded file: `my_file.txt'

For more usage instructions, try `embedtool --help`. A Haskell API to the
functionality of `embedtool` is also included in the `Data.Embed.File`
module.
