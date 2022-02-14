1.3.1.0 2022-02-13
==================
- add parseOptsPure, twidth

1.3.0.1 2021-10-12
==================
- upgrade dependencies, including monadio-plus to 2.0.0.0

1.3.0.0 2021-08-26
==================
- make parseOpts_ pure in args (that is, take a non-IO args)

1.2.1.0 2021-08-23
==================
- add parseOpts_ (takes args as argument)

1.2.0.0 2021-08-15
==================
- remove sepByNE (to parser-plus)
- add readMCommaSet, readNT

1.1.1.0 2021-08-03
==================
- add parseNE, sepByNE, parsecReadM

1.1.0.0 2021-08-01
==================
- replace parseOpts with parseOpts', which is now deprecated

1.0.3.0 2021-07-31
==================
- add parseOpts', which hopefully has better `hsubparser` handling

1.0.2.0 2020-09-17
==================
- better help handling
- add facilities for working with Options.Applicative.Help.Pretty.Doc

1.0.1.0 2020-01-06
==================
- argS -> argT, add optT

1.0.0.0 2019-01-02
==================
- factored out from minfo
