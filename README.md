## What's in this library?

Stateful transducers and streaming-preserving group operations for the folds in
Gabriel Gonzalez's [foldl](http://hackage.haskell.org/package/foldl) package.

## When to use this library?

- When you want to wrap a stateful decoder over a **Fold**. An example is
decoding UTF-8: the decoder must be stateful because a multi-byte character may
have been split across two blocks of bytes.

- When you want to tweak the stream of data that arrives into a **Fold**, but
only at certain positions. Stripping whitespace at the beginning of a text
stream, for example.

- When you want to perform group operations without breaking "streaminess",
similar to what [pipes-group](http://hackage.haskell.org/package/pipes-group)
does.

## Why use this library for grouping instead of **pipes-group**?

Grouping fold-side instead of producer-side has the advantage that, since the
results are still **Fold**s, you can combine them using **Applicative**.

Also, **Fold**s can work with sources other than **Producer**s from pipes.

Grouping fold-side has limitations as well:

- You can't perform bracketing operations like "withFile" that span the folding
of an entire group. You can do that in pipes-group.

- You have more flexibility in pipes-group to decide how to fold a group based
on previous results.
