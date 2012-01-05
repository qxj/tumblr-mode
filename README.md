## Install

Download `tumblr-mode.el` into your `load-path`, then following code in
to your `~/.emacs`:

    (require 'tumblr-mode)

And specify your tumblr settings:

- `tumblr-email` registered email in tumblr.com
- `tumblr-password` registered password in tumblr.com
- `tumblr-hostname` the group name you want to post

For example:

    (setq tumblr-email xxx@gmail.com
          tumblr-password yyy
          tumblr-hostname zzz.tumblr.com)

**Notice**: Tumblr API does not supply *HTTPS* connection neither v1 nor
v2, so please make sure your network environment is safe, and take care
your password at your own risk.

## Features
### Manage posts

You can list all your regular posts by `M-x tumblr-list-posts` in a
buffer named `*tumblr-mode*`, then you can view and edit them.

### Create posts

Call `M-x tumblr-new-post` and give it a title to create a new post.

You can also specify title, tags, etc. in the header of the post (as
`octopress`). For example:

    --
    title: one owesome article from tumblr-mode
    tags: tumblr-mode, emacs
    state: published
    --

    blah...blah...

After that, run `M-x tumblr-save-post` to submit your post to
tumblr.com.

Thanks and enjoy it.
