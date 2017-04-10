# mastodon.el
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

Emacs client for [Mastodon](https://github.com/tootsuite/mastodon)

## Installation

Clone this repository and add the lisp directory to your load path. Then, require it and go.

```elisp
(add-to-list 'load-path "/path/to/mastodon.el/lisp")
(require 'mastodon)
```

I'll make mastdon.el available on MELPA when I feel like it's reached a stable state.

## Usage

### Instance

Set `mastodon-instance-url` in your `.emacs` or `customize`. Defaults to the [flagship](https://mastodon.social).

```elisp
(setq mastodon-instance-url "https://my.instance.url")
```
### App registration

`M-x mastodon-register`

Retrieves `client_id` and `client_secret` tokens. They're stored in the `mastdon-token-file`.
This value can be customized too, and defaults to `${EMACS_HOME_DIRECTORY}/mastodon.plstore`.

### Toot toot

`M-x mastodon-toot`

Pops a new buffer/window with a `mastodon-toot` minor mode. Enter the contents of your toot here. `C-c C-c` sends the toot. 
`C-c C-k` cancels. Both actions kill the buffer and window.

If you have not previously authenticated, you will be prompted for your account email and password. **NOTE**: Email and 
password are NOT stored by mastodon.el. 

Authentication stores your access token in the `mastodon--api-token-string` variable. It is not stored on your filesystem, so 
you will have to re-authenticate when you close/reopen Emacs.

## Contributing 

PRs, issues, and feature requests are very welcome! 

### Features

1. Create an [issue](https://github.com/jdenen/mastodon.el/issues) detailing the feature you'd like to add.
2. I'll give you a thumbs up and assign you the issue.
3. Fork the repository and create a branch.
4. Create a pull request referencing the issue created in step 1.

### Fixes

1. In an [issue](https://github.com/jdenen/mastodon.el/issues), let me know that you're working to fix it.
2. I'll assign you the issue.
3. Fork the repository and create a branch.
4. Create a pull request referencing the issue from step 1.

## Connect

If you want to get in touch with me, give me a [toot](https://mastodon.social/@johnson) or leave an [issue](https://github.com/jdenen/mastodon.el/issues).
