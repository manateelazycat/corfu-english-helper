# What's this ?
This is english helper extension that base on Emacs corfu.

<img src="./screenshot.png" width="400">

# Install
1. Copy file [corfu-english-helper.el](corfu-english-helper.el) and [corfu-english-helper-data.el](corfu-english-helper-data.el) to directory ~/elisp/

2. And set in your ~/.emacs like this:
```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
```

3. And the following to your ~/.emacs startup file.
```Elisp
(require 'corfu-english-helper)
```

4. Execute command `toggle-corfu-english-helper' to write english on the fly!

# Customize your dict.
Default english dictionary is generate from stardict KDict dictionary with below command

```Shell
python ./stardict.py stardict-kdic-ec-11w-2.4.2/kdic-ec-11w.ifo
```

You can replace with your favorite stardict dictionary's info filepath to generate your own corfu-english-helper-data.el .

# Acknowledgements
I create [company-english-helper](https://github.com/manateelazycat/company-english-helper), this package is port to corfu-mode, most code of corfu version is written by [theFool32](https://github.com/theFool32).
