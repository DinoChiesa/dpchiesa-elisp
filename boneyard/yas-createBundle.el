

(yas/compile-bundle "yasnippet.el"
                      "yasnippet-bundle.el"
                      '("c:/users/dino/elisp/snippets")
                      "(yas/initialize-bundle)
                        ### autoload
                        (require 'yasnippet-bundle)`"
                      "dropdown-list.el")

(require 'yasnippet-bundle)
(setq yas/snippet-dirs "c:/users/dino/elisp/snippets")

(yas/reload-all)
