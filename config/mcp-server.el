;; -*- lexical-binding: t -*-
;; MCP Server configuration
;; https://github.com/rhblind/emacs-mcp-server

(require 'mcp-server)
(setq mcp-server-security-dangerous-functions nil)
(setq mcp-server-security-allowed-dangerous-functions '(browse-url
                                                        call-process
                                                        copy-file
                                                        delete-directory
                                                        delete-file
                                                        dired
                                                        eval
                                                        find-file
                                                        find-file-literally
                                                        find-file-noselect
                                                        getenv
                                                        insert-file-contents
                                                        kill-emacs
                                                        load
                                                        make-directory
                                                        process-environment
                                                        rename-file
                                                        require
                                                        save-buffers-kill-emacs
                                                        save-buffers-kill-terminal
                                                        save-current-buffer
                                                        server-force-delete
                                                        server-start
                                                        set-buffer
                                                        set-file-modes
                                                        set-file-times
                                                        shell-command
                                                        shell-command-to-string
                                                        shell-environment
                                                        start-process
                                                        switch-to-buffer
                                                        url-retrieve
                                                        url-retrieve-synchronously
                                                        view-file
                                                        with-current-buffer
                                                        write-region))
;; Start manually on-demand
;; (mcp-server-start-unix)
