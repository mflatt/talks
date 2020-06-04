#lang racket/gui

(message-box/custom "Example GUI"
                    "Would you like to continue?"
                    "Continue"
                    "Stop"
                    #f
                    #f
                    '(default=1))
