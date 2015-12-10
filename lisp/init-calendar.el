(require 'calendar)
(setq mark-diary-entries-in-calendar t diary-entry-marker "D"
      mark-holidays-in-calendar t calendar-today-marker "T")
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(provide 'init-calendar)
