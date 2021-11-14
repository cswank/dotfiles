;;; my-weather --- Summary
;;; setup vars for 'forecast' package
;;; Commentary:
;;; Code:

(require 'forecast)
(setq calendar-latitude 39.720949
      calendar-longitude -104.990847
      calendar-location-name "Denver, USA"
	  forecast-units (quote us)
      forecast-api-key (getenv "FORECAST_API_KEY"))
(provide 'my-weather)
;;; my-weather.el ends here
