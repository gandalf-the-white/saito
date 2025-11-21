(in-package :saito)

(defun geocode-city (city &key (count 1) (language "en"))
  "Resolve CITY name into (lat lon name country admin1 timezone) using Open-Meteo."
  (let* ((base "https://geocoding-api.open-meteo.com/v1/search")
         (params `(("name"     . ,city)
                   ("count"    . ,(princ-to-string count))
                   ("language" . ,language)
                   ("format"   . "json")))
         (body-str (http-get base :parameters params))
         (data     (parse body-str :as :hash-table))
         (results  (gethash "results" data)))
    (dbg "Geocoding response: ~A" body-str)
    (if (and results (plusp (length results)))
        (let* ((first (cond
                        ((vectorp results) (aref results 0))
                        ((listp results)   (first results))
                        (t                 results)))
               (lat      (gethash "latitude"  first))
               (lon      (gethash "longitude" first))
               (name     (gethash "name"      first))
               (country  (gethash "country"   first))
               (admin1   (gethash "admin1"    first))
               (timezone (gethash "timezone"  first)))
          (values lat lon name country admin1 timezone))
        (values nil nil nil nil nil nil))))

(defun get-weather-from-api (city)
  "Return a short English weather description for CITY using Open-Meteo."
  (multiple-value-bind (lat lon name country admin1 timezone)
      (geocode-city city)
    (unless lat
      (return-from get-weather-from-api
        (format nil "I could not find the city \"~A\" in the Open-Meteo database." city)))
    (let* ((url (format nil
                        "https://api.open-meteo.com/v1/forecast?latitude=~,5F&longitude=~,5F&current_weather=true&timezone=auto"
                        lat lon))
           (body-str (http-get url))
           (data     (parse body-str :as :hash-table))
           (current  (gethash "current_weather" data)))
      (dbg "Forecast response: ~A" body-str)
      (unless current
        (return-from get-weather-from-api
          (format nil "Could not retrieve current weather for ~A." (or name city))))
      (let* ((temp        (or (gethash "temperature"   current) 0.0))
             (windspeed   (or (gethash "windspeed"     current) 0.0))
             (winddir     (or (gethash "winddirection" current) 0.0))
             (weathercode (gethash "weathercode"      current)))
        (format nil
                "Weather in ~A (~@[~A, ~]~A, timezone ~A): ~,1F C, wind ~,1F km/h (direction ~,0F degrees), weather code ~:[unknown~;~:*~A~]."
                (or name city)
                admin1
                country
                timezone
                temp
                windspeed
                winddir
                weathercode)))))

(defun tool-get-weather (args-ht)
  "MCP tool handler for get_weather."
  (let ((city (or (and args-ht (gethash "city" args-ht))
                  (and args-ht (gethash "location" args-ht))
                  "Paris")))
    (get-weather-from-api city)))

(register-mcp-tool
 (make-mcp-tool
  :name "get_weather"
  :title "Weather Information"
  :description "Get the current weather from Open-Meteo for a given city."
  :input-schema
  '(("type" . "object")
    ("properties" .
     (("city" .
       (("type" . "string")
        ("description" . "City name, for example \"Paris\".")))))
    ("required" . ("city")))
  :handler #'tool-get-weather))
