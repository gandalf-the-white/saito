(in-package :saito)

;; (defun geocode-city (city &key (count 1) (language "fr"))
;;   "Résout le nom de ville CITY via l’API de geocoding Open-Meteo."
;;   (let* ((base "https://geocoding-api.open-meteo.com/v1/search")
;;          (params `(("name"     . ,city)
;;                    ("count"    . ,(princ-to-string count))
;;                    ("language" . ,language)
;;                    ("format"   . "json")))
;;          (body-str (http-get base :parameters params))
;;          (data     (parse body-str :as :hash-table))
;;          (results  (gethash "results" data)))
;;     (dbg "Open-Meteo geocode body = ~A" body-str)
;;     (if (and results (plusp (length results)))
;;         (let* ((first (cond
;;                         ((vectorp results) (aref results 0))
;;                         ((listp results)   (first results))
;;                         (t                 results)))
;;                (lat      (gethash "latitude"  first))
;;                (lon      (gethash "longitude" first))
;;                (name     (gethash "name"      first))
;;                (country  (gethash "country"   first))
;;                (admin1   (gethash "admin1"    first))
;;                (timezone (gethash "timezone"  first)))
;;           (values lat lon name country admin1 timezone))
;;         (values nil nil nil nil nil nil))))

;; (defun get-weather-from-api (city)
;;   "Retourne une phrase en français décrivant la météo actuelle de CITY
;;    en utilisant Open-Meteo (géocodage + forecast)."
;;   (multiple-value-bind (lat lon name country admin1 timezone)
;;       (geocode-city city)
;;     (unless lat
;;       (return-from get-weather-from-api
;;         (format nil "Je n'ai pas trouvé la ville \"~A\" dans la base Open-Meteo." city)))
;;     (let* ((url (format nil
;;                         "https://api.open-meteo.com/v1/forecast?latitude=~,5F&longitude=~,5F&current_weather=true&timezone=auto"
;;                         lat lon))
;;            (body-str (http-get url))
;;            (data     (parse body-str :as :hash-table))
;;            (current  (gethash "current_weather" data)))
;;       (dbg "Open-Meteo forecast body = ~A" body-str)
;;       (unless current
;;         (return-from get-weather-from-api
;;           (format nil "Impossible de récupérer la météo actuelle pour ~A." (or name city))))
;;       (let* ((temp        (or (gethash "temperature"   current) 0.0))
;;              (windspeed   (or (gethash "windspeed"     current) 0.0))
;;              (winddir     (or (gethash "winddirection" current) 0.0))
;;              (weathercode (gethash "weathercode"      current)))
;;         (format nil
;;                 "Météo pour ~A (~@[~A, ~]~A, fuseau ~A) : ~,1F°C, vent ~,1F km/h (direction ~,0F°), code météo ~:[inconnu~;~:*~A~]."
;;                 (or name city)
;;                 admin1
;;                 country
;;                 timezone
;;                 temp
;;                 windspeed
;;                 winddir
;;                 weathercode)))))

;; ;; Schéma JSON du tool pour Ollama
;; (defparameter *weather-tool-schema*
;;   '(("type" . "function")
;;     ("function" .
;;      (("name" . "get_weather")
;;       ("description" . "Obtenir la météo actuelle d'une ville avec Open-Meteo.")
;;       ("parameters" .
;;        (("type" . "object")
;;         ("required" "city")
;;         ("properties" .
;;          (("city" .
;;                   (("type" . "string")
;;                    ("description" . "Nom de la ville, par exemple \"Paris\".")))))))))))

;; ;; Handler appelé par la couche tools-core
;; (defun make-weather-tool ()
;;   (make-instance 'tool
;;                  :name "get_weather"
;;                  :schema *weather-tool-schema*
;;                  :handler
;;                  (lambda (args-ht)
;;                    (let ((city (gethash "city" args-ht)))
;;                      (let ((result (get-weather-from-api city)))
;;                        `(("role" . "tool")
;;                          ("tool_name" . "get_weather")
;;                          ("content" . ,result)))))))

;; ;; Enregistrement au chargement du fichier
;; (register-tool (make-weather-tool))
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
    (dbg "Open-Meteo geocode body = ~A" body-str)
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
  "Return a short English sentence describing the current weather in CITY using Open-Meteo."
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
      (dbg "Open-Meteo forecast body = ~A" body-str)
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

(defparameter *weather-tool-schema*
  '(("type" . "function")
    ("function" .
     (("name" . "get_weather")
      ("description" . "Get the current weather for a given city using Open-Meteo.")
      ("parameters" .
       (("type" . "object")
        ("required" "city")
        ("properties" .
         (("city" .
                  (("type" . "string")
                   ("description" . "City name, for example \"Paris\".")))))))))))


(defun make-weather-tool ()
  (make-instance 'tool
                 :name "get_weather"
                 :schema *weather-tool-schema*
                 :handler
                 (lambda (args-ht)
                   (let ((city (gethash "city" args-ht)))
                     (let ((result (get-weather-from-api city)))
                       `(("role" . "tool")
                         ("tool_name" . "get_weather")
                         ("content" . ,result)))))))

(register-tool (make-weather-tool))
