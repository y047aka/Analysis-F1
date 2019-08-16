module Driver exposing (carNumberToDriverName)


carNumberToDriverName : String -> String
carNumberToDriverName carNumber =
    case carNumber of
        "3" ->
            "Daniel Ricciardo"

        "4" ->
            "Lando Norris"

        "5" ->
            "Sebastian Vettel"

        "7" ->
            "Kimi Räikkönen"

        "8" ->
            "Romain Grosjean"

        "10" ->
            "Pierre Gasly"

        "11" ->
            "Sergio Pérez"

        "16" ->
            "Charles Leclerc"

        "18" ->
            "Lance Stroll"

        "20" ->
            "Kevin Magnussen"

        "23" ->
            "Alexander Albon"

        "26" ->
            "Daniil Kvyat"

        "27" ->
            "Nico Hülkenberg"

        "33" ->
            "Max Verstappen"

        "44" ->
            "Lewis Hamilton"

        "55" ->
            "Carlos Sainz"

        "63" ->
            "George Russell"

        "77" ->
            "Valtteri Bottas"

        "88" ->
            "Robert Kubica"

        "99" ->
            "Antonio Giovinazzi"

        _ ->
            ""
